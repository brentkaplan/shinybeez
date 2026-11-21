# Uploads from locales whose Excel writes `;`-separated CSV with decimal commas
# (pt-BR, most of Europe) were read with a hard-coded "," delimiter. The whole
# file became one column named "id;x;y", so demand said "The first column is not
# `id`", discounting said "Unrecognised discounting data format", and the mapper
# had a single column to work with (0 of 23 discounting sessions got through,
# 2026-09-21..10-20).

box::use(
  testthat[...],
  vroom,
)

box::use(
  app / logic / upload_read,
  app / logic / validate,
)

write_upload <- function(lines, ext = "csv", env = parent.frame()) {
  path <- withr::local_tempfile(fileext = paste0(".", ext), .local_envir = env)
  writeLines(lines, path, useBytes = TRUE)
  path
}

normalise <- function(dat) {
  colnames(dat) <- trimws(tolower(colnames(dat)))
  dat
}

describe("read_upload() delimiter", {
  it("reads a semicolon CSV with decimal commas as separate numeric columns", {
    path <- write_upload(c("id;x;y", "1;1;0,95", "1;7;0,8", "2;1;0,9", "2;7;0,6"))
    dat <- upload_read$read_upload(path, "csv")
    expect_equal(colnames(dat), c("id", "x", "y"))
    expect_equal(dat$y, c(0.95, 0.8, 0.9, 0.6))
    expect_equal(attr(dat, "delim"), ";")
    expect_true(isTRUE(validate$check_data(normalise(dat), type = "discounting")))
  })

  it("reads a semicolon CSV with dot decimals without inventing grouping marks", {
    path <- write_upload(c("id;0.5;1.0", "p1;10.5;8.0", "p2;9.25;7.0"))
    dat <- upload_read$read_upload(path, "csv")
    expect_equal(colnames(dat), c("id", "0.5", "1.0"))
    expect_equal(dat[["0.5"]], c(10.5, 9.25))
  })

  it("reads a pt-BR demand export with R$ decimal-comma price headers", {
    path <- write_upload(c("id;R$0;R$0,50;R$1;R$2", "1;10;8;5;0", "2;9;7,5;4;1"))
    dat <- normalise(upload_read$read_upload(path, "csv"))
    expect_equal(colnames(dat), c("id", "r$0", "r$0.50", "r$1", "r$2"))
    expect_equal(dat[["r$0.50"]], c(8, 7.5))
    expect_true(isTRUE(validate$check_data(dat, type = "demand")))
  })

  it("keeps a comma CSV whose text has semicolons comma-delimited", {
    path <- write_upload(c("id,price;USD;gross;net,y", "p1,1,2", "p2,3,4"))
    dat <- upload_read$read_upload(path, "csv")
    expect_equal(colnames(dat), c("id", "price;USD;gross;net", "y"))
    expect_equal(attr(dat, "delim"), ",")
  })

  it("respects quoted semicolons in a semicolon file", {
    path <- write_upload(c("id;note;y", "1;\"a;b\";0,5", "2;\"c\";0,25"))
    dat <- upload_read$read_upload(path, "csv")
    expect_equal(colnames(dat), c("id", "note", "y"))
    expect_equal(dat$note, c("a;b", "c"))
    expect_equal(dat$y, c(0.5, 0.25))
  })

  it("strips trailing semicolon junk from a comma CSV", {
    path <- write_upload(c("id,r$0,r$0.5,r$1;;;;", "1,10,8,5;;;;", "2,9,7,4;;;;"))
    dat <- normalise(upload_read$read_upload(path, "csv"))
    expect_equal(colnames(dat), c("id", "r$0", "r$0.5", "r$1"))
    expect_equal(dat[["r$1"]], c(5, 4))
    expect_true(isTRUE(validate$check_data(dat, type = "demand")))
  })

  it("reads every bundled template exactly as a plain comma read does", {
    dir <- file.path(find_project_root(), "app/static/data/templates")
    for (f in list.files(dir, pattern = "\\.csv$")) {
      path <- file.path(dir, f)
      expected <- vroom$vroom(path, delim = ",", show_col_types = FALSE)
      got <- upload_read$read_upload(path, "csv")
      expect_identical(attr(got, "decimal_commas")$n, 0L, info = f)
      attr(got, "delim") <- NULL
      attr(got, "decimal_commas") <- NULL
      expect_equal(as.data.frame(got), as.data.frame(expected), info = f)
    }
  })

  it("reads a tsv with tabs", {
    path <- write_upload(c("id\tx\ty", "1\t1\t0.9"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_equal(colnames(dat), c("id", "x", "y"))
    expect_equal(attr(dat, "delim"), "\t")
  })

  it("returns NULL for an unreadable file", {
    expect_null(upload_read$read_upload(tempfile(fileext = ".csv"), "csv"))
  })
})

describe("read_upload() edges", {
  it("accepts an upper-case extension", {
    path <- write_upload(c("id,x,y", "1,1,0.9"), ext = "CSV")
    expect_equal(colnames(upload_read$read_upload(path, "CSV")), c("id", "x", "y"))
  })

  it("strips a byte-order mark before sniffing a semicolon file", {
    path <- write_upload(c("\ufeffid;x;y", "1;1;0,9", "1;7;0,5"))
    dat <- upload_read$read_upload(path, "csv")
    expect_equal(colnames(dat), c("id", "x", "y"))
    expect_equal(attr(dat, "delim"), ";")
  })

  it("reads an empty file as zero rows, not an error", {
    path <- write_upload(character(0))
    dat <- upload_read$read_upload(path, "csv")
    expect_true(is.null(dat) || nrow(dat) == 0)
  })
})

describe("read_upload() review cases", {
  it("keeps a single real trailing semicolon in a comma file", {
    path <- write_upload(c("id,note;", "1,paid;", "2,pending;"))
    dat <- upload_read$read_upload(path, "csv")
    expect_equal(colnames(dat), c("id", "note;"))
    expect_equal(dat[["note;"]], c("paid;", "pending;"))
  })

  it("converts decimal commas cell by cell when a column has a text cell", {
    path <- write_upload(c("id;x;y", "a;0,5;10", "a;bad;8", "a;1,5;6"))
    dat <- upload_read$read_upload(path, "csv")
    expect_equal(dat$x, c("0.5", "bad", "1.5"))
    expect_equal(dat$y, c(10, 8, 6))
  })
})

describe("read_upload() and a wide indifference-point file with a text cell", {
  it("reads the column as text and validation then rejects the cell by name", {
    # 2026-10-27: one stray text cell turned a delay column into character and the pivot
    # that ran before coercion aborted the session. The read is right; the guard is new.
    path <- write_upload(c(
      "id,1,5,10,100", "p1,0.95,0.80,0.60,0.20", "p2,0.90,n/a,0.55,0.15", "p3,0.85,0.70,0.50,0.10"
    ))
    dat <- normalise(upload_read$read_upload(path, "csv"))
    expect_type(dat[["5"]], "character")
    res <- validate$check_data(dat, type = "discounting")
    expect_identical(validate$reason_code(res), "wide_ip_nonnumeric_cells")
    expect_match(res, '"n/a"', fixed = TRUE)
  })
})

# A comma inside a number is a decimal mark in Brazil and most of Europe (7,5 is seven and
# a half) and a thousands separator in the US and Mexico (1,234 is one thousand). The
# reader never guesses from one cell. It reads commas as decimal marks only when the file
# shows that is what they are: some number could not be a thousands value (7,5 or 0,25 or
# 12,50) and nothing in the file uses a decimal point. It then says so. A file whose only
# comma numbers look like 1,234, or one that mixes commas and points, is left as typed for
# validation to reject by name. vroom's default read "7,5" as 75 without a word.
describe("read_upload() and decimal commas in comma and tab files", {
  commas <- function(dat) attr(dat, "decimal_commas")

  it("reads them as decimals in a tab file and reports what it converted", {
    path <- write_upload(c("id\tx\ty", "a\t1\t7,5", "a\t2\t0,25", "a\t4\t3"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(dat$y, c(7.5, 0.25, 3))
    expect_identical(commas(dat)$n, 2L)
    expect_identical(commas(dat)$examples, c("7,5", "0,25"))
  })

  it("reads quoted ones as decimals in a comma file, never as a larger number", {
    dat <- upload_read$read_upload(
      write_upload(c("id,x,y", "a,1,\"7,5\"", "a,2,\"12,50\"", "a,4,3")), "csv"
    )
    expect_identical(dat$y, c(7.5, 12.5, 3))
    expect_identical(commas(dat)$n, 2L)
  })

  it("does the same when the comma file ends each line with semicolon junk", {
    lines <- paste0(c("id,x,y", "a,1,\"7,5\"", "a,2,3"), ";;;")
    dat <- upload_read$read_upload(write_upload(lines), "csv")
    expect_identical(dat$y, c(7.5, 3))
  })

  it("reads 1,234 as 1.234 once another cell has shown commas are decimals", {
    path <- write_upload(c("id\tx\ty", "a\t1\t7,5", "a\t2\t1,234"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(dat$y, c(7.5, 1.234))
  })

  it("takes 0,250 as proof of a decimal comma: no thousands value starts with zero", {
    path <- write_upload(c("id\tx\ty", "a\t1\t0,250", "a\t2\t1,500"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(dat$y, c(0.25, 1.5))
  })

  it("converts negative values and decimal-comma price headers", {
    path <- write_upload(c("id\t0\t0,5\t2,5", "a\t10\t8\t-1,5"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(colnames(dat), c("id", "0", "0.5", "2.5"))
    expect_identical(dat[["2.5"]], -1.5)
    expect_identical(commas(dat)$n, 3L)
  })

  it("converts a cell far down a long file instead of losing it to type guessing", {
    rows <- paste0(seq_len(3000), "\t10\t2")
    rows[2500] <- "2500\t10\t7,5"
    dat <- upload_read$read_upload(write_upload(c("id\tx\ty", rows), ext = "tsv"), "tsv")
    expect_identical(dat$y[2500], 7.5)
    expect_identical(dat$y[2499], 2)
  })

  it("leaves a file alone when every comma number could be a thousands value", {
    path <- write_upload(c("id\tx\ty", "a\t1\t1,234", "a\t2\t2,500"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(dat$y, c("1,234", "2,500"))
    expect_identical(commas(dat)$n, 0L)

    res <- validate$check_data(normalise(dat), type = "demand")
    expect_type(res, "character")
    expect_match(res, "\"1,234\"", fixed = TRUE)
  })

  it("leaves a file alone when it mixes decimal commas and decimal points", {
    path <- write_upload(c("id\tx\ty", "a\t0.5\t7,5", "a\t2\t3"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(dat$y, c("7,5", "3"))
    expect_identical(commas(dat)$n, 0L)

    res <- validate$check_data(normalise(dat), type = "demand")
    expect_match(res, "\"7,5\"", fixed = TRUE)
  })

  # Type guessing turns "1.0" into 1 and so hides the decimal point: the file's own text
  # is what counts.
  it("sees a decimal point that type guessing would hide", {
    for (x in list(c("1.0", "2.0"), c("1.", "2"), c("1.5e-2", "2"))) {
      path <- write_upload(
        c("id\tx\ty", paste0("a\t", x[1], "\t7,5"), paste0("a\t", x[2], "\t3")), ext = "tsv"
      )
      dat <- upload_read$read_upload(path, "tsv")
      expect_identical(dat$y, c("7,5", "3"), info = x[1])
      expect_identical(commas(dat)$n, 0L, info = x[1])
    }
  })

  it("leaves price headers alone when one uses a decimal point and another a comma", {
    path <- write_upload(c("id\tR$0.50\tR$1,50", "a\t10\t8"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(colnames(dat), c("id", "R$0.50", "R$1,50"))
    expect_identical(commas(dat)$n, 0L)
  })

  it("reads a decimal comma written with a plus sign", {
    path <- write_upload(c("id\tx\ty", "a\t1\t+7,5", "a\t2\t3"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(dat$y, c(7.5, 3))
    expect_identical(commas(dat)$n, 1L)
  })

  it("does not touch text that merely contains a comma", {
    dat <- upload_read$read_upload(
      write_upload(c("id,note,y", "a,\"red, blue\",\"7,5\"", "b,\"1,2,3\",2")), "csv"
    )
    expect_identical(dat$note, c("red, blue", "1,2,3"))
    expect_identical(dat$y, c(7.5, 2))
  })

  # Labels are not measurements. An id or a group label that happens to look like a comma
  # number is never rewritten, and says nothing about how the file writes its decimals.
  it("leaves id and group labels exactly as typed while converting measurements", {
    path <- write_upload(
      c("id\tgroup\tx\ty", "01,5\t1,5\t1\t7,5", "1,50\t2,5\t2\t3"), ext = "tsv"
    )
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(dat$id, c("01,5", "1,50"))
    expect_identical(dat$group, c("1,5", "2,5"))
    expect_identical(dat$y, c(7.5, 3))
    expect_identical(commas(dat)$n, 1L)
  })

  it("does not take a label like 1,5 as proof that 1,234 is a decimal", {
    path <- write_upload(
      c("id\tgroup\tx\ty", "a\t1,5\t1\t1,234", "b\t2,5\t2\t2,500"), ext = "tsv"
    )
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(dat$y, c("1,234", "2,500"))
    expect_identical(commas(dat)$n, 0L)
  })

  it("does not let a decimal point in an id veto clear decimal-comma measurements", {
    path <- write_upload(c("subject\tx\ty", "1.5\t1\t7,5", "2.5\t2\t3"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(dat$y, c(7.5, 3))
  })

  # A long file names its measurements: x and y (or y_ll4). Any other column is a label or
  # a covariate the reader knows nothing about, whatever it is called. A `dose` column
  # holding "1,5" used to count as proof of decimal commas and turned y 1,234 into 1.234.
  it("in a long file takes evidence from x and y only, not from a column such as dose", {
    path <- write_upload(c(
      "id,x,y,dose", '1,0,"1,234","1,5"', '1,1,"2,345","1,5"', '2,0,"3,456","2,5"'
    ))
    dat <- upload_read$read_upload(path, "csv")
    expect_identical(dat$y, c("1,234", "2,345", "3,456"))
    expect_identical(dat$dose, c("1,5", "1,5", "2,5"))
    expect_identical(attr(dat, "decimal_commas")$n, 0L)
    expect_match(
      validate$check_data(normalise(dat), type = "mixed_effects_demand"), "1,234", fixed = TRUE
    )
  })

  it("in a long file converts x and y and leaves every other column as typed", {
    path <- write_upload(c(
      "monkey,x,y,y_ll4,drug,dose",
      'a,"0,5","7,5","0,875",coc,"0,001"',
      'a,1,"6,25","0,796",coc,"0,001"'
    ))
    dat <- upload_read$read_upload(path, "csv")
    expect_identical(dat$x, c(0.5, 1))
    expect_identical(dat$y, c(7.5, 6.25))
    expect_identical(dat$y_ll4, c(0.875, 0.796))
    expect_identical(dat$dose, c("0,001", "0,001"))
    expect_identical(attr(dat, "decimal_commas")$n, 5L)
  })

  it("does not let a decimal point in a long file's covariate veto decimal-comma x and y", {
    path <- write_upload(c("id,x,y,dose", '1,"0,5","7,5",0.001', '1,1,"6,5",0.001'))
    dat <- upload_read$read_upload(path, "csv")
    expect_identical(dat$y, c(7.5, 6.5))
    expect_identical(dat$dose, c(0.001, 0.001))
  })

  it("still converts named response columns in a file that is not long", {
    path <- write_upload(c("id\tbeer_free\tbeer_paid", "1\t7,5\t3,5", "2\t6,5\t2,5"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(dat$beer_free, c(7.5, 6.5))
    expect_identical(dat$beer_paid, c(3.5, 2.5))
  })

  it("converts a decimal comma inside a currency price header", {
    path <- write_upload(c("id\tR$0,50\tR$1\t2,50\u20ac", "a\t10\t8\t2"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(colnames(dat), c("id", "R$0.50", "R$1", "2.50\u20ac"))
    expect_identical(commas(dat)$n, 2L)
  })

  it("reports nothing for a file with no comma numbers", {
    dat <- upload_read$read_upload(write_upload(c("id,x,y", "a,1,7.5", "a,2,3")), "csv")
    expect_identical(commas(dat)$n, 0L)
  })
})

describe("read_upload() reports the decimal commas it converts in a semicolon file", {
  it("counts the converted cells and keeps examples", {
    dat <- upload_read$read_upload(write_upload(c("id;x;y", "a;0,5;10", "a;1,5;8", "a;2;6")), "csv")
    expect_identical(dat$x, c(0.5, 1.5, 2))
    expect_identical(attr(dat, "decimal_commas")$n, 2L)
    expect_identical(attr(dat, "decimal_commas")$examples, c("0,5", "1,5"))
  })
})

# The comma and tab reader never rewrites an id or group label. The semicolon reader did:
# id "01,5" became 1.5, the same id as "1,50".
describe("read_upload() and labels in a semicolon file", {
  it("leaves id and group labels exactly as typed while converting measurements", {
    path <- write_upload(c("id;group;x;y", "01,5;1,5;0;7,5", "1,50;2,5;0;6,5"))
    dat <- upload_read$read_upload(path, "csv")
    expect_identical(dat$id, c("01,5", "1,50"))
    expect_identical(dat$group, c("1,5", "2,5"))
    expect_identical(dat$y, c(7.5, 6.5))
    expect_identical(attr(dat, "decimal_commas")$n, 2L)
  })

  it("still reads a plain numeric id as a number", {
    path <- write_upload(c("id;x;y", "1;0;7,5", "2;0;6,5"))
    dat <- upload_read$read_upload(path, "csv")
    expect_identical(dat$id, c(1L, 2L))
  })
})

describe("read_upload() and a plus sign in a semicolon file", {
  it("reads +7,5 as 7.5, as it does in a tab file", {
    dat <- upload_read$read_upload(write_upload(c("id;x;y", "a;1;+7,5", "a;2;3")), "csv")
    expect_identical(dat$y, c(7.5, 3))
  })
})

describe("decimal_comma_notice", {
  it("says how many cells were converted, with an example", {
    path <- write_upload(c("id\tx\ty", "a\t1\t7,5", "a\t2\t0,25"), ext = "tsv")
    msg <- upload_read$decimal_comma_notice(upload_read$read_upload(path, "tsv"))
    expect_identical(
      msg,
      "Commas were read as decimal marks in 2 values (for example \"7,5\" as 7.5)."
    )
  })

  it("is NULL when nothing was converted", {
    dat <- upload_read$read_upload(write_upload(c("id,x,y", "a,1,7.5")), "csv")
    expect_null(upload_read$decimal_comma_notice(dat))
  })
})
