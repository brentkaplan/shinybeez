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
      attr(got, "delim") <- NULL
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

# In a comma-delimited file a quoted "7,5" is ambiguous: a decimal comma (7.5) or, read
# as a grouping mark, 75. It must never be silently read as either; the cell stays text
# so validation can name it. A quoted "1,234" is treated the same way.
describe("read_upload() and a quoted number containing a comma", {
  wide_demand <- c("id,0,1,5", "1,10,\"7,5\",2", "2,9,6,\"1,234\"")

  it("keeps the cell as typed in a comma CSV", {
    dat <- upload_read$read_upload(write_upload(wide_demand), "csv")
    expect_identical(dat[["1"]], c("7,5", "6"))
    expect_identical(dat[["5"]], c("2", "1,234"))
    expect_equal(dat[["0"]], c(10, 9))
  })

  it("keeps the cell as typed in a comma CSV with trailing semicolon junk", {
    dat <- upload_read$read_upload(write_upload(paste0(wide_demand, ";;;")), "csv")
    expect_identical(dat[["1"]], c("7,5", "6"))
  })

  it("keeps the cell as typed in a TSV", {
    path <- write_upload(c("id\t0\t1", "1\t10\t7,5", "2\t9\t6"), ext = "tsv")
    dat <- upload_read$read_upload(path, "tsv")
    expect_identical(dat[["1"]], c("7,5", "6"))
  })

  # Column types are guessed from a sample of rows. A cell beyond the sample used to be
  # forced into the guessed type: the text became NA with only a parsing warning.
  it("keeps the cell as typed when it sits far down a long file", {
    rows <- paste0(seq_len(3000), ",10,2")
    rows[2500] <- "2500,10,\"7,5\""
    dat <- upload_read$read_upload(write_upload(c("id,0,1", rows)), "csv")
    expect_identical(dat[["1"]][2500], "7,5")
    expect_identical(dat[["1"]][2499], "2")

    tsv <- gsub(",", "\t", rows[-2500], fixed = TRUE)
    tsv <- append(tsv, "2500\t10\t7,5", after = 2499)
    dat <- upload_read$read_upload(write_upload(c("id\t0\t1", tsv), ext = "tsv"), "tsv")
    expect_identical(dat[["1"]][2500], "7,5")
  })

  it("is then rejected by demand validation, naming the cell", {
    dat <- normalise(upload_read$read_upload(write_upload(wide_demand), "csv"))
    res <- validate$check_data(dat, type = "demand")
    expect_type(res, "character")
    expect_match(res, '"7,5"', fixed = TRUE)
  })
})
