# ─────────────────────────────────────────────
# Stage 1: Build stage — install R packages
# ─────────────────────────────────────────────
FROM rocker/r-ver:4.5.1 AS builder

LABEL maintainer="Brent Kaplan <bkaplan.ku@gmail.com>"

# Build deps + BLAS/LAPACK + CA certs + locale + headers for common packages
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    curl libcurl4-openssl-dev libcairo2-dev libssl-dev locales \
    build-essential gfortran libopenblas-dev liblapack-dev pkg-config \
    ca-certificates cmake libicu-dev libxml2-dev pandoc unixodbc-dev \
    && update-ca-certificates \
    && locale-gen en_US.UTF-8 \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8

# PPM binary repo for Ubuntu Noble (24.04) — skips compilation for most packages
ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/noble/latest
# Put renv cache inside the project; avoid symlinks so it copies cleanly to runtime
ENV RENV_PATHS_CACHE=/root/shinybeez/renv/cache
ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE
# Disable pak — its strict resolver conflicts with lockfile pins; renv native works fine
ENV RENV_CONFIG_PAK_ENABLED=FALSE

# Parallelize compilation
RUN mkdir -p /root/.R && printf "MAKEFLAGS=-j%s\n" "$(nproc)" > /root/.R/Makevars

WORKDIR /root/shinybeez

# Copy only what's needed for the package layer
COPY renv.lock .
COPY renv/ renv/

# Restore packages (cache renv downloads with BuildKit; PPM binaries skip compilation)
RUN --mount=type=cache,target=/root/shinybeez/renv/cache \
    --mount=type=cache,target=/root/.cache/R \
    R -q -e "Sys.setenv(RENV_CONFIG_REPOS_OVERRIDE = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE')); \
    options(repos = c(CRAN = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE')), \
            Ncpus = parallel::detectCores()); \
    install.packages('renv', repos = 'https://cloud.r-project.org'); \
    renv::consent(provided = TRUE); \
    renv::init(bare = TRUE); \
    renv::restore();"

# Strip native libs and remove docs to reduce image size
RUN find renv/library -type f -name '*.so' -exec strip --strip-unneeded {} + 2>/dev/null || true && \
    find renv/library -type f -name '*.a' -delete && \
    find renv/library -type d \( -name help -o -name doc -o -name html -o -name examples \) -prune -exec rm -rf {} + && \
    rm -rf /tmp/* /root/.cache/R/*

# ─────────────────────────────────────────────
# Stage 2: Runtime stage — only app and R libs
# ─────────────────────────────────────────────
FROM rocker/r-ver:4.5.1 AS runtime

LABEL maintainer="Brent Kaplan <bkaplan.ku@gmail.com>"

# Runtime libs only (no compilers); include fonts/graphics + CA certs
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    curl libcurl4 libssl3 libcairo2 libxml2 libfontconfig1 libfreetype6 \
    libharfbuzz0b libfribidi0 libpng16-16 libtiff6 libxt6 libodbc2 ca-certificates \
    && update-ca-certificates \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8

# Create non-root user for running the app
RUN useradd -r -m -s /usr/sbin/nologin shiny

WORKDIR /home/shiny/shinybeez

# Copy R packages from builder
COPY --from=builder /root/shinybeez/renv.lock ./renv.lock
COPY --from=builder /root/shinybeez/renv      ./renv

# .Rprofile for renv activation at runtime
COPY .Rprofile ./.Rprofile

# Create log and data directories (don't copy local files into image)
RUN mkdir -p logs data

# Application files (after libs so app edits don't invalidate the package layer)
COPY app.R       ./app.R
COPY config.yml  ./config.yml
COPY rhino.yml   ./rhino.yml
COPY app/        app/
COPY Rprofile.site /usr/lib/R/etc/

# Pre-compile bytecode for heavy packages at build time (saves 5-15s on cold start)
RUN R -q -e " \
  library(shiny); library(bslib); library(DT); \
  library(beezdemand); library(nlme); library(emmeans); \
  library(ggplot2); library(dplyr); library(rhino); \
  library(esquisse); library(ggprism); library(openxlsx); \
  cat('Warmup complete\n')"

# Set ownership and switch to non-root user
RUN chown -R shiny:shiny /home/shiny/shinybeez
USER shiny

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/shiny/shinybeez', host='0.0.0.0', port=3838)"]
