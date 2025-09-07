# ─────────────────────────────────────────────
# Stage 1: Build stage — install R packages
# ─────────────────────────────────────────────
FROM rocker/r-ver:4.5.1 AS builder

LABEL maintainer="Brent Kaplan <bkaplan.ku@gmail.com>"

# System dependencies for R packages
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
    curl \
    cmake \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libssl-dev \
    locales \
    && locale-gen en_US.UTF-8 \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Set environment variables
ENV LANG=en_US.UTF-8
ENV RENV_PATHS_CACHE=/root/shinybeez/renv/.cache
ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/latest
ENV RENV_CONFIG_PAK_ENABLED=TRUE

# Working directory
WORKDIR /root/shinybeez

# Add renv files and your local tarball
COPY .Rprofile .
COPY renv.lock .
COPY renv/ renv/
COPY beezdemand_0.1.3.tar.gz .

# Use BuildKit cache mount
RUN --mount=type=cache,target=/root/.cache/R R -e '\
    Sys.setenv(RENV_CONFIG_REPOS_OVERRIDE = Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE")); \
    options(repos = c(CRAN = Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE")), pkgType = "binary", Ncpus = parallel::detectCores()); \
    install.packages("renv", repos = Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE"), type = "binary"); \
    install.packages("pak",  repos = "https://r-lib.github.io/p/pak/stable"); \
    renv::init(bare = TRUE); \
    renv::restore();'

# Strip unnecessary files from packages
RUN find renv/library -type f -name '*.so' -exec strip --strip-unneeded {} + 2>/dev/null || true && \
    find renv/library -type f -name '*.a' -delete && \
    find renv/library -type d \( -name help -o -name doc -o -name html -o -name examples \) -prune -exec rm -rf {} + && \
    rm -rf /tmp/* /root/.cache/R/*

# ─────────────────────────────────────────────
# Stage 2: Runtime stage — only app and R libs
# ─────────────────────────────────────────────
FROM rocker/r-ver:4.5.1 AS runtime

LABEL maintainer="Brent Kaplan <bkaplan.ku@gmail.com>"

# Copy R packages from builder
COPY --from=builder /root/shinybeez/renv /root/shinybeez/renv
COPY --from=builder /root/shinybeez/renv.lock /root/shinybeez/renv.lock
COPY --from=builder /root/shinybeez/.Rprofile /root/shinybeez/.Rprofile
COPY --from=builder /root/shinybeez/beezdemand_0.1.3.tar.gz /root/shinybeez/beezdemand_0.1.3.tar.gz

# System deps required *at runtime* only
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
    curl \
    libcurl4 \
    libcairo2 \
    libssl3 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Optional: additional cleanup
RUN rm -rf \
    /var/log/dpkg.log \
    /var/lib/dpkg/status-old \
    /var/cache/debconf/templates.dat

# Copy application files
WORKDIR /root/shinybeez
COPY app.R ./app.R
COPY config.yml ./config.yml
COPY rhino.yml ./rhino.yml
COPY app/ app/
COPY Rprofile.site /usr/lib/R/etc/
COPY logs/ logs/

# Expose port
EXPOSE 3838

# Launch command
CMD ["R", "-e", "shiny::runApp('/root/shinybeez', host='0.0.0.0', port=3838)"]

# export DOCKER_BUILDKIT=1
# docker buildx build --platform linux/x86_64 -t brentkaplan/shinybeez
