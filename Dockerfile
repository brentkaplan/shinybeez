FROM rocker/r-ver:4.4.1

LABEL maintainer="Brent Kaplan <bkaplan.ku@gmail.com>"

# 1) Install system dependencies
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
        sudo \
        curl \
        g++ \
        make \
        cmake \
        libcurl4-gnutls-dev \
        libcairo2-dev \
        libssl-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/* /tmp/*

# 2) Copy renv files before restoring
WORKDIR /root/shinybeez
COPY .Rprofile ./
COPY renv.lock .
COPY renv/ renv/

# 3) Set up environment variables
ENV RENV_PATHS_CACHE=/root/renvcache

# 4) Install renv + restore packages
RUN R -e 'install.packages("renv", repos = "https://cloud.r-project.org"); renv::restore(); renv::clean()' && rm -rf /root/.cache/R/renv

# 5) Copy app code
COPY app.R ./app.R
COPY config.yml ./config.yml
COPY rhino.yml ./rhino.yml
COPY app/ app/
COPY Rprofile.site /usr/lib/R/etc/
COPY logs/ logs/

# 6) Expose the shiny port
EXPOSE 3838

# 7) Set the container command
CMD ["R", "-e", "shiny::runApp('/root/shinybeez', host='0.0.0.0', port=3838)"]
