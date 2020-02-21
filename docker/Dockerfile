FROM rocker/r-ver:3.6.2

RUN apt-get update && apt-get -y install \
   libcurl4-openssl-dev \
   libgit2-dev \
   libpq-dev \
   libssl-dev

COPY docker/bin /usr/local/bin/
RUN sed  -i'' '/mran.microsoft.com/d' /usr/local/lib/R/etc/Rprofile.site

RUN install_packages --repo=https://vimc.github.io/drat \
    DBI \
    R6 \
    RPostgres \
    RSQLite \
    bit64 \
    gert \
    vaultr \
    yaml \
    withr \
    writexl

COPY . /src
RUN R CMD INSTALL /src && rm -rf /src
