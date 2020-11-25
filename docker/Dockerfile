FROM rocker/r-ver:4.0.3

RUN apt-get update && apt-get -y install \
   libcurl4-openssl-dev \
   libgit2-dev \
   libpq-dev \
   libssl-dev

COPY docker/bin /usr/local/bin/
COPY docker/Rprofile.site /usr/local/lib/R/etc/Rprofile.site

RUN install_packages \
    DBI \
    R6 \
    RPostgres \
    RSQLite \
    bit64 \
    docopt \
    gert \
    testthat \
    vaultr \
    yaml \
    withr \
    writexl

ARG GIT_ID='UNKNOWN'
ARG GIT_BRANCH='UNKNOWN'
ARG DETTL_VERSION='UNKNOWN'
ENV DETTL_GIT_HASH $GIT_ID
ENV DETTL_GIT_BRANCH $GIT_BRANCH
ENV DETTL_VERSION $DETTL_VERSION

COPY . /src
RUN R CMD INSTALL /src && rm -rf /src

ENTRYPOINT ["/usr/local/bin/dettl"]
