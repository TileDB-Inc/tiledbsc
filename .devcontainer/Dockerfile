# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.217.2/containers/ubuntu/.devcontainer/base.Dockerfile

# [Choice] Ubuntu version (use hirsuite or bionic on local arm64/Apple Silicon): hirsute, focal, bionic
ARG VARIANT="hirsute"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

# Adapted from:
#   https://github.com/rocker-org/rocker/blob/master/r-ubuntu/focal/Dockerfile

# [Optional] Uncomment this section to install additional OS packages.
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
        software-properties-common \
        dirmngr \
        ed \
        less \
        locales \
        vim-tiny \
        wget \
        ca-certificates \
        dirmngr \
        gpg-agent \
    && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
        # && add-apt-repository --enable-source --yes "ppa:marutter/rrutter4.0" \
        # && add-apt-repository --enable-source --yes "ppa:c2d4u.team/c2d4u4.0+"

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8 \
    && echo "deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/" > /etc/apt/sources.list.d/cran.list

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

## This was not needed before but we need it now
ENV DEBIAN_FRONTEND noninteractive

## Otherwise timedatectl will get called which leads to 'no systemd' inside Docker
ENV TZ UTC

# Now install R and littler, and create a link for littler in /usr/local/bin
# Default CRAN repo is now set by R itself, and littler knows about it too
# r-cran-docopt is not currently in c2d4u so we install from source
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        r-base \
        r-base-dev \
        r-recommended \
        r-cran-littler \
    && ln -s /usr/lib/R/site-library/littler/examples/build.r /usr/local/bin/build.r \
    && ln -s /usr/lib/R/site-library/littler/examples/check.r /usr/local/bin/check.r \
    && ln -s /usr/lib/R/site-library/littler/examples/install.r /usr/local/bin/install.r \
    && ln -s /usr/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
    && ln -s /usr/lib/R/site-library/littler/examples/installBioc.r /usr/local/bin/installBioc.r \
    && ln -s /usr/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
    && ln -s /usr/lib/R/site-library/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
    && ln -s /usr/lib/R/site-library/littler/examples/installDeps.r /usr/local/bin/installDeps.r \
    && rm -rf /var/lib/apt/lists/* \
    && chgrp 1000 /usr/local/lib/R/site-library

COPY Rprofile.site /etc/R

## Install "some" libraries we are likely to need to run-time:
##   libudunits2-0 (package units, sf)
##   libxml2       (package xml2, tidyverse)
##   libhdf5       (package rhdf5)
## and install docopt used by some of the littsr scripts
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libhdf5-dev \
        libudunits2-0 \
        libxml2 \
        libgeos-dev \
        libglpk-dev \
    && install.r docopt devtools \
    && rm -rf /tmp/downloaded_packages/* /tmp/*.rds \
    && chmod a+w /tmp/downloaded_packages
