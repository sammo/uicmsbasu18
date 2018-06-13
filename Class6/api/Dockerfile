# Init
#  Dockerfiles almost always start with a FROM, which defines the base Docker
#  image we are using for this new image.
FROM jupyter/minimal-notebook@sha256:14a4706df16a0452565c2fdc7284c062cb248b80ee19666b00e27b9871d0c132
LABEL maintainer="Sam.Mourad@crowe.com"

# OS Upgrades
#  We switch to the root user so we can install things within the container.
#  This is because the jupyter image we are starting from has explicitly
#  changed the user away from root.
#  libpq-dev is required to install R
USER root
RUN apt-get update && apt-get install -y \
       libpq-dev

# Install R and packages
#  conda is a nice package/environment manager (comes in the jupyter image) that
#  can install R and packages.  For this image we will install mro-base (R
#  markdown) using conda and install packages using R itself.
RUN conda install mro-base && \
    R -e "install.packages(c(\
        'RPostgreSQL', \
        'plumber', \
        'readr', \
        'jsonlite'))"

# Move in API code/models
#  In this linux environment we are creating a directory at the root path called
#  work, and adding a directory inside called api.  Here we will mirror all the
#  code for our API in its exact structure.
RUN mkdir -p /work/api/models
COPY code /work/api
COPY models /work/api/models

# Setup workdir
#  This changes the default working directory to where our API specific code
#  lives.
WORKDIR /work/api

# Inform user of API port
#  Inside of our plumber API, we have chosen for the API to "live" at port 8000.
#  This tells the future user of this Docker image that port 8000 is important
#  for this image.  If we did not expose this port, the API could still be
#  accessed but it is best practice to tell future users of this image which
#  ports are of interest to them.
EXPOSE 8000

# Run API as default entrypoint
#  This is the command that runs when the container is built.  It runs main.R
#  which starts the API web service and assigns it to communicate over port 8000
ENTRYPOINT Rscript /work/api/main.R
