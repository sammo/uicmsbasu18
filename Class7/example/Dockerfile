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
RUN conda install mro-base \
                  r-randomforest \
                  r-plyr && \
    R -e "install.packages(c( \
            'fastICA', \
            'rsvd' \
          ))"

# This allows us to access the programming enviornment withint he container
RUN conda install r-irkernel && \
    R -e "IRkernel::installspec(user=FALSE)"

# Create the internal folder strucutre for this project
RUN mkdir -p /work/code && \
    mkdir -p /work/data
COPY code /work/code
COPY data /work/data

# Setup workdir
#  This changes the default working directory to where our API specific code
#  lives.
WORKDIR /work/code

# This runs a jupyter notebook.  This allows us to access the enviornment we
#  make within this container using a "kernel".  Although a more advanced topic
#  jupyter is a nice way of doing "enviornment control"
ENTRYPOINT jupyter notebook --allow-root
