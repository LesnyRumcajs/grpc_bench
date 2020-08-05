FROM r-base:4.0.2

WORKDIR /app
COPY proto /app/proto

## Install grpc and protobuf from deb
RUN apt-get update && apt-get install -y \
  r-cran-rprotobuf r-cran-testthat r-cran-rcpp \
  libgrpc-dev libgrpc++-dev && \
  apt-get clean && rm -rf /var/lib/apt/lists/

## install R packages
## processx - suggested package for unit testing demo(helloclient)
## remotes - required to installGithub
RUN install.r docopt processx remotes futile.logger
RUN installGithub.r -d FALSE nfultz/grpc && rm -rf /tmp/*

COPY r_grpc /app

ENTRYPOINT [ "Rscript", "helloserver.R" ]
