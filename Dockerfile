FROM rocker/r-ver:4.4.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev libcurl4-openssl-dev libssl-dev libxml2-dev cmake \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Dependencies come straight from DESCRIPTION (renv optional, not required)
COPY DESCRIPTION .
RUN Rscript -e 'install.packages("remotes"); remotes::install_deps(dependencies = TRUE)'

COPY . .
RUN Rscript -e 'install.packages(".", repos = NULL, type = "source")'

CMD ["Rscript", "-e", "targets::tar_make()"]
