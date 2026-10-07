# File: Dockerfile  (base/trainer image)
FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev libcurl4-openssl-dev libssl-dev libxml2-dev \
    libudunits2-dev libgdal-dev cmake \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
RUN Rscript -e "install.packages('renv'); renv::restore()"

COPY . .
RUN Rscript -e "install.packages('.', repos = NULL, type = 'source')"

CMD ["Rscript", "-e", "targets::tar_make()"]