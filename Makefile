# File: Makefile
.PHONY: setup install db-migrate db-seed train predict monitor test api shiny renv-lock docker-build docker-up

setup:
    Rscript -e 'renv::init(bare = TRUE)'

install:
    Rscript -e 'renv::install(); devtools::install(quick = TRUE)'

renv-lock:
    Rscript -e 'renv::snapshot(force = TRUE)'

db-migrate:
    Rscript -e 'rpredictmaint::run_migrations()'

db-seed:
    Rscript database/seed/seed_database.R

train:
    Rscript -e 'targets::tar_make(names = dplyr::starts_with(c("raw_","validated","cleaned","machine_","features_","labels_","feature_cols","splits","tuned_","fit_","val_","ensemble_","registry_","train_","batch_","rul_predictions")))'

predict:
    Rscript -e 'rpredictmaint::run_batch_predictions()'

monitor:
    Rscript -e 'targets::tar_make(names = dplyr::starts_with(c("drift_","perf_","monitor_")))'

test:
    Rscript -e 'devtools::test()'

api:
    Rscript -e 'rpredictmaint::run_api(port = 8000)'

shiny:
    Rscript shiny/app.R

docker-build:
    docker compose build

docker-up:
    docker compose up -d