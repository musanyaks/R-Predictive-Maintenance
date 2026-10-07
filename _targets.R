# File: _targets.R  (MUST live at project root — tar_make() looks for it here)
library(targets)

source("pipelines/data_pipeline.R")
source("pipelines/training_pipeline.R")
source("pipelines/prediction_pipeline.R")
source("pipelines/monitoring_pipeline.R")

tar_option_set(
  packages = c(
    "rpredictmaint", "dplyr", "tidyr", "purrr", "lubridate",
    "arrow", "xgboost", "isotree", "ranger", "workflows", "parsnip"
  ),
  format = "parquet",
  error = "workspace"
)

c(
  data_pipeline_plan(),
  training_pipeline_plan(),
  prediction_pipeline_plan(),
  monitoring_pipeline_plan()
)