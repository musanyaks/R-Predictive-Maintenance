# File: pipelines/prediction_pipeline.R
prediction_pipeline_plan <- function() {
  list(
    targets::tar_target(
      batch_run,
      run_batch_predictions(),
      cue = targets::tar_cue(mode = "always")  # re-run every tar_make
    ),
    targets::tar_target(rul_predictions_parquet, {
      arrow::write_parquet(batch_run, "data/processed/rul_predictions.parquet")
      batch_run
    })
  )
}