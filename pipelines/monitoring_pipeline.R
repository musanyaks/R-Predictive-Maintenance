# File: pipelines/monitoring_pipeline.R
monitoring_pipeline_plan <- function() {
  list(
    targets::tar_target(monitor_results, run_model_monitoring(),
                        cue = targets::tar_cue(mode = "always")),
    targets::tar_target(drift_report, monitor_results$drift),
    targets::tar_target(perf_report,  monitor_results$performance),
    targets::tar_target(monitor_summary, list(drift = drift_report,
                                              perf = perf_report))
  )
}