# File: R/models/rul_regression.R
#' RUL label = days until next maintenance event
add_rul_label <- function(features_df, maintenance_df) {
  events <- maintenance_df |>
    dplyr::select(machine_id, event_time = performed_at) |>
    dplyr::arrange(machine_id, event_time)

  features_df |>
    dplyr::group_by(machine_id) |>
    dplyr::group_modify(function(d, key) {
      ev <- sort(events$event_time[events$machine_id == key$machine_id])
      d$rul_days <- vapply(d$reading_time, function(t) {
        nxt <- ev[ev > t]
        if (length(nxt) == 0) NA_real_
        else as.numeric(difftime(min(nxt), t, units = "days"))
      }, numeric(1))
      d
    }) |>
    dplyr::ungroup()
}

#' Train random forest RUL regressor (tidymodels workflow)
train_rul_workflow <- function(train_df, feature_cols) {
  cols <- make.names(feature_cols)
  d <- train_df[stats::complete.cases(train_df[c("rul_days", cols)]),
                c("rul_days", cols)]
  spec <- parsnip::rand_forest(trees = 300, min_n = 10) |>
          parsnip::set_engine("ranger", num.threads = 4) |>
          parsnip::set_mode("regression")
  wf <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(stats::as.formula(
      paste("rul_days ~", paste(cols, collapse = " + "))))
  generics::fit(wf, data = d)
}

#' Predict RUL in days (clipped at 0)
predict_rul <- function(model, new_df, feature_cols) {
  cols <- make.names(feature_cols)
  ok   <- stats::complete.cases(new_df[cols])
  d    <- new_df[ok, cols, drop = FALSE]
  pred <- parsnip::predict(model, d)$.pred
  dplyr::bind_cols(
    new_df[c("machine_id", "reading_time")][ok, ],
    dplyr::tibble(rul_days = pmax(0, pred))
  )
}