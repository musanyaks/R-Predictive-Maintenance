# File: R/prediction/maintenance_recommendation.R
#' Rule-based maintenance action from risk + explanation
recommend_maintenance <- function(machine_id, machine_name, risk_level,
                                  risk_score, rul_days, explanation) {
  base <- dplyr::case_when(
    risk_level == "critical" ~ 1L,
    risk_level == "high"     ~ 2L,
    risk_level == "medium"   ~ 3L,
    TRUE                     ~ 4L
  )
  action <- dplyr::case_when(
    risk_level == "critical" ~ "Immediate inspection + scheduled shutdown",
    risk_level == "high"     ~ "Schedule maintenance within 3 days",
    risk_level == "medium"   ~ "Increase monitoring frequency; plan next window",
    TRUE                     ~ "Routine monitoring"
  )
  due_by <- dplyr::case_when(
    risk_level == "critical" ~ now_utc() + lubridate::hours(24),
    risk_level == "high"     ~ now_utc() + lubridate::days(3),
    risk_level == "medium"   ~ now_utc() + lubridate::days(7),
    TRUE                     ~ now_utc() + lubridate::days(30)
  )
  dplyr::tibble(
    machine_id = machine_id,
    machine_name = machine_name,
    action = action,
    priority = base,
    due_by = due_by,
    risk_score = risk_score,
    rul_days = rul_days,
    rationale = explanation$summary
  )
}