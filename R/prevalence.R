



nordcanstat_prevalence <- function(
  x,
  subject_id_col_nm,
  prevalence_time_scale_col_nm,
  prevalence_time_points,
  prevalence_window_widths,
  stratum_col_nms = NULL,
  adjust_col_nms = NULL,
  subset = NULL,
  adjust_weigths = NULL
) {
  # use nordcanstat_prevalent_subject_count and then use nordcanstat_rate
  # TODO: use basicepistats::stat_prevalence_something
}
