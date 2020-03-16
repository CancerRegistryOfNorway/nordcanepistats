




nordcanstat_prevalent_subject_count <- function(
  x,
  subject_id_col_nm,
  prevalence_time_scale_col_nm,
  prevalence_time_points,
  prevalence_window_widths,
  stratum_col_nms = NULL,
  subset = NULL
) {
  # split x at prevalence_time_points and count number of subjects identified
  # by subject_id_col_nm
}

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
}
