




nordcanstat_settings <- function(function_name) {
  stopifnot(
    length(function_name) == 1L,
    function_name %in% paste0("nordcanstat_", c("prevalent_subject_count"))
  )

  lexis_col_nms <- nordcancore::nordcan_lexis_col_nms()

  if (function_name == "nordcanstat_prevalent_subject_count") {
    settings <- list(
      follow_up_time_col_nm = lexis_col_nms["follow_up_time"],
      follow_up_time_window_widths = c(1, 3, 5, 10, Inf),
      subject_id_col_nm = lexis_col_nms["subject_id"],
      observation_time_points = stop("TODO: these come from nordcancore I suppose"),
      entry_time_col_nm = lexis_col_nms["entry"]
    )
  }


  return(settings)
}





