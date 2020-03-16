




nordcanstat_count <- function(
  x,
  stratum_col_nms = NULL,
  subset = NULL
) {
  col_lvl_space <- nordcancore::get_column_level_space(stratum_col_nms)
  basicepstats::stat_count(
    x = x,
    stratum_col_nms = stratum_col_nms,
    subset = subset,
    column_level_space = col_lvl_space
  )
}

