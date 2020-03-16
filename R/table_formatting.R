




format_nordcanstat_table <- function(
  nordcanstat_table,
  stratum_col_nms,
  stat_col_nms
) {
  stratum_col_nms <- sort(stratum_col_nms)
  stat_col_nms <- sort(stat_col_nms)
  if (identical(stratum_col_nms, c("dg_y", "sex")) && identical(stat_col_nms, "incidence_nordic")) {
    out <- format_sex_year_table(nordcanstat_table)
  }
  # etc.

  return(out)
}


format_sex_year_table <- function(
  nordcanstat_table
) {

}

# etc.


