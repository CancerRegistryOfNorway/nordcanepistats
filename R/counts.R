




#' @title Cancer Record Counts
#' @description
#' Compute counts of cancer records.
#' @param x see [basicepistats::stat_count]
#' @param by see [basicepistats::stat_count]
#' @param subset see [basicepistats::stat_count]
#' @param subset_style see [basicepistats::stat_count]
#' @export
#' @importFrom basicepistats stat_count
#' @importFrom data.table :=
#' @family nordcanstat
nordcanstat_count <- function(
  x,
  by = NULL,
  subset = NULL,
  subset_style = "zeros"
) {
  if (is.character(by)) {
    by <- nordcancore::get_column_level_space(stratum_col_nms)
  }
  count_dt <- basicepstats::stat_count(
    x = x,
    by = by,
    subset = subset,
    subset_style = subset_style
  )
  return(count_dt[])
}









