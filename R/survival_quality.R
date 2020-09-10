



#' @title Survival Quality Statistics
#' @description
#' Compute basic survival quality statistics.
#' @param x see [basicepistats::stat_count]
#' @param by see [basicepistats::stat_count]
#' @param subset see [basicepistats::stat_count]
#' @param subset_style see [basicepistats::stat_count]
#' @export
#' @importFrom basicepistats stat_count
#' @importFrom data.table :=
#' @family nordcanstat
nordcanstat_survival_quality <- function(
  x,
  by = NULL,
  subset = NULL,
  subset_style = "zeros"
) {

  # if (is.character(by)) {
  #   by <- nordcancore::get_column_level_space(by)
  # }
  count_dt <- basicepistats::stat_count(
    x = x,
    by = by,
    subset = subset,
    subset_style = subset_style
  )
  stratum_col_nms <- setdiff(names(count_dt), "N")

  subsets <- list(
    "percentage excl. due to age 90+" = x[["age_year"]] >= 90.0,
    "percentage excl. due to DCO" = x[["excl_surv_dco"]] == 1,
    "percentage excl. due to autopsy" = x[["excl_surv_autopsy"]] == 1,
    "percentage excl. due to neg follow up" = x[["excl_surv_negativefou"]] == 1
  )

  lapply(names(subsets), function(new_col_nm) {
    subset <- subset_and(subset, subsets[[new_col_nm]])
    count_dt_new_col <- basicepistats::stat_count(
      x = x,
      by = by,
      subset = subset,
      subset_style = subset_style
    )
    if (length(stratum_col_nms) == 0L) {
      count_dt[
        j = (new_col_nm) := count_dt_new_col[["N"]]
      ]
    } else {
      i.N <- NULL # appease R CMD CHECK
      count_dt[
        i = count_dt_new_col,
        on = stratum_col_nms,
        j = (new_col_nm) := i.N
      ]
      count_dt[
        i = is.na(count_dt[[new_col_nm]]),
        j = (new_col_nm) := 0L
      ]
    }
    count_dt[
      j = (new_col_nm) := .SD[[1L]] / .SD[[2L]],
      .SDcols = c(new_col_nm, "N")
    ]
    NULL
  })

  return(count_dt[])
}
