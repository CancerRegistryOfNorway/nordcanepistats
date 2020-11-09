



#' @title Survival Quality Statistics
#' @description
#' Compute basic survival quality statistics.
#' @param x see [basicepistats::stat_count]
#' @param by see [basicepistats::stat_count]
#' @param subset see [basicepistats::stat_count]
#' @param subset_style see [basicepistats::stat_count]
#' @export
#' @importFrom data.table := .SD
#' @family nordcanstat
nordcanstat_survival_quality <- function(
  x,
  by = NULL,
  subset = NULL,
  subset_style = "zeros"
) {
  count_dt <- nordcanstat_count(
    x = x,
    by = by,
    subset = subset,
    subset_style = subset_style
  )
  stratum_col_nms <- setdiff(names(count_dt), "cancer_record_count")

  subsets <- list(
    "cancer_record_count_included" = x[["excl_surv_total"]] == 0,
    "percentage_included"= x[["excl_surv_total"]] == 0,
    "percentage_excl_surv_age" = x[["excl_surv_age"]] == 1,
    "percentage_excl_surv_dco" = x[["excl_surv_dco"]] == 1,
    "percentage_excl_surv_autopsy" = x[["excl_surv_autopsy"]] == 1,
    "percentage_excl_surv_negativefou" = x[["excl_surv_negativefou"]] == 1,
    "percentage_excl_surv_duplicate" = x[["excluded_multiple"]] == 1,
    "percentage_not_reported_in_nordcan" = x[["entity_level_10"]] %in% c(999L, NA)
  )

  lapply(names(subsets), function(new_col_nm) {
    subset <- nordcancore::subset_and(subset, subsets[[new_col_nm]])
    count_dt_new_col <- nordcanstat_count(
      x = x,
      by = by,
      subset = subset,
      subset_style = subset_style
    )
    if (length(stratum_col_nms) == 0L) {
      count_dt[
        j = (new_col_nm) := count_dt_new_col[["cancer_record_count"]]
      ]
    } else {
      i.cancer_record_count <- NULL # appease R CMD CHECK
      count_dt[
        i = count_dt_new_col,
        on = stratum_col_nms,
        j = (new_col_nm) := i.cancer_record_count
      ]
      count_dt[
        i = is.na(count_dt[[new_col_nm]]),
        j = (new_col_nm) := 0L
      ]
    }
    if (grepl("^percentage", new_col_nm)) {
      count_dt[
        j = (new_col_nm) := .SD[[1L]] / .SD[[2L]],
        .SDcols = c(new_col_nm, "cancer_record_count")
      ]
      count_dt[
        j = (new_col_nm) := round(100 * .SD[[1L]], 2L),
        .SDcols = new_col_nm
      ]
    }
    NULL
  })

  return(count_dt[])
}

