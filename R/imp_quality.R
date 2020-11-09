
#' @title Incidence, Mortality and Prevalence Quality Statistics
#' @description
#' Compute basic IMP quality statistics.
#' @param x see [basicepistats::stat_count]
#' @param cancer_death_count_dataset `[data.table]` (mandatory, no default)
#'
#'
#' @param by see [basicepistats::stat_count]
#' @param subset see [basicepistats::stat_count]
#' @param subset_style see [basicepistats::stat_count]
#' @export
#' @importFrom data.table := .SD
#' @family nordcanstat
nordcanstat_imp_quality <- function(
  x,
  cancer_death_count_dataset,
  by = NULL,
  subset = NULL,
  subset_style = "zeros"
) {
  dbc::assert_is_data.table(x)
  dbc::assert_is_data.table(cancer_death_count_dataset)
  cdcd_col_nms <- "cancer_death_count"
  if (is.character(by)) {
    cdcd_col_nms <- union(by, cdcd_col_nms)
    dbc::assert_has_only_names(
      cancer_death_count_dataset, required_names = cdcd_col_nms
    )
  } else {
    dbc::assert_has_names(
      cancer_death_count_dataset, required_names = cdcd_col_nms
    )
  }
  count_dt <- nordcanstat_count(
    x = x,
    by = by,
    subset = subset,
    subset_style = subset_style
  )
  stratum_col_nms <- setdiff(names(count_dt), "cancer_record_count")

  # cytology, hist. of metastasis, hist. of primary tumor
  mv_levels <- c(5L, 6L, 7L)
  subsets <- list(
    "cancer_record_count_included" = x[["excl_imp_total"]] == 0L,
    "percentage_included"= x[["excl_imp_total"]] == 0L,
    "percentage_mv" = x[["bod"]] %in% mv_levels,
    "percentage_dco" = x[["bod"]] == 0L
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

  i.cancer_death_count <- cancer_record_count <- NULL # to appease R CMD CHECK
  count_dt[
    i = cancer_death_count_dataset,
    on = eval(stratum_col_nms),
    j = "mi_ratio" := i.cancer_death_count / cancer_record_count
  ]

  return(count_dt[])
}
