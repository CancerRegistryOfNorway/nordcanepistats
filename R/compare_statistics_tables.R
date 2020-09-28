

nordcan_statistics_table_names_for_comparison <- function() {
  c("cancer_record_count_dataset",
    "cancer_death_count_dataset",
    "prevalent_patient_count_dataset",
    "imp_quality_statistics_dataset",
    "survival_quality_statistics_dataset",
    "survival_statistics_dataset")
}


#' @title Compare NORDCAN Statistics
#' @description
#' Compare statistics between two versions of NORDCAN.
#' @param current_stat_table_list `[list]` (mandatory, no default)
#'
#' list of statistics tables; current version; intended for the output of
#' [nordcan_statistics_tables]
#' @param old_stat_table_list `[list]` (mandatory, no default)
#'
#' list of statistics tables; current version; intended for the output of
#' [nordcan_statistics_tables] from a previous run
#' @details
#' Any comparisons have been defined on the basis of the name of the table,
#' i.e. `cancer_record_count_dataset` can have a different set of comparisons
#' than `survival_statistics_dataset`.
#'
#' All comparisons between two statistics from the current and old version
#' are done by subtracting the old statistic from the new. All p-values are
#' estimated for these differences.
#'
#' Comparisons are only performed on statistics in strata which exist in both
#' the old and current versions. E.g. if the old version contains counts
#' up to 2018 and the current one up to 2019, no comparisons are done on 2019
#' counts.
#'
#' @return
#' A list with elements
#'
#' - `summary`: a data.table that summarizes every comparison performed,
#'   where the summaries are stratified by table name and statistic column name
#' - `comparisons`: a list of data.tables, where one row in a data.table
#'   corresponds to one comparison between an old and a new statistic.
#' @export
#' @importFrom stats p.adjust
#' @importFrom data.table setkeyv rbindlist setDT := set
compare_nordcan_statistics_table_lists <- function(
  current_stat_table_list,
  old_stat_table_list
) {
  dbc::assert_is_uniquely_named_list(current_stat_table_list)
  dbc::assert_is_uniquely_named_list(old_stat_table_list)
  table_names <- intersect(names(current_stat_table_list),
                           names(old_stat_table_list))
  is_df <- vapply(table_names, function(table_name) {
    is.data.frame(current_stat_table_list[[table_name]])
  }, logical(1L))
  table_names <- table_names[is_df]
  stratum_col_nm_set <- nordcancore::nordcan_metadata_column_name_set(
    "column_name_set_stratum_column_name_set"
  )
  comparisons <- lapply(table_names, function(table_name) {
    message("* nordcanepistats::compare_nordcan_statistics_table_lists: ",
            "comparing current_stat_table_list$", table_name,
            " to old_stat_table_list$" , table_name, "...")
    x <- current_stat_table_list[[table_name]]
    y <- old_stat_table_list[[table_name]]
    xy_stratum_col_nm_set <- intersect(
      stratum_col_nm_set,
      intersect(names(x), names(y))
    )

    x <- x[
      i = y,
      on = eval(xy_stratum_col_nm_set),
      nomatch = 0L,
      j = .SD,
      .SDcols = names(x)
    ]
    data.table::setkeyv(x, xy_stratum_col_nm_set)
    y <- y[
      i = x,
      on = eval(xy_stratum_col_nm_set),
      nomatch = 0L,
      j = .SD,
      .SDcols = names(y)
    ]
    data.table::setkeyv(y, xy_stratum_col_nm_set)
    comparison_dt <- compare_tables(x = x, y = y, table_name = table_name)
    cbind(
      x[j = .SD, .SDcols = xy_stratum_col_nm_set],
      comparison_dt
    )
  })
  names(comparisons) <- table_names


  p_value_dt <- data.table::rbindlist(lapply(table_names, function(table_name) {
    dt <- comparisons[[table_name]]
    data.table::setDT(list(
      table_name = rep(table_name, nrow(dt)),
      p_value = dt[["p_value"]]
    ))
  }))

  p_value_dt[
    j = "p_value_bh" := stats::p.adjust(p_value_dt$p_value, method = "BH")
  ]

  lapply(table_names, function(table_name) {
    subset <- p_value_dt[["table_name"]] == table_name
    dt <- p_value_dt[subset, ]
    data.table::set(
      x = comparisons[[table_name]],
      j = "p_value_bh",
      value = dt[["p_value_bh"]]
    )
    NULL
  })

  summary <- data.table::rbindlist(lapply(table_names, function(table_name) {
    dt <- comparisons[[table_name]]
    dt_summary <- dt[
      j = list(
        n_tests = .N,
        n_p_value_bh_lte_001 = sum(dt$p_value_bh < 0.01, na.rm = TRUE),
        n_p_value_bh_lte_005 = sum(dt$p_value_bh < 0.05, na.rm = TRUE),
        n_p_value_lte_001 = sum(dt$p_value < 0.01, na.rm = TRUE),
        n_p_value_lte_005 = sum(dt$p_value < 0.05, na.rm = TRUE),
        min_stat_value = min(dt$stat_value, na.rm = TRUE),
        median_stat_value = median(dt$stat_value, na.rm = TRUE),
        min_stat_value = max(dt$stat_value, na.rm = TRUE),
        n_na_p_value = sum(is.na(dt$p_value)),
        n_na_x = sum(is.na(dt$x)),
        n_na_y = sum(is.na(dt$y)),
        n_na_x_and_na_y = sum(is.na(dt$x) & is.na(dt$y))
      ),
      keyby = c("stat_type", "column_name")
    ]
    dt_summary <- cbind(table_name = table_name, dt_summary)
    return(dt_summary[])
  }))

  list(summary = summary, comparisons = comparisons)
}


compare_tables <- function(
  x,
  y,
  table_name
) {
  dbc::assert_is_data.table(x)
  dbc::assert_is_data.table(y)
  dbc::assert_is_character_nonNA_atom(table_name)

  switch(
    table_name,
    cancer_record_count_dataset = compare_count_tables(
      x = x, y = y, count_col_nm = "cancer_record_count"
    ),
    cancer_death_count_dataset = compare_count_tables(
      x = x, y = y, count_col_nm = "cancer_death_count"
    ),
    prevalent_patient_count_dataset = compare_count_tables(
      x = x, y = y, count_col_nm = "prevalent_patient_count"
    ),
    imp_quality_statistics_dataset = compare_imp_quality_statistics_tables(
      x = x, y = y,
      prop_col_nms = names(x)[grepl("percent", names(x))],
      count_col_nm = "cancer_record_count"
    ),
    survival_quality_statistics_dataset = compare_survival_quality_statistics_tables(
      x = x, y = y,
      prop_col_nms = names(x)[grepl("percent", names(x))],
      count_col_nm = "cancer_record_count"
    ),
    survival_statistics_dataset = compare_survival_statistics_tables(
      x = x, y = y,
      surv_col_nm = "cns"
    ),
    stop("Internal error: no comparison function defined for table named ",
         deparse(table_name), "; try removing this table from at least one ",
         "of your lists and trying again; you should report this error ",
         "if the table name corresponded to one of the statistics tables ",
         "produced by nordcanepistats::nordcan_statistics_tables")
  )
}


#' @importFrom data.table rbindlist
compare_imp_quality_statistics_tables <- function(
  x, y, prop_col_nms, count_col_nm
  ) {
  dt <- data.table::rbindlist(lapply(prop_col_nms, function(prop_col_nm) {
    compare_proportion_tables(
      x = x, y = y, prop_col_nm = prop_col_nm
    )
  }))
  dt <- rbind(
    dt,
    compare_rate_tables(x = x, y = y, ratio_col_nm = "mi_ratio",
                        count_col_nm = count_col_nm)
  )
  return(dt[])
}

#' @importFrom data.table rbindlist
compare_survival_quality_statistics_tables <- function(
  x, y, prop_col_nms, count_col_nm
) {
  dbc::assert_is_data.table_with_required_names(
    x,
    required_names = c(prop_col_nms, count_col_nm)
  )
  dbc::assert_is_data.table_with_required_names(
    y,
    required_names = c(prop_col_nms, count_col_nm)
  )
  dt <- data.table::rbindlist(lapply(prop_col_nms, function(prop_col_nm) {
    compare_proportion_tables(
      x = x,
      y = y, prop_col_nm = prop_col_nm, count_col_nm = count_col_nm
    )
  }))
  return(dt[])
}

#' @importFrom data.table :=
compare_survival_statistics_tables <- function(
  x, y, surv_col_nm
) {
  dt <- compare_survivals(x = x[[surv_col_nm]], y = y[[surv_col_nm]])

  dt[, "column_name" := surv_col_nm]
  return(dt[])
}

#' @importFrom data.table :=
compare_count_tables <- function(x, y, count_col_nm) {
  dt <- compare_counts(
    x = x[[count_col_nm]],
    y = y[[count_col_nm]]
  )
  dt[, "column_name" := count_col_nm]
  return(dt[])
}

#' @importFrom data.table :=
compare_rate_tables <- function(x, y, count_col_nm, rate_col_nm) {
  dt <- compare_rates(
    x = x[[rate_col_nm]],
    y = y[[rate_col_nm]],
    x_count = x[[count_col_nm]],
    y_count = y[[count_col_nm]]
  )
  dt[, "column_name" := rate_col_nm]
  return(dt[])
}

#' @importFrom data.table :=
compare_proportion_tables <- function(x, y, prop_col_nm, count_col_nm) {
  dt <- compare_proportions(
    x = x[[prop_col_nm]],
    y = y[[prop_col_nm]],
    x_count = x[[count_col_nm]],
    y_count = y[[count_col_nm]]
  )
  dt[, "column_name" := prop_col_nm]
  return(dt[])
}

#' @importFrom dbc assert_prod_input_is_integer_gtezero_vector
#' @importFrom data.table setDT :=
#' @importFrom skellam pskellam
compare_counts <- function(
  x,
  y
) {
  dbc::assert_prod_input_is_integer_gtezero_vector(x)
  dbc::assert_prod_input_is_integer_gtezero_vector(y)

  dt <- data.table::setDT(list(x = x, y = y))
  dt[, "stat_type" := "count_x_minus_y"]
  dt[, "stat_value" := dt$x - dt$y]
  dt[
    j = "p_value" := 2 * skellam::pskellam(q = abs(dt$stat_value),
                                           lambda1 = (dt$x + dt$y) / 2.0,
                                           lower.tail = FALSE)

  ]
  return(dt[])
}


#' @importFrom dbc assert_prod_input_is_number_gtezero_vector
#' @importFrom data.table setDT := .N setcolorder
compare_survivals <- function(
  x,
  y
) {
  dbc::assert_prod_input_is_number_gtezero_vector(x)
  dbc::assert_prod_input_is_number_gtezero_vector(y)

  dt <- data.table::setDT(list(x = x, y = y))
  dt[, "stat_type" := "survival_x_minus_y"]
  dt[, "stat_value" := dt$x - dt$y]
  dt[, "p_value" := NA_real_]
  return(dt[])
}


#' @importFrom dbc assert_prod_input_is_number_vector
#' assert_prod_input_is_integer_gtezero_vector
#' @importFrom stats pbeta
#' @importFrom data.table setDT :=
compare_proportions <- function(
  x,
  y,
  x_count,
  y_count
) {
  dbc::assert_prod_input_is_number_vector(x)
  dbc::assert_prod_input_is_number_vector(y)
  eval_env <- environment()
  dbc::report_to_assertion(
    dbc::tests_to_report(
      tests = c("is.na(x) | (x >= 0.0 & x <= 100.0)",
                "is.na(y) | (y >= 0.0 & y <= 100.0)"),
      fail_messages = c(
        "first five invalid values: ${deparse(utils::head(x[wh_fail], 5))}",
        "first five invalid values: ${deparse(utils::head(y[wh_fail], 5))}"
      ),
      env = eval_env
    )
  )

  dt <- data.table::setDT(list(x = x, y = y))
  dt[, "stat_type" := "proportion_x_minus_y"]
  dt[, "stat_value" := dt$x - dt$y]
  dt[, "p_value" := NA_real_]
  return(dt[])
}



#' @importFrom dbc assert_prod_input_is_integer_gtezero_vector
#' @importFrom data.table setDT :=
#' @importFrom skellam pskellam
compare_rates <- function(
  x,
  y,
  x_count,
  y_count
) {
  # in the case of mi_ratio, this is a poisson model where the cancer record
  # count is the poisson offset.

  dt <- data.table::setDT(list(x = x, y = y))
  dt[, "stat_type" := "ratio_x_minus_y"]
  dt[, "stat_value" := dt$x - dt$y]
  x_as_count <- as.integer(round(x * x_count))
  y_as_count <- as.integer(round(y * y_count))
  abs_count_diff <- abs(x_as_count - y_as_count)
  count_sum  <- x_as_count + y_as_count

  dt[
    j = "p_value" := 2 * skellam::pskellam(q = abs_count_diff,
                                           lambda1 = count_sum / 2.0,
                                           lower.tail = FALSE)
  ]
  return(dt[])
}





