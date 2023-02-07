

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
#' @importFrom data.table := .SD
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
    message("* comparing current_stat_table_list$", table_name,
            " to old_stat_table_list$" , table_name, "...")
    t <- proc.time()
    new <- data.table::copy(current_stat_table_list[[table_name]])
    old <- data.table::copy(old_stat_table_list[[table_name]])
    xy_stratum_col_nm_set <- intersect(
      stratum_col_nm_set,
      intersect(names(new), names(old))
    )
    new <- new[
      i = old,
      on = eval(xy_stratum_col_nm_set),
      nomatch = 0L,
      j = .SD,
      .SDcols = names(new)
    ]
    data.table::setkeyv(new, xy_stratum_col_nm_set)
    old <- old[
      i = new,
      on = eval(xy_stratum_col_nm_set),
      nomatch = 0L,
      j = .SD,
      .SDcols = names(old)
    ]
    data.table::setkeyv(old, xy_stratum_col_nm_set)
    comparison_dt <- compare_tables(new = new, old = old, table_name = table_name)
    out <- cbind(
      new[j = .SD, .SDcols = xy_stratum_col_nm_set],
      comparison_dt
    )
    message("* done; time used: ", gsub("elapsed.*", "", data.table::timetaken(t)))
    return(out[])
  })
  names(comparisons) <- table_names


  message("* adjusting p-values...")
  t_p_value <- proc.time()
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
  message("* done; time used: ", gsub("elapsed.*", "", data.table::timetaken(t_p_value)))

  message("* computing summary table...")
  t_summary <- proc.time()
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
        median_stat_value = stats::median(dt$stat_value, na.rm = TRUE),
        max_stat_value = max(dt$stat_value, na.rm = TRUE),
        n_na_p_value = sum(is.na(dt$p_value)),
        n_na_x = sum(is.na(dt$new)),
        n_na_y = sum(is.na(dt$old)),
        n_na_x_and_na_y = sum(is.na(dt$new) & is.na(dt$old))
      ),
      keyby = c("stat_type", "column_name")
    ]
    dt_summary <- cbind(table_name = table_name, dt_summary)
    return(dt_summary[])
  }))
  message("* done; time used: ", gsub("elapsed.*", "", data.table::timetaken(t_summary)))

  list(summary = summary, comparisons = comparisons)
}


compare_tables <- function(
  new,
  old,
  table_name
) {
  dbc::assert_is_data.table(new)
  dbc::assert_is_data.table(old)
  dbc::assert_is_character_nonNA_atom(table_name)

  switch(
    table_name,
    cancer_record_count_dataset = compare_count_tables(
      new = new, old = old, count_col_nm = "cancer_record_count"
    ),
    cancer_death_count_dataset = compare_count_tables(
      new = new, old = old, count_col_nm = "cancer_death_count"
    ),
    prevalent_patient_count_dataset = compare_count_tables(
      new = new, old = old, count_col_nm = "prevalent_patient_count"
    ),
    imp_quality_statistics_dataset = compare_imp_quality_statistics_tables(
      new = new, old = old,
      prop_col_nms = names(new)[grepl("percent", names(new))],
      count_col_nm = "cancer_record_count"
    ),
    survival_quality_statistics_dataset = compare_survival_quality_statistics_tables(
      new = new, old = old,
      prop_col_nms = names(new)[grepl("percent", names(new))],
      count_col_nm = "cancer_record_count"
    ),
    survival_statistics_dataset = compare_survival_statistics_tables(
      new = new, old = old,
      surv_col_nm = "cns"
    ),
    stop("Internal error: no comparison function defined for table named ",
         deparse(table_name), "; try removing this table from at least one ",
         "of your lists and trying again; you should report this error ",
         "if the table name corresponded to one of the statistics tables ",
         "produced by nordcanepistats::nordcan_statistics_tables")
  )
}


compare_imp_quality_statistics_tables <- function(
  new, old, prop_col_nms, count_col_nm
) {
  dt <- data.table::rbindlist(lapply(prop_col_nms, function(prop_col_nm) {
    compare_proportion_tables(
      new = new, old = old, prop_col_nm = prop_col_nm
    )
  }))
  dt <- rbind(
    dt,
    compare_rate_tables(new = new, old = old, rate_col_nm = "mi_ratio",
                        count_col_nm = count_col_nm)
  )
  return(dt[])
}

compare_survival_quality_statistics_tables <- function(
  new, old, prop_col_nms, count_col_nm
) {
  dbc::assert_is_data.table_with_required_names(
    new,
    required_names = c(prop_col_nms, count_col_nm)
  )
  dbc::assert_is_data.table_with_required_names(
    old,
    required_names = c(prop_col_nms, count_col_nm)
  )
  dt <- data.table::rbindlist(lapply(prop_col_nms, function(prop_col_nm) {
    compare_proportion_tables(
      new = new,
      old = old, prop_col_nm = prop_col_nm, count_col_nm = count_col_nm
    )
  }))
  return(dt[])
}

#' @importFrom data.table :=
compare_survival_statistics_tables <- function(
  new, old, surv_col_nm
) {
  dt <- compare_survivals(new = new[[surv_col_nm]], old = old[[surv_col_nm]])

  dt[, "column_name" := surv_col_nm]
  return(dt[])
}

#' @importFrom data.table :=
compare_count_tables <- function(new, old, count_col_nm) {
  dt <- compare_counts(
    new = new[[count_col_nm]],
    old = old[[count_col_nm]]
  )
  dt[, "column_name" := count_col_nm]
  return(dt[])
}

#' @importFrom data.table :=
compare_rate_tables <- function(new, old, count_col_nm, rate_col_nm) {
  dt <- compare_rates(
    new = new[[rate_col_nm]],
    old = old[[rate_col_nm]],
    x_count = new[[count_col_nm]],
    y_count = old[[count_col_nm]]
  )
  dt[, "column_name" := rate_col_nm]
  return(dt[])
}

#' @importFrom data.table :=
compare_proportion_tables <- function(new, old, prop_col_nm, count_col_nm) {
  dt <- compare_proportions(
    new = new[[prop_col_nm]],
    old = old[[prop_col_nm]],
    x_count = new[[count_col_nm]],
    y_count = old[[count_col_nm]]
  )
  dt[, "column_name" := prop_col_nm]
  return(dt[])
}


#' @importFrom data.table :=
compare_counts <- function(
  new,
  old
) {
  dbc::assert_prod_input_is_integer_gtezero_vector(new)
  dbc::assert_prod_input_is_integer_gtezero_vector(old)

  dt <- data.table::setDT(list(new = new, old = old))
  dt[, "stat_type" := "count_new_minus_old"]
  dt[, "stat_value" := dt$new - dt$old]
  h0_means <- (dt$new + dt$old) / 2.0
  dt[
    j = "p_value" := 2 * (1 - skellam_cdf(q = abs(dt$stat_value),
                                          mu1 = h0_means, mu2 = h0_means))

  ]
  dt[dt$new == 0L & dt$old == 0L, "p_value" := NA_real_]
  return(dt[])
}


#' @importFrom data.table := .N
compare_survivals <- function(
  new,
  old
) {
  dbc::assert_prod_input_is_number_gtezero_vector(new)
  dbc::assert_prod_input_is_number_gtezero_vector(old)

  dt <- data.table::setDT(list(new = new, old = old))
  dt[, "stat_type" := "survival_new_minus_old"]
  dt[, "stat_value" := dt$new - dt$old]
  dt[, "p_value" := NA_real_]
  return(dt[])
}


#' @importFrom data.table :=
compare_proportions <- function(
  new,
  old,
  x_count,
  y_count
) {
  dbc::assert_prod_input_is_number_vector(new)
  dbc::assert_prod_input_is_number_vector(old)
  eval_env <- environment()
  dbc::report_to_assertion(
    dbc::expressions_to_report(
      expressions = c("is.na(new) | (new >= 0.0 & new <= 100.0)",
                "is.na(old) | (old >= 0.0 & old <= 100.0)"),
      fail_messages = c(
        "first five invalid values: ${deparse(utils::head(new[wh_fail], 5))}",
        "first five invalid values: ${deparse(utils::head(old[wh_fail], 5))}"
      ),
      env = eval_env
    )
  )

  dt <- data.table::setDT(list(new = new, old = old))
  dt[, "stat_type" := "proportion_new_minus_old"]
  dt[, "stat_value" := dt$new - dt$old]
  dt[, "p_value" := NA_real_]
  return(dt[])
}



#' @importFrom data.table :=
compare_rates <- function(
  new,
  old,
  x_count,
  y_count
) {
  # in the case of mi_ratio, this is a poisson model where the cancer record
  # count is the poisson offset.

  dt <- data.table::setDT(list(new = new, old = old))
  dt[, "stat_type" := "ratio_new_minus_old"]
  dt[, "stat_value" := dt$new - dt$old]
  x_as_count <- as.integer(round(new * x_count))
  y_as_count <- as.integer(round(old * y_count))
  abs_count_diff <- abs(x_as_count - y_as_count)
  h0_means  <- (x_as_count + y_as_count) / 2L

  dt[
    j = "p_value" := 2 * (1 - skellam_cdf(q = abs_count_diff,
                                          mu1 = h0_means,
                                          mu2 = h0_means))
  ]
  return(dt[])
}





#' @describeIn compare_nordcan_statistics_table_lists plots time series of
#' comparison results (`compare_nordcan_statistics_table_lists` output);
#' see Details
#' @details
#'
#' `plot_nordcan_statistics_table_comparisons` produces .png files into dir
#' `nordcancore::get_global_nordcan_settings()[["work_dir"]]`. Each file is a
#' grid of entity-specific plots. One .png corresponds to one dataset taken from
#' `x$comparisons`. Only the following datasets are plotted:
#'
#' - cancer_record_count_dataset: column `sex` is summed out before plotting.
#' - cancer_death_count_dataset: column `sex` is summed out before plotting.
#' - prevalent_patient_count_dataset column `sex` is summed out before plotting,
#'   and only `full_years_since == "0 - 999` data is used.
#'
#' @export
#' @param x `[list]` (mandatory, no default)
#'
#' the output of `compare_nordcan_statistics_table_lists`
plot_nordcan_statistics_table_comparisons <- function(x) {
  nordcan_version <- nordcancore::nordcan_metadata_nordcan_version()
  old_version <- x$version2compare
  version_tag <- paste0("_v", nordcan_version, "_vs_v", old_version)
  dbc::assert_user_input_is_list(x)
  dbc::assert_user_input_has_names(x, required_names = c("comparisons"))
  dbc::assert_user_input_is_list(x[["comparisons"]])
  x <- x$comparisons
  participant_info <- nordcancore::nordcan_metadata_participant_info()
  topregion_number <- participant_info$topregion_number
  dataset_names <- c("cancer_death_count_dataset", "cancer_record_count_dataset",
                     "prevalent_patient_count_dataset")
  dataset_names <- intersect(dataset_names, names(x))
  stratum_col_nm_set <- nordcancore::nordcan_metadata_column_name_set("column_name_set_stratum_column_name_set")

  entity_label <- nordcancore:::entity_label

  lapply(dataset_names, function(dataset_name) {
    dbc::assert_user_input_is_data.table_with_required_names(x = x[[dataset_name]],
                                                             x_nm = paste0("x$comparisons$", dataset_name),
                                                             required_names = c("sex", "entity", "stat_value"))
    dt <- x[[dataset_name]]
    subset <- dt[["region"]] == topregion_number
    is_prev <- dataset_name == "prevalent_patient_count_dataset"
    if (is_prev && "full_years_since_entry" %in% names(dt)) {
      subset <- subset & dt[["full_years_since_entry"]] == "0 - 999"
    }
    dt_stratum_col_nms <- c("year", "observation_year", "yoi", "entity")
    dt_stratum_col_nms <- dt_stratum_col_nms[dt_stratum_col_nms %in% names(dt)]
    dt <- dt[i = subset, j = lapply(.SD, sum), .SDcols = "stat_value",
             keyby = eval(dt_stratum_col_nms)]
    pdf_file_path <- paste0(nordcancore::get_global_nordcan_settings()[["work_dir"]],
                            "/", dataset_name, version_tag, ".pdf")
    pdf_file_path <- normalizePath(pdf_file_path, mustWork = FALSE)
    grDevices::pdf(pdf_file_path, width = 11.7*1.5, height = 8.3*1.5)
    entity_no_set <- sort(unique(dt$entity))
    entity_no_set_size <- length(entity_no_set)
    old_mar <- graphics::par("mar")
    old_mfrow <- graphics::par("mfrow")
    on.exit({
      grDevices::dev.off()
      graphics::par(mar = old_mar, mfrow = old_mfrow)
    })
    x_col_nm <- intersect(names(dt), c("year", "observation_year", "yoi"))[1L]

    graphics::par(omi = c(7,7,5, 3)/14, mar = c(3,2,3,1), mfrow = c(7, ceiling(entity_no_set_size/7)))

    lapply(entity_no_set, function(entity_no) {
      entity_dt <- dt[dt$entity == entity_no, ]
      txt <- ifelse(entity_no %in% entity_label$value,
                    paste(entity_label[entity_label$value == entity_no, c("value", "label")], collapse = ": "),
                    entity_no)


      plot(x = entity_dt[[x_col_nm]], y = entity_dt[["stat_value"]], cex = 0.5, lwd = 0.5,
           xlab = "", ylab = "")
      ## check if the title is too long, if it's too long, then break it into lines
      txt_1 <- strsplit(txt, " ")[[1]]
      txt_2 <- rep(" ", length(txt_1))
      for (i in 2:length(txt_1)) {
        n <- ifelse (length(which(txt_2 == "\n")) == 0, 1, max(which(txt_2 == "\n"))+1)
        tmp <- paste(paste(txt_1[n:i], txt_2[n:i], sep = ""), collapse = "")
        if (strwidth(tmp) > 60) { txt_2[i-1] <- "\n"}
      }
      txt_0 <- paste(paste(txt_1, txt_2, sep = ""), collapse = "")

      title(main = txt_0, cex.main = 1)
    })
    graphics::par(omi = rep(0,4), mar = c(5, 5, 2, 2), mfrow = c(1,1), new = TRUE)

    plot(0,0, xlab = "Year", ylab = "Difference", main = paste0(dataset_name, version_tag), axes = FALSE, type = "n")

    message("* saved plot grid of comparisons by entity for ",
            dataset_name, " to ", pdf_file_path)
  })
  return(invisible(NULL))

}

#' @title a wrapper for comparing current statistics with archived statistics
#'
#' @export
compare_nordcan_statistics <- function(stats_current, stats_archived, ds_nms) {

  if (!file.exists(stats_current)) {
    stop(sprintf("'stats_current' (%s) not exist!", stats_current))
  }

  if (!file.exists(stats_archived)) {
    stop(sprintf("'stats_archived' (%s) not exist!", stats_archived))
  }

  ## import old and new version of statistics tables
  old_statistics <- nordcanepistats::read_nordcan_statistics_tables(stats_archived)

  ## Read above saved results back into R.
  statistics <- readRDS(stats_current)

  ## Start the comparison
  comparison <- nordcanepistats::compare_nordcan_statistics_table_lists(
    current_stat_table_list = statistics[ds_nms],
    old_stat_table_list = old_statistics[ds_nms]
  )

  ## Version number to compared
  version2compare <- regmatches(stats_archived, regexec("[0-9]+[.][0-9]+", stats_archived))[[1]]
  stopifnot(numeric_version(version2compare) >= "9.0", numeric_version(version2compare) <= "9.10")
  comparison$version2compare <- version2compare

  ## Including undefined ICD version & codes
  if (file.exists("undefined_icd_version_and_codes.csv")) {
    undefined_icd <- data.table::fread(file_population)
    comparison$undefined_icd_version_and_codes <- undefined_icd
  }

  return(comparison)
}

