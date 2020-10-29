

#' @title NORDCAN Statistics Tables
#' @description
#' Compute all necessary statistics for NORDCAN into a single list of tables.
#' @param cancer_record_dataset `[data.table]` (mandatory, no default)
#'
#' as output by
#' [nordcanpreprocessing::nordcan_processed_cancer_record_dataset]
#'
#' @param general_population_size_dataset `[data.table]` (mandatory, no default)
#' the dataset of population sizes as per the call for data
#'
#' @param national_population_life_table `[data.table]` (mandatory, no default)
#' the life table as per the call for data
#'
#' @param cancer_death_count_dataset `[data.table]` (mandatory, no default)
#' dataset containing numbers of cancer deaths; see Details
#'
#' @param stata_exe_path `[character]` (mandatory, no default)
#'
#' pased to [nordcansurvival::nordcanstat_survival]
#' @param output_objects `[NULL, character]` (optional, default `NULL`)
#' this argument can be used to select which elements of the output list to
#' produce in this function.
#'
#' - `NULL`: no limit, i.e. produce everything
#' - `character`: vector of element names to produce (only); see section
#'   **Value** and
#'   `nordcan_statistics_tables_output_object_space()` for all options
#'
#' @details
#' You need to form `cancer_death_count_dataset` yourself using the raw data
#' you have using one of two methods.
#' If you have a dataset of cancer death counts as described in the call for
#' data, do
#'
#' ```
#' cdcd <- nordcanpreprocessing::nordcan_processed_cancer_death_count_dataset(
#'   my_raw_cdcd
#' )
#' ```
#'
#' where `my_raw_cdcd` is your dataset of cancer death counts as per the call
#' for data.
#'
#' If you want to compute the counts using your cancer record dataset, do
#'
#' ```
#' cdcd <- nordcanepistats::nordcanstat_count(
#'   processed_cancer_record_dataset,
#'   by = c("sex", "entity", "yoi", "region", "agegroup"),
#'   subset = died_from_cancer == TRUE
#' )
#' data.table::setnames(cdcd,
#'                      c("N", "yoi"), c("cancer_death_count", "year"))
#' ```
#'
#' where `processed_cancer_record_dataset` is your cancer record dataset after
#' processing
#' (see [nordcanpreprocessing::nordcan_processed_cancer_record_dataset]),
#' and in this example the information on who died of which cancer is identified
#' in the logical vector `died_from_cancer`, which you need to define. It should
#' be of length `nrow(processed_cancer_record_dataset)`. One person can
#' naturally only die once, so there can be at most one `TRUE` value per person.
#'
#' @eval nordcan_statistics_tables_output_objects_options()
#'
#' @examples
#'
#' \dontrun{
#' nc_stats <- nordcan_statistics_tables(
#'  cancer_record_dataset = crd,
#'  cancer_death_count_dataset = cdcd,
#'  general_population_size_dataset = gpsd,
#'  national_population_life_table = lt,
#'  stata_exe_path = "stata.exe"
#' )
#' }
#' @export
nordcan_statistics_tables <- function(
  cancer_record_dataset,
  cancer_death_count_dataset,
  general_population_size_dataset,
  national_population_life_table,
  stata_exe_path,
  output_objects = NULL
) {
  t_start <- proc.time()

  dbc::assert_user_input_is_one_of(
    output_objects,
    funs = list(dbc::assert_is_NULL, dbc::assert_is_character_nonNA_vector)
  )
  if (is.null(output_objects)) {
    output_objects <- nordcan_statistics_tables_output_object_space()
  }

  # dataset validation ---------------------------------------------------------
  message("* nordcanepistats::nordcan_statistics_tables: validating your ",
          "datasets...")
  dbc::assert_user_input_is_data.table_with_required_names(
    cancer_record_dataset,
    required_names = nordcancore::nordcan_metadata_column_name_set(
      "column_name_set_processed_cancer_record_dataset"
    )
  )
  if ("cancer_death_count_dataset" %in% output_objects) {
    nordcanpreprocessing::assert_dataset_is_valid(
      cancer_death_count_dataset,
      dataset_name = "processed_cancer_death_count_dataset"
    )
  }
  if ("survival_statistics_dataset" %in% output_objects) {
    dbc::assert_user_input_file_exists(stata_exe_path)
    nordcanpreprocessing::assert_dataset_is_valid(
      national_population_life_table,
      dataset_name = "national_population_life_table"
    )
  }
  if ("general_population_size_dataset" %in% output_objects) {
    nordcanpreprocessing::assert_dataset_is_valid(
      general_population_size_dataset,
      dataset_name = "general_population_size_dataset"
    )
  }
  message("* nordcanepistats::nordcan_statistics_tables: done.")


  # output list creation -------------------------------------------------------
  output <- list()
  if ("session_info" %in% output_objects) {
    output[["session_info"]] <- session_info()
  }
  if ("cancer_death_count_dataset" %in% output_objects) {
    output[["cancer_death_count_dataset"]] <- cancer_death_count_dataset
  }
  if ("general_population_size_dataset" %in% output_objects) {
    output[["general_population_size_dataset"]] <- general_population_size_dataset
  }

  # cancer_record_count_dataset ------------------------------------------------
  if ("cancer_record_count_dataset" %in% output_objects) {
    message("* nordcanepistats::nordcan_statistics_tables: started computing ",
            "cancer_record_count_dataset at ", as.character(Sys.time()), "...")
    t <- proc.time()
    output[["cancer_record_count_dataset"]] <- tryCatch(
      nordcanstat_count(
        x = cancer_record_dataset,
        by = c("yoi","sex","region","agegroup","entity"),
        subset = cancer_record_dataset[["excl_imp_total"]] == 0L
      ),
      error = function(e) e
    )
    message("* nordcanepistats::nordcan_statistics_tables: done computing ",
            "cancer_record_count_dataset; ",
            data.table::timetaken(t))
  }
  # prevalent_patient_count_dataset ------------------------------------------
  if ("prevalent_patient_count_dataset" %in% output_objects) {
    message("* nordcanepistats::nordcan_statistics_tables: started computing ",
            "prevalent_patient_count_dataset at ",
            as.character(Sys.time()), "...")
    t <- proc.time()
    output[["prevalent_patient_count_dataset"]] <- tryCatch(
      expr = nordcanstat_year_based_prevalent_subject_count(
        x = cancer_record_dataset,
        by = c("sex", "region", "agegroup", "entity"),
        subset = cancer_record_dataset[["excl_imp_total"]] == 0L
      ),
      error = function(e) e
    )
    message("* nordcanepistats::nordcan_statistics_tables: done computing ",
            "prevalent_patient_count_dataset; ",
            data.table::timetaken(t))
  }
  # imp_quality_statistics_dataset ---------------------------------------------
  if ("imp_quality_statistics_dataset" %in% output_objects) {
    message("* nordcanepistats::nordcan_statistics_tables: started computing ",
            "imp_quality_statistics_dataset at ",
            as.character(Sys.time()), "...")
    t <- proc.time()
    cdcd <- data.table::copy(cancer_death_count_dataset)
    min_period <- min(cancer_record_dataset$period, na.rm = TRUE)
    max_period <- max(cancer_record_dataset$period, na.rm = TRUE)
    period_breaks <- c(seq(min_period, max_period, 5L), Inf)
    cdcd[
      j = "period" := cut(
        x = cdcd$year, breaks = period_breaks, labels = FALSE, right = FALSE
      )
    ]
    cdcd[j = "period" := period_breaks[cdcd$period]]
    cdcd <- cdcd[!is.na(cdcd$period), ]
    cdcd[, "year" := NULL]
    cdcd <- cdcd[
      j = lapply(.SD, sum),
      .SDcols = "cancer_death_count",
      keyby = c("sex", "period", "entity", "region")
    ]
    output[["imp_quality_statistics_dataset"]] <- tryCatch(
      expr = nordcanstat_imp_quality(
        x = cancer_record_dataset,
        cancer_death_count_dataset = cdcd,
        by = c("sex", "period", "entity", "region")
      ),
      error = function(e) e
    )
    message("* nordcanepistats::nordcan_statistics_tables: done computing ",
            "imp_quality_statistics_dataset; ",
            data.table::timetaken(t))
  }

  # survival_quality_statistics_dataset ----------------------------------------
  if ("survival_quality_statistics_dataset" %in% output_objects) {
    message("* nordcanepistats::nordcan_statistics_tables: started computing ",
            "survival_quality_statistics_dataset at ",
            as.character(Sys.time()), "...")
    t <- proc.time()
    output[["survival_quality_statistics_dataset"]] <- tryCatch(
      expr = nordcanstat_survival_quality(
        x = cancer_record_dataset,
        by = c("sex", "period", "entity")
      ),
      error = function(e) e
    )
    message("* nordcanepistats::nordcan_statistics_tables: done computing ",
            "survival_quality_statistics_dataset; ",
            data.table::timetaken(t))
  }

  # survival_statistics_dataset ------------------------------------------------
  if ("stata_info" %in% output_objects) {
    message("* nordcanepistats::nordcan_statistics_tables: testing that you ",
            "can run stata...")
    output[["stata_info"]] <- tryCatch(
      nordcansurvival::get_stata_info(stata_exe_path),
      error = function(e) e
    )
    message("* nordcanepistats::nordcan_statistics_tables: done.")
  }


  if ("survival_statistics_example" %in% output_objects) {
    message("* nordcanepistats::nordcan_statistics_tables: testing that you ",
            "can run an example of nordcansurvival::survival_statistics...")
    ss_output <- tryCatch(
      expr = {
        infile <-  paste0(system.file(package = "nordcansurvival"),
                          "/stata/demo/NCS_NO_anonymous_example_data.dta")
        lifetable <- paste0(system.file(package = "nordcansurvival"),
                            "/stata/demo/NO_2018_lifetable.dta")
        nordcansurvival::survival_statistics(
          cancer_record_dataset_path  = infile ,
          national_population_life_table_path = lifetable,
          stata_exe_path = stata_exe_path
        )
      },
      error = function(e) e
    )
    ss_output_path <- "survival/survival_statistics_output.csv"
    if (!inherits(ss_output, "error") && file.exists(ss_output_path)) {
      ss_output <- data.table::fread(ss_output_path)
    }
    output[["survival_statistics_example"]] <- ss_output
    message("* nordcanepistats::nordcan_statistics_tables: done.")
  }

  if ("survival_statistics_dataset" %in% output_objects) {
    message("* nordcanepistats::nordcan_statistics_tables: started computing ",
            "survival_statistics_dataset at ",
            as.character(Sys.time()), "...")
    t <- proc.time()
    output[["survival_statistics_dataset"]] <- tryCatch(
      expr = nordcansurvival::nordcanstat_survival(
        cancer_record_dataset = cancer_record_dataset,
        national_population_life_table = national_population_life_table,
        stata_exe_path = stata_exe_path
      ),
      error = function(e) e
    )
    message("* nordcanepistats::nordcan_statistics_tables: done computing ",
            "survival_statistics_dataset; ",
            data.table::timetaken(t))
  }

  # final touches --------------------------------------------------------------
  message("* nordcanepistats::nordcan_statistics_tables: finished; ",
          data.table::timetaken(t_start))
  return(output)
}


#' @export
#' @rdname nordcan_statistics_tables
#' @details
#' `nordcan_statistics_tables_output_object_space` simply returns a character
#' vector of possible names of the list output by `nordcan_statistics_tables`
nordcan_statistics_tables_output_object_space <- function() {
  names(nordcan_statistics_tables_output_object_space_summaries())
}

nordcan_statistics_tables_output_object_space_summaries <- function() {
  c(
    "session_info" = c(
      "Log of current time and R session info as output by sessionInfo()"
    ),
    "cancer_death_count_dataset" = paste0(
      "The same dataset returned as-is as given to arg ",
      "cancer_death_count_dataset"
    ),
    "general_population_size_dataset" = paste0(
      "The same dataset returned as-is as given to arg ",
      "general_population_size_dataset"
    ),
    "cancer_record_count_dataset" = paste0(
      "Dataset of cancer record counts computed using ",
      "nordcanepistats::nordcanstat_count"
    ),
    "prevalent_patient_count_dataset" = paste0(
      "Dataset of prevalent cancer patients counts computed using ",
      "nordcanepistats::nordcanstat_year_based_prevalent_subject_count"
    ),
    "imp_quality_statistics_dataset" = paste0(
      "Quality statistics computed using ",
      "nordcanepistats::nordcanstat_imp_quality"
    ),
    "survival_quality_statistics_dataset" = paste0(
      "Quality statistics computed using ",
      "nordcanepistats::nordcanstat_survival_quality"
    ),
    "stata_info" = c(
      "Info about stata as returned by nordcansurvival::get_stata_info"
    ),
    "survival_statistics_example" = paste0(
      "Results from nordcansurvival::survival_statistics using an example ",
      "dataset stored into the nordcansurvival package"
    ),
    "survival_statistics_dataset" = c(
      "Results from nordcansurvival::nordcanstat_survival using your datasets"
    )
  )
}

nordcan_statistics_tables_output_objects_options <- function() {
  obj_summaries <- nordcan_statistics_tables_output_object_space_summaries()
  c(
    "@return",
    "The output is a list with elements: ",
    paste0(" - `\"", names(obj_summaries),"\"`: ", obj_summaries),
    "",
    "unless argument `output_objects` is used to limit the output elements."
  )
}



