

#' @title NORDCAN Statistics Tables
#'
#' @description
#' Compute all necessary statistics for NORDCAN into a single list of tables.
#' @param cancer_record_dataset `[data.table]` (mandatory, no default)
#'   as output by [nordcanpreprocessing::nordcan_processed_cancer_record_dataset]
#'
#' @param general_population_size_dataset `[data.table]` (mandatory, no default)
#'   the dataset of population sizes as per the call for data
#'
#' @param national_population_life_table `[data.table]` (mandatory, no default)
#' the life table as per the call for data
#'
#' @param cancer_death_count_dataset `[data.table]` (mandatory, no default)
#' dataset containing numbers of cancer deaths; see Details
#'
#' @param stata_exe_path `[character]` (mandatory, no default)
#'   pased of Stata for conducting survival analysis
#'
#' @param output_objects `[NULL, character]` (optional, default `NULL`)
#'   this argument can be used to select which elements of the output list to
#'   produce in this function.
#'
#' - `NULL`: no limit, i.e. produce everything
#' - `character`: vector of element names to produce (only); see section
#'   **Value** and
#'   `nordcan_statistics_tables_output_object_space()` for all options
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
  output_objects = NULL,
  subset = NULL,
  survival_test_sample = FALSE,
  survival_trace = FALSE
) {
  t_start <- proc.time()

  ## Check output_objects is valid.
  if (is.null(output_objects)) {
    output_objects <- nordcan_statistics_tables_output_object_space()
  } else {
    dbc::assert_is_character_nonNA_vector(output_objects)
    dbc::assert_vector_elems_are_in_set(
      output_objects,
      set = nordcan_statistics_tables_output_object_space()
    )
  }

  ## dataset validation
  message("* validating your datasets...")
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
  if (any(grepl("survival_statistics", output_objects))) {
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
  message("* validation finished.")


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
    message("*  started computing 'cancer_record_count_dataset' at ",
            as.character(Sys.time()), "...")
    t <- proc.time()
    output[["cancer_record_count_dataset"]] <- tryCatch(
      nordcanstat_count(
        x = cancer_record_dataset,
        by = c("yoi","sex","region","agegroup","entity"),
        subset = cancer_record_dataset[["excl_imp_total"]] == 0L
      ),
      error = function(e) e
    )
    message("* finished computing 'cancer_record_count_dataset'; time used: ",
            gsub("elapsed.*", "", data.table::timetaken(t)))
  }
  # prevalent_patient_count_dataset ------------------------------------------
  if ("prevalent_patient_count_dataset" %in% output_objects) {
    message("*  started computing 'prevalent_patient_count_dataset' at ",
            as.character(Sys.time()), "...")
    t <- proc.time()

    output[["prevalent_patient_count_dataset"]] <- tryCatch({
      tmp <- nordcanepistats::nordcanstat_year_based_prevalent_patient_count(
        x = cancer_record_dataset,
        by = c("sex", "region", "yob", "entity"),
        subset = cancer_record_dataset[["excl_imp_total"]] == 0L
      )
      tmp[, agegroup := floor((observation_year - yob)/5)+1]
      tmp[agegroup >= 18, agegroup := 18]
      tmp <- tmp[, .(prevalent_patient_count = sum(prevalent_patient_count)), by = .(
        sex, region, agegroup, entity, observation_year, full_years_since_entry
      )]
      tmp <- tmp[agegroup >= 1, ]
      tmp
    }, error = function(e) e )

    # output[["prevalent_patient_count_dataset"]] <- tryCatch(
    #   expr = nordcanstat_year_based_prevalent_patient_count(
    #     x = cancer_record_dataset,
    #     by = c("sex", "region", "agegroup", "entity"),
    #     subset = cancer_record_dataset[["excl_imp_total"]] == 0L
    #   ),
    #   error = function(e) e
    # )

    message("* finished computing 'prevalent_patient_count_dataset'; time used: ",
            gsub("elapsed.*", "", data.table::timetaken(t)))
  }
  # imp_quality_statistics_dataset ---------------------------------------------
  if ("imp_quality_general_statistics_dataset" %in% output_objects) {
    message("*  started computing 'imp_quality_general_statistics_dataset' at ",
            as.character(Sys.time()), "...")
    t <- proc.time()
    cdcd <- data.table::copy(cancer_death_count_dataset)
    min_period_5 <- min(cancer_record_dataset$period_5, na.rm = TRUE)
    max_period_5 <- max(cancer_record_dataset$period_5, na.rm = TRUE)
    period_5_breaks <- c(seq(min_period_5, max_period_5, 5L), Inf)
    cdcd[
      j = "period_5" := cut(
        x = cdcd$year, breaks = period_5_breaks, labels = FALSE, right = FALSE
      )
    ]
    cdcd[j = "period_5" := period_5_breaks[cdcd$period_5]]
    cdcd <- cdcd[!is.na(cdcd$period_5), ]
    cdcd[, "year" := NULL]
    cdcd <- cdcd[
      j = lapply(.SD, sum),
      .SDcols = "cancer_death_count",
      keyby = c("sex", "period_5", "entity", "region")
    ]
    output[["imp_quality_general_statistics_dataset"]] <- tryCatch(
      expr = nordcanstat_imp_quality(
        x = cancer_record_dataset,
        cancer_death_count_dataset = cdcd,
        by = c("sex", "period_5", "entity", "region"), type = "general"
      ),
      error = function(e) e
    )
    message("* finished computing 'imp_quality_general_statistics_dataset'; time used: ",
            gsub("elapsed.*", "", data.table::timetaken(t)))
  }

  if ("imp_quality_exclusion_statistics_dataset" %in% output_objects) {
    message("*  started computing 'imp_quality_exclusion_statistics_dataset' at ",
            as.character(Sys.time()), "...")
    t <- proc.time()
    cdcd <- data.table::copy(cancer_death_count_dataset)
    min_period_5 <- min(cancer_record_dataset$period_5, na.rm = TRUE)
    max_period_5 <- max(cancer_record_dataset$period_5, na.rm = TRUE)
    period_5_breaks <- c(seq(min_period_5, max_period_5, 5L), Inf)
    cdcd[
      j = "period_5" := cut(
        x = cdcd$year, breaks = period_5_breaks, labels = FALSE, right = FALSE
      )
    ]
    cdcd[j = "period_5" := period_5_breaks[cdcd$period_5]]
    cdcd <- cdcd[!is.na(cdcd$period_5), ]
    cdcd[, "year" := NULL]
    cdcd <- cdcd[
      j = lapply(.SD, sum),
      .SDcols = "cancer_death_count",
      keyby = c("sex", "period_5", "entity", "region")
    ]
    output[["imp_quality_exclusion_statistics_dataset"]] <- tryCatch(
      expr = nordcanstat_imp_quality(
        x = cancer_record_dataset,
        cancer_death_count_dataset = cdcd,
        by = c("sex", "period_5", "entity", "region"), type = "exclusion"
      ),
      error = function(e) e
    )
    message("* finished computing 'imp_quality_exclusion_statistics_dataset'; time used: ",
            gsub("elapsed.*", "", data.table::timetaken(t)))
  }

  # survival_quality_statistics_dataset ----------------------------------------
  if ("survival_quality_statistics_dataset" %in% output_objects) {
    message("*  started computing 'survival_quality_statistics_dataset' at ",
            as.character(Sys.time()), "...")
    t <- proc.time()
    output[["survival_quality_statistics_dataset"]] <- tryCatch(
      expr = nordcanstat_survival_quality(
        x = cancer_record_dataset,
        by = c("sex", "period_5", "entity")
      ),
      error = function(e) e
    )
    message("* finished computing 'survival_quality_statistics_dataset'; time used: ",
            gsub("elapsed.*", "", data.table::timetaken(t)))
  }

  stata_exist <- tryCatch(
    dbc::assert_user_input_file_exists(stata_exe_path),
    error = function(e) e
  )
  if (!inherits(stata_exist, "error")) {stata_exist <- TRUE}


  # survival_statistics_dataset ------------------------------------------------
  if ("stata_info" %in% output_objects) {
    message("* testing that you can run stata...")
    if(is.logical(stata_exist) ) {
      output[["stata_info"]] <- tryCatch(
        nordcansurvival::get_stata_info(stata_exe_path),
        error = function(e) e
      )
      message("* yes!")
    } else {message(stata_exist)}
  }


  if ("survival_statistics_example" %in% output_objects) {
    message("* testing that it can run the example of 'nordcansurvival' package.")
    if(is.logical(stata_exist) ) {
      ss_output <- tryCatch(
        expr = {
          infile <-  paste0(system.file(package = "nordcansurvival"),
                            "/stata/demo/NCS_NO_anonymous_example_data.dta")
          lifetable <- paste0(system.file(package = "nordcansurvival"),
                              "/stata/demo/NO_2018_lifetable.dta")
          nordcansurvival::survival_statistics(
            infile = infile ,
            lifetable = lifetable,
            stata_exe_path = stata_exe_path,
            standstrata = "agegroup_ICSS",
            iweight = "weights_ICSS",
            by = c("entity", "sex", "period")
          )
        },
        error = function(e) e
      )
      wd <- nordcancore::get_global_nordcan_settings()[["work_dir"]]
      ss_output_path <- paste0(
        wd,
        "/survival/NCS_NO_anonymous_example_data_result_dir/",
        "NCS_NO_anonymous_example_data_result.csv"
      )

      if (!inherits(ss_output, "error") && file.exists(ss_output_path)) {
        ss_output <- data.table::fread(ss_output_path)
        output[["survival_statistics_example"]] <- ss_output
        message("* yes!")
      } else {
        message("* oops! it failed!!")
      }
    } else {message(stata_exist)}


  }


  if (any(grepl("survivaltime", output_objects))) {
    message("*  started computing 'survival_statistics_period_5/10_dataset' at ",
            as.character(Sys.time()), "...")

    surv_ds_nms <- output_objects[grepl("survivaltime", output_objects)]

    if(is.logical(stata_exist) ) {
      t <- proc.time()
      surv_output <- tryCatch(
        expr = nordcansurvival::nordcanstat_survival(
          cancer_record_dataset = cancer_record_dataset,
          national_population_life_table = national_population_life_table,
          stata_exe_path = stata_exe_path,
          surv_ds_nms = surv_ds_nms,
          subset = subset,
          survival_test_sample = survival_test_sample,
          survival_trace = survival_trace
        ),
        error = function(e) e
      )
      if (inherits(surv_output, c("error", "try-error"))) {
        surv_output <- list(surv_output, surv_output)
        names(surv_output) <- surv_ds_nms
      }
      output[surv_ds_nms] <- surv_output
      message("* finished computing 'survival_statistics_period_*_dataset'; time used: ",
              gsub("elapsed.*", "", data.table::timetaken(t)))
    } else {message(stata_exist)}

  }



  ## Checking whether there is any error in 'output'
  message("*  started checking if there is any error in calculated result.")
  for (elem_nm in names(output))  {
    elem <- output[[elem_nm]]
    if (inherits(elem, "error")) {
      message("ERROR: could not produce result ", deparse(elem_nm), "; please ",
              "report the error printed below to the NORDCAN R framework ",
              "maintainers (unless you can see that you have made some mistake)")
      str(elem)
      NULL
    }
  }
  message("*  finished checking if there is any error in calculated result.")

  # final touches --------------------------------------------------------------
  message("--- All processes finished. Total time used: ",
          gsub("elapsed.*", "", data.table::timetaken(t_start)))

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
      "nordcanepistats::nordcanstat_year_based_prevalent_patient_count"
    ),
    "imp_quality_general_statistics_dataset" = paste0(
      "Quality statistics computed using ",
      "nordcanepistats::nordcanstat_imp_quality"
    ),
    "imp_quality_exclusion_statistics_dataset" = paste0(
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
    # "survival_statistics_example" = paste0(
    #   "Results from nordcansurvival::survival_statistics using an example ",
    #   "dataset stored into the nordcansurvival package"
    # ),


    "survival_statistics_agestandardised_survivaltime_05_period_05" = ' 5 year "age_standarized" survival statistics based on   5-year period;',
    "survival_statistics_agestandardised_survivaltime_05_period_10" = ' 5 year "age_standarized" survival statistics based on  10-year period;',
    "survival_statistics_agestandardised_survivaltime_10_period_05" = '10 year "age_standarized" survival statistics based on   5-year period;',
    "survival_statistics_agestandardised_survivaltime_10_period_10" = '10 year "age_standarized" survival statistics based on  10-year period;',

    "survival_statistics_agespecific_survivaltime_05_period_05" = ' 5 year "age_specific" survival statistics based on   5-year period;',
    "survival_statistics_agespecific_survivaltime_05_period_10" = ' 5 year "age_specific" survival statistics based on  10-year period;',
    "survival_statistics_agespecific_survivaltime_10_period_05" = '10 year "age_specific" survival statistics based on   5-year period;',
    "survival_statistics_agespecific_survivaltime_10_period_10" = '10 year "age_specific" survival statistics based on  10-year period;'


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



