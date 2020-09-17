
#' @title NORDCAN Statistics Payload
#' @description
#' Compute all necessary statistics for NORDCAN into a single payload.
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
#'                      c("N", "yoi"), c("death_count", "year"))
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

# ####data
# cancer_death_count_dataset=data.table::data.table(read.csv("Cancer_death_count_dataset.csv"))
# cancer_record_dataset=data.table::data.table(read.csv("Cancer_record_dataset.csv"))
# cancer_case_dataset=data.table::data.table(read.csv("enriched.csv"))

#' @importFrom data.table timetaken
#' @importFrom dbc assert_user_input_is_data.table
#' @importFrom nordcansurvival get_stata_info, nordcanstat_survival
nordcan_statistics_tables <- function(
  cancer_record_dataset,
  cancer_death_count_dataset,
  general_population_size_dataset,
  national_population_life_table,
  stata_exe_path
) {
  t_start <- proc.time()

  message("* nordcanepistats::nordcan_statistics_tables: validating your ",
          "datasets...")
  dbc::assert_user_input_is_data.table_with_required_names(
    cancer_record_dataset,
    required_names = nordcancore::nordcan_metadata_column_name_set(
      "column_name_set_processed_cancer_record_dataset"
    )
    )
  dbc::assert_user_input_file_exists(stata_exe_path)
  nordcanpreprocessing::assert_dataset_is_valid(
    cancer_death_count_dataset,
    dataset_name = "processed_cancer_death_count_dataset"
  )
  nordcanpreprocessing::assert_dataset_is_valid(
    general_population_size_dataset,
    dataset_name = "general_population_size_dataset"
  )
  nordcanpreprocessing::assert_dataset_is_valid(
    national_population_life_table,
    dataset_name = "national_population_life_table"
  )
  message("* nordcanepistats::nordcan_statistics_tables: done.")


  payload <- list(
    cancer_death_count_dataset = cancer_death_count_dataset,
    general_population_size_dataset = general_population_size_dataset
  )

  message("* nordcanepistats::nordcan_statistics_tables: testing that you ",
          "can run stata...")
  stata_info <- nordcansurvival::get_stata_info(stata_exe_path)
  payload[["stata_info"]] <- stata_info
  message("* nordcanepistats::nordcan_statistics_tables: succeeded.")

  # cancer_case_count_dataset --------------------------------------------------
  message("* nordcanepistats::nordcan_statistics_tables: started computing ",
          "cancer_case_count_dataset at ", as.character(Sys.time()), "...")
  t <- proc.time()
  payload[["cancer_case_count_dataset"]] <- tryCatch(nordcanstat_count(
    x = cancer_record_dataset,
    by = c("yoi","sex","region","agegroup","entity")
  ), error = function(e) e)
  message("* nordcanepistats::nordcan_statistics_tables: done computing ",
          "cancer_case_count_dataset; ",
          data.table::timetaken(t))

  # prevalent_cancer_patient_count_dataset -------------------------------------
  message("* nordcanepistats::nordcan_statistics_tables: started computing ",
          "prevalent_cancer_patient_count_dataset at ",
          as.character(Sys.time()), "...")
  t <- proc.time()
  dt <- tryCatch(nordcanstat_year_based_prevalent_subject_count(
    x = cancer_record_dataset, by = c("sex", "region", "agegroup", "entity")
  ), error = function(e) e)
  payload[["prevalent_cancer_patient_count_dataset"]] <- dt
  message("* nordcanepistats::nordcan_statistics_tables: done computing ",
          "prevalent_cancer_patient_count_dataset; ",
          data.table::timetaken(t))

  # survival_quality_dataset ---------------------------------------------------
  message("* nordcanepistats::nordcan_statistics_tables: started computing ",
          "survival_quality_dataset at ",
          as.character(Sys.time()), "...")
  t <- proc.time()
  dt <- tryCatch(nordcanstat_survival_quality(
    x = cancer_record_dataset, by = c("sex", "period", "agegroup", "entity")
  ), error = function(e) e)
  payload[["survival_quality_dataset"]] <- dt
  message("* nordcanepistats::nordcan_statistics_tables: done computing ",
          "survival_quality_dataset; ",
          data.table::timetaken(t))


  # survival_dataset -----------------------------------------------------------
  # message("* nordcanepistats::nordcan_statistics_tables: started computing ",
  #         "survival_dataset at ",
  #         as.character(Sys.time()), "...")
  # t <- proc.time()
  # dt <- tryCatch(nordcansurvival::nordcanstat_survival(
  #   cancer_record_dataset = cancer_record_dataset,
  #   national_population_life_table = national_population_life_table,
  #   stata_exe_path = stata_exe_path
  # ), error = function(e) e)
  # payload[["survival_dataset"]] <- dt
  # message("* nordcanepistats::nordcan_statistics_tables: done computing ",
  #         "survival_dataset; ",
  #         data.table::timetaken(t))

  # final touches --------------------------------------------------------------
  dbc::assert_dev_output_is_uniquely_named_list(payload)
  dbc::assert_dev_output_has_names(
    payload,
    required_names = nordcan_statistics_tables_output_names()
  )
  message("* nordcanepistats::nordcan_statistics_tables: finished; ",
          data.table::timetaken(t_start))
  return(payload)
}



nordcan_statistics_tables_input_names <- function() {
  c("cancer_record_dataset",
    "cancer_death_count_dataset",
    "general_population_size_dataset")
}

nordcan_statistics_tables_output_names <- function() {
  c("cancer_death_count_dataset",
    "cancer_case_count_dataset",
    "prevalent_cancer_patient_count_dataset",
    "survival_quality_dataset",
    # "survival_dataset",
    "general_population_size_dataset")
}
