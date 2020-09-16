
#' @title NORDCAN Statistics Payload
#' @description
#' Compute all necessary statistics for NORDCAN into a single payload.
#' @param datasets `[list]` (mandatory, no default)
#'
#' a list containing the following datasets:
#' - `cancer_record_dataset`: as output by
#'   [nordcanpreprocessing::nordcan_processed_cancer_record_dataset]
#' - `general_population_size_dataset`: the dataset of population sizes
#'   as per the call for data
#' - `national_population_life_table`: the life table as per the call for data
#' - `cancer_death_count_dataset`: dataset containing numbers of cancer deaths;
#'   see Details
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
#'   datasets = list(
#'     cancer_record_dataset = crd,
#'     cancer_death_count_dataset = cdcd,
#'     general_population_size_dataset = gpsd,
#'     national_population_life_table = lt
#'   )
#' )
#' }
#' @export

# ####data
# cancer_death_count_dataset=data.table::data.table(read.csv("Cancer_death_count_dataset.csv"))
# cancer_record_dataset=data.table::data.table(read.csv("Cancer_record_dataset.csv"))
# cancer_case_dataset=data.table::data.table(read.csv("enriched.csv"))

nordcan_statistics_tables <- function(
  datasets = list(
    cancer_death_count_dataset=cancer_death_count_dataset,
    cancer_record_dataset=cancer_record_dataset
  )
) {
  dbc::assert_dev_input_is_uniquely_named_list(datasets)
  dbc::assert_dev_input_has_names(
    datasets,
    required_names = nordcan_statistics_input_names()
  )
  lapply(nordcan_statistics_tables_input_names(), function(dataset_name) {
    if (dbc::get_dev_mode() == TRUE) {
      nordcanpreprocessing::assert_dataset_is_valid(
        x = datasets[[dataset_name]], dataset_name = dataset_name
      )
    }
    NULL
  })

  payload <- list(
    cancer_death_count_dataset = cancer_death_count_dataset,
    cancer_case_count_dataset = nordcanstat_count(
      x = cancer_case_dataset,
      by = c("yoi","sex","region","agegroup","entity")
    ),
    prevalent_cancer_patient_count_dataset = nordcanstat_year_based_prevalent_subject_count(
      x = cancer_case_dataset, by = c("sex", "region", "agegroup", "entity")
    ),
    general_population_size_dataset = datasets[["general_population_size_dataset"]]
  )

  dbc::assert_dev_output_is_uniquely_named_list(payload)
  dbc::assert_dev_output_has_names(
    payload,
    required_names = nordcan_statistics_tables_output_names()
  )
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
    "general_population_size_dataset")
}
