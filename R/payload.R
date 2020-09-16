
#' @title NORDCAN Statistics Payload
#' @description
#' Compute all necessary statistics for NORDCAN into a single payload.
#' @param datasets `[list]` (mandatory, no default)
#'
#' a list containing the followin datasets:
#' - `cancer_record_dataset`: as output by
#'   [nordcanpreprocessing::nordcan_processed_cancer_record_dataset]
#' - `general_population_size_dataset`: see
#'   [nordcanpreprocessing::nordcan_general_population_size_dataset]
#' - `national_population_life_table`: see
#'   [nordcanpreprocessing::nordcan_national_population_life_table]
#' - `cancer_death_count_dataset`: dataset containing numbers of cancer deaths;
#'   the creation of this dataset is explained in
#'   [nordcanpreprocessing::nordcan_processed_cancer_death_count_dataset]
#'
#' @examples
#'
#' nc_stats <- nordcan_statistics_tables(
#'   datasets = list(
#'     cancer_record_dataset = my_crd,
#'     cancer_death_count_dataset = my_cdcd,
#'     general_population_size_dataset = my_gpsd,
#'     national_population_life_table = my_lt
#'   )
#' )
#' @export

# ####data
# cancer_death_count_dataset=data.table::data.table(read.csv("Cancer_death_count_dataset.csv"))
# cancer_record_dataset=data.table::data.table(read.csv("Cancer_record_dataset.csv"))
# cancer_case_dataset=data.table::data.table(read.csv("enriched.csv"))

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
