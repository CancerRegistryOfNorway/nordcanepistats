

#' @title NORDCAN Statistics Payload
#' @description
#' Compute all necessary statistics for NORDCAN into a single payload.
#' @param datasets `[list]` (mandatory, no default)
#'
#' a list containing datasets. the following are mandatory:
#' - `cancer_record_dataset`: each row is a cancer record as output by
#'   [nordcanpreprocessing::nordcan_incidence_dataset]
#' - `population_mean_size_dataset`: see
#'   [nordcanpreprocessing::nordcan_population_mean_size_dataset]
#' - `population_mortality_dataset`: see
#'   [nordcanpreprocessing::nordcan_population_mortality_dataset]
#'
#' the following are optional and are computed within this function if not
#' supplied:
#' - `cancer_death_count_dataset`: contains the numbers of cancer deaths; see
#'   [nordcanpreprocessing::nordcan_cancer_death_count_dataset]
#' @examples
#'
#' nc_stats_payload <- nordcanepistats::nordcan_statistics_payload(
#'   datasets = list(
#'     cancer_record_dataset = my_crd,
#'     cancer_death_count_dataset = my_cdcd,
#'     population_mean_size_dataset = my_mps,
#'     population_mortality_dataset = my_md
#'   )
#' )
#' @export

nordcan_statistics_payload <- function(
  datasets
) {
  stopifnot(
    inherits(datasets, "list"),
    vapply(datasets, data.table::is.data.table, logical(1L)),
    !is.null(names(datasets)),
    names(datasets) %in% c(
      "cancer_record_dataset",
      "cancer_death_count_dataset",
      "population_mean_size_dataset",
      "population_mortality_dataset"
    ),
    c("cancer_record_dataset", "population_mean_size_dataset",
      "population_mortality_dataset") %in% names(datasets)
  )
  # TODO in the future: check each individual dataset before use using
  # nordcanpreprocessing functions

  # TODO for sasha:
  # - if cancer_death_count_dataset is not given in datasets, emit a message
  #   about it and compute the dataset inside this function
  # - applying basicepistats::stat_table_list and the stat functions in
  #   nordcanepistats, write functionality to create lists of stats tables
  #   (one for incidence, one for mortality if needed, one for prevalence,
  #    one for survival, one or more for survival quality)
  # - you will probably need to update basicepistats funs to look like
  #   basicepistats functions more closely
  # - at the end of the function combine the all tables by theme (incidence,
  #   mortality) into as many tables as there are themes. each of them should
  #   contain the information as specified on our wiki, e.g.
  #   see https://github.com/CancerRegistryOfNorway/NORDCAN/wiki/Module-IMP-quality
  #   for survival quality
  # - you don't have a dataset to test with at this point
  # - if any functionality is missing in one of the nordcanstat_*
  #   functions or elsewhere, let me know


  payload <- list(
    # fill here also
  )
  stopifnot(
    c("cancer_case_count_dataset", "cancer_death_count_dataset",
      "cancer_survival_dataset", "cancer_survival_quality_dataset") %in%
      names(payload),
    inherits(payload, "list"),
    vapply(payload, inherits, logical(1L), what = "data.table")
  )
  return(payload)
}

