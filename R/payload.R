
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

# ####working directory
# setwd("")
#  
# ####packages
# library(basicepistats)
#
# ####data
#
# ###mortality
# cancer_death_count_dataset=data.table::data.table(read.csv("Cancer_death_count_dataset.csv"))
# cancer_record_dataset=data.table::data.table(read.csv("Cancer_record_dataset.csv"))
# 
# ###incidence
# cancer_case_dataset=data.table::data.table(read.csv("Cancer_case_dataset.csv"))
# cancer_case_dataset$date_of_incidence=as.Date(cancer_case_dataset$date_of_incidence,"%d.%m.%Y")
# cancer_case_dataset$end_of_followup=as.Date(cancer_case_dataset$end_of_followup,"%d.%m.%Y")

nordcan_statistics_payload <- function(
  datasets=list(
    cancer_death_count_dataset=cancer_death_count_dataset,
    cancer_record_dataset=cancer_record_dataset,
    cancer_case_dataset=cancer_case_dataset
  )
) {
  
  stopifnot(
    inherits(datasets, "list"),
    vapply(datasets, data.table::is.data.table, logical(1L)),
    !is.null(names(datasets)),
    names(datasets) %in% c("cancer_death_count_dataset","cancer_record_dataset","cancer_case_dataset"),
    c("cancer_record_dataset","cancer_case_dataset") %in% names(datasets)
  )
  
  '%ni%'=Negate('%in%')
  if('cancer_death_count_dataset' %in% names(datasets)){
    message("cancer_death_count_dataset is given in datasets and 
            need not to be computed inside this function!")
  } 
  else if('cancer_death_count_dataset' %ni% names(datasets)){
    cancer_death_count_dataset=unique(
      basicepistats::stat_count(x=cancer_record_dataset,by=cancer_record_dataset)
    )
  }
  
  payload=list(
    cancer_death_count_dataset=cancer_death_count_dataset,
    cancer_case_count_dataset=data.table::as.data.table(basicepistats::stat_table_list(
      varying_arg_list = list(
        by = list(c("year","sex","region","age","entity"))
      ),
      fixed_arg_list = list(x = cancer_case_dataset),
      stat_fun_nm = "stat_count"
    )),
  cancer_prevalence=data.table::as.data.table(nordcanepistats::nordcanstat_prevalent_subject_count(cancer_case_dataset))
  )
  
  stopifnot(
    c("cancer_death_count_dataset","cancer_case_count_dataset")
    %in%
      names(payload),
    inherits(payload, "list"),
    vapply(payload, inherits, logical(1L), what = "data.table")
  )
  return(payload)
}
