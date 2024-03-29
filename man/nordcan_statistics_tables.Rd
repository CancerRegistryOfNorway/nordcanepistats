% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nordcan_statistics_tables.R
\name{nordcan_statistics_tables}
\alias{nordcan_statistics_tables}
\alias{nordcan_statistics_tables_output_object_space}
\title{NORDCAN Statistics Tables}
\usage{
nordcan_statistics_tables(
  cancer_record_dataset,
  cancer_death_count_dataset,
  general_population_size_dataset,
  national_population_life_table,
  stata_exe_path,
  output_objects = NULL,
  subset = NULL,
  survival_test_sample = FALSE,
  survival_trace = FALSE
)

nordcan_statistics_tables_output_object_space()
}
\arguments{
\item{cancer_record_dataset}{\verb{[data.table]} (mandatory, no default)
as output by \link[nordcanpreprocessing:nordcan_processed_cancer_record_dataset]{nordcanpreprocessing::nordcan_processed_cancer_record_dataset}}

\item{cancer_death_count_dataset}{\verb{[data.table]} (mandatory, no default)
dataset containing numbers of cancer deaths; see Details}

\item{general_population_size_dataset}{\verb{[data.table]} (mandatory, no default)
the dataset of population sizes as per the call for data}

\item{national_population_life_table}{\verb{[data.table]} (mandatory, no default)
the life table as per the call for data}

\item{stata_exe_path}{\verb{[character]} (mandatory, no default)
pased of Stata for conducting survival analysis}

\item{output_objects}{\verb{[NULL, character]} (optional, default \code{NULL})
this argument can be used to select which elements of the output list to
produce in this function.
\itemize{
\item \code{NULL}: no limit, i.e. produce everything
\item \code{character}: vector of element names to produce (only); see section
\strong{Value} and
\code{nordcan_statistics_tables_output_object_space()} for all options
}}
}
\value{
The output is a list with elements:
\itemize{
\item \code{"session_info"}: Log of current time and R session info as output by sessionInfo()
\item \code{"cancer_death_count_dataset"}: The same dataset returned as-is as given to arg cancer_death_count_dataset
\item \code{"general_population_size_dataset"}: The same dataset returned as-is as given to arg general_population_size_dataset
\item \code{"cancer_record_count_dataset"}: Dataset of cancer record counts computed using nordcanepistats::nordcanstat_count
\item \code{"prevalent_patient_count_dataset"}: Dataset of prevalent cancer patients counts computed using nordcanepistats::nordcanstat_year_based_prevalent_patient_count
\item \code{"imp_quality_general_statistics_dataset"}: Quality statistics computed using nordcanepistats::nordcanstat_imp_quality
\item \code{"imp_quality_exclusion_statistics_dataset"}: Quality statistics computed using nordcanepistats::nordcanstat_imp_quality
\item \code{"survival_quality_statistics_dataset"}: Quality statistics computed using nordcanepistats::nordcanstat_survival_quality
\item \code{"stata_info"}: Info about stata as returned by nordcansurvival::get_stata_info
\item \code{"survival_statistics_agestandardised_survivaltime_05_period_05"}:  5 year "age_standarized" survival statistics based on   5-year period;
\item \code{"survival_statistics_agestandardised_survivaltime_05_period_10"}:  5 year "age_standarized" survival statistics based on  10-year period;
\item \code{"survival_statistics_agestandardised_survivaltime_10_period_05"}: 10 year "age_standarized" survival statistics based on   5-year period;
\item \code{"survival_statistics_agestandardised_survivaltime_10_period_10"}: 10 year "age_standarized" survival statistics based on  10-year period;
\item \code{"survival_statistics_agespecific_survivaltime_05_period_05"}:  5 year "age_specific" survival statistics based on   5-year period;
\item \code{"survival_statistics_agespecific_survivaltime_05_period_10"}:  5 year "age_specific" survival statistics based on  10-year period;
\item \code{"survival_statistics_agespecific_survivaltime_10_period_05"}: 10 year "age_specific" survival statistics based on   5-year period;
\item \code{"survival_statistics_agespecific_survivaltime_10_period_10"}: 10 year "age_specific" survival statistics based on  10-year period;
}

unless argument \code{output_objects} is used to limit the output elements.
}
\description{
Compute all necessary statistics for NORDCAN into a single list of tables.
}
\details{
\code{nordcan_statistics_tables_output_object_space} simply returns a character
vector of possible names of the list output by \code{nordcan_statistics_tables}
}
\examples{

\dontrun{
nc_stats <- nordcan_statistics_tables(
 cancer_record_dataset = crd,
 cancer_death_count_dataset = cdcd,
 general_population_size_dataset = gpsd,
 national_population_life_table = lt,
 stata_exe_path = "stata.exe"
)
}
}
