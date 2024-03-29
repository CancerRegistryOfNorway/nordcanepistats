% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/counts.R
\name{nordcanstat_count}
\alias{nordcanstat_count}
\title{Cancer Record Counts}
\usage{
nordcanstat_count(
  x,
  by = NULL,
  entities = NULL,
  subset = NULL,
  subset_style = "zeros"
)
}
\arguments{
\item{x}{\verb{[data.table]} (mandatory, no default)

the NORDCAN cancer record dataset after preprocessing; see also the
underlying implementation \link[basicepistats:counts]{basicepistats::stat_count}}

\item{by}{see \link[basicepistats:counts]{basicepistats::stat_count}

exceptions to \verb{[basicepistats::stat_count]}:
\itemize{
\item if \code{by} is a character vector of column names in \code{x}, the allowed
combinations of those columns are retrieved using
\verb{[nordcancore::nordcan_metadata_column_level_space_dt]}, and the output
will include all of these combinations
\item if \code{by} is a character vector and one element of it is
\code{"entity"}, then output is also stratified by entity number
(the entity numbers will be in column \code{entity} in output); the
entities to compute results by are supplied via \code{entities}
\item if \code{by} is a character vector and one element of it is
\code{"region"}, then output is also stratified by region number, i.e. by
every sub-region (if applicable) and also the top region
}}

\item{entities}{\verb{[NULL, integer]} (optional, default \code{NULL})

see \code{by} for how to stratify output by entity number.
\itemize{
\item \code{NULL}: output of
\code{nordcancore::nordcan_metadata_entity_no_set("all")}
is used
\item \code{integer}: these entity numbers are used
}}

\item{subset}{see \link[basicepistats:counts]{basicepistats::stat_count}}

\item{subset_style}{see \link[basicepistats:counts]{basicepistats::stat_count}}
}
\description{
Compute counts of cancer records.
}
\seealso{
Other nordcanstat: 
\code{\link{nordcanstat_imp_quality}()},
\code{\link{nordcanstat_survival_quality}()},
\code{\link{nordcanstat_year_based_prevalent_patient_count}()}
}
\concept{nordcanstat}
