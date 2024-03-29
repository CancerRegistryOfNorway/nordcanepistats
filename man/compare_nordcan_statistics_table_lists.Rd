% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_statistics_tables.R
\name{compare_nordcan_statistics_table_lists}
\alias{compare_nordcan_statistics_table_lists}
\alias{plot_nordcan_statistics_table_comparisons}
\title{Compare NORDCAN Statistics}
\usage{
compare_nordcan_statistics_table_lists(
  current_stat_table_list,
  old_stat_table_list
)

plot_nordcan_statistics_table_comparisons(x)
}
\arguments{
\item{current_stat_table_list}{\verb{[list]} (mandatory, no default)

list of statistics tables; current version; intended for the output of
\link{nordcan_statistics_tables}}

\item{old_stat_table_list}{\verb{[list]} (mandatory, no default)

list of statistics tables; current version; intended for the output of
\link{nordcan_statistics_tables} from a previous run}

\item{x}{\verb{[list]} (mandatory, no default)

the output of \code{compare_nordcan_statistics_table_lists}}
}
\value{
A list with elements
\itemize{
\item \code{summary}: a data.table that summarizes every comparison performed,
where the summaries are stratified by table name and statistic column name
\item \code{comparisons}: a list of data.tables, where one row in a data.table
corresponds to one comparison between an old and a new statistic.
}
}
\description{
Compare statistics between two versions of NORDCAN.
}
\details{
Any comparisons have been defined on the basis of the name of the table,
i.e. \code{cancer_record_count_dataset} can have a different set of comparisons
than \code{survival_statistics_dataset}.

All comparisons between two statistics from the current and old version
are done by subtracting the old statistic from the new. All p-values are
estimated for these differences.

Comparisons are only performed on statistics in strata which exist in both
the old and current versions. E.g. if the old version contains counts
up to 2018 and the current one up to 2019, no comparisons are done on 2019
counts.

\code{plot_nordcan_statistics_table_comparisons} produces .png files into dir
\code{nordcancore::get_global_nordcan_settings()[["work_dir"]]}. Each file is a
grid of entity-specific plots. One .png corresponds to one dataset taken from
\code{x$comparisons}. Only the following datasets are plotted:
\itemize{
\item cancer_record_count_dataset: column \code{sex} is summed out before plotting.
\item cancer_death_count_dataset: column \code{sex} is summed out before plotting.
\item prevalent_patient_count_dataset column \code{sex} is summed out before plotting,
and only \verb{full_years_since == "0 - 999} data is used.
}
}
\section{Functions}{
\itemize{
\item \code{plot_nordcan_statistics_table_comparisons()}: plots time series of
comparison results (\code{compare_nordcan_statistics_table_lists} output);
see Details

}}
