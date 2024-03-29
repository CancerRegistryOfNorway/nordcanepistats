% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_nordcan_statistics_tables.R
\name{write_maintainer_summary_zip}
\alias{write_maintainer_summary_zip}
\title{write_maintainer_summary_zip}
\usage{
write_maintainer_summary_zip(x)
}
\arguments{
\item{x}{output of nordcanepistats::compare_nordcan_statistics_table_lists}
}
\description{
write_maintainer_summary_zip
}
\details{
This function will create a zip file (maintainer_summary.zip under work directory), which contains a 'comparison_summary.csv' &
several png files generated by function nordcanepistats::plot_nordcan_statistics_table_comparisons. this zip is intended to be sent
to maintainers as proof that nothing is amiss in results (at least in the tested ones)
}
