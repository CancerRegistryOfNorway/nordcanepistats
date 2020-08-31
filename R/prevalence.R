



#' @title Prevalence
#' @description
#' Compute numbers of people alive at specific time points who have had
#' a cancer diagnosis in their past.
#' @param x see [basicepistats::stat_prevalent_subject_count]
#' @param by see [basicepistats::stat_prevalent_subject_count]
#' @param subset see [basicepistats::stat_prevalent_subject_count]
#' @param subset_style see [basicepistats::stat_prevalent_subject_count]
#' @export
#' @importFrom basicepistats stat_prevalent_subject_count
#' @family nordcanstat
nordcanstat_prevalent_subject_count <- function(
  x,
  by = NULL,
  subset = NULL,
  subset_style = "zeros"
) {
  settings <- nordcanstat_settings("nordcanstat_prevalent_subject_count")
  arg_list <- c(mget(c("x", "by", "subset", "subset_style")), settings)
  do.call(basicepistats::stat_prevalent_subject_count, arg_list)

}
