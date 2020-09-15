



#' @title Prevalence
#' @description
#' Compute numbers of people alive at specific time points who have had
#' a cancer diagnosis in their past.
#' @template param_x
#' @template param_by
#' @template param_entities
#' @param subset see [basicepistats::stat_year_based_prevalent_subject_count]
#' @param subset_style see [basicepistats::stat_year_based_prevalent_subject_count]
#' @export
#' @importFrom basicepistats stat_year_based_prevalent_subject_count
#' @family nordcanstat
nordcanstat_year_based_prevalent_subject_count <- function(
  x,
  by = NULL,
  entities = NULL,
  subset = NULL,
  subset_style = "zeros"
) {
  settings <- nordcanstat_settings("nordcanstat_prevalent_subject_count")
  arg_list <- c(mget(c("x", "by", "subset", "subset_style")), settings)

  # ensure that prevalence is calculated correctly for the last year used in
  # NORDCAN --- having the exit year of those who were in follow-up at the end
  # of the study period to be larger than last year of observation ensures
  # that they count towards prevalent subjects in the last year of observation.
  # see ?basicepistats::stat_year_based_prevalent_subject_count.
  # the way this is done here avoids taking a copy of any other data, only the
  # year of exit is copied.
  exit_year_col_nm <- settings[["exit_year_col_nm"]]
  use_x <- data.table::setDT(lapply(names(x), function(col_nm) {
    col <- x[[col_nm]]
    if (col_nm == exit_year_col_nm) {
      col <- col + 0L
      survived_last_year <- x[["vit_sta"]] == 1L
      col[survived_last_year] <- max(settings[["observation_years"]]) + 1L
    }
    col
  }))
  data.table::setnames(use_x, names(use_x), names(x))
  arg_list[["x"]] <- use_x

  nordcanstat_by_entity_column(
    entities = entities,
    arg_list = arg_list,
    basicepistats_fun = basicepistats::stat_year_based_prevalent_subject_count
  )
}
