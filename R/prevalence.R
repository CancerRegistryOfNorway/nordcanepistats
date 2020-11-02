



#' @title Prevalence
#' @description
#' Compute numbers of people alive at specific time points who have had
#' a cancer diagnosis in their past.
#' @template param_x
#' @template param_by
#' @template param_entities
#' @param observation_years `[NULL, integer]` (optional, default `NULL`)
#'
#' - `NULL`: NORDCAN default set of years is used
#' - `integer`: only observe prevalence at the ends of these years; see
#'   [basicepistats::stat_year_based_prevalent_subject_count] for more
#'   information
#' @param subset see [basicepistats::stat_year_based_prevalent_subject_count]
#' @param subset_style see [basicepistats::stat_year_based_prevalent_subject_count]
#' @export
#' @family nordcanstat
nordcanstat_year_based_prevalent_patient_count <- function(
  x,
  by = NULL,
  entities = NULL,
  observation_years = NULL,
  subset = NULL,
  subset_style = "zeros"
) {
  # just to ensure settings are set
  nordcancore::get_global_nordcan_settings()

  settings <- nordcanstat_settings("nordcanstat_prevalent_subject_count")
  col_nms <- unlist(settings[
    c("entry_year_col_nm", "exit_year_col_nm", "subject_id_col_nm")
  ])
  col_nms <- union("vit_sta", col_nms)
  dbc::assert_prod_input_is_data.table_with_required_names(
    x,
    required_names = col_nms
  )
  dbc::assert_prod_input_is_one_of(
    x = observation_years,
    funs = c("report_is_NULL", "report_is_integer_nonNA_vector")
  )
  arg_list <- c(mget(c("x", "by", "subset", "subset_style")), settings)
  if (!is.null(observation_years)) {
    arg_list[["observation_years"]] <- observation_years
  }

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
      last_year <- nordcancore::nordcan_metadata_nordcan_year()
      survived_last_year <- x[["vit_sta"]] == 1L & col == last_year
      col[survived_last_year] <- max(last_year) + 1L
    }
    col
  }))
  data.table::setnames(use_x, names(use_x), names(x))
  arg_list[["x"]] <- use_x

  dt <- nordcanstat_by_entity(
    entities = entities,
    arg_list = arg_list,
    basicepistats_fun = basicepistats::stat_year_based_prevalent_subject_count,
    loop_over = "entity_numbers"
  )

  dt <- add_margin_to_regional_count_dt(dt, count_col_nm = "N")

  dt <- remove_regional_counts_before_start_year(
    dt, year_col_nm = "observation_year"
  )

  data.table::setkeyv(dt, setdiff(names(dt), "N"))
  data.table::setnames(dt, "N", "prevalent_patient_count")
  return(dt[])
}





