




#' @title Cancer Record Counts
#' @description
#' Compute counts of cancer records.
#' @template param_x
#' @template param_by
#' @template param_entities
#' @param subset see [basicepistats::stat_count]
#' @param subset_style see [basicepistats::stat_count]
#' @export
#' @importFrom data.table :=
#' @family nordcanstat
nordcanstat_count <- function(
  x,
  by = NULL,
  entities = NULL,
  subset = NULL,
  subset_style = "zeros"
) {
  dt <- nordcanstat_by_entity(
    entities = entities,
    arg_list = mget(names(formals(basicepistats::stat_count))),
    basicepistats_fun = basicepistats::stat_count,
    loop_over = "entity_columns"
  )

  dt <- add_margin_to_regional_count_dt(dt, count_col_nm = "N")

  data.table::setkeyv(dt, setdiff(names(dt), "N"))
  data.table::setnames(dt, "N", "cancer_record_count")
  return(dt[])
}






