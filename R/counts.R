




#' @title Cancer Record Counts
#' @description
#' Compute counts of cancer records.
#' @template param_x
#' @template param_by
#' @template param_entities
#' @param subset see [basicepistats::stat_count]
#' @param subset_style see [basicepistats::stat_count]
#' @export
#' @family nordcanstat
nordcanstat_count <- function(
  x,
  by = NULL,
  entities = NULL,
  subset = NULL,
  subset_style = "zeros"
) {
  dt <- nordcanstat_by_entity_column(
    entities = entities,
    arg_list = mget(names(formals(basicepistats::stat_count))),
    basicepistats_fun = basicepistats::stat_count
  )
  data.table::setnames(dt, "N", "cancer_record_count")
  return(dt[])
}






