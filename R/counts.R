




#' @title Cancer Record Counts
#' @description
#' Compute counts of cancer records.
#' @param x see [basicepistats::stat_count]
#' @param by see [basicepistats::stat_count]
#' @param subset see [basicepistats::stat_count]
#' @param subset_style see [basicepistats::stat_count]
#' @export
#' @importFrom basicepistats stat_count
#' @importFrom data.table :=
#' @family nordcanstat
nordcanstat_count <- function(
  x,
  by = NULL,
  entities = nordcancore::nordcan_metadata_entity_no_set("cancer_record_count"),
  subset = NULL,
  subset_style = "zeros"
) {
  if (is.character(by) && "entity" %in% by) {
    arg_list <- mget(names(formals(nordcanstat_count)))
    arg_list[["by"]] <- setdiff(arg_list[["by"]], "entity")
    if (length(arg_list[["by"]]) == 0L) {
      arg_list[["by"]] <- NULL
    } else {
      arg_list[["by"]] <- nordcancore::nordcan_metadata_column_level_space_dt(
        arg_list[["by"]]
      )
    }

    output <- compute_by_entity_column(
      x = x,
      entities = entities,
      fun = nordcanstat_count,
      arg_list = arg_list
    )
    return(output[])
  }
  if (is.character(by)) {
    by <- nordcancore::nordcan_metadata_column_level_space_dt(by)
  }
  count_dt <- basicepistats::stat_count(
    x = x,
    by = by,
    subset = subset,
    subset_style = subset_style
  )
  return(count_dt[])
}









