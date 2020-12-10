




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
  # just to ensure settings are set
  nordcancore::get_global_nordcan_settings()

  arg_list <- mget(names(formals(basicepistats::stat_count)))
  dt <- nordcanstat_by_entity(
    entities = entities,
    arg_list = arg_list,
    basicepistats_fun = basicepistats::stat_count,
    loop_over = "entity_columns"
  )

  info <- nordcancore::nordcan_metadata_participant_info()
  if (is.character(by) && "region" %in% by && info[["has_sub_regions"]]) {
    arg_list[["by"]] <- setdiff(arg_list[["by"]], "region")
    if (length(arg_list[["by"]]) == 0L) {
      arg_list[["by"]] <- NULL
    }
    dt_topregion <- nordcanstat_by_entity(
      entities = entities,
      arg_list = arg_list,
      basicepistats_fun = basicepistats::stat_count,
      loop_over = "entity_columns"
    )
    topregion_number <- info[["topregion_number"]]
    dt_topregion[, "region" := topregion_number]
    data.table::setcolorder(dt_topregion, names(dt))
    dt <- rbind(dt, dt_topregion)
    data.table::setcolorder(dt, by)
    data.table::setkeyv(dt, by)
  }

  dt <- remove_regional_counts_before_start_year(
    dt, year_col_nm = "yoi"
  )

  if (is.character(by)) {
    data.table::setcolorder(dt, by)
    data.table::setkeyv(dt, by)
  } else {
    data.table::setkeyv(dt, setdiff(names(dt), "N"))
  }
  data.table::setnames(dt, "N", "cancer_record_count")
  return(dt[])
}






