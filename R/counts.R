




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
  dt <- nordcanstat_by_entity_column(
    entities = entities,
    arg_list = mget(names(formals(basicepistats::stat_count))),
    basicepistats_fun = basicepistats::stat_count
  )

  if (is.character(by) && "region" %in% by) {
    by <- setdiff(by, "region")
    region_dt <- nordcancore::nordcan_metadata_column_level_space_dt("region")
    participant_info <- nordcancore::nordcan_metadata_participant_info()
    topregion_number <- participant_info[["topregion_number"]]
    if (nrow(region_dt) > 1L) {
      # NORDCAN participant actually has some sub-regions, so need to re-compute
      # while ignoring sub-regions to get the marginal figures
      dt <- rbind(
        dt,
        nordcanstat_by_entity_column(
          entities = entities,
          arg_list = mget(names(formals(basicepistats::stat_count))),
          basicepistats_fun = basicepistats::stat_count
        )[, "region" := topregion_number][]
      )
    }
  }

  data.table::setkeyv(dt, setdiff(names(dt), "N"))
  data.table::setnames(dt, "N", "cancer_record_count")
  return(dt[])
}






