




nordcanstat_settings <- function(function_name) {
  stopifnot(
    length(function_name) == 1L,
    function_name %in% "nordcanstat_prevalent_subject_count"
  )

  prevalence_col_nms <- nordcancore::nordcan_metadata_column_name_set(
    "column_name_set_prevalence"
  )

  if (function_name == "nordcanstat_prevalent_subject_count") {
    message("TODO for Joonas: embed observation_years setting into ",
            "nordcancore")
    settings <- list(
      entry_year_col_nm = prevalence_col_nms["entry_year"],
      exit_year_col_nm = prevalence_col_nms["exit_year"],
      subject_id_col_nm = prevalence_col_nms["subject_id"],
      observation_years = 2018L,
      maximum_follow_up_years = c(1L, 3L, 5L, 10L, 1000L)
    )
  }


  return(settings)
}



subset_and <- function(
  subset1,
  subset2
) {
  dbc::assert_prod_input_is_one_of(
    x = subset1,
    funs = c("report_is_NULL", "report_is_logical", "report_is_integer")
  )
  dbc::assert_prod_input_is_one_of(
    x = subset2,
    funs = c("report_is_NULL", "report_is_logical", "report_is_integer")
  )
  if (is.null(subset1) && is.null(subset2)) {
    return(NULL)
  }
  if (is.null(subset1)) {
    return(subset2)
  }
  if (is.null(subset2)) {
    return(subset1)
  }
  if (is.logical(subset1) && is.logical(subset2)) {
    return(subset1 & subset2)
  } else if (is.integer(subset1) && is.integer(subset2)) {
    return(intersect(subset1, subset2))
  } else {
    sl <- list(subset1, subset2)
    sl <- lapply(sl, function(s) {
      if (is.logical(s)) {
        which(s)
      } else {
        s
      }
    })
    subset1 <- sl[[1L]]
    subset2 <- sl[[2L]]
    return(intersect(subset1, subset2))
  }
}

#' @title Stratification by Entity
#' @description
#' Compute statistics by entity number.
#' @param x `[data.table]` (mandatory, no default)
#'
#' a NORDCAN cancer record dataset
#' @template param_entities
#' @param fun `[function]` (mandatory, no default)
#'
#' function that is applied to each entity / entity column; see Details
#' @param arg_list `[list]` (mandatory, no default)
#'
#' a `list` containing arguments to pass to `fun`; see Details
#' @details
#'
#' `compute_by_entity` effectively loops over each entity number given in
#' `entities` and computes the results separately for each.
#'
#' `compute_by_entity_column` loops over each column defining entities in
#' `x`. For each such column, `arg_list[["by"]]` is created or modified to have
#'  (also) the column in question, limited to only those entities supplied in
#'  `entities`.
#'
#' Both of these two functions are intended to be applied internally
#' by the `nordcanstat_` family of functions and not by the user.
#'
#' `fun` is called using `arg_list` after the appropriate entity- or
#' entity column-specific modification using `[base::do.call]`.
#'
#' `compute_by_entity` modifies an element in `arg_list` named
#' `x`, intended to contain the dataset cancer record. `x` is subset into
#' each entity number separately before calling `fun`, i.e.
#' `arg_list[["x"]] <- x[has_first_entity_no, ]` for the first entity and so on.
#'
#' `compute_by_entity_column` modifies or creates `arg_list[["by"]]` before
#' calling `fun`. `x` is not subset by this function. Therefore
#' `compute_by_entity_column` is a more efficient but also less flexible
#' version of `compute_by_entity`.
#'
#' @return
#' Either a `list` or a `data.table`. Initially results are collected into a
#' `list` where each element is the set of results for an individual entity
#' or entity column. If every element of this list is a `data.table`,
#' then every element is combined into one long `data.table` where column
#' `"entity"` will identify the entity. In the case of
#' `compute_by_entity` the column `"entity"` is created and in the case of
#' `compute_by_entity_column` each entity column is renamed to `"entity"`
#' in the entity column-specific results before combining.
#'
#'
#' @importFrom dbc assert_is_data_table assert_is_function
#' assert_is_list assert_is_integer_nonNA_vector
#' @importFrom nordcancore in_entity_set
#' @importFrom data.table is.data.table rbindlist set setcolorder
#' @export
compute_by_entity <- function(
  x,
  entities,
  fun,
  arg_list
) {
  dbc::assert_is_data_table(x)
  dbc::assert_is_integer_nonNA_vector(entities)
  dbc::assert_is_function(fun)
  dbc::assert_is_list(arg_list)
  stopifnot("x" %in% names(formals(fun)))

  entities <- unique(entities)
  results_by_entity <- lapply(entities, function(entity_no) {
    is_entity <- nordcancore::in_entity_set(x = x, entities = entity_no)
    arg_list[["x"]] <- x[is_entity, ]
    do.call(fun, arg_list)
  })

  if (all(vapply(results_by_entity, data.table::is.data.table, logical(1L)))) {
    results_by_entity <- lapply(seq_along(results_by_entity), function(i) {
      entity_no <- entities[i]
      dt <- results_by_entity[[i]]
      data.table::set(dt, j = "entity", value = rep(entity_no, nrow(dt)))
      data.table::setcolorder(dt, c("entity", setdiff(names(dt), "entity")))
      return(dt[])
    })
    results_by_entity <- data.table::rbindlist(results_by_entity)
  }

  return(results_by_entity)
}


#' @importFrom dbc assert_is_data_table assert_is_function
#' assert_is_list assert_is_integer_nonNA_vector
#' @importFrom nordcancore in_entity_set
#' @importFrom data.table .SD setnames data.table rbindlist
#' @rdname compute_by_entity
#' @export
compute_by_entity_column <- function(
  x,
  entities,
  fun,
  arg_list
) {
  dbc::assert_is_data_table(x)
  dbc::assert_prod_input_is_one_of(
    x = arg_list[["by"]],
    funs = c("report_is_data.table", "report_is_NULL")
  )
  dbc::assert_is_integer_nonNA_vector(entities)
  dbc::assert_is_function(fun)
  dbc::assert_is_list(arg_list)
  stopifnot(
    c("x", "by") %in% names(formals(fun))
  )

  entity_dt <- nordcancore::nordcan_metadata_column_level_space_dt(
    nordcancore::nordcan_metadata_column_name_set("column_name_set_entity")
  )
  in_entity_set <- nordcancore::in_entity_set(entity_dt, entities)
  entity_dt <- entity_dt[in_entity_set, ]
  col_has_queried_entities <- vapply(entity_dt, function(entity_column) {
    any(entities %in% entity_column)
  }, logical(1L))
  entity_dt <- entity_dt[, .SD, .SDcols = which(col_has_queried_entities)]

  results_by_entity_col <- lapply(names(entity_dt), function(entity_col_nm) {
    col_entity_set <- intersect(x[[entity_col_nm]], entities)
    by_entity <- data.table::data.table(
      entity = intersect(nordcancore::nordcan_metadata_entity_no_set("all"),
                         entity_dt[[entity_col_nm]])
    )
    data.table::setnames(by_entity, "entity", entity_col_nm)
    if (is.null(arg_list[["by"]])) {
      arg_list[["by"]] <- by_entity
    } else {
      arg_list[["by"]] <- list(by = arg_list[["by"]], entity = by_entity)
    }
    do.call(fun, arg_list)
  })

  is_dt <- vapply(results_by_entity_col, data.table::is.data.table, logical(1L))
  if (all(is_dt)) {
    results_by_entity_col <- lapply(
      seq_along(results_by_entity_col),
      function(i) {
        dt <- results_by_entity_col[[i]]
        entity_col_nm <- names(entity_dt)[i]
        data.table::setnames(dt, entity_col_nm, "entity", skip_absent = TRUE)
        return(dt[])
      })
    results_by_entity_col <- data.table::rbindlist(results_by_entity_col)
    data.table::setkeyv(results_by_entity_col,
                        union(names(results_by_entity_col), "entity"))
  }

  return(results_by_entity_col)
}



#' @rdname compute_by_entity
#' @export
#' @template param_x
#' @template param_by
#' @template param_entities
#' @param subset see the same argument in e.g. [basicepistats::stat_count]
#' @param subset_style see the same argument in e.g. [basicepistats::stat_count]
#' @param basicepistats_fun `[function]` (mandatory, no default)
#'
#' function from **basicepistats**, e.g. `[basicepistats::stat_count]`;
#' this function will be run to produce results by entity number
#' @importFrom dbc assert_prod_input_is_data.table
#' assert_prod_input_is_one_of assert_prod_input_is_one_of
#' @importFrom nordcancore nordcan_metadata_entity_no_set
#' assert_prod_input_entities nordcan_metadata_column_level_space_dt
nordcanstat_by_entity_column <- function(
  entities,
  arg_list,
  basicepistats_fun
) {
  dbc::assert_prod_input_is_uniquely_named_list(arg_list)
  dbc::assert_prod_input_has_names(arg_list, required_names = c("x", "by"))
  dbc::assert_prod_input_is_data.table(arg_list[["x"]])
  dbc::assert_prod_input_is_one_of(
    x = arg_list[["by"]],
    funs = c("report_is_NULL", "report_is_character_nonNA_vector",
             "report_is_data.table")
  )
  dbc::assert_prod_input_is_one_of(
    x = entities,
    funs = c("report_is_NULL", "report_is_integer_nonNA_vector")
  )
  if (is.null(entities)) {
    entities <- nordcancore::nordcan_metadata_entity_no_set("all")
  } else {
    nordcancore::assert_prod_input_entities(entities)
  }
  statfun_arg_nms <- names(formals(basicepistats_fun))
  statfun_arg_list <- arg_list[statfun_arg_nms]
  names(statfun_arg_list) <- statfun_arg_nms
  if (is.character(statfun_arg_list[["by"]]) && "entity" %in% statfun_arg_list[["by"]]) {
    statfun_arg_list[["by"]] <- setdiff(statfun_arg_list[["by"]], "entity")
    if (length(statfun_arg_list[["by"]]) == 0L) {
      statfun_arg_list[["by"]] <- NULL
    } else {
      statfun_arg_list[["by"]] <- nordcancore::nordcan_metadata_column_level_space_dt(
        statfun_arg_list[["by"]]
      )
    }

    stat_dt <- compute_by_entity_column(
      x = statfun_arg_list[["x"]],
      entities = entities,
      fun = basicepistats_fun,
      arg_list = statfun_arg_list
    )
    return(stat_dt[])
  }
  if (is.character(statfun_arg_list[["by"]])) {
    statfun_arg_list[["by"]] <- nordcancore::nordcan_metadata_column_level_space_dt(
      statfun_arg_list[["by"]]
    )
  }
  stat_dt <- do.call(basicepistats_fun, statfun_arg_list[["by"]])
  return(stat_dt[])
}



