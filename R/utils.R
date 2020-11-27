

#' @rdname entity_strata
#' @export
#'
nordcanstat_metadata_statistics_tables_names <- function(
  participant_name = nordcancore::nordcan_metadata_participant_info()[["name"]]
) {
  dt <- data.table::setDT(data.table::copy(
    nordcancore::get_internal_dataset("statistics_tables_names", "nordcanepistats")
  ))
  dt[
    j = "csv_file_name" := tolower(gsub(
      "%%PARTICIPANT_NAME%%",
      participant_name,
      dt[["csv_file_name"]]
    ))
  ]
  return(dt[])
}




nordcanstat_settings <- function(function_name) {
  stopifnot(
    length(function_name) == 1L,
    function_name %in% "nordcanstat_prevalent_subject_count"
  )

  prevalence_col_nms <- nordcancore::nordcan_metadata_column_name_set(
    "column_name_set_prevalence"
  )
  gs <- nordcancore::get_global_nordcan_settings()
  first_obs_y <- gs[["stat_prevalent_subject_count_first_year"]]
  last_obs_y <- nordcancore::nordcan_metadata_nordcan_year()
  obs_yrs <- first_obs_y:last_obs_y

  if (function_name == "nordcanstat_prevalent_subject_count") {
    settings <- list(
      entry_year_col_nm = prevalence_col_nms["entry_year"],
      exit_year_col_nm = prevalence_col_nms["exit_year"],
      subject_id_col_nm = prevalence_col_nms["subject_id"],
      observation_years = obs_yrs,
      maximum_follow_up_years = c(1L, 3L, 5L, 10L, 1000L)
    )
  }


  return(settings)
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
#' `loop_over_entity_numbers` effectively loops over each entity number given in
#' `entities` and computes the results separately for each.
#'
#' `loop_over_entity_columns` loops over each column defining entities in
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
#' `loop_over_entity_numbers` modifies an element in `arg_list` named
#' `subset`, intended to subset the cancer record dataset. If `fun` does not
#' have an argument named `subset`, the dataset `x` is itself subset before
#' passing to `fun`. This is done separately for each entity number.
#'
#' `loop_over_entity_columns` modifies or creates `arg_list[["by"]]` before
#' calling `fun`. `x` is not subset by this function. Therefore
#' `loop_over_entity_columns` is a more efficient but also less flexible
#' version of `loop_over_entity_numbers`.
#'
#' @return
#' Either a `list` or a `data.table`. Initially results are collected into a
#' `list` where each element is the set of results for an individual entity
#' or entity column. If every element of this list is a `data.table`,
#' then every element is combined into one long `data.table` where column
#' `"entity"` will identify the entity. In the case of
#' `loop_over_entity_numbers` the column `"entity"` is created and in the case of
#' `loop_over_entity_columns` each entity column is renamed to `"entity"`
#' in the entity column-specific results before combining.
#' @name entity_strata
NULL


#' @rdname entity_strata
#' @export
loop_over_entity_numbers <- function(
  x,
  entities,
  fun,
  arg_list
) {
  dbc::assert_is_data_table(x)
  dbc::assert_is_integer_nonNA_vector(entities)
  dbc::assert_is_function(fun)
  dbc::assert_is_list(arg_list)
  stopifnot(
    "x" %in% names(formals(fun))
  )

  entities <- unique(entities)
  results_by_entity <- lapply(entities, function(entity_no) {
    is_entity <- nordcancore::in_entity_set(x = x, entities = entity_no)
    if ("subset" %in% names(formals(fun))) {
      arg_list[["subset"]] <- nordcancore::subset_and(
        arg_list[["subset"]], is_entity
      )
    } else {
      arg_list[["x"]] <- x[is_entity, ]
    }

    call_with_arg_list(fun_nm = "fun", arg_list = arg_list)
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


#' @importFrom data.table .SD
#' @rdname entity_strata
#' @export
loop_over_entity_columns <- function(
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
    call_with_arg_list("fun", arg_list = arg_list)
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



#' @rdname entity_strata
#' @export
#' @param basicepistats_fun `[function]` (mandatory, no default)
#'
#' function from **basicepistats**, e.g. `[basicepistats::stat_count]`;
#' this function will be run to produce results by entity number
#' @param loop_over `[character]` (mandatory, default `"entity_numbers"`)
#'
#' - `"entity_columns"`: function `loop_over_entity_columns` is called internally
#' - `"entity_numbers"`: function `loop_over_entity_numbers` is called internally
nordcanstat_by_entity <- function(
  entities,
  arg_list,
  basicepistats_fun,
  loop_over = c("entity_numbers", "entity_columns")[1L]
) {
  dbc::assert_prod_input_is_character_nonNA_atom(loop_over)
  dbc::assert_atom_is_in_set(
    loop_over, set = c("entity_columns", "entity_numbers")
  )
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
  dbc::assert_prod_input_is_function(basicepistats_fun)
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

    loop_fun <- switch(
      loop_over,
      entity_numbers = loop_over_entity_numbers,
      entity_columns = loop_over_entity_columns
    )
    stat_dt <- loop_fun(
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
  stat_dt <- call_with_arg_list("basicepistats_fun", arg_list = arg_list)
  return(stat_dt[])
}




session_info <- function() {
  tf <- tempfile()
  on.exit(unlink(tf))
  sink(file = tf)
  cat("System time at start:", as.character(Sys.time()), "\n")
  cat("sessionInfo() output:\n")
  print(utils::sessionInfo())
  sink(file = NULL)

  readLines(tf)
}





call_with_arg_list <- function(
  fun_nm,
  arg_list = NULL,
  envir = parent.frame(1L)
) {
  dbc::assert_prod_input_is_character_nonNA_atom(fun_nm)
  fun <- tryCatch(
    eval(substitute(get(fun_nm, mode = "function")),
         envir = envir),
    error = function(e) e
  )
  if (!is.function(fun)) {
    fun <- tryCatch(
      eval(substitute(get(fun_nm, mode = "function")),
           envir = parent.frame(1L)),
      error = function(e) e
    )
  }
  if (!is.function(fun)) {
    fun <- tryCatch(
      eval(substitute(get(fun_nm, mode = "function")),
           envir = environment(call_with_arg_list)),
      error = function(e) e
    )
  }
  if (!is.function(fun)) {
    stop("internal error: could not retrieve fun named ", deparse(fun_nm))
  }

  if (is.null(arg_list)) {
    arg_list <- mget(names(formals(fun)), envir = envir)
  }
  dbc::assert_prod_input_is_list(arg_list)

  is_unnamed_arg <- names(arg_list) == ""
  n_unnamed_args <- sum(is_unnamed_arg)
  if (n_unnamed_args > 0L) {
    names(arg_list)[is_unnamed_arg] <- paste0(
      "unnamed_argument_", 1:n_unnamed_args
    )
  }

  fun_env <- new.env(parent = envir)
  fun_env[[fun_nm]] <- fun
  arg_env <- new.env(parent = fun_env)
  lapply(seq_along(arg_list), function(i) {
    arg_env[[names(arg_list)[i]]] <- arg_list[[i]]
  })

  call_string <- paste0(
    fun_nm, "(\n",
    paste0("  ", names(arg_list), " = ", names(arg_list), collapse = ",\n"),
    "\n)"
  )
  call_string <- gsub("unnamed_argument_[0-9]+ = ", "", call_string)

  call <- parse(text = call_string)[[1L]]
  eval_env <- new.env(parent = arg_env)
  eval(call, envir = eval_env)
}




add_margin_to_regional_count_dt <- function(dt, count_col_nm = "N") {

  by <- setdiff(names(dt), count_col_nm)

  if ("region" %in% by) {
    region_dt <- nordcancore::nordcan_metadata_column_level_space_dt("region")
    participant_info <- nordcancore::nordcan_metadata_participant_info()
    topregion_number <- participant_info[["topregion_number"]]
    if (nrow(region_dt) > 1L) {
      # NORDCAN participant actually has some sub-regions, so need to
      # marginalise over them to get the country-level figures
      dt_by_region <- dt[
        i = dt[["region"]] != topregion_number
      ]
      nonregion_by <- setdiff(by, "region")
      dt_margin <- dt[
        j = lapply(.SD, sum),
        .SDcols = "N",
        keyby = eval(nonregion_by)
      ][
        j = "region" := topregion_number
      ]
      dt <- rbind(dt_by_region, dt_margin, use.names = TRUE)
    }
  }

  return(dt[])
}



remove_regional_counts_before_start_year <- function(dt, year_col_nm) {
  dbc::assert_prod_input_is_data.table(dt)
  dbc::assert_prod_input_is_character_nonNA_atom(year_col_nm)
  if (all(c("region", year_col_nm) %in% names(dt))) {
    participant_info <- nordcancore::nordcan_metadata_participant_info()
    topregion_number <- participant_info[["topregion_number"]]
    dbc::assert_prod_interim_is_integer_nonNA_atom(topregion_number)
    gs <- nordcancore::get_global_nordcan_settings()
    fy <- gs[["regional_data_first_year"]]
    dbc::assert_prod_interim_is_integer_nonNA_atom(fy)
    subset <- dt[["region"]] == topregion_number | (
      dt[["region"]] != topregion_number & dt[[year_col_nm]] >= fy
    )
    dt <- dt[subset, ]
  }
  return(dt[])
}











