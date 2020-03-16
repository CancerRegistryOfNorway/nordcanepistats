




# the point of this function is mostly easy readability. we can also assume
# the vital status column and subset to cancer deaths internally.
# most countries get their mortality from the general statistics
# office and not from their cancer dataset, so this function would not be
# used by those countries.
# optionally, the output's value columns can be renamed to reflect that
# they are mortality rates.
nordcanstat_mortality <- function(
  x,
  stratum_col_nms = NULL,
  adjust_col_nms = NULL,
  subset = NULL,
  adjust_weigths = NULL
) {

  vital_status_col_nm <- "status"
  is_dead <- x[[vital_status_col_nm]] == 1L
  if (is.null(subset)) {
    subset <- rep(TRUE, nrow(x))
  } else if (is.integer(subset)) {
    subset <- intersect(subset, which(is_dead))
  } else {
    subset <- subset & is_dead
  }

  dt <- nordcanstat_rate(
    x,
    stratum_col_nms,
    adjust_col_nms,
    subset,
    adjust_weigths
  )
  data.table::setnames(dt, sub("^rate", "mortality", names(dt)))
  dt[]
}


