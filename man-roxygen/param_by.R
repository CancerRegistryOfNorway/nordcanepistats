
#' @param by see [basicepistats::stat_count]
#'
#' exceptions to `[basicepistats::stat_count]`:
#' - if `by` is a character vector of column names in `x`, the allowed
#'   combinations of those columns are retrieved using
#'   `[nordcancore::nordcan_metadata_column_level_space_dt]`, and the output
#'   will include all of these combinations
#' - additionally if `by` is a character vector and one element of it is
#'   `"entity"`, then output is also stratified by entity number
#'   (the entity numbers will be in column `entity` in output); the
#'   entities to compute results by are supplied via `entities`
