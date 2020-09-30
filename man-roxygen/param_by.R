
#' @param by see [basicepistats::stat_count]
#'
#' exceptions to `[basicepistats::stat_count]`:
#' - if `by` is a character vector of column names in `x`, the allowed
#'   combinations of those columns are retrieved using
#'   `[nordcancore::nordcan_metadata_column_level_space_dt]`, and the output
#'   will include all of these combinations
#' - if `by` is a character vector and one element of it is
#'   `"entity"`, then output is also stratified by entity number
#'   (the entity numbers will be in column `entity` in output); the
#'   entities to compute results by are supplied via `entities`
#' - if `by` is a character vector and one element of it is
#'   `"region"`, then output is also stratified by region number, i.e. by
#'   every sub-region (if applicable) and also the top region
