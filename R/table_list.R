




nordcanstat_table_list <- function(
  x,
  stratum_col_nm_sets = list(c("sex", "BoD"), c("sex", "agegroup", "BoD")),
  stat_fun = nordcanstat_incidence,
  stat_fun_arg_list = list(subset = NULL)
) {
  stat_fun_arg_list[["x"]] <- x
  lapply(stratum_col_nm_sets, function(stratum_col_nm_set) {
    arg_list <- stat_fun_arg_list
    arg_list[["stratum_col_nms"]] <- stratum_col_nm_set
    do.call(stat_fun, arg_list)
  })
}




