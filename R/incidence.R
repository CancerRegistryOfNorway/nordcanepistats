



#
# # the point of this function is mostly easy readability.
# # optionally, the output's value columns can be renamed to reflect that
# # they are incidence rates.
# nordcanstat_incidence <- function(
#   x,
#   stratum_col_nms = NULL,
#   adjust_col_nms = NULL,
#   subset = NULL,
#   adjust_weigths = NULL
# ) {
#   dt <- nordcanstat_rate(
#     x,
#     stratum_col_nms,
#     adjust_col_nms,
#     subset,
#     adjust_weigths
#   )
#   data.table::setnames(dt, sub("^rate", "incidence", names(dt)))
#   dt[]
# }
#


