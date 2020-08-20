nordcan_statistics_payload <- function(
  datasets=list(
    mortality_count=mortality_count,
    mortality_raw=mortality_raw,
    incidence=incidence
  )
) {
  
  stopifnot(
    inherits(datasets, "list"),
    vapply(datasets, data.table::is.data.table, logical(1L)),
    !is.null(names(datasets)),
    names(datasets) %in% c("mortality_count","mortality_raw","incidence"),
    c("mortality_raw","incidence") %in% names(datasets)
  )
  
  '%ni%'=Negate('%in%')
  if('mortality_count' %in% names(datasets)){message("mortality_count is given in datasets and need not to be computed inside this function!")} else if('mortality_count' %ni% names(datasets)) {mortality_count=unique(stat_count(x=mortality_raw,by=mortality_raw))}
  
  payload=list(
    mortality_count=mortality_count,
  incidence_stat_table=as.data.table(stat_table_list(
    varying_arg_list = list(
    by = list(c("year","sex","region","age","entity"))
  ),
  fixed_arg_list = list(x = incidence),
  stat_fun_nm = "stat_count"
))
  )
  
  stopifnot(
     c("mortality_count","incidence_stat_table")
      %in%
      names(payload),
    inherits(payload, "list"),
    vapply(payload, inherits, logical(1L), what = "data.table")
  )
  write.csv(mortality_count,"Result Mortality.csv",row.names=F)
  write.csv(mortality_count,"Result Incidence.csv",row.names=F)
  return(payload)
}
