


statistics_tables_names <- data.table::fread(
  "data-raw/statistics_tables_names.csv"
)
usethis::use_data(statistics_tables_names, internal = TRUE, overwrite = TRUE)


