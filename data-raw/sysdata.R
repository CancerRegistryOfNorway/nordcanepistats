


statistics_tables_names <- data.table::fread(
  "data-raw/statistics_tables_names.csv"
)

stopifnot(
  !duplicated(statistics_tables_names$object_name),
  !duplicated(statistics_tables_names$csv_file_name)
)

usethis::use_data(statistics_tables_names, internal = TRUE, overwrite = TRUE)


