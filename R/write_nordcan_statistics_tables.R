





write_nordcan_statistics_tables <- function(x, purpose = "archive") {
  ## Check is the x is a list.
  dbc::assert_prod_input_is_uniquely_named_list(x)
  dbc::assert_prod_input_is_character_nonNA_atom(purpose)
  dbc::assert_prod_input_atom_is_in_set(purpose, set = c("archive", "sending"))
  lapply(names(x), function(elem_nm) {
    dbc::assert_prod_input_is_one_of(
      x = x[[elem_nm]],
      x_nm = paste0("x$", elem_nm),
      funs = list(dbc::assert_is_data.table,
                  dbc::assert_is_character_nonNA_vector)
    )
  })

  ## Get global settings of Nordcan
  Global_nordcan_settings <- nordcancore::get_global_nordcan_settings()
  ## Get work directory
  work_dir <- Global_nordcan_settings$work_dir
  ## Create temporary directory for storing the output of nordcan_statistics_tables;
  temp_dir <- sprintf("%s/%s", work_dir,  nordcancore::random_names()[1])
  dir.create(temp_dir)
  wd <- getwd()
  ## Delete the folder when the function exit;
  on.exit({
    setwd(wd)
    if (dir.exists(temp_dir)) {
      unlink(temp_dir, recursive = TRUE, force = TRUE)
    }
  })

  object_csv_table_names <- nordcanepistats::nordcanstat_metadata_statistics_tables_names()
  if (purpose == "archive") {
    data.table::set(
      object_csv_table_names,
      j = "csv_file_name",
      value = paste0(object_csv_table_names[["object_name"]], ".csv")
    )
  }

  ## Write elements of x to temporary directory.
  lapply(names(x), function(elem_nm) {
    elem <- x[[elem_nm]]
    if (is.character(elem) && purpose == "archive") {
      writeLines(text = elem,
                 con = sprintf("%s/%s.txt", temp_dir, elem_nm))
    } else if (data.table::is.data.table(elem)) {
      id <- which(object_csv_table_names$object_name == elem_nm)
      if (length(id) > 0) {
        elem_nm_csv <- object_csv_table_names$csv_file_name[id]
      } else {
        elem_nm_csv <- paste0(elem_nm, ".csv")
      }
      data.table::fwrite(x = elem,
                         file = sprintf("%s/%s", temp_dir, elem_nm_csv),
                         sep = ";")
    }
  })

  ## zip files
  setwd(temp_dir)
  zip_file_path <- sprintf("%s/nordcan_statistics_tables.zip", work_dir)
  zip::zip(zipfile = zip_file_path,
           files = list.files(temp_dir, full.names = FALSE))
  message("* nordcanepistats::write_nordcan_statistics_tables: wrote .zip ",
          "into \"", zip_file_path, "\"")

  return(invisible(NULL))
}




#' @title Write NORDCAN Statistics Tables to Zip
#' @description
#' Write the output of [nordcan_statistics_tables] into a .zip file.
#'
#' @param x `[list]` (mandatory, no default)
#'
#' list of statistics tables (data.tables) and any associated logs
#' (character vectors)
#' @export
#' @name write_nordcan_statistics_tables


#' @rdname write_nordcan_statistics_tables
#' @export
#' @details
#' - `write_nordcan_statistics_tables_for_archive` writes data.tables as .csv
#'   character vectors as .txt files and compresses them into
#'   `nordcan_statistics_tables.zip` in the current working directory
#'   set using [nordcancore::set_global_nordcan_settings]
#' @examples
#'
#' \dontrun{
#' library("data.table")
#' td <- tempdir()
#' nordcancore::set_global_nordcan_settings(
#'   work_dir = td,
#'   participant_name = "Norway",
#'   first_year_incidence = 1953L,
#'   first_year_mortality = 1953L,
#'   first_year_region = 1953L,
#'   last_year_incidence = 2018L,
#'   last_year_mortality = 2018L,
#'   last_year_survival = 2018L
#' )
#'
#'
#' x <- list(log1 = letters, log2 = LETTERS[1:5],
#'           output1 = data.table::as.data.table(cars),
#'           output2 = data.table::as.data.table(CO2))
#'
#'
#' nordcanepistats::write_nordcan_statistics_tables_for_archive(x = x)
#'
#' zip_file_path <- paste0(td, "/nordcan_statistics_tables.zip")
#' if (file.exists(zip_file_path)) {
#'   message("wrote zip file to ", deparse(zip_file_path))
#'   file.remove(zip_file_path)
#' } else {
#'   stop("example has failed. please notify authors")
#' }
#'
#' }
#'
write_nordcan_statistics_tables_for_archive <- function(x) {
  dbc::assert_user_input_is_uniquely_named_list(x)
  lapply(names(x), function(elem_nm) {
    dbc::assert_user_input_is_one_of(
      x = x[[elem_nm]],
      x_nm = paste0("x$", elem_nm),
      funs = list(dbc::assert_is_data.table,
                  dbc::assert_is_character_nonNA_vector)
    )
  })

  write_nordcan_statistics_tables(x = x, purpose = "archive")
}



#' @rdname write_nordcan_statistics_tables
#' @export
#' @details
#' - `write_nordcan_statistics_tables_for_sending` writes data.tables as .csv
#'   and compresses them into
#'   `nordcan_statistics_tables.zip` in the current working directory
#'   set using [nordcancore::set_global_nordcan_settings]; does NOT
#'   save any logs (character vectors in `x`) as .txt files
#' @examples
#' \dontrun{
#' library("data.table")
#' nordcancore::set_global_nordcan_settings(
#'   work_dir = ".",
#'   participant_name = "Norway",
#'   first_year_incidence = 1953L,
#'   first_year_mortality = 1953L,
#'   first_year_region = 1953L,
#'   last_year_incidence = 2018L,
#'   last_year_mortality = 2018L,
#'   last_year_survival = 2018L
#' )
#'
#' # log1 and log2 are NOT saved
#' x <- list(log1 = letters, log2 = LETTERS[1:5],
#'           output1 = data.table::as.data.table(cars),
#'           output2 = data.table::as.data.table(CO2))
#'
#'
#' write_nordcan_statistics_tables_for_sending(x = x)
#' }
write_nordcan_statistics_tables_for_sending <- function(
  x
) {
  dbc::assert_user_input_is_uniquely_named_list(x)
  lapply(names(x), function(elem_nm) {
    dbc::assert_user_input_is_one_of(
      x = x[[elem_nm]],
      x_nm = paste0("x$", elem_nm),
      funs = list(dbc::assert_is_data.table,
                  dbc::assert_is_character_nonNA_vector)
    )
  })

  write_nordcan_statistics_tables(x = x, purpose = "sending")
}







#' @rdname write_nordcan_statistics_tables
#' @export
#' @param zip_file_path `[character]` (mandatory, no default)
#'
#' path to an existing zip file
#' @details
#' - `read_nordcan_statistics_tables` uncompresses a zip file and reads into R
#'   all .csv files as data.tables and .txt files as character vectors that it
#'   contained
#' @examples
#' \dontrun{
#' statistics <- read_nordcan_statistics_tables("nordcan_statistics_tables.zip")
#' }
read_nordcan_statistics_tables <- function(
  zip_file_path
) {
  dbc::assert_user_input_file_exists(zip_file_path)
  stopifnot(grepl("\\.zip$", zip_file_path))

  r <- nordcancore::random_names(n_random_names = 1L,
                                 exclude_names = dir("."))
  d <- dir.create(r, recursive = TRUE)
  on.exit(unlink(r, recursive = TRUE, force = TRUE))

  zip::unzip(zipfile = zip_file_path, exdir = r)

  file_ext_re <- "\\.((csv)|(txt))$"
  file_paths <- dir(r, pattern = file_ext_re, full.names = TRUE)
  output <- lapply(file_paths, function(file_path) {
    file_ext <- ifelse(grepl("\\.csv", file_path), "csv", "txt")
    switch(
      file_ext,
      csv = data.table::fread(file_path),
      txt = readLines(file_path)
    )
  })
  file_names <- dir(r, pattern = file_ext_re, full.names = FALSE)
  object_csv_table_names <- nordcanepistats::nordcanstat_metadata_statistics_tables_names()

  for (i in 1:length(file_names)) {
    id <- which(object_csv_table_names$csv_file_name == file_names[i])
    if (length(id) > 0) {
      file_names[i] <- object_csv_table_names$object_name[id]
    } else {
      file_names[i] <- sub("\\.((csv)|(txt))$", "", file_names[i])
    }
  }

  names(output) <- file_names

  return(output)
}



#' @title  write_maintainer_summary_zip
#' @param x  output of nordcanepistats::compare_nordcan_statistics_table_lists
#'
#' @details This function will create a zip file (maintainer_summary.zip under work directory), which contains a 'comparison_summary.csv' &
#' several png files generated by function nordcanepistats::plot_nordcan_statistics_table_comparisons. this zip is intended to be sent
#'  to maintainers as proof that nothing is amiss in results (at least in the tested ones)
#' @export



write_maintainer_summary_zip <- function(x) {

  ## x is a list which is the output of nordcanepistats::compare_nordcan_statistics_table_lists
  ## list(summary = summary, comparisons = comparisons)
  dbc::assert_user_input_is_list(x)
  dbc::assert_user_input_has_names(
    x, required_names = c("summary", "comparisons")
  )

  ## Get global settings of Nordcan
  Global_nordcan_settings <- nordcancore::get_global_nordcan_settings()
  ## Get work directory
  work_dir <- Global_nordcan_settings$work_dir

  ## Write summary to 'comparison_summary.csv';
  data.table::fwrite(x = x$summary,
                     file = sprintf("%s/comparison_summary.csv", work_dir),
                     sep = ";")

  ## png files
  nordcanepistats::plot_nordcan_statistics_table_comparisons(x)

  log_file_name <- "session_info.txt"
  log_file_path <- paste0(work_dir, "/", log_file_name)
  writeLines(session_info(), log_file_path)

  files_list <- c(
    log_file_name,
    "comparison_summary.csv",
    "cancer_death_count_dataset.png",
    "cancer_record_count_dataset.png",
    "prevalent_patient_count_dataset.png"
  )
  files_list <- intersect(files_list, dir(work_dir))

  nordcan_version <- nordcancore::nordcan_metadata_nordcan_version()
  participant_name <- tolower(
    nordcancore::nordcan_metadata_participant_info()[["name"]]
  )
  zip_file_name <- paste0(
    "nordcan_", nordcan_version, "_maintainer_summary_for_",
    participant_name, ".zip"
  )
  zip_file_path <- paste0(work_dir, "/", zip_file_name)
  zip_file_path <- normalizePath(zip_file_path, mustWork = FALSE)
  zip::zip(zipfile = zip_file_path, files = files_list)

  message("* nordcanepistats::write_maintainer_summary_zip: ",
          "wrote zip to  ", zip_file_path)

  return(invisible(NULL))
}




