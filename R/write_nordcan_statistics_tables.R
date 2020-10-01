




write_nordcan_statistics_tables <- function(x, purpose = "archive") {
  ## Check is the x is a list.
  if (!is.list(x)) {
    stop("'x' must be a 'list'!")
  } else {

    class_list <- rep(NA, length(x))

    for (i in 1:length(x)) {
      if ("character" %in% class(x[[i]])) {
        class_list[i] <- "character"
      } else if ("data.table" %in% class(x[[i]])) {
        class_list[i] <- "data.table"
      }
    }

    id <- which(is.na(class_list))
    if (length(id) > 0) {
      stop(sprintf("Class of variable: %s is not supported! \n
                   The classes of the elements of 'x' must be 'character' or 'data.table'",
                   paste(class_list[id], collapse = ",")))
    }
  }

  ## Get global settings of Nordcan
  Global_nordcan_settings <- nordcancore::get_global_nordcan_settings()
  ## Get work directory
  work_dir <- Global_nordcan_settings$work_dir
  ## Create temporary directory for storing the output of nordcan_statistics_tables;
  temp_dir <- sprintf("%s/%s", work_dir,  nordcancore::random_names()[1])
  dir.create(temp_dir)
  ## Delete the folder when the function exit;
  on.exit({
    if (dir.exists(temp_dir)) {
      unlink(temp_dir, recursive = TRUE)
    }
  })

  if (dir.exists(temp_dir)) {
    ## Write elements of x to temporary directory.
    for (i in 1:length(x)) {
      cls <- class(x[[i]])
      if ("character" %in% cls) {
        if (purpose == "archive") {
          writeLines(text = x[[i]],
                     con = sprintf("%s/%s.txt", temp_dir, names(x)[i]))
        }
      } else {
        data.table::fwrite(x = x[[i]],
                  file = sprintf("%s/%s.csv", temp_dir, names(x)[i]),
                  sep = ";")
      }
    }

    ## zip files
    wd <- getwd()
    setwd(temp_dir); on.exit({setwd(wd)}, add = TRUE)
    utils::zip(zipfile = sprintf("%s/nordcan_statistics_tables.zip", work_dir),
               files = list.files(temp_dir, full.names = FALSE))

    return(invisible(NULL))
  }
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
#' nordcancore::set_global_nordcan_settings(
#'   work_dir = ".",
#'   participant_name = "Norway",
#'   stat_cancer_record_count_first_year = 1954L,
#'   stat_prevalent_subject_count_first_year = 1954L,
#'   stat_cancer_death_count_first_year = 1954L,
#'   stat_survival_follow_up_first_year = 1954L
#' )
#'
#'
#' x <- list(log1 = letters, log2 = LETTERS[1:5],
#'           output1 = data.table::as.data.table(cars),
#'           output2 = data.table::as.data.table(CO2))
#'
#'
#' write_nordcan_statistics_tables_for_archive(x = x)
#' }
write_nordcan_statistics_tables_for_archive <- function(x) {
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
#'   stat_cancer_record_count_first_year = 1954L,
#'   stat_prevalent_subject_count_first_year = 1954L,
#'   stat_cancer_death_count_first_year = 1954L,
#'   stat_survival_follow_up_first_year = 1954L
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

  r <- nordcancore::random_names(n_random_names = 1L)
  d <- dir.create(r, recursive = TRUE)
  on.exit(unlink(r, recursive = TRUE))

  utils::unzip(zipfile = zip_file_path, exdir = r)

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
  names(output) <- sub(file_ext_re, "", file_names)

  return(output)
}





