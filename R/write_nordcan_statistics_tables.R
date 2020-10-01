




write_nordcan_statistics_tables <- function(input, purpose = "archive") {
  ## Check is the input is a list.
  # purpose:
  # a character which can be 'archive' or 'sending'. 'archive means the function
  # will zip all elements (including log files) of input into a zip file'.
  # 'sending' means the function will zip only data tables into a zip file.

  if (!is.list(input)) {
    stop("'input' (nordcan_statistics_tables) must be a 'list'!")
  } else {

    class_list <- rep(NA, length(input))

    for (i in 1:length(input)) {
      if ("character" %in% class(input[[i]])) {
        class_list[i] <- "character"
      } else if ("data.table" %in% class(input[[i]])) {
        class_list[i] <- "data.table"
      }
    }

    id <- which(is.na(class_list))
    if (length(id) > 0) {
      stop(sprintf("Class of variable: %s is not supported! \n
                   The classes of the elements of 'input' must be 'character' or 'data.table'",
                   paste(class_list[id], collapse = ",")))
    }
  }


  ## Get global settings of Nordcan
  Global_nordcan_settings <- nordcancore::get_global_nordcan_settings()
  ## Get work directory
  work_dir <- Global_nordcan_settings$work_dir
  ## Create temporary directory for storing the output of nordcan_statistics_tables;
  temp_dir <- normalizePath(sprintf("%s/%s", work_dir,  nordcancore::random_names()[1]))
  dir.create(temp_dir)
  ## Delete the folder when the function exit;
  on.exit({
    if (dir.exists(temp_dir)) {
      unlink(temp_dir, recursive = TRUE)
    }
  })


  if (dir.exists(temp_dir)) {
    ## Write elements of input to temporary directory.
    for (i in 1:length(input)) {
      cls <- class(input[[i]])
      if (cls == "character") {
        if (purpose == "archive") {
          writeLines(text = input[[i]],
                     con = normalizePath(sprintf("%s/%s.txt", temp_dir, names(input)[i])))
        }
      } else {
        write.csv(x = input[[i]],
                  file = normalizePath(sprintf("%s/%s.csv", temp_dir, names(input)[i])),
                  sep = ";")
      }
    }


    ## zip files

    zip(zipfile = sprintf("%s/nordcan_statistics_tables.zip", work_dir),
        files = list.files(temp_dir, full.names = TRUE))



  }

}



#' @title Write NORDCAN Statistics Tables to Zip
#' @description
#' Write the output of [nordcan_statistics_tables] into a .zip file.
#'
#' @param input a list of output of nordcan statistics tables
#' @return a zip file named 'nordcan_statistics_tables.zip' will be save in
#' current work directory (which should be setup before using
#' [nordcancore::set_global_nordcan_settings])
#' @export
#' @name write_nordcan_statistics_tables


#' @rdname write_nordcan_statistics_tables
#' @export
#' @details
#' - `write_nordcan_statistics_tables_for_archive` writes data.tables as .csv
#'   character vectors as .txt files
#' @examples
#'
#' \dontrun{
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
#' input <- list(log1 = letters, log2 = LETTERS[1:5],
#'                output1 = as.data.table(cars), output2 = as.data.table(CO2))
#'
#'
#' write_nordcan_statistics_tables_for_archive(input = input)
#' }
write_nordcan_statistics_tables_for_archive <- function(input) {
  write_nordcan_statistics_tables(input = input, purpose = "archive")
}



#' @rdname write_nordcan_statistics_tables
#' @export
#' @details
#' - `write_nordcan_statistics_tables_for_archive` writes data.tables as .csv
#'   character vectors as .txt files
#' @examples
#' \dontrun{
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
#' input <- list(log1 = letters, log2 = LETTERS[1:5],
#'               output1 = data.table::as.data.table(cars),
#'               output2 = data.table::as.data.table(CO2))
#'
#'
#' write_nordcan_statistics_tables_for_archive(input = input)
#' }


write_nordcan_statistics_tables_for_sending <- function(input) {
  write_nordcan_statistics_tables(input = input, purpose = "sending")
}



