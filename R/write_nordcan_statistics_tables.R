
#' Write nordcan statistics tables to a zip file. 
#' 
#' @details The output of 'nordcan statistics tables' includes log file and data tables. The function 'write_nordcan_statistics_tables' will write all elements of 'nordcan statistics tables' into a zip file for further distribution or archivement. Based on different 'purpose', the function can zip all elements into a zip file (archive), or zip only data tables into a zip file (sending). The function will create a temporary folder and write all elements into that folder. After zipping all files in that temporary folder, the folder will be deleted to keep the work directory clean. 
#'  @param input a list contains log files and data tables. 
#'  @param purpose a character which can be 'archive' or 'sending'. 'archive means the function will zip all elements (including log files) of input into a zip file'. 'sending' means the function will zip only data tables into a zip file. 
#'  @return a zip file named 'nordcan_statistics_tables.zip' will be save in current work directory (which should be setup before using nordcancore::set_global_nordcan_settings)





write_nordcan_statistics_tables <- function(input, purpose = "archive") {
  ## Check is the input is a list.
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
#' 
#' @details  This function is a wrapper of 'write_nordcan_statistics_tables' with its argument 'purpose' set to "archive". It will save all elements of input into a zip file (nordcan_statistics_tables.zip) under current work directory. 
#' 
#' @param input a list of output of nordcan statistics tables
#' @return a zip file named 'nordcan_statistics_tables.zip' will be save in current work directory (which should be setup before using nordcancore::set_global_nordcan_settings)
#' @export
#' @example 
#' 
#' \dontrun{
#' nordcancore::set_global_nordcan_settings(
#'   work_dir = "/home/",
#'   participant_name = "Norway",
#'   stat_cancer_record_count_first_year = 1954L,
#'   stat_prevalent_subject_count_first_year = 1954L,
#'   stat_cancer_death_count_first_year = 1954L,
#'   stat_survival_follow_up_first_year = 1954L
#' )
#' 
#' 
#' input <- list(log1 = letters, log2 = LETTERS[1:5], output1 = as.data.table(cars), output2 = as.data.table(CO2))
#' 
#' 
#' write_nordcan_statistics_tables_for_archive(input = input)
#' write_nordcan_statistics_tables_for_sending(input = input)
#' }




write_nordcan_statistics_tables_for_archive <- function(input) {
  write_nordcan_statistics_tables(input = input, purpose = "archive")
}



#' @title Write NORDCAN Statistics Tables to Zip
#' 
#' @details This function is a wrapper of 'write_nordcan_statistics_tables' with its argument 'purpose' set to "sending". It will save all data tables (NOT including log files) of input into a zip file (nordcan_statistics_tables.zip) under current work directory. 
#' 
#' @param input a list of output of nordcan statistics tables
#' 
#' @return a zip file named 'nordcan_statistics_tables.zip' will be save in current work directory (which should be setup before using nordcancore::set_global_nordcan_settings)
#' @export
#' @example 
#' 
#' \dontrun{
#' nordcancore::set_global_nordcan_settings(
#'   work_dir = "/home/",
#'   participant_name = "Norway",
#'   stat_cancer_record_count_first_year = 1954L,
#'   stat_prevalent_subject_count_first_year = 1954L,
#'   stat_cancer_death_count_first_year = 1954L,
#'   stat_survival_follow_up_first_year = 1954L
#' )
#' 
#' 
#' input <- list(log1 = letters, log2 = LETTERS[1:5], output1 = as.data.table(cars), output2 = as.data.table(CO2))
#' 
#' 
#' write_nordcan_statistics_tables_for_archive(input = input)
#' write_nordcan_statistics_tables_for_sending(input = input)
#' }


write_nordcan_statistics_tables_for_sending <- function(input) {
  write_nordcan_statistics_tables(input = input, purpose = "sending")
}



