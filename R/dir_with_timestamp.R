#' dir_with_timestamp
#'
#' dir_with_timestamp is a wrapper around [base::dir()], with some
#' additional capabilities like filtering out folders with identical names but
#' old timestamps.
#'
#' @param path A string with the path to the folder which is to be listed.
#' @param pattern A string with a regex pattern with which to filter.
#' @param full.names Boolean, if true return full paths.
#' @param name_timestamp_seperator A string with the name-timestamp seperator.
#' @param timestamp_format A string with the timestamp format, as input for
#'   [base::as.POSIXct()].
#' @param remove_timestamp Boolean, if true remove timestamp.
#' @param only_most_recent Boolean, if true remove folders with identical name
#'   but older timestamps.
#' @param ... Parameters that are passed on to [base::dir()].
#'
#' @export
#'
#' @return A vector of strings with folder paths.
dir_with_timestamp <- function(path = ".",
                               pattern = "",
                               full.names = FALSE,
                               name_timestamp_seperator = "_",
                               timestamp_format = "%Y-%m-%d_%H.%M.%S",
                               remove_timestamp = FALSE,
                               only_most_recent = FALSE,
                               ...) {
  # Pass along all ... input to the dir function.
  my_folders <- dir(path = path, pattern = pattern, full.names = full.names, ...)

  # Get regex pattern of timestamp
  regex_ts <- gsub("%[mdHMS]", "\\\\d{2}", timestamp_format)
  regex_ts <- gsub("%Y", "\\\\d{4}", regex_ts)
  regex_ts <- paste0(name_timestamp_seperator, regex_ts)

  # Get names without timestamp
  my_folders_wo_timeStamp <- sub(regex_ts, "", my_folders)

  # If no duplicates should be removed, return now
  if (!only_most_recent) {
    if (remove_timestamp) return(my_folders_wo_timeStamp) else return(my_folders)
  }

  if (any(duplicated(my_folders_wo_timeStamp))) {
    # Get names of the duplicated folders.
    duplicated_folders <- my_folders_wo_timeStamp[duplicated(my_folders_wo_timeStamp)]

    # For each duplicated run, get the newest one, and remove the other (old
    # ones) from "my_folders".
    for (name in duplicated_folders) {
      # Get folders with the same name
      folders_with_same_name <- grep(paste0(name,regex_ts), my_folders, value = TRUE)

      # Get timestamps of these folders
      timeStamps <- as.POSIXct(sub(paste0(name, name_timestamp_seperator),
                                   "",
                                   folders_with_same_name),
                               format = timestamp_format)

      # Get the newest folder, i.e. the one with the 'maximum' timestamp.
      newest_duplicate <- folders_with_same_name[which.max(timeStamps)]

      # The other folders are then the old_duplicates
      old_duplicates <- folders_with_same_name[grep(newest_duplicate,
                                                    folders_with_same_name,
                                                    invert = TRUE)]

      # Remove the old_duplicates.
      my_folders <- my_folders[!my_folders %in% old_duplicates]
    }
  }

  # Remove timestamp of folder list wo duplicates
  my_folders_wo_timeStamp <- sub(regex_ts, "", my_folders)
  if (remove_timestamp) return(my_folders_wo_timeStamp) else return(my_folders)
}
