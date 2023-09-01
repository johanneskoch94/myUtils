#' rm_timestamp
#'
#' rm_timestamp removes timestamps from strings.
#'
#' @param strings A vector of stings.
#' @param sep A string with the timestamp seperator.
#' @param timestamp_format A string with the timestamp format, as input for
#'   as.POSIXct.
#'
#' @return A vecotr of strings with the timestamps removed.
#' @export
rm_timestamp <- function(strings, sep = "_", timestamp_format = "%Y-%m-%d_%H.%M.%S") {

  # Get regex pattern of timestamp
  regex_timestamp <- gsub("%[mdHMS]", "\\\\d{2}", timestamp_format)
  regex_timestamp <- gsub("%Y", "\\\\d{4}", regex_timestamp)
  regex_timestamp <- paste0(sep, regex_timestamp)

  # Substitute timestamps with nothing (thereby removing them)
  sub(regex_timestamp, "", strings)
}
