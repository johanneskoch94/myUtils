#' get_REMIND_run_names
#'
#' get_REMIND_run_names returns the names of REMIND runs.
#'
#' @param paths A vector of stings made up of either:
#' \itemize{
#'   \item Names of the REMIND output-folders,
#'   \item Paths to the REMIND output-folders,
#'   \item Paths to files within the REMIND output-folders.
#' }
#'
#' @return A vector of strings with the names of the REMIND runs.
#' @export
get_REMIND_run_names <- function(paths) {
  # Remove timestamps
  rm_timestamp(ifelse(dir.exists(paths),
                      basename(paths),
                      ifelse(dirname(paths) != ".", basename(dirname(paths)), paths)))
}
