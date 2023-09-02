#' read_items_from_mifs
#'
#' read_items_from_mifs reads specific items from the remind mif files.
#'
#' @param mif_filepaths A vector of strings containing the file paths.
#' @param regexs_of_items A vector of regex patterns identifying the variables to extract.
#' @param returnAsMagpie If TRUE return magclass object instead of a list of tibbles.
#'
#' @return A list of tibbles (default) or a single magpie object.
#' @export
read_items_from_mifs <- function (mif_filepaths, regexs_of_items, returnAsMagpie = FALSE) {
  # Collapse the regexs into a single regex
  regex <- paste0("(", paste(regexs_of_items, collapse = ")|("), ")")

  # Read and filter the mifs
  my_data <- purrr::map(mif_filepaths,
                        function(y) {
                          h1 <- magclass::read.report(y, as.list = FALSE)
                          h2 <- h1[, , grep(regex, magclass::getNames(h1), value = TRUE)]
                          return(h2)
                        })

  # Depending on retrun type use mbind or as.quitte
  if (returnAsMagpie) {
    my_data <- magclass::mbind(as.vector(my_data))
  } else {
    my_data <- purrr::map(my_data, tibble::as_tibble)
  }

  my_data
}
