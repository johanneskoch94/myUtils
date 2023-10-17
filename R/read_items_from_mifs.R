#' read_items_from_mifs
#'
#' read_items_from_mifs reads specific items from the remind mif files.
#'
#' @param mif_filepaths A vector of strings containing the file paths.
#' @param regexs_of_items A vector of regex patterns identifying the variables to extract.
#' @param col_types A string with the col_type specification for readr::read_delim
#'
#' @return A tibble
#' @export
read_items_from_mifs <- function (mif_filepaths, regexs_of_items, col_types = "cccccddddddddddddddddddd-") {
  rlang::check_installed("readr")

  # Collapse the regexs into a single regex
  regex <- paste0("(", paste(regexs_of_items, collapse = ")|("), ")")

  purrr::map(mif_filepaths, function(x) {
    readr::read_delim(x,
                      delim = ";",
                      na = "N/A",
                      col_types = col_types,
                      name_repair = "minimal",
                      progress = FALSE,
                      lazy = TRUE) %>%
      dplyr::filter(grepl(regex, .data$Variable)) %>%
      tidyr::pivot_longer(tidyselect::starts_with("2"), names_to = "year") %>%
      dplyr::rename_with(tolower)
  }) %>% purrr::list_rbind()
}
