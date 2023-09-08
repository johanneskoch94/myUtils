#' read_items_from_gdxs
#'
#' Extract select items from gdxs.
#'
#' @param gdx_filepaths A vector of strings with the paths to all gdx files.
#' @param gdx_items A vector of strings with the gdx items. The gams suffix notation can be used to specify the field.
#'  (defaults to .l, i.e. level).
#' @param remind_names If true, assume filepaths to remind output folders/fulldata.gdx files.
#'
#' @return A list of tibbles with the data for each gdx_item requested.
#'
#' @examples \dontrun{
#' read_items_from_gdxs(gdx_filepaths = c("file1.gdx", "file2.gdx"),
#'                      gdx_items = c("item1", "item2.l", "item3.m"))}
#' @export
read_items_from_gdxs <- function(gdx_filepaths, gdx_items, remind_names = TRUE) {

  rlang::check_installed("gamstransfer")

  # Check file paths
  if (!all(file.exists(gdx_filepaths))) {
    rlang::abort("Files do not exist.")
  }

  # Get run (=file) names and make them unique
  run_names <- if (remind_names) get_REMIND_run_names(gdx_filepaths) else basename(gdx_filepaths)
  run_names <- make.names(run_names, unique = TRUE)
  names(gdx_filepaths) <- run_names

  item_names <- sub("\\..*", "", gdx_items)
  names(item_names) <- item_names

  # Load data
  l <- purrr::map(gdx_filepaths, function(file, name) {
    x <- gamstransfer::Container$new()
    x$read(file, item_names)
    purrr::map(item_names, ~tibble::as_tibble(x[.x]$records))
  })
  # Restructure list by items
  x <- purrr::map(item_names, ~purrr::map_dfr(l, .x, .id = "run"))

  # Sets are returned with a _<DIGIT> suffix. Remove the suffix, but make sure names are unique.
  numbered_sets <- purrr::map(x, ~colnames(.x)[grepl("_\\d$", colnames(.x))])
  clean_sets <- purrr::map(numbered_sets, ~make.unique(sub("_\\d$", "", .x), sep = "_"))
  sets_2_rename <- purrr::map2(numbered_sets, clean_sets, ~`names<-`(.x, .y))

  # Figure out which fields are requested, and thus which ones to drop from loaded data
  item_fields <- purrr::map2_chr(gdx_items, item_names, ~sub(.y, "", .x))
  item_fields <- sub("^$", ".l", item_fields)
  field_names <- c(".l" = "level", ".m" = "marginal", ".lo" = "lower", ".up" = "upper", ".scale" = "scale")
  for (p in seq_along(field_names)) item_fields <- sub(names(field_names)[p], field_names[p], item_fields)
  names(item_fields) <- item_names
  fields_2_drop <- purrr::map(item_fields, ~field_names[field_names != .x])

  x %>%
    # Keep only columns of interest
    purrr::map2(fields_2_drop, ~dplyr::select(.x, -tidyselect::any_of(.y))) %>%
    # Rename columns
    purrr::map2(item_fields, ~dplyr::rename(.x, "value" = tidyselect::all_of(.y))) %>%
    purrr::map2(sets_2_rename, ~dplyr::rename(.x, tidyselect::all_of(.y))) %>%
    # Convert factors to character
    purrr::map(~dplyr::mutate(.x, dplyr::across(tidyselect::where(is.factor), as.character)))
}
