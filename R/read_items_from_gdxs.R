#' read_items_from_gdxs
#'
#' Extract select items from gdxs.
#'
#' @param gdx_filepaths A vector of strings with the paths to all gdx files.
#' @param gdx_items A list of lists, with "name" and "field" sublist-names of
#'   select gdx_items.
#' @param remind_names If true, assume filepaths to remind output
#'   folders/fulldata.gdx files.
#'
#' @return A list of tibbles with the data for each gdx_item requested.
#'
#' @examples \dontrun{
#' read_items_from_gdxs(gdx_filepaths = c("file1.gdx",
#'                                        "file2.gdx"),
#'                      gdx_items = list(list(name = "item1", field = "l"),
#'                                       list(name = "item2", field = "m")))}
#'
#' @export
#'
read_items_from_gdxs <- function(gdx_filepaths, gdx_items, remind_names = TRUE) {
  # Add "field" entry if missing
  gdx_items <- purrr::map(gdx_items,
                          ~ if(!"field" %in% names(.x)) c(.x, "field" = "l") else .x)

  # Get run (=file) names and make them unique
  run_names <- if (remind_names) get_REMIND_run_names(gdx_filepaths) else basename(gdx_filepaths)
  run_names <- make.names(run_names, unique = T)

  possible_read_gdx <- purrr::possibly(quitte::read.gdx, otherwise = "Item does not exist in gdx.")

  # Get gdx data and combine into tibble if possible
  gdx_data <- tidyr::expand_grid(file = paste(gdx_filepaths,run_names),
                                 item = paste(purrr::map_chr(gdx_items,`$`,"name"),
                                              purrr::map_chr(gdx_items,`$`,"field"))) %>%
    tidyr::separate(.data$file, c("file","name"), " ") %>%
    tidyr::separate(.data$item, c("item","field"), " ") %>%
    dplyr::nest_by(.data$item, .data$field, .key = "gdxs") %>%
    # Read in data
    dplyr::mutate(my_data = purrr::map(.data$gdxs$file,
                                       ~ possible_read_gdx(.x, .data$item, .data$field)) %>%
             rlang::set_names(.data$gdxs$name) %>%
             list() %>%
             rlang::set_names(.data$item)) %>%
    dplyr::pull(.data$my_data) %>%
    purrr::map(~ purrr::map_dfr(.x, ~..1, .id = "run") %>%
                 dplyr::rename("value" = tidyselect::last_col()))
    # CHECK the ">=" !!!!!!!!!!!!!!!!!!!!! TO DO !!!!!!!!!!!!!!!!!!!!!!!!!!
    # map(~if( sum(map_chr(.x[[1]], class) == "numeric")>=0 ) {map_dfr(.x, ~..1, .id = "run") %>%
    #     rename("value" = last_col())} else {.x})

  return(gdx_data)
}
