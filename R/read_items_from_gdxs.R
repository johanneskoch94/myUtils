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
#' @importFrom purrr map map_chr map_dfr
#' @importFrom quitte read.gdx
#' @importFrom dplyr %>% rename nest_by mutate pull
#' @importFrom tidyr separate expand_grid
#' @importFrom tidyselect last_col
#' @importFrom rlang set_names .data
read_items_from_gdxs <- function(gdx_filepaths, gdx_items, remind_names = T) {
  # Add "field" entry if missing
  gdx_items <- map(gdx_items, ~if( !"field" %in% names(.x) ) {c(.x, "field" = "l")} else {.x} )

  # Get run (=file) names and make them unique
  run_names <- if (remind_names) get_REMIND_run_names(gdx_filepaths) else basename(gdx_filepaths)
  run_names <- make.names(run_names, unique = T)

  # Get gdx data and combine into tibble if possible
  gdx_data <- expand_grid(file = paste(gdx_filepaths,run_names),
                          item = paste(map_chr(gdx_items,`$`,"name"), map_chr(gdx_items,`$`,"field"))) %>%
    separate(.data$file, c("file","name"), " ") %>%
    separate(.data$item, c("item","field"), " ") %>%
    nest_by(.data$item, .data$field, .key = "gdxs") %>%
    # Read in data
    mutate(my_data = map(.data$gdxs$file, quitte::read.gdx, .data$item, .data$field) %>%
             set_names(.data$gdxs$name) %>%
             list() %>%
             set_names(.data$item)) %>%
    pull(.data$my_data) %>%
    map(~ map_dfr(.x, ~..1, .id = "run") %>% rename("value" = last_col()))
    # CHECK the ">=" !!!!!!!!!!!!!!!!!!!!! TO DO !!!!!!!!!!!!!!!!!!!!!!!!!!
    # map(~if( sum(map_chr(.x[[1]], class) == "numeric")>=0 ) {map_dfr(.x, ~..1, .id = "run") %>%
    #     rename("value" = last_col())} else {.x})

  return(gdx_data)
}
