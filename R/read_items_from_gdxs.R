#' read_items_from_gdxs
#'
#' Extract select items from gdxs.
#'
#' @param gdx_filepaths A vector of strings with the paths to all gdx files.
#' @param gdx_items A list of lists, with "name" and "field" sublist-names of
#'   select gdx_items.
#' @param remind_names If true, assume filepaths to remind output
#'   folders/fulldata.gdx files.
#' @return A list of tibbles with the data for each gdx_item requested.
#' @export
#'
#' @importFrom quitte read.gdx
#' @importFrom dplyr %>% rename tibble sym left_join
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom rlang :=
read_items_from_gdxs <- function(gdx_filepaths, gdx_items, remind_names = T) {
  # Declare return variable
  gdx_data <- NULL

  # Get run (=file) names
  run_names <- if (remind_names) get_REMIND_run_names(gdx_filepaths) else basename(gdx_filepaths)

  # Whatever the case, check that the run_names are unqiue. If not, number them.
  run_names <- make.names(run_names, unique = T)

  # Loop over gdx_items
  for (gdx_item in gdx_items) {
    # Tell the user what's going on.
    cat(paste0("  Reading in ",gdx_item$name," from gdxs "))
    showWarning <- FALSE

    # Loop over gdx files and load in the item data. The data is first loaded as
    # tmp_item_data, manipulated a bit, and then added to the item_data object.
    item_data <- NULL
    for (gdx_file in gdx_filepaths) {
      run_name <- run_names[which(gdx_filepaths==gdx_file)]

      # Load in gdx data as quitte object, using the quitte function read.gdx.
      tmp_item_data <- quitte::read.gdx(gdx_file,
                                        requestList.name = gdx_item$name,
                                        fields = gdx_item$field)

      # In the case only a single variable is returned, a tibble has to be explicitly created...
      if (is.data.frame(tmp_item_data)) {
        tmp_item_data <- tmp_item_data %>%
          rename(!!run_name := names(tmp_item_data)[grepl("^m$|^M$|^l$|^L$|^value$", names(tmp_item_data))])
      } else {
        tmp_item_data <- tibble(ttot = "-", all_regi = "GLO", !!sym(run_name) := tmp_item_data)
      }

      # Append the "tmp"_item_data to tibble object.
      if (gdx_file==gdx_filepaths[1]) {
        item_data <- tmp_item_data
      } else {
        if (dim(item_data)[1]!=dim(tmp_item_data)[1]) showWarning <- TRUE
        item_data <- suppressMessages(left_join(item_data, tmp_item_data))
      }

      # Share progress with user.
      cat(".")
    }
    # Show warning if necessary
    if (showWarning) cat("\t Warning: gdxs don't have same granularity!")

    # Save item data in named list.
    item_data <- item_data %>% pivot_longer(all_of(run_names), names_to = "run")
    gdx_data[[gdx_item$name]] <- item_data

    cat(" done.\n")
  }

  return(gdx_data)
}


#' read_items_from_gdxs2
#'
#' @param gdx_filepaths s
#' @param gdx_items s
#' @param remind_names s
#'
#' @return s
#' @export
#'
#' @importFrom purrr map map_chr map_dfr
#' @importFrom quitte read.gdx
#' @importFrom dplyr %>% rename nest_by mutate pull
#' @importFrom tidyr separate expand_grid
#' @importFrom tidyselect last_col
#' @importFrom rlang set_names
read_items_from_gdxs2 <- function(gdx_filepaths, gdx_items, remind_names = T) {
  # Add "field" entry if missing
  gdx_items <- map(gdx_items, ~if( !"field" %in% names(.x) ) {c(.x, "field" = "l")} else {.x} )

  # Get run (=file) names and make them unique
  run_names <- if (remind_names) get_REMIND_run_names(gdx_filepaths) else basename(gdx_filepaths)
  run_names <- make.names(run_names, unique = T)

  # Get gdx data and combine into tibble if possible
  gdx_data <- expand_grid(file = paste(gdx_filepaths,run_names),
                          item = paste(map_chr(gdx_items,`$`,"name"), map_chr(gdx_items,`$`,"field"))) %>%
    separate(file, c("file","name"), " ") %>%
    separate(item, c("item","field"), " ") %>%
    nest_by(item, field, .key = "gdxs") %>%
    mutate(data = list(map(gdxs$file, quitte::read.gdx, item, field) %>%
                         set_names(gdxs$name)) %>%
             set_names(item)) %>%
    pull(data) %>%
    map(~if( sum(map_chr(.x[[1]], class) == "numeric")>1 ) {map_dfr(.x, ~..1, .id = "run") %>%
        rename("value" = last_col())} else {.x})

  return(gdx_data)
}
