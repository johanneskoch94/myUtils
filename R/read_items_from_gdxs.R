#' read_items_from_gdxs
#'
#' Extract select items from gdxs.
#'
#' @param gdx_filepaths A vector of strings with the paths to all gdx files.
#' @param gdx_items A list of lists, with "name" and "field" sublist-names of select
#' gdx_items.
#' @return A list of tibbles with the data for each gdx_item requested.
#' @export
#'
#' @importFrom quitte read.gdx
#' @importFrom dplyr %>% rename tibble sym left_join
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom rlang :=
read_items_from_gdxs <- function(gdx_filepaths, gdx_items) {
  # Declare return variable
  gdx_data <- NULL

  # Get run names
  run_names <- get_REMIND_run_names(gdx_filepaths)

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

      # Append the "tmp"_mif_data to MAgPIE object.
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
