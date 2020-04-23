#' read_items_from_mifs
#'
#' read_items_from_mifs reads specific items from the remind reporting mif
#' files.
#'
#' @param mif_filepaths A vector of strings containing the paths to the mif
#'   files with the raw data.
#' @param regexs_of_items A vector of regex patterns identifying the variables
#'   to extract from the mif files.
#' @param returnAsMagpie Logical indicating return type. Either list of quitte
#'   objects, or magpie.
#' @param n_cores Integer indicating the number of cores to be used in parallel
#'
#' @return my_data Either a list of quitte objects (default) or a single magpie
#'   object,
#'
#' @export
#'
#' @importFrom magclass read.report getNames mbind
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>%
#' @importFrom pbapply pblapply
read_items_from_mifs <- function (mif_filepaths,
                                  regexs_of_items,
                                  returnAsMagpie = FALSE,
                                  n_cores = 1) {
  # Collpase the regexs into a single regex
  regex <- paste(regexs_of_items, collapse = "|")

  # Read and filter the mifs
  if (n_cores == 1) {
    my_data <- pblapply(mif_filepaths,
                        function(y) read.report(y, as.list = FALSE) %>% `[`(,,grep(regex, getNames(.), value=T)))
  } else {
    my_data <- mclapply(mif_filepaths,
                        function(y) read.report(y, as.list = FALSE) %>% `[`(,,grep(regex, getNames(.), value=T)),
                        mc.cores = n_cores)
  }


  # Depending on retrun type use mbind or as.quitte
  if (returnAsMagpie) {
    my_data <- mbind(as.vector(my_data))
  } else {
    my_data <- lapply(my_data, as.quitte)
  }

  return(my_data)
}
