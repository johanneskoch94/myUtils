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
#' @importFrom pbapply pblapply
#' @importFrom parallel mclapply
read_items_from_mifs <- function (mif_filepaths,
                                  regexs_of_items,
                                  returnAsMagpie = FALSE,
                                  n_cores = 1) {
  # Collpase the regexs into a single regex
  regex <- paste0("(",paste(regexs_of_items, collapse = ")|("),")")

  # Read and filter the mifs
  if (n_cores == 1) {
    my_data <- pbapply::pblapply(mif_filepaths,
                                 function(y){
                                   h1 <- magclass::read.report(y, as.list = FALSE)
                                   h2 <- h1[,,grep(regex, magclass::getNames(h1), value=T)]
                                   return(h2)
                                 })
  } else {
    my_data <- parallel::mclapply(mif_filepaths,
                                  function(y){
                                    h1 <- magclass::read.report(y, as.list = FALSE)
                                    h2 <- h1[,,grep(regex, magclass::getNames(h1), value=T)]
                                  },
                                  mc.cores = n_cores)
  }


  # Depending on retrun type use mbind or as.quitte
  if (returnAsMagpie) {
    my_data <- magclass::mbind(as.vector(my_data))
  } else {
    my_data <- lapply(my_data, quitte::as.quitte)
  }

  return(my_data)
}
