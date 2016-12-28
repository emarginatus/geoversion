#' test if characters represent valid CRS
#' @param x the character vector to test
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>%
#' @importFrom sp CRS
valid_crs <- function(x){
  assert_that(is.character(x))
  sapply(
    x,
    function(crs){
      current_class <- try(CRS(crs), silent = TRUE) %>%
        class()
      !"try-error" %in% current_class
    }
  ) %>%
    unname()
}
