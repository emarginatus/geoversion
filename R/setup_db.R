#' Write empty tables to the database
#' @param connection a DBIconnection
#' @export
#' @importFrom assertthat assert_that
#' @importFrom DBI dbWriteTable
setup_db <- function(connection){
  assert_that(inherits(connection, "DBIConnection"))
  structure <- list(
    coordinates = data.frame(
      hash = character(0),
      succession = integer(0),
      x = numeric(0),
      y = numeric(0),
      stringsAsFactors = FALSE
    ),
    staging_coordinates = data.frame(
      hash = character(0),
      succession = integer(0),
      x = numeric(0),
      y = numeric(0),
      stringsAsFactors = FALSE
    ),
    feature = data.frame(
      hash = character(0),
      type = character(0),
      stringsAsFactors = FALSE
    ),
    staging_feature = data.frame(
      hash = character(0),
      type = character(0),
      stringsAsFactors = FALSE
    ),
    features = data.frame(
      hash = character(0),
      feature = character(0),
      stringsAsFactors = FALSE
    ),
    staging_features = data.frame(
      hash = character(0),
      feature = character(0),
      stringsAsFactors = FALSE
    ),
    element = data.frame(
      hash = character(0),
      features = character(0),
      spawn = numeric(0),
      destroy = numeric(0),
      stringsAsFactors = FALSE
    ),
    staging_element = data.frame(
      hash = character(0),
      features = character(0),
      spawn = numeric(0),
      destroy = numeric(0),
      stringsAsFactors = FALSE
    ),
    layerelement = data.frame(
      layer = character(0),
      id = integer(0),
      hash = character(0)
    ),
    staging_layerelement = data.frame(
      layer = character(0),
      id = integer(0),
      hash = character(0)
    ),
    layer = data.frame(
      hash = character(0),
      name = character(0),
      type = character(0),
      spawn = numeric(0),
      destroy = numeric(0)
    ),
    staging_layer = data.frame(
      hash = character(0),
      name = character(0),
      type = character(0),
      spawn = numeric(0),
      destroy = numeric(0)
    ),
    attribute = data.frame(
      id = character(0),
      name = character(0),
      type = character(0),
      stringsAsFactors = FALSE
    ),
    staging_attribute = data.frame(
      id = character(0),
      name = character(0),
      type = character(0),
      stringsAsFactors = FALSE
    ),
    attributevalue = data.frame(
      element = character(0),
      attribute = character(0),
      value = character(0),
      spawn = numeric(0),
      destroy = numeric(0),
      stringsAsFactors = FALSE
    ),
    staging_attributevalue = data.frame(
      element = integer(0),
      attribute = character(0),
      value = character(0),
      spawn = numeric(0),
      destroy = numeric(0),
      stringsAsFactors = FALSE
    )
  )
  for (i in seq_along(structure)) {
    dbWriteTable( # nolint
      conn = connection,
      name = names(structure)[i],
      value = structure[[i]])
  }
}
