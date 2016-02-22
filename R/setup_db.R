#' Write empty tables to the database
#' @param connection a DBIconnection
#' @export
#' @importFrom assertthat assert_that
#' @importFrom DBI dbWriteTable
setup_db <- function(connection){
  assert_that(inherits(connection, "DBIConnection"))
  structure <- list(
    coordinates = data.frame(
      Hash = character(0),
      Order = integer(0),
      X = numeric(0),
      Y = numeric(0),
      stringsAsFactors = FALSE
    ),
    feature = data.frame(
      Hash = character(0),
      Type = character(0),
      stringsAsFactors = FALSE
    ),
    features = data.frame(
      Hash = character(0),
      Feature = character(0),
      stringsAsFactors = FALSE
    ),
    element = data.frame(
      ID = integer(0),
      Features = character(0),
      stringsAsFactors = FALSE
    ),
    attribute = data.frame(
      ID = character(0),
      Name = character(0),
      Type = character(0),
      stringsAsFactors = FALSE
    ),
    attributevalue = data.frame(
      Element = character(0),
      Attribute = character(0),
      Value = character(0),
      stringsAsFactors = FALSE
    )
  )
  for (i in seq_along(structure)) {
    dbWriteTable(
      conn = connection,
      name = names(structure)[i],
      value = structure[[i]])
  }
}
