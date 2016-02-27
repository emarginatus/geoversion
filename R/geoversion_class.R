#' The geoVersion class
#' @name geoVersion-class
#' @rdname geoVersion-class
#' @exportClass geoVersion
#' @aliases geoVersion-class
#' @importFrom methods setClass
#' @importClassesFrom sp CRS
#' @docType class
setClass(
  "geoVersion",
  representation = representation(
    Coordinates = "data.frame",
    Feature = "data.frame",
    Features = "data.frame",
    LayerElement = "data.frame",
    Attribute = "data.frame",
    AttributeValue = "data.frame",
    CRS = "CRS"
  ),
  prototype = prototype(
    Coordinates = data.frame(
      hash = character(0),
      order = integer(0),
      x = numeric(0),
      y = numeric(0),
      stringsAsFactors = FALSE
    ),
    Feature = data.frame(
      hash = character(0),
      type = character(0),
      stringsAsFactors = FALSE
    ),
    Features = data.frame(
      hash = character(0),
      feature = character(0),
      stringsAsFactors = FALSE
    ),
    LayerElement = data.frame(
      id = integer(0),
      features = character(0),
      stringsAsFactors = FALSE
    ),
    Attribute = data.frame(
      id = character(0),
      name = character(0),
      type = character(0),
      stringsAsFactors = FALSE
    ),
    AttributeValue = data.frame(
      element = character(0),
      attribute = character(0),
      value = character(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
setValidity(
  "geoVersion",
  function(object){
    if (anyDuplicated(object@Feature$hash)) {
      stop("Duplicated hashes in the Feature slot")
    }
    if (any(!object@Feature$type %in% c("S", "H"))) {
      stop("Feature type can be only 'S' or 'H'")
    }
  }
)
