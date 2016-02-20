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
    CRS = "CRS"
  ),
  prototype = prototype(
    Coordinates = data.frame(
      Hash = character(0),
      Order = integer(0),
      X = numeric(0),
      Y = numeric(0),
      stringsAsFactors = FALSE
    ),
    Feature = data.frame(
      Hash = character(0),
      Type = character(0),
      stringsAsFactors = FALSE
    ),
    Features = data.frame(
      Hash = character(0),
      Feature = character(0),
      stringsAsFactors = FALSE
    ),
    LayerElement = data.frame(
      ID = integer(0),
      Features = character(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
setValidity(
  "geoVersion",
  function(object){
    if (anyDuplicated(object@Feature$Hash)) {
      stop("Duplicated hashes in the Feature slot")
    }
    if (any(!object@Feature$Type %in% c("S", "H"))) {
      stop("Feature type can be only 'S' or 'H'")
    }
  }
)
