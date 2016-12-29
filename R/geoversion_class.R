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
    Transformation = "data.frame",
    Reference = "data.frame"
  ),
  prototype = prototype(
    Coordinates = data.frame(
      hash = character(0),
      succession = integer(0),
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
      crs = character(0),
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
    ),
    Transformation = data.frame(
      source_crs = character(0),
      target_crs = character(0),
      stringsAsFactors = FALSE
    ),
    Reference = data.frame(
      source_crs = character(0),
      source_x = numeric(0),
      source_y = numeric(0),
      target_x = numeric(0),
      target_y = numeric(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name noNA
setValidity(
  "geoVersion",
  function(object){
    assert_that(has_name(object@Coordinates, "hash"))
    assert_that(has_name(object@Coordinates, "succession"))
    assert_that(has_name(object@Coordinates, "x"))
    assert_that(has_name(object@Coordinates, "y"))

    assert_that(has_name(object@Feature, "hash"))
    assert_that(has_name(object@Feature, "type"))

    assert_that(has_name(object@Features, "hash"))
    assert_that(has_name(object@Features, "feature"))

    assert_that(has_name(object@LayerElement, "id"))
    assert_that(has_name(object@LayerElement, "features"))
    assert_that(has_name(object@LayerElement, "crs"))

    assert_that(has_name(object@Attribute, "id"))
    assert_that(has_name(object@Attribute, "name"))
    assert_that(has_name(object@Attribute, "type"))

    assert_that(has_name(object@AttributeValue, "element"))
    assert_that(has_name(object@AttributeValue, "attribute"))
    assert_that(has_name(object@AttributeValue, "value"))

    assert_that(has_name(object@Transformation, "source_crs"))
    assert_that(has_name(object@Transformation, "target_crs"))

    assert_that(has_name(object@Reference, "source_crs"))
    assert_that(has_name(object@Reference, "source_x"))
    assert_that(has_name(object@Reference, "source_y"))
    assert_that(has_name(object@Reference, "target_x"))
    assert_that(has_name(object@Reference, "target_y"))

    assert_that(noNA(object@Coordinates)) #nolint
    assert_that(noNA(object@Feature)) #nolint
    assert_that(noNA(object@Features)) #nolint
    assert_that(noNA(object@LayerElement[, c("id", "features")])) #nolint
    assert_that(noNA(object@Attribute)) #nolint
    assert_that(noNA(object@AttributeValue)) #nolint
    assert_that(noNA(object@Transformation$source_crs)) #nolint
    assert_that(noNA(object@Reference)) #nolint

    if (anyDuplicated(object@Coordinates[, c("hash", "succession")])) {
      stop("Duplicated hash - succession combinations in the Coordinates slot")
    }
    if (anyDuplicated(object@Feature$hash)) {
      stop("Duplicated hashes in the Feature slot")
    }
    if (anyDuplicated(object@LayerElement$id)) {
      stop("Duplicated id in the LayerElement slot")
    }
    if (anyDuplicated(object@LayerElement$features)) {
      stop("Duplicated features in the LayerElement slot")
    }
    if (anyDuplicated(object@Attribute$id)) {
      stop("Duplicated id in the Atribute slot")
    }
    if (anyDuplicated(object@AttributeValue[, c("element", "attribute")])) {
      stop("Duplicated id in the AtributeValue slot")
    }
    if (anyDuplicated(object@Transformation$source_crs)) {
      stop("Duplicated source_crs in the Transformation slot")
    }

    if (any(!object@Feature$type %in% c("S", "H"))) {
      stop("Feature type can be only 'S' or 'H'")
    }

    assert_that(all(object@Coordinates$hash %in% object@Feature$hash))
    assert_that(all(object@Feature$hash %in% object@Coordinates$hash))
    assert_that(all(object@Features$feature %in% object@Feature$hash))
    assert_that(all(object@LayerElement$features %in% object@Features$hash))
    assert_that(all(object@Attribute$id %in% object@AttributeValue$attribute))
    assert_that(all(object@AttributeValue$attribute %in% object@Attribute$id))
    assert_that(all(object@LayerElement$id %in% object@AttributeValue$element))
    assert_that(all(object@AttributeValue$element %in% object@LayerElement$id))
    assert_that(
      all(object@Transformation$source_crs %in% object@Reference$source_crs)
    )
    assert_that(
      all(object@Reference$source_crs %in% object@Transformation$source_crs)
    )

    test_crs <- unique(object@LayerElement$crs)
    no_transformation <- valid_crs(test_crs)
    if (
      !all(test_crs[!no_transformation] %in% object@Transformation$source_crs)
    ) {
      stop(
"All non standards CRS in the LayerElement slot must have a match in source_crs field
of the Transformation slot"
      )
    }
    if (
      !all(object@Transformation$source_crs %in% test_crs[!no_transformation])
    ) {
      stop(
"Unrequired crs in the source_crs field of the Transformation slot"
      )
    }
    test_crs <- unique(object@Transformation$target_crs)
    if (!all(valid_crs(test_crs))) {
      stop("All crs in the to field target_crs the Transformation slot must be valid")
    }
  }
)
