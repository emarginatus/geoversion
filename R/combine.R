#' Combine objects
#' @param ... the objects
#' @name combine
#' @rdname combine
#' @exportMethod combine
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "combine",
  def = function(...){
    standard.generic("combine") #nocov
  }
)

#' @rdname combine
#' @importFrom methods setMethod new
#' @importFrom dplyr %>% bind_rows distinct_
#' @include geoversion_class.R
setMethod(
  f = "combine",
  signature = "geoVersion",
  definition = function(...){
    coordinates <- lapply(
      list(...),
      function(x){
        x@Coordinates
      }
    ) %>%
      bind_rows() %>%
      distinct_() %>%
      as.data.frame()
    feature <- lapply(
      list(...),
      function(x){
        x@Feature
      }
    ) %>%
      bind_rows() %>%
      distinct_() %>%
      as.data.frame()
    features <- lapply(
      list(...),
      function(x){
        x@Features
      }
    ) %>%
      bind_rows() %>%
      distinct_() %>%
      as.data.frame()
    layer.element <- lapply(
      list(...),
      function(x){
        x@LayerElement
      }
    ) %>%
      bind_rows() %>%
      distinct_() %>%
      as.data.frame()
    attribute <- lapply(
      list(...),
      function(x){
        x@Attribute
      }
    ) %>%
      bind_rows() %>%
      distinct_() %>%
      as.data.frame()
    attribute.value <- lapply(
      list(...),
      function(x){
        x@AttributeValue
      }
    ) %>%
      bind_rows() %>%
      distinct_() %>%
      as.data.frame()
    crs <- lapply(
      list(...),
      function(x){
        x@CRS
      }
    ) %>%
      unique()
    if (length(crs) > 1) {
      stop("CRS not unique", call. = FALSE)
    }
    new(
      "geoVersion",
      Coordinates = coordinates,
      Feature = feature,
      Features = features,
      LayerElement = layer.element,
      Attribute = attribute,
      AttributeValue = attribute.value,
      CRS = crs[[1]]
    )
  }
)
