#' Convert a spatial object
#' @param object the object to convert
#' @param stable.id the name of the variable that holds the stable ID
#' @param ... other parameters
#' @name convert
#' @rdname convert
#' @exportMethod convert
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "convert",
  def = function(object, stable.id, ...){
    standard.generic("convert") #nocov
  }
)

#' @rdname convert
#' @importFrom methods setMethod
#' @importClassesFrom sp Polygon
#' @importFrom digest sha1
#' @importFrom methods new
setMethod(
  f = "convert",
  signature = "Polygon",
  definition = function(object, stable.id, ...){
    hash <- sha1(list(hole = object@hole, coords = object@coords))
    coordinates <- data.frame(
      hash,
      seq_len(nrow(object@coords)),
      object@coords,
      stringsAsFactors = FALSE
    )
    colnames(coordinates) <- c("hash", "succession", "x", "y")
    return(
      new(
        "geoVersion",
        Coordinates = coordinates,
        Feature = data.frame(
          hash = hash,
          type = ifelse(object@hole, "H", "S"),
          stringsAsFactors = FALSE
        )
      )
    )
  }
)

#' @rdname convert
#' @importFrom methods setMethod
#' @importClassesFrom sp Polygons
setMethod(
  f = "convert",
  signature = "Polygons",
  definition = function(object, stable.id, ...){
    poly <- convert(object@Polygons)
    poly@Features <- data.frame(
      hash = object@ID,
      feature = poly@Feature$hash,
      stringsAsFactors = FALSE
    )
    return(poly)
  }
)

#' @rdname convert
#' @importFrom methods setMethod
#' @importFrom dplyr %>% group_by_ summarise_ mutate_ inner_join select_ filter_ rowwise mutate_ rename_ mutate_each funs arrange_ do_ distinct_ left_join
#' @importFrom digest sha1
#' @importFrom tidyr gather unnest_
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom methods validObject
#' @importClassesFrom sp SpatialPolygonsDataFrame
setMethod(
  f = "convert",
  signature = "SpatialPolygonsDataFrame",
  definition = function(object, stable.id, ...){
    assert_that(is.string(stable.id))
    assert_that(has_name(object, stable.id))
    dots <- list(...)
    if (is.null(dots$crs.id)) {
      crs <- object@proj4string@projargs %>%
        rep(length(object))
      dots$transformation <- data.frame(
        source_crs = character(0),
        target_crs = character(0),
        stringsAsFactors = FALSE
      )
      dots$reference <- data.frame(
        source_crs = character(0),
        source_x = numeric(0),
        source_y = numeric(0),
        target_x = numeric(0),
        target_y = numeric(0),
        stringsAsFactors = FALSE
      )
    } else {
      assert_that(is.string(dots$crs.id))
      assert_that(has_name(object, dots$crs.id))
      crs <- object[[dots$crs.id]]
      test_crs <- unique(crs)
      no_transformation <- valid_crs(as.character(test_crs))
      if (all(no_transformation)) {
        dots$transformation <- data.frame(
          source_crs = character(0),
          target_crs = character(0),
          stringsAsFactors = FALSE
        )
        dots$reference <- data.frame(
          source_crs = character(0),
          source_x = numeric(0),
          source_y = numeric(0),
          target_x = numeric(0),
          target_y = numeric(0),
          stringsAsFactors = FALSE
        )
      } else {
        if (is.null(dots$reference) || is.null(dots$transformation)) {
          stop("Non standard CRS require both reference and transformation.")
        }
        dots$reference <- dots$reference %>%
          group_by_(~source_crs) %>%
          do_(
            ref = ~select_(., ~source_x, ~source_y, ~target_x, ~target_y) %>%
              arrange_(~source_x, ~source_y, ~target_x, ~target_y) %>%
              mutate_(
                hash = ~list(source_x, source_y, target_x, target_y) %>%
                  sha1()
              )
          ) %>%
          unnest_(~ref)
        crs_hash <- dots$reference %>%
          distinct_(~source_crs, ~hash) %>%
          rename_(old = ~source_crs)
        dots$transformation <- dots$transformation %>%
          left_join(crs_hash, by = c("target_crs" = "old")) %>%
          transmute_(~source_crs, target_crs = ~ifelse(is.na(hash), target_crs, hash)) %>%
          inner_join(crs_hash, by = c("source_crs" = "old")) %>%
          transmute_(source_crs = ~hash, ~target_crs)
        crs <- data.frame(
          source_crs = crs,
          stringsAsFactors = FALSE
        ) %>%
          left_join(
            dots$reference %>%
              distinct_(~source_crs, ~hash),
            by = "source_crs"
          ) %>%
          select_(~hash) %>%
          unlist() %>%
          unname()
        dots$reference <- dots$reference %>%
          select_(~-source_crs, source_crs = ~hash)
      }
      object[[dots$crs.id]] <- NULL
    }
    poly <- convert(object@polygons)
    hash <- poly@Features %>%
      group_by_(~hash) %>%
      summarise_(
        features = ~sha1(sort(feature))
      )
    if (isTRUE(all.equal(rownames(object@data), hash$hash))) {
      hash <- hash %>%
        mutate_(
          id = ~object@data[hash, stable.id],
          crs = ~crs
        )
    } else {
      hash <- hash %>%
        mutate_(
          id = ~object@data[, stable.id],
          crs = ~crs
        )
    }
    poly@Features <- poly@Features %>%
      inner_join(
        hash,
        by = "hash"
      ) %>%
      select_(
        hash = ~features,
        ~feature
      )
    poly@LayerElement <- hash %>%
      select_(~id, ~features, ~crs) %>%
      as.data.frame()
    poly@Attribute <- data.frame(
      name = colnames(object@data),
      type = sapply(object@data, class),
      stringsAsFactors = FALSE
    ) %>%
      filter_(~name != stable.id) %>%
      rowwise() %>%
      mutate_(
        id = ~sha1(list(name = name, type = type))
      ) %>%
      as.data.frame()
    poly@AttributeValue <- object@data %>%
      rename_(element = stable.id) %>%
      group_by_(~element) %>%
      mutate_each(funs(as.character)) %>%
      gather(key = "name", value = "value", na.rm = TRUE, -1) %>%
      inner_join(
        poly@Attribute,
        by = "name"
      ) %>%
      select_(~element, attribute = ~id, ~value) %>%
      as.data.frame()
    poly@Transformation <- dots$transformation
    poly@Reference <- dots$reference
    validObject(poly)
    return(poly)
  }
)

#' @rdname convert
#' @importFrom methods setMethod
setMethod(
  f = "convert",
  signature = "list",
  definition = function(object, stable.id, ...){
    do.call(
      combined,
      lapply(object, convert)
    )
  }
)
