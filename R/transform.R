#' apply a transformation to a geoVersion object
#' @param object a geoVersion object
#' @param models a set of transformation models
#' @export
#' @importFrom assertthat assert_that has_name
#' @importFrom methods validObject
#' @importFrom dplyr %>% inner_join select_ group_by_ mutate_ bind_rows left_join rowwise do_ transmute_ semi_join
#' @importFrom tidyr nest_ spread_ unnest_
#' @importFrom stats predict
transform <- function(object, models){
  assert_that(inherits(object, "geoVersion"))
  assert_that(inherits(models, "tbl"))
  validObject(object)
  assert_that(has_name(models, "source_crs"))
  assert_that(has_name(models, "model"))
  assert_that(has_name(models, "shift_x"))
  assert_that(has_name(models, "shift_y"))

  original <- object@Coordinates %>%
    inner_join(
      object@Features,
      by = c("hash" = "feature")
    ) %>%
    inner_join(
      x = object@LayerElement %>%
        select_(~features, ~crs),
      by = c("features" = "hash.y")
    ) %>%
    select_(~crs, ~hash, ~succession, ~x, ~y) %>%
    inner_join(
      models %>%
        select_(~source_crs, ~shift_x, ~shift_y),
      by = c("crs" = "source_crs")
    ) %>%
    group_by_(~crs)
  object@Coordinates <- original %>%
    mutate_(
      variable = ~"x",
      a = ~x - shift_x,
      b = ~-y + shift_y
    ) %>%
    bind_rows(
      original %>%
        mutate_(
          variable = ~"y",
          a = ~y - shift_y,
          b = ~x - shift_x
        )
    ) %>%
    select_(~crs, ~hash, ~succession, ~variable, ~a, ~b) %>%
    nest_("coordinates", c("hash", "succession", "variable", "a", "b")) %>%
    left_join(models, by = c("crs" = "source_crs")) %>%
    rowwise() %>%
    do_(
      output = ~
        .$coordinates %>%
          select_(~hash, ~succession, ~variable),
      target = ~predict(.$model, newdata = .$coordinates)
    ) %>%
    unnest_(c("output", "target")) %>%
    spread_(key_col = "variable", value_col = "target") %>%
    as.data.frame()
  object@LayerElement <- object@LayerElement %>%
    left_join(
      object@Transformation %>%
        mutate_(trans = ~TRUE),
      by = c("crs" = "source_crs")
    ) %>%
    transmute_(
      ~id,
      ~features,
      crs = ~ifelse(trans, target_crs, crs)
    )
  object@Transformation <- object@Transformation %>%
    semi_join(object@LayerElement, by = c("source_crs" = "crs"))
  object@Reference <- object@Reference %>%
    semi_join(object@LayerElement, by = c("source_crs" = "crs"))
  validObject(object)
  return(object)
}
