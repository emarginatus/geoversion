#' calculate the rotation models for a geoVersion object
#' @export
#' @param object a geoVersion object
#' @importFrom assertthat assert_that
#' @importFrom methods validObject
#' @importFrom stats residuals coef lm
#' @importFrom dplyr %>% group_by_ mutate_ bind_rows do_ distinct_
#' @importFrom tidyr unnest_
rotation <- function(object){
  assert_that(inherits(object, "geoVersion"))
  validObject(object)

  rmse <- function(model){
    delta <- residuals(model) %>%
      matrix(ncol = 2)
    delta <- delta ^ 2
    rowSums(delta) %>%
      mean() %>%
      sqrt()
  }

  angle <- function(model){
    angle <- atan2(coef(model)["b"], coef(model)["a"]) %>%
      unname()
    angle * 180 / pi
  }

  object@Reference %>%
    group_by_(~source_crs) %>%
    mutate_(
      target = ~target_x,
      variable = ~"x",
      shift_x = ~mean(source_x),
      shift_y = ~mean(source_y),
      a = ~source_x - shift_x,
      b = ~-source_y + shift_y
    ) %>%
    bind_rows(
      object@Reference %>%
      group_by_(~source_crs) %>%
        mutate_(
          target = ~target_y,
          variable = ~"y",
          shift_x = ~mean(source_x),
          shift_y = ~mean(source_y),
          a = ~source_y - shift_y,
          b = ~source_x - shift_x
        )
    ) %>%
    do_(
      shift = ~distinct_(., ~shift_x, ~shift_y),
      model = ~lm(formula = target ~ variable + a + b, data = .)
    ) %>%
    mutate_(
      rmse = ~rmse(model),
      angle = ~angle(model)
    ) %>%
    unnest_("shift", .drop = FALSE)
}
