context("transform")
test_that("transform() works with a single transformation", {
  object <- convert(
    object = sppolydf_90ccw,
    stable.id = "PermanentID",
    crs.id = "crs",
    reference = reference_90ccw,
    transformation = transformation_90ccw
  )
  model <- rotation(object)
  expect_is(
    object_trans <- transform(object, model),
    "geoVersion"
  )
})
test_that("transform() works with chained transformation", {
  object <- convert(
    object = sppolydf_multi,
    stable.id = "PermanentID",
    crs.id = "crs",
    reference = reference_multi,
    transformation = transformation_multi
  )
  model <- rotation(object)
  expect_is(
    object_trans <- transform(object, model),
    "geoVersion"
  )
  expect_identical(
    nrow(object_trans@Transformation),
    0L
  )
})
