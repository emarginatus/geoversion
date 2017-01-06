context("transform")
test_that("transform() works", {
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
