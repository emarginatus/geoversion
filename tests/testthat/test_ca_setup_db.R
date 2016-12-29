context("setup_db")
test_that(
  "Start with an empty database",
  expect_identical(DBI::dbListTables(connection), character(0))
)
test_that(
  "setup_db creates the correct tables and fields", {
    setup_db(connection)
    expect_identical(
      DBI::dbListTables(connection),
      c(
        "attribute", "attributevalue", "coordinates", "crs", "element",
        "feature", "features", "layer", "layerelement", "reference",
        "staging_attribute", "staging_attributevalue", "staging_coordinates",
        "staging_crs", "staging_element", "staging_feature", "staging_features",
        "staging_layer", "staging_layerelement", "staging_reference",
        "staging_transformation", "transformation"
      )
    )
    expect_identical(
      DBI::dbListFields(connection, "attribute"),
      c("id", "name", "type")
    )
    expect_identical(
      DBI::dbListFields(connection, "attribute"),
      DBI::dbListFields(connection, "staging_attribute")
    )
    expect_identical(
      DBI::dbListFields(connection, "attributevalue"),
      c("element", "attribute", "value", "spawn", "destroy")
    )
    expect_identical(
      DBI::dbListFields(connection, "attributevalue"),
      DBI::dbListFields(connection, "staging_attributevalue")
    )
    expect_identical(
      DBI::dbListFields(connection, "coordinates"),
      c("hash", "succession", "x", "y")
    )
    expect_identical(
      DBI::dbListFields(connection, "coordinates"),
      DBI::dbListFields(connection, "staging_coordinates")
    )
    expect_identical(
      DBI::dbListFields(connection, "element"),
      c("hash", "features", "spawn", "destroy")
    )
    expect_identical(
      DBI::dbListFields(connection, "element"),
      DBI::dbListFields(connection, "staging_element")
    )
    expect_identical(
      DBI::dbListFields(connection, "feature"),
      c("hash", "type")
    )
    expect_identical(
      DBI::dbListFields(connection, "feature"),
      DBI::dbListFields(connection, "staging_feature")
    )
    expect_identical(
      DBI::dbListFields(connection, "features"),
      c("hash", "feature")
    )
    expect_identical(
      DBI::dbListFields(connection, "features"),
      DBI::dbListFields(connection, "staging_features")
    )
    expect_identical(
      DBI::dbListFields(connection, "layer"),
      c("hash", "name", "type", "spawn", "destroy")
    )
    expect_identical(
      DBI::dbListFields(connection, "layer"),
      DBI::dbListFields(connection, "staging_layer")
    )
    expect_identical(
      DBI::dbListFields(connection, "crs"),
      c("element", "value", "spawn", "destroy")
    )
    expect_identical(
      DBI::dbListFields(connection, "crs"),
      DBI::dbListFields(connection, "staging_crs")
    )
    expect_identical(
      DBI::dbListFields(connection, "layerelement"),
      c("layer", "id", "hash")
    )
    expect_identical(
      DBI::dbListFields(connection, "layerelement"),
      DBI::dbListFields(connection, "staging_layerelement")
    )
    expect_identical(
      DBI::dbListFields(connection, "reference"),
      c("source_crs", "source_x", "source_y", "target_x", "target_y")
    )
    expect_identical(
      DBI::dbListFields(connection, "reference"),
      DBI::dbListFields(connection, "staging_reference")
    )
    expect_identical(
      DBI::dbListFields(connection, "transformation"),
      c("source_crs", "target_crs")
    )
    expect_identical(
      DBI::dbListFields(connection, "transformation"),
      DBI::dbListFields(connection, "staging_transformation")
    )
})
