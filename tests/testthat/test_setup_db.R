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
        "attribute", "attributevalue", "coordinates", "element", "feature",
        "features"
      )
    )
    expect_identical(
      DBI::dbListFields(connection, "attribute"),
      c("id", "name", "type")
    )
    expect_identical(
      DBI::dbListFields(connection, "attributevalue"),
      c("element", "attribute", "value", "spawn", "destroy")
    )
    expect_identical(
      DBI::dbListFields(connection, "coordinates"),
      c("hash", "order", "x", "y")
    )
    expect_identical(
      DBI::dbListFields(connection, "element"),
      c("id", "features", "spawn", "destroy")
    )
    expect_identical(
      DBI::dbListFields(connection, "feature"),
      c("hash", "type")
    )
    expect_identical(
      DBI::dbListFields(connection, "features"),
      c("hash", "feature")
    )
})
