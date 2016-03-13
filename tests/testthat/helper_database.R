db.path <- tempfile(fileext = ".sql")
connection <- DBI::dbConnect(RSQLite::SQLite(), db.path)
