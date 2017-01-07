df <- data.frame(
  PermanentID = 1:3,
  Text = c("A", "B", "C"),
  Factor = factor(c("A", "B", "C")),
  Integer = 1:3,
  Numeric = c(0.5, pi, log(pi)),
  Logical = c(TRUE, FALSE, NA),
  stringsAsFactors = FALSE
)
polygon <- list(
  sp::Polygon(
    cbind(
      c(0, 0, 1, 1),
      c(0, 1, 1, 0)
    ),
    hole = FALSE
  ),
  sp::Polygon(
    cbind(
      c(1, 1, 2, 2),
      c(0, 1, 1, 0)
    ),
    hole = FALSE
  ),
  sp::Polygon(
    cbind(
      rev(c(1.25, 1.25, 1.75, 1.75)),
      rev(c(0.25, 0.75, 0.75, 0.25))
    ),
    hole = TRUE
  )
)
polygons <- list(
  sp::Polygons(polygon[1], 1),
  sp::Polygons(polygon[2:3], 2),
  sp::Polygons(polygon[2], 3)
)
sppoly <- sp::SpatialPolygons(polygons)
sppolydf <- sp::SpatialPolygonsDataFrame(sppoly[1:2], df[1:2, ])
sppolydf.bis <- sp::SpatialPolygonsDataFrame(
  sppoly[c(1, 3)],
  df[1:2, ],
  match.ID = FALSE
)
rownames(df) <- c("A", "B", "C")
sppolydf2 <- sp::SpatialPolygonsDataFrame(
  sppoly[1:2],
  df[1:2, ],
  match.ID = FALSE
)
reference_90ccw <- data.frame(
  source_crs = "1",
  source_x = c(0, 0, 1, 1),
  source_y = c(0, 1, 0, 1),
  target_x = c(1, 0, 1, 0),
  target_y = c(0, 0, 1, 1),
  stringsAsFactors = FALSE
)
transformation_90ccw <- data.frame(
  source_crs = "1",
  target_crs = NA_character_,
  stringsAsFactors = FALSE
)
sppolydf_90ccw <- sppolydf
sppolydf_90ccw$crs <- "1"
sppolydf_multi <- sppolydf
sppolydf_multi$crs <- c("1", "2")
reference_multi <- data.frame(
  source_crs = c("1", "1", "1", "1", "2", "2", "2", "2"),
  source_x = c(0, 1, 1, 0, 0, 0, 1, 1),
  source_y = c(0, 0, 1, 1, 0, 1, 1, 0),
  target_x = c(1, 1, 0, 0, 0.5, -0.5, 0.5, 1.5),
  target_y = c(0, 1, 1, 0, -0.5, 0.5, 1.5, 0.5),
  stringsAsFactors = FALSE
)
transformation_multi <- data.frame(
  source_crs = c("1", "2"),
  target_crs = c("2", NA),
  stringsAsFactors = FALSE
)
