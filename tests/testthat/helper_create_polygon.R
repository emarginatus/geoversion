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
sppoly <- sp::SpatialPolygons(polygons) #nolint
sppolydf <- sp::SpatialPolygonsDataFrame(sppoly[1:2], df[1:2, ]) #nolint
sppolydf.bis <- sp::SpatialPolygonsDataFrame( #nolint
  sppoly[c(1, 3)],
  df[1:2, ],
  match.ID = FALSE
)
rownames(df) <- c("A", "B", "C")
sppolydf2 <- sp::SpatialPolygonsDataFrame(sppoly[1:2], df[1:2, ], match.ID = FALSE) #nolint
reference_90ccw <- data.frame(
  source_crs = 1,
  source_x = c(0, 0, 1, 1),
  source_y = c(0, 1, 0, 1),
  target_x = c(1, 0, 1, 0),
  target_y = c(0, 0, 1, 1)
)
transformation_90ccw <- data.frame(
  source_crs = 1,
  target_crs = NA_character_,
  stringsAsFactors = FALSE
)
sppolydf_90ccw <- sppolydf
sppolydf_90ccw$crs <- 1
