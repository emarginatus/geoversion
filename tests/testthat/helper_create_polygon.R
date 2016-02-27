df <- data.frame(
  PermanentID = 1:2,
  Text = c("A", "B"),
  Factor = factor(c("A", "B")),
  Integer = 1:2,
  Numeric = c(0.5, pi),
  Logical = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)
polygon <- list(
  Polygon(
    cbind(
      c(0, 0, 1, 1),
      c(0, 1, 1, 0)
    ),
    hole = FALSE
  ),
  Polygon(
    cbind(
      c(1, 1, 2, 2),
      c(0, 1, 1, 0)
    ),
    hole = FALSE
  ),
  Polygon(
    cbind(
      rev(c(1.25, 1.25, 1.75, 1.75)),
      rev(c(0.25, 0.75, 0.75, 0.25))
    ),
    hole = TRUE
  )
)
polygons <- list(
  Polygons(polygon[1], 1),
  Polygons(polygon[2:3], 2)
)
sppoly <- SpatialPolygons(polygons) #nolint
sppolydf <- SpatialPolygonsDataFrame(sppoly, df) #nolint
rownames(df) <- c("A", "B")
sppolydf2 <- SpatialPolygonsDataFrame(sppoly, df, match.ID = FALSE) #nolint
