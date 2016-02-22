<!-- README.md is generated from README.Rmd. Please edit that file -->
<table style="width:49%;">
<colgroup>
<col width="11%" />
<col width="18%" />
<col width="6%" />
<col width="12%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Branch</th>
<th align="left">Build status</th>
<th align="left">Code</th>
<th align="left">coverage</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Master</td>
<td align="left"><a href="https://app.wercker.com/project/bykey/bd4930c4d92c16decdde4d37c735a894"><img src="https://app.wercker.com/status/bd4930c4d92c16decdde4d37c735a894/m/master" title="wercker status" alt="wercker status" /></a></td>
<td align="left"><a href="https://codecov.io/github/emarginatus/geoversion?branch=master"><img src="https://codecov.io/github/emarginatus/geoversion/coverage.svg?branch=master" alt="codecov.io" /></a></td>
<td align="left"><img src="https://codecov.io/github/emarginatus/geoversion/branch.svg?branch=master" alt="codecov.io" /></td>
</tr>
<tr class="even">
<td align="left">Develop</td>
<td align="left"><a href="https://app.wercker.com/project/bykey/bd4930c4d92c16decdde4d37c735a894"><img src="https://app.wercker.com/status/bd4930c4d92c16decdde4d37c735a894/m/develop" title="wercker status" alt="wercker status" /></a></td>
<td align="left"><a href="https://codecov.io/github/emarginatus/geoversion?branch=develop"><img src="https://codecov.io/github/emarginatus/geoversion/coverage.svg?branch=develop" alt="codecov.io" /></a></td>
<td align="left"><img src="https://codecov.io/github/emarginatus/geoversion/branch.svg?branch=develop" alt="codecov.io" /></td>
</tr>
</tbody>
</table>

Store spatial objects from the sp package into a database while maintaining the version history. A specific version of the spatial object can be retrieved.

Convert a `SpatialPolygonsDataFrame` object into a `geoVersion` object
======================================================================

``` r
library(sp)
data(meuse.riv)
meuse.sr <- SpatialPolygonsDataFrame(
  SpatialPolygons(
    list(
      Polygons(list(Polygon(meuse.riv)), "meuse.riv")
    )
  ),
  data = data.frame(StableID = 5, Name = "Meuse", Extra = pi),
  match.ID = FALSE
)
spplot(meuse.sr, zcol = "Extra")
```

![](README-create_polygon-1.png)<!-- -->

``` r
library(geoversion)
gv <- convert(object = meuse.sr, id = "StableID")
str(gv)
#> Formal class 'geoVersion' [package "geoversion"] with 7 slots
#>   ..@ Coordinates   :'data.frame':   176 obs. of  4 variables:
#>   .. ..$ Hash : chr [1:176] "8c6dc85a2e6e40ccc7fb2d232fb9efa3a3644b40" "8c6dc85a2e6e40ccc7fb2d232fb9efa3a3644b40" "8c6dc85a2e6e40ccc7fb2d232fb9efa3a3644b40" "8c6dc85a2e6e40ccc7fb2d232fb9efa3a3644b40" ...
#>   .. ..$ Order: int [1:176] 1 2 3 4 5 6 7 8 9 10 ...
#>   .. ..$ X    : num [1:176] 182004 182137 182252 182314 182332 ...
#>   .. ..$ Y    : num [1:176] 337679 337570 337414 337285 337122 ...
#>   ..@ Feature       :'data.frame':   1 obs. of  2 variables:
#>   .. ..$ Hash: chr "8c6dc85a2e6e40ccc7fb2d232fb9efa3a3644b40"
#>   .. ..$ Type: chr "S"
#>   ..@ Features      :'data.frame':   1 obs. of  2 variables:
#>   .. ..$ Hash   : chr "3d7262777d8d2b6f32e126ed65e56f444f3c2959"
#>   .. ..$ Feature: chr "8c6dc85a2e6e40ccc7fb2d232fb9efa3a3644b40"
#>   ..@ LayerElement  :'data.frame':   1 obs. of  2 variables:
#>   .. ..$ ID      : num 5
#>   .. ..$ Features: chr "3d7262777d8d2b6f32e126ed65e56f444f3c2959"
#>   ..@ Attribute     :'data.frame':   2 obs. of  3 variables:
#>   .. ..$ Name: chr [1:2] "Name" "Extra"
#>   .. ..$ Type: chr [1:2] "factor" "numeric"
#>   .. ..$ ID  : chr [1:2] "e25b6b14bdb4b59b1e562c6cf15ba61b92df9e24" "d7d20d2ea2d2ffc9bde6adbd549d381a7e80b25c"
#>   ..@ AttributeValue:'data.frame':   2 obs. of  3 variables:
#>   .. ..$ Element  : num [1:2] 5 5
#>   .. ..$ Attribute: chr [1:2] "e25b6b14bdb4b59b1e562c6cf15ba61b92df9e24" "d7d20d2ea2d2ffc9bde6adbd549d381a7e80b25c"
#>   .. ..$ Value    : chr [1:2] "Meuse" "3.14159265358979"
#>   ..@ CRS           :Formal class 'CRS' [package "sp"] with 1 slot
#>   .. .. ..@ projargs: chr NA
```

Convert a `geoVersion` object in to a `SpatialPolygonsDataFrame`
================================================================

``` r
gv.sp <- as_sp(gv)
str(gv.sp)
#> Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
#>   ..@ data       :'data.frame':  1 obs. of  3 variables:
#>   .. ..$ ID   : num 5
#>   .. ..$ Extra: num 3.14
#>   .. ..$ Name : chr "Meuse"
#>   ..@ polygons   :List of 1
#>   .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
#>   .. .. .. ..@ Polygons :List of 1
#>   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
#>   .. .. .. .. .. .. ..@ labpt  : num [1:2] 180183 331123
#>   .. .. .. .. .. .. ..@ area   : num 2122714
#>   .. .. .. .. .. .. ..@ hole   : logi FALSE
#>   .. .. .. .. .. .. ..@ ringDir: int 1
#>   .. .. .. .. .. .. ..@ coords : num [1:176, 1:2] 182004 182137 182252 182314 182332 ...
#>   .. .. .. ..@ plotOrder: int 1
#>   .. .. .. ..@ labpt    : num [1:2] 180183 331123
#>   .. .. .. ..@ ID       : chr "3d7262777d8d2b6f32e126ed65e56f444f3c2959"
#>   .. .. .. ..@ area     : num 2122714
#>   ..@ plotOrder  : int 1
#>   ..@ bbox       : num [1:2, 1:2] 178304 325698 182332 337685
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : chr [1:2] "x" "y"
#>   .. .. ..$ : chr [1:2] "min" "max"
#>   ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#>   .. .. ..@ projargs: chr NA
spplot(gv.sp, zcol = "Extra")
```

![](README-geoversion_polygon-1.png)<!-- -->
