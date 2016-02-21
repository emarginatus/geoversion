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
      Polygons(list(Polygon(meuse.riv)),"meuse.riv")
    )
  ),
  data = data.frame(StableID = 5, Name = "Meuse", Extra = pi),
  match.ID = FALSE
)
```

``` r
library(geoversion)
gv <- convert(meuse.sr, "StableID")
str(gv)
#> Formal class 'geoVersion' [package "geoversion"] with 7 slots
#>   ..@ Coordinates   :'data.frame':   176 obs. of  4 variables:
#>   .. ..$ Hash : chr [1:176] "dea69640de078d783a4bcc2b09c29db160f9ccf1" "dea69640de078d783a4bcc2b09c29db160f9ccf1" "dea69640de078d783a4bcc2b09c29db160f9ccf1" "dea69640de078d783a4bcc2b09c29db160f9ccf1" ...
#>   .. ..$ Order: int [1:176] 1 2 3 4 5 6 7 8 9 10 ...
#>   .. ..$ X    : num [1:176] 182004 182137 182252 182314 182332 ...
#>   .. ..$ Y    : num [1:176] 337679 337570 337414 337285 337122 ...
#>   ..@ Feature       :'data.frame':   1 obs. of  2 variables:
#>   .. ..$ Hash: chr "dea69640de078d783a4bcc2b09c29db160f9ccf1"
#>   .. ..$ Type: chr "S"
#>   ..@ Features      :'data.frame':   1 obs. of  2 variables:
#>   .. ..$ Hash   : chr "4100286602275916cfdeec84a475182e39093111"
#>   .. ..$ Feature: chr "dea69640de078d783a4bcc2b09c29db160f9ccf1"
#>   ..@ LayerElement  :'data.frame':   1 obs. of  2 variables:
#>   .. ..$ ID      : num NA
#>   .. ..$ Features: chr "4100286602275916cfdeec84a475182e39093111"
#>   ..@ Attribute     :'data.frame':   2 obs. of  3 variables:
#>   .. ..$ Name: chr [1:2] "Name" "Extra"
#>   .. ..$ Type: chr [1:2] "factor" "numeric"
#>   .. ..$ ID  : chr [1:2] "b0ec3e8ac69ad43534535b975f526ec525db56e0" "ceea71f11c253d9b58a092460507983ceb876841"
#>   ..@ AttributeValue:'data.frame':   2 obs. of  3 variables:
#>   .. ..$ Element  : num [1:2] 5 5
#>   .. ..$ Attribute: chr [1:2] "b0ec3e8ac69ad43534535b975f526ec525db56e0" "ceea71f11c253d9b58a092460507983ceb876841"
#>   .. ..$ Value    : chr [1:2] "Meuse" "3.14159265358979"
#>   ..@ CRS           :Formal class 'CRS' [package "sp"] with 1 slot
#>   .. .. ..@ projargs: chr NA
```
