# TRAMO-SEATS full dictionary

Function listing the format and description for all output objects
(series, diagnostics, parameters) available with
[`tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
function. Can be used to generate an output non available by default
with userdefined option in
[`tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)function
(see examples).

## Usage

``` r
tramoseats_full_dictionary()
```

## Value

returns a data frame containing format and description, for all output
objects (series, diagnostics, parameters) available with
[`tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)function

## See also

`tramoseats_dictionary` for an abbreviated version of the output
description

## Examples

``` r
# visualize the dictionary
# first 10 lines
tramoseats_full_dictionary()[1:10,]
#>           name                                               description
#> 1       period                                      period of the series
#> 2   span.start                  start of the considered (partial) series
#> 3     span.end                    end of the considered (partial) series
#> 4       span.n      number of periods in the considered (partial) series
#> 5 span.missing number of missing values in the considered (partial) s...
#> 6          log                                         log-transformtion
#> ...
#> 
#>  For a complete list of all outputs, please call summary()
#> 
#>  For more informations about the type, the java class of the output or additive details, call `View()`.
# for more structured information call `View(tramoseats_full_dictionary())`
# extract names of output of interest
user_defined_output <- tramoseats_full_dictionary()[95,1]
user_defined_output
#> [1] "ylin"
# generate the corresponding output in an estimation
library(rjd3toolkit)
y <- rjd3toolkit::ABS$X0.2.09.10.M
m<-tramoseats(y,"rsafull", userdefined=user_defined_output)
# retrieve user defined output
tail(m$user_defined$ylin)
#>           Mar      Apr      May      Jun      Jul      Aug
#> 2017 1389.905 1492.068 1460.957 1540.453 1468.642 1279.390
```
