# Set an option for tramoseats

Set an option for tramoseats

## Usage

``` r
get_tramoseats_option(name)
```

## Arguments

- name:

  Name of the option

## Value

The requested option or NULL if it doesn't exist

## Examples

``` r
tramoseats_option("test", "DUMMY")
get_tramoseats_option("test")
#> [1] "DUMMY"
```
