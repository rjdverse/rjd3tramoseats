# Deprecated functions

Deprecated functions

## Usage

``` r
fast_tramoseats(
  ts,
  spec = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"),
  context = NULL,
  userdefined = NULL
)

fast_tramo(
  ts,
  spec = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"),
  context = NULL,
  userdefined = NULL
)

spec_tramoseats(
  name = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
)

spec_tramo(name = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"))

userdefined_variables_tramoseats(x = c("TRAMO-SEATS", "TRAMO"))
```

## Arguments

- ts:

  a univariate time series.

- spec:

  the model specification. Can be either the name of a predefined
  specification or a user-defined specification.

- context:

  the dictionary of variables.

- userdefined:

  a vector containing the additional output variables (see
  [`tramoseats_dictionary()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_dictionary.md)).

- name:

  the name of a predefined specification.

- x:

  useless parameter

## Value

All these functions are deprecated and return the same value as the
function that replaces them:

- `spec_tramoseats()` returns the same value as
  [`tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.md)

- `spec_tramo()` returns the same value as
  [`tramo_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.md)

- `fast_tramoseats()` returns the same value as
  [`tramoseats_fast()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)

- `fast_regarima()` returns the same value as `regarima_fast()`

- [`.jtramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
  returns the same value as `jtramoseats()`

- `userdefined_variables_tramoseats()` returns the same value as
  [`tramoseats_dictionary()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_dictionary.md)
