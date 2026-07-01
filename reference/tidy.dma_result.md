# Tidy a dma_result object

Extract estimates, standard errors, and confidence intervals from a
dma_result object into a tidy data frame.

## Usage

``` r
tidy.dma_result(x, ...)
```

## Arguments

- x:

  An object of class `dma_result`.

- ...:

  Additional arguments (ignored).

## Value

A data.frame with columns: term, estimate, std.error, conf.low,
conf.high.
