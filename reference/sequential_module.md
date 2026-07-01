# Sequential neural network module function factory

Creates a factory function that returns a sequential neural network
module with the specified architecture. Used for Riesz representer
estimation.

## Usage

``` r
sequential_module(layers = 1, hidden = 20, dropout = 0.1)
```

## Arguments

- layers:

  \[numeric(1)\]  
  Number of hidden layers.

- hidden:

  \[numeric(1)\]  
  Number of hidden units.

- dropout:

  \[numeric(1)\]  
  Dropout rate.

## Value

A function that takes input dimension and returns a torch nn_module.
