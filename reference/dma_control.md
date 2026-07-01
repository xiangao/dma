# Control parameters for dma

Control parameters for dma

## Usage

``` r
dma_control(
  crossfit_folds = 10L,
  zprime_folds = 1L,
  noise_dim = 5L,
  hidden_dim = 100L,
  num_layer = 3L,
  num_epochs = 500L,
  lr = 0.001,
  riesz_epochs = 100L,
  riesz_lr = 0.01,
  batch_size = 64L,
  riesz_batch_size = 64L,
  device = "cpu"
)
```

## Arguments

- crossfit_folds:

  \[`numeric(1)`\]  
  Number of crossfit folds.

- zprime_folds:

  \[`numeric(1)`\]  
  Number of folds for calculating Z'.

- noise_dim:

  \[`numeric(1)`\]  
  Engression noise dimension.

- hidden_dim:

  \[`numeric(1)`\]  
  NN hidden layer dimension.

- num_layer:

  \[`numeric(1)`\]  
  Number of NN layers for engression.

- num_epochs:

  \[`numeric(1)`\]  
  Training epochs for outcome regressions.

- lr:

  \[`numeric(1)`\]  
  Learning rate for outcome regressions.

- riesz_epochs:

  \[`numeric(1)`\]  
  Training epochs for Riesz representer.

- riesz_lr:

  \[`numeric(1)`\]  
  Learning rate for Riesz representer.

- batch_size:

  \[`numeric(1)`\]  
  Batch size for outcome regressions.

- riesz_batch_size:

  \[`numeric(1)`\]  
  Batch size for Riesz representer.

- device:

  \[`character(1)`\]  
  Torch device: "cpu", "cuda", or "mps".

## Value

A list of control parameters.
