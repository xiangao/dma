# CLAUDE.md

## Project Overview

dma (Distributional Mediation Analysis) is an R package for causal mediation analysis using energy regression (engression). It implements the estimation framework from crumble (Liu, Williams, Rudolph & Díaz, 2024) but replaces `mlr3superlearner` with `engression` for outcome regressions and uses engression-style neural networks for Riesz representer estimation.

Improvements over the original enmediation package:
- Observation weights propagated through all nuisance estimation (theta and alpha), not just the EIF stage
- Richer result object storing trained models, folds, and data for diagnostics
- Coefficient plot (`plot.dma_result()`) and counterfactual density plot (`plot_counterfactual_density()`)

## Package Structure

```
dma/
├── R/
│   ├── dma.R                  # Main orchestration function
│   ├── dma_control.R          # Control parameters
│   ├── result.R               # Result constructor + plot method
│   ├── theta.R                # Outcome regressions via engression (weighted)
│   ├── alpha.R                # Density ratio wrapper
│   ├── phi.R                  # Cascaded alpha estimation (weighted)
│   ├── riesz_nn.R             # Riesz representer NN training loop
│   ├── sequential_module.R    # NN architecture factory
│   ├── eif.R                  # EIF computation + ife inference
│   ├── params.R               # Effect parameterizations (data regime tuples)
│   ├── data.R                 # S7 data container with shifted regimes
│   ├── vars.R                 # S7 variable specification class
│   ├── estimates.R            # Final effect calculations (contrasts)
│   ├── permutation.R          # Z' permutation for RI/RT effects
│   ├── shift.R                # Treatment/censoring shifting
│   ├── helpers.R              # Cross-fitting, OHE, recombination utilities
│   ├── make_dataset.R         # Torch dataset factory for Riesz NN
│   ├── assertions.R           # Input validation
│   ├── print.R                # S3 print method
│   ├── tidy.R                 # S3 tidy method
│   └── plot_distribution.R    # Counterfactual density plotting
└── vignettes/
    ├── dma.Rmd                # Comparison with crumble
    ├── effect-types.Rmd       # All four effect types
    └── advantages-of-engression.Rmd  # Distributional advantages
```

## Build & Install

```bash
cd ~/projects/software
R CMD build dma --no-build-vignettes
R CMD INSTALL dma_0.1.0.tar.gz
```

## Architecture

### Estimation Pipeline

1. **θ outcome regressions** (theta.R): Nested pseudo-outcome cascade using `engression()` with observation weights. Natural path: θ³(A,W,Z,M→Y) → θ²(A,W,Z→b3) → θ¹(A,W→b2). Randomized adds θ⁴ level. Trained models are stored for post-hoc analysis.
2. **α density ratios** (phi.R → alpha.R → riesz_nn.R): Cascaded Riesz representer NNs. Each level uses previous level's output multiplied by observation weights. Loss: E[α²] - 2·w·f(α,X').
3. **EIF** (eif.R): Natural: α³(Y-θ³) + α²(b3-θ²) + α¹(b2-θ¹) + b1. Randomized: 4-term version.
4. **Inference** (estimates.R): Contrasts of EIF components → ife objects with point estimates, SEs, CIs.

### Key Implementation Details

- `prepare_engression_x()`: One-hot encodes factor/character columns, drops zero-variance columns for training, aligns prediction columns to training reference via `ref_cols`
- `ref_levels`: Factor/character levels captured from full dataset at initialization, ensuring consistent OHE across cross-fitting folds
- `params.R`: Data regime tuples — each effect type specifies which shifted datasets (data_0, data_1, data_0zp, data_1zp) to evaluate θ levels on
- `make_dataset.R`: Creates torch datasets with multiple shifted data views for Riesz NN training
- RI/RT effects require `Rsymphony` (soft dependency, checked at runtime)

## Dependencies

Core: `engression`, `torch`, `origami`, `S7`, `ife`, `checkmate`, `cli`, `data.table`, `coro`, `generics`, `Matrix`
Optional: `Rsymphony` (for RI/RT effects — needs `coinor-libsymphony-dev` system library), `ggplot2` (for plotting)

## Conventions

- Use `cache: false` for all torch-dependent chunks in Rmd/vignettes
- Engression internally standardizes inputs — constant columns cause NaN; use `prepare_engression_x()` with `ref_cols` for prediction alignment
- S7 classes for data structures (dma_vars, dma_data); S3 class for output (dma_result)
