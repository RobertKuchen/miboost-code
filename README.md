# Code for: *MIBoost: A gradient boosting algorithm for variable selection after multiple imputation*

This repository contains the R code for the simulation study reported in the paper *MIBoost: A gradient boosting algorithm for variable selection after multiple imputation*.

This README briefly describes the simulation study and analysis workflow. For a more detailed description of the simulation workflow, see `WORKFLOW.md`.

## Description

The code generates simulated datasets with missing values, performs multiple imputation, applies several competing variable-selection methods, and summarizes the resulting performance measures in a LaTeX table.

The simulation study is carried out for two predictor dimensions (`p = 50` and `p = 100`) with:

- sample size `n_total = 500`
- `n_sim = 50` simulation replications
- `M = 10` imputations
- `k = 5` cross-validation folds

The compared methods are:

- EA-Boosting
- MIBoost
- SaLASSO
- SaENET

## Main workflow

The full analysis is controlled by `run_all.R`.

The workflow consists of:

1. simulation of incomplete datasets
2. split into training and test data
3. multiple imputation of training data and application of the imputation model to test data
4. construction of internal cross-validation splits
5. fitting of EA-Boosting, MIBoost, SaLASSO, and SaENET
6. creation of a LaTeX summary table

## Output

Intermediate and final results are written to the `output/` directory, including:

- `FULL_SIM_LIST_50.RData`
- `FULL_SIM_LIST_100.RData`
- `EA_Boosting_50.RData`
- `EA_Boosting_100.RData`
- `MIBoost_50.RData`
- `MIBoost_100.RData`
- `salasso_results_50.RData`
- `salasso_results_100.RData`
- `saenet_results_50.RData`
- `saenet_results_100.RData`
- `results_table.tex`

## Computation time

Approximate runtimes on the author’s machine, measured using `system.time()` and reported as elapsed (wall-clock) time, were as follows:

- `MI_Simulation.R`: about 2 hours 46 minutes
- `EA_Boosting_Wrapper.R`: about 21 minutes
- `MIBoost_Wrapper.R`: about 36 minutes
- `SaLASSO_wrapper.R`: about 2 hours 41 minutes
- `SaENET_wrapper.R`: about 46 hours 35 minutes

## Reproducibility

The simulation uses fixed random seeds and project-relative file paths.

Package versions are managed with `renv`. To restore the package environment, open the project and run:

```r
install.packages("renv")   # if needed
renv::restore()
```
