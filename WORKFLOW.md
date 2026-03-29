# Code for: *MIBoost: A gradient boosting algorithm for variable selection after multiple imputation*

This repository contains the R code used for the simulation study in the paper *MIBoost: A gradient boosting algorithm for variable selection after multiple imputation*.

The purpose of the simulation study is to compare the proposed **MIBoost** procedure with competing methods for variable selection after multiple imputation. The code generates simulated datasets with missing values, performs multiple imputation, fits several competing methods, and summarizes the resulting performance measures in a LaTeX table.

## Project overview

The complete workflow is controlled by `run_all.R`. Running this script from the project root reproduces the simulation study and creates the main output files.

The simulation study is carried out for two predictor dimensions:

- `p = 50`
- `p = 100`

The main settings are:

- total sample size: `n_total = 500`
- number of simulation replications: `n_sim = 50`
- number of imputations: `M = 10`
- number of cross-validation folds: `k = 5`

## Workflow description

### 1. Simulation of incomplete data

For each value of `p`, the code generates `n_sim` simulated datasets with missing values. These data are created using the simulation functions provided in the project and form the basis for all subsequent analyses.

### 2. Training/test splitting

For each simulated dataset, the observations are split into:

- a training set containing 80% of the observations
- a test set containing the remaining 20%

This split is performed separately for each simulation replication.

### 3. Multiple imputation and preprocessing

Missing values are imputed using the `mice` package.

For each simulation replication:

- the imputation model is fitted using the training data only
- the fitted imputation model is then applied to the corresponding test data

This avoids leakage of information from the test data into the training stage.

After imputation, predictor variables are grand-mean centered. The centering constants are computed from the imputed training data only and are then applied to both the training and test data.

### 4. Cross-validation data construction

Within each training dataset, 5-fold cross-validation is performed.

For each fold, the training data are split further into:

- a tier-II training set
- a validation set

Again, imputation is performed using only the tier-II training data, and the resulting imputation models are applied to the corresponding validation data. Predictor variables are centered using grand means computed from the imputed tier-II training data only.

These multiply imputed training, validation, and test objects are then stored for later model fitting.

### 5. Stored simulated data objects

For each value of `p`, the simulation stage creates one object containing:

- imputed full training data
- imputed test data
- imputed cross-validation training data
- imputed cross-validation validation data

These objects are saved as:

- `output/FULL_SIM_LIST_50.RData`
- `output/FULL_SIM_LIST_100.RData`

Each object contains the following components:

- `X_LIST_TRAIN`
- `Y_LIST_TRAIN`
- `X_LIST_TEST`
- `Y_LIST_TEST`
- `X_LIST_CV`
- `Y_LIST_CV`
- `X_LIST_VAL_CV`
- `Y_LIST_VAL_CV`

## Compared methods

After the simulation data have been generated and saved, the following methods are fitted.

### EA-Boosting

Gradient boosting is applied to the multiply imputed datasets, and the resulting estimates are combined by **estimate averaging**.

Saved output:

- `output/EA_Boosting_50.RData`
- `output/EA_Boosting_100.RData`

### MIBoost

The proposed **MIBoost** algorithm is applied to the same multiply imputed datasets.

Saved output:

- `output/MIBoost_50.RData`
- `output/MIBoost_100.RData`

### SaLASSO

The stacked adaptive LASSO (**SaLASSO**) is fitted in two stages:

1. a preliminary run is used to determine a suitable lambda sequence
2. the final SaLASSO model is then fitted using that sequence

Saved output:

- `output/salasso_results_50.RData`
- `output/salasso_results_100.RData`

### SaENET

The stacked adaptive elastic net (**SaENET**) is fitted analogously:

1. a preliminary run is used to determine a suitable lambda sequence
2. the final SaENET model is then fitted using that sequence

Saved output:

- `output/saenet_results_50.RData`
- `output/saenet_results_100.RData`

## Result summary

After all methods have been fitted, the code summarizes the simulation results in a LaTeX table.

The following performance measures are reported:

- **MSPE**: Mean Squared Prediction Error
- **TPP**: True Positive Proportion
- **TNP**: True Negative Proportion
- **# Selec.**: average number of selected variables
- method-specific tuning quantities such as selected lambda, alpha, or stopping iteration

The final LaTeX table is written to:

- `output/results_table.tex`

## Computation time

Approximate runtimes on the author’s machine, measured using `system.time()` and reported as elapsed (wall-clock) time, were as follows:

- `MI_Simulation.R`: about 2 hours 46 minutes
- `EA_Boosting_Wrapper.R`: about 21 minutes
- `MIBoost_Wrapper.R`: about 36 minutes
- `SaLASSO_wrapper.R`: about 2 hours 41 minutes
- `SaENET_wrapper.R`: about 36 hours 35 minutes

## Citation

The archived version of this repository is available on Zenodo at DOI: `10.5281/zenodo.19322771`.

## Reproducibility

The simulation code uses fixed random seeds to ensure reproducibility. In particular, dataset generation is controlled by `set.seed(123)` within the simulation workflow.

All file paths in the code are written relative to the project root directory. The code should therefore be run from the root of the R project.

Package versions are managed using `renv`. To restore the package environment, open the project and run:

```r
install.packages("renv")   # if needed
renv::restore()
```

