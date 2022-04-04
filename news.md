# BHAM (development version 0.2.0.9000)
## Add non-linear plotting functions
* Currently implemented for bamlasso models, without error prevention mechanisms.

# BHAM 0.2.0
## Model Change
* Add hierarchical enforcement to the bi-level selection by imposing a dependency to the non-linear components inclusion indicator prior

## Implementation
* Add Cox Proportional hazard additive models via `bacoxph` and `bamlasso(family="cox")`
* Add model tuning functions via `tune.bgam` and `cv.bgam`
* Add variable selection function for bamlasso models via `bamlasso_var_selection`


# BHAM 0.1.0.9000
## Start the Package
* Implemented the Bayesian hierarchical additive model with EM-Iterative Weighted Least Squares and EM-Coordinate Descent algorithms
* Implemented some supporting functions for model tuning and final model presentations

# BHAM 0.1.0.9001

* Added a `NEWS.md` file to track changes to the package.
