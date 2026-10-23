# glmdisc 0.8.1

Version 0.8.0 was submitted to CRAN but not released; its changes are listed below. This version
answers the comments of the CRAN team.

* The documentation of every exported function now has a `\value` section that describes the
  class and the meaning of the result (`plot()` is documented as called for its side effects).
* New examples for `print()`, `summary()` and `predictlogisticRegression()`.
* `plot()` restores the graphical parameters of the user (it used to leave `ask` set to `FALSE`).
* The arXiv paper is cited through its DOI in the `Description`, `Authors@R` contains the ORCID iDs
  of three authors and the `URL` field lists the package website and its repository.
* The plot test no longer leaves an `Rplots.pdf` in the test directory.

# glmdisc 0.8.0

This release re-submits the package to CRAN, from which it was archived on 2023-02-26 because
the maintainer's email address was undeliverable (the maintainer address is updated).

## Bug fixes

* **The SEM-Gibbs chain now actually iterates.** After the first sweep the quantized values
  were stored as character vectors, `nlevels()` of which is 0: every feature was declared
  "disappeared" and `glmdisc()` stopped at iteration 2, returning a random quantization.
  On a simulated example (5 continuous features, 3000 observations, true model piecewise constant)
  this gave a test Gini of 0.62 against 0.75 for a plain logistic regression; the fixed
  algorithm reaches 0.76.
* The validation log-likelihood used when `criterion` is `"aic"` or `"bic"` with a validation
  set ignored the observations of class 0.
* The BIC used `log(n)` instead of `log(n_train)` as penalty.
* When `test = TRUE` and `validation = TRUE`, the best iteration was selected on the *test*
  set and the performance reported on the *validation* set (the sets were swapped). Selection
  is now done on the validation set and the performance reported on the test set, as
  documented. `proportions` is `c(test, validation)`.
* Interactions involving a feature were dropped by substring matching (`V1` removed `V10:V12`),
  which broke every dataset with 10 features or more.
* `normalizedGini()` depended on the order of the observations when predictions are tied
  (always the case for discretized models). It is now computed from ranks (2 AUC - 1).
* `predictlogisticRegression()` aborted the R session when its arguments were not
  conformable; it now raises an error.
* `predict()` on a `glmdisc` object keeps one output per input row and works through
  `stats::predict()` as well (an S3 method is now registered).
* Missing values of factor features are handled (as a level of their own).

## Changes

* The core of the algorithm was rewritten on integer-coded quantizations: no more design
  matrices rebuilt at each iteration, one logistic regression lookup instead of one per level.
* The link between a continuous feature and its quantized version (`reg_type = "poly"`) is fitted
  by a dedicated penalized Newton solver (C++, RcppEigen) with warm start instead of
  `nnet::multinom`: about 9 times faster for the same likelihood. A sweep on 3000 observations and
  5 features takes about 25 ms.
* `best.disc[[1]]` is now a regular `glm` object fitted on the training and validation sets
  (`summary()` gives standard errors); `best.disc[[2]]` holds the fitted links.
  The `encoder` element of `parameters` was removed.
* Integer columns are accepted as numeric features; character and logical columns as factors.
* The Metropolis-Hastings search of interactions now samples the distribution it targets: the
  BIC is halved (the thesis used `exp(-delta BIC)`, the square of the BIC posterior), the
  asymmetry of the proposal is corrected, and the pairwise prior is clamped to [0.01, 0.99]
  (a prior of exactly 1, which happens for a strong effect such as an XOR, made the corrected
  chain unable to enter the pair). On the benchmark datasets the performance is unchanged
  within noise; the rule of the thesis can be restored with
  `options(glmdisc.mh_scale = 1, glmdisc.mh_hastings = FALSE, glmdisc.mh_clamp = 0)`.
* New optional argument `patience` to stop the chain when the criterion stops improving.
* `fast` is deprecated and ignored (sampling is always vectorized).
* `contr.ltfr()` (a re-export wrapper of `caret`) was removed.
* Dependencies reduced to `gam`, `MASS`, `Rcpp`, `RcppNumerical` (`caret`, `dplyr`, `magrittr`,
  `nnet`, `NPflow` and `Rfast` are no longer needed).
* The plot/predict methods are documented as S4 methods; the CITATION file was modernized.
* CI: standard `r-lib/actions` workflows (check on Linux, macOS and Windows; coverage; styler; pkgdown).
* Added `inst/benchmarks/real_data.R`, a comparison with other ways of preparing features for
  logistic regression on real and simulated data.
