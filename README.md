<!-- badges: start -->
[![R-CMD-check](https://github.com/adimajo/glmdisc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adimajo/glmdisc/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/adimajo/glmdisc/branch/master/graph/badge.svg)](https://app.codecov.io/gh/adimajo/glmdisc?branch=master)
<!-- badges: end -->

# glmdisc: discretization and grouping for logistic regression

`glmdisc` discretizes continuous features, merges the levels of categorical features and
(optionally) searches pairwise interactions *jointly with* the logistic regression fitted on
them. The result is a regular, interpretable logistic regression on a small number of
levels per feature. It implements the SEM-Gibbs algorithm of *Formalization and study of
statistical problems in Credit Scoring*, Ehrhardt A. (see the
[manuscript](https://github.com/adimajo/manuscrit_these) or the
[web article](https://adimajo.github.io/discretization.html)).

> **Status.** The package was archived on CRAN on 2023-02-26 (the maintainer's email address
> bounced) and a re-submission is in preparation. Meanwhile, install it from GitHub.

## Installation

``` r
# install.packages("pak")
pak::pak("adimajo/glmdisc")
```

## Example

``` r
library(glmdisc)

set.seed(1)
x <- matrix(runif(3000), ncol = 3)                       # 3 continuous features
steps <- apply(x, 2, function(col) as.integer(cut(col, c(0, 1 / 3, 2 / 3, 1))))
y <- rbinom(1000, 1, plogis(rowSums(sapply(1:3, function(j) c(-1.5, 0.5, 1.5)[steps[, j]])) - 1.5))

fit <- glmdisc(x, y, iter = 300, m_start = 10, interact = FALSE)

cutpoints(fit)                        # where each feature is cut
summary(fit)                          # the final logistic regression (a regular glm)
predict(fit, as.data.frame(x))[1:5]   # probabilities for new data
```

See `vignette("glmdisc")` for the interface (`validation`, `test`, `criterion`, `interact`,
`reg_type`, `patience`, ...).

## What to expect: measured results

The numbers below come from `inst/benchmarks/real_data.R` (20 random 70/30 splits per dataset, 300
SEM iterations, `m_start = 10`; the full table and the raw results are in
`inst/benchmarks/results/`). They are test-set **Gini** coefficients (2 AUC - 1; higher is
better); "vs raw" is the paired difference with a logistic regression on the raw features
(mean ± standard error). `glmdisc` uses a validation set and the Gini to pick the
discretization, `glmdisc (BIC)` uses all the training data and the BIC.

| Dataset | raw logit | quantile bins (5) | MDLP bins | WoE bins (`scorecard`) | `glmdisc` | `glmdisc` (BIC) | `glmdisc` + interactions |
|---|---|---|---|---|---|---|---|
| Simulated, step-wise logit | 0.690 | 0.679 | **0.731** | 0.729 | 0.712 | 0.718 | 0.714 |
| Simulated, U-shaped logit | 0.083 | 0.723 | **0.758** | 0.731 | 0.696 | 0.691 | 0.701 |
| Simulated, XOR interaction | -0.001 | -0.004 | 0.000 | 0.000 | 0.009 | -0.005 | **0.541** |
| German credit (1000 × 20) | **0.552** | 0.535 | 0.520 | **0.553** | 0.529 | 0.522 | - |
| Pima diabetes (532 × 7) | **0.704** | 0.647 | 0.633 | 0.678 | 0.613 | 0.635 | 0.584 |
| ISLR `Default` (10000 × 3) | **0.896** | 0.787 | 0.869 | 0.864 | 0.857 | 0.861 | 0.861 |
| AER `CreditCard` (1319 × 10) | 0.670 | 0.687 | 0.526 | 0.670 | 0.667 | **0.689** | 0.668 |

What this says, honestly:

* **`glmdisc` is not a general-purpose accuracy booster.** On real datasets whose log-odds are
  close to linear (German credit, Pima, `Default`) a plain logistic regression on the raw
  features is as good or better, and every binning method, `glmdisc` included, loses a little
  by discretizing. The only real-data gain is on `CreditCard` (BIC variant: +0.018 ± 0.005).
* **Against other binning methods it is competitive, not better.** Even on data simulated from a
  piecewise-constant model (its home setting) supervised MDLP and WoE binning are slightly
  better (+0.01 to +0.06 Gini) and deterministic. The SEM chain selects a few too many
  levels (e.g. it keeps levels for pure-noise features).
* **Where it is unique: interactions.** On a pure XOR effect none of the other methods can
  find the interaction; `glmdisc(interact = TRUE)` goes from 0 to 0.54 Gini. On the real
  datasets the interaction search does not help (on Pima it is 0.03 ± 0.03 Gini below
  `glmdisc` without interactions, not significant). The search samples the BIC posterior
  of the set of interactions; the rule of the thesis gives the same results, see
  `inst/benchmarks/interactions.R`. `glmdisc` also handles missing values natively (they
  form their own level).
* **Cost.** A sweep takes about 25 ms for 3000 observations and 5 features (versus 150-300
  ms for the previous implementation, which in fact stopped after one sweep, see `NEWS.md`);
  a fit of 300 iterations takes 3-22 s (six fits running in parallel on an 8-core laptop).
  For comparison, in the same benchmark MDLP binning takes 0.4-16 s and WoE binning 2-10 s;
  the raw logit and quantile bins are instantaneous.
* **Convergence is slow in iterations.** On the step-wise simulation the test Gini with the BIC
  rises from 0.720 (50 iterations) to 0.731 (300), 0.740 (1000) and 0.743 (3000), and the best
  iteration is still near the end of the chain: most of the benefit needs about 1000
  iterations. Use `patience` to stop earlier. (`inst/benchmarks/convergence.R`)

Reproduce with `Rscript inst/benchmarks/real_data.R 20 300` (needs `scorecard`, `discretization`,
`ISLR`, `AER`).

## Documentation

* Help pages and vignette: `?glmdisc`, `vignette("glmdisc")`.
* Online: <https://adimajo.github.io/glmdisc>.
* Questions and bug reports: <https://github.com/adimajo/glmdisc/issues>.

## References

* Celeux, G., Chauveau, D., Diebolt, J. (1995), On Stochastic Versions of the EM Algorithm.
  Research Report RR-2514, INRIA.
* Ehrhardt, A., Biernacki, C., Vandewalle, V., Heinrich, P., Beben, S. (2019), Feature
  quantization for parsimonious and interpretable predictive models. arXiv:1903.08920.
