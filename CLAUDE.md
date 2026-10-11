# glmdisc

R package from a PhD thesis: SEM-Gibbs discretization of continuous features and grouping of
factor levels for logistic regression, with Metropolis-Hastings search of pairwise
interactions. Entry point: `glmdisc()` in `R/semDiscretization.R`. S4 class `glmdisc`.

## Layout

| File | Role |
|---|---|
| `R/semDiscretization.R` | `glmdisc()`: checks, split, call the engine, final `glm`, build the S4 object |
| `R/sem_engine.R` | the SEM loop (`sem_engine()`), Metropolis-Hastings step, interaction initialization |
| `R/link_models.R` | links p(e\|x): numeric (multinomial logit via C++, or `MASS::polr`) and factor (contingency table) |
| `R/logit_codes.R` | logistic regression on integer-coded features; linear predictors by coefficient lookup |
| `R/features.R` | feature typing, factor levels / NA handling for training and new data |
| `R/discretize.link.R` | `discretize_link()`, level naming, `quantized_frame()` |
| `R/utils.R` | small tested helpers (`sample_rows`, `resample_unseen_labels`, ...) |
| `R/method_*.R`, `R/generic_*.R` | S4 methods: `predict` (also S3), `discretize`, `cutpoints`, `plot` |
| `src/multinomial_link.cpp` | penalized Newton solver for the 1-covariate multinomial logit (RcppEigen) |
| `src/rcpp_predictlogisticRegression.cpp` | `predictlogisticRegression()` |
| `inst/benchmarks/real_data.R` | comparison on real datasets (not run on CRAN) |

Key design points (read before changing the engine):
- Quantized values are **integer labels**, not factors. Label `m_start + 1` is reserved for
  missing values of numeric features. Anything stored as character/factor between sweeps
  silently breaks `nlevels()`; this once made the whole chain stop after one sweep.
- A sweep fits p(y|e) on the current labels, then per feature fits the link, takes its MAP
  (`emap`) and draws a new label from p(e|x) p(y|e,e_-j). The returned model is fitted on
  the MAP quantization. `S` in a link = labels the MAP may return (seen in training rows).
- Test / validation rows may never carry a label that no training row has
  (`resample_unseen_labels()`).
- The interaction search samples prior x `exp(-BIC / 2)` exactly (`mh_log_acceptance()`): halved
  score, Hastings correction for the asymmetric proposal, `p_delta` clamped to [0.01, 0.99]
  (an exact 0 or 1 makes the corrected chain unable to enter or leave a pair). The rule of the
  thesis is available with `options(glmdisc.mh_scale = 1, glmdisc.mh_hastings = FALSE,
  glmdisc.mh_clamp = 0)`.

## Commands

R 4.6.1 comes from the CRAN apt repo (`noble-cran40`); R packages live in `~/R/library`
(Posit binaries). Always set `R_LIBS_USER=$HOME/R/library`.

```bash
# regenerate NAMESPACE / Rd / RcppExports (roxygen without compiling, see below)
Rscript -e 'Rcpp::compileAttributes("."); roxygen2::roxygenise(".", load_code = roxygen2::load_source)'
# install, test, check
R CMD INSTALL --no-docs -l /tmp/glmdisc-lib . && Rscript -e '.libPaths(c("/tmp/glmdisc-lib", .libPaths())); testthat::test_local(".")'
R CMD build . && R CMD check --as-cran glmdisc_*.tar.gz
# reproduce the CRAN incoming checks, including the spell check of DESCRIPTION
# (needs aspell and aspell-en; the variable is set on the CRAN machines, not by --as-cran)
_R_CHECK_CRAN_INCOMING_USE_ASPELL_=true R CMD check --as-cran glmdisc_*.tar.gz
# style (CI fails on a diff)
Rscript -e 'styler::style_pkg()'
# benchmark
Rscript inst/benchmarks/real_data.R 20 300 benchmark-results
```

Pitfalls:
- **Delete `src/*.o` and `src/*.so` before installing** if anything else compiled the
  package (devtools / roxygen with a compiling loader use debug flags and leave objects
  behind; installing then reuses them and everything is ~10x slower with Eigen assertions on).
- Use `roxygenise(load_code = roxygen2::load_source)` so roxygen does not compile.
- Tests must be seeded (`set.seed()` at the top of each test); the chain is stochastic.
- Plain `Rscript` in the repo is safe (no project `.Rprofile`/renv any more).

## Conventions

- tidyverse style via `styler` (CI checks `styler::style_pkg(dry = "fail")`).
- roxygen2 documentation, testthat 3rd edition, no new `Imports` without discussion
  (current: gam, graphics, MASS, methods, Rcpp, RcppNumerical, stats).
- No claims in docs / README without a number from `inst/benchmarks/`.

## Working agreements

- When a system library or standard package is missing, ask the user to
  `sudo apt install ...` it (they prefer that to rebuilding or working around things).
- Maintainer address for CRAN: adrien.ehrhardt@polytechnique.edu (the previous address
  bounced, which is why the package was archived on 2023-02-26).
