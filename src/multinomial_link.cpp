// Multinomial logistic regression with a single (standardized) covariate.
//
// Model: P(cls = k | z) = exp(a_k + b_k z) / sum_l exp(a_l + b_l z), class 0 is
// the baseline (a_0 = b_0 = 0). Fitted by penalized Newton with step halving.
// This is the "link" model p(e | x) of the SEM-Gibbs algorithm; it replaces
// nnet::multinom, which dominated the run time (BFGS, no warm start).
// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include <cmath>
#include <limits>

using Eigen::MatrixXd;
using Eigen::VectorXd;

namespace {

// log-likelihood minus ridge penalty (baseline column is not penalized)
double penalized_loglik(const VectorXd& z, const Rcpp::IntegerVector& cls,
                        const MatrixXd& coef, double ridge) {
  const int n = z.size(), K = coef.cols();
  double ll = 0.0;
  std::vector<double> eta(K);
  for (int i = 0; i < n; i++) {
    double mx = -std::numeric_limits<double>::infinity();
    for (int k = 0; k < K; k++) {
      eta[k] = coef(0, k) + coef(1, k) * z[i];
      if (eta[k] > mx) mx = eta[k];
    }
    double s = 0.0;
    for (int k = 0; k < K; k++) s += std::exp(eta[k] - mx);
    ll += eta[cls[i]] - mx - std::log(s);
  }
  double pen = 0.0;
  for (int k = 1; k < K; k++) pen += coef(0, k) * coef(0, k) + coef(1, k) * coef(1, k);
  return ll - 0.5 * ridge * pen;
}

void class_probs(const VectorXd& z, const MatrixXd& coef, MatrixXd& P) {
  const int n = z.size(), K = coef.cols();
  std::vector<double> eta(K);
  for (int i = 0; i < n; i++) {
    double mx = -std::numeric_limits<double>::infinity();
    for (int k = 0; k < K; k++) {
      eta[k] = coef(0, k) + coef(1, k) * z[i];
      if (eta[k] > mx) mx = eta[k];
    }
    double s = 0.0;
    for (int k = 0; k < K; k++) {
      eta[k] = std::exp(eta[k] - mx);
      s += eta[k];
    }
    for (int k = 0; k < K; k++) P(i, k) = eta[k] / s;
  }
}

}  // namespace

//' Fit a multinomial logit with one covariate (internal).
//'
//' @param z Numeric covariate (standardized by the caller).
//' @param cls Class of each observation, integers in 0..K-1.
//' @param K Number of classes.
//' @param start 2 x K matrix of starting values (row 1: intercepts, row 2: slopes; column 1 ignored).
//' @param ridge Ridge penalty on the non-baseline coefficients.
//' @param maxit Maximum number of Newton iterations.
//' @param tol Convergence tolerance on the largest step.
//' @return A list with the 2 x K coefficient matrix, the penalized log-likelihood, the
//' number of iterations and a convergence flag.
//' @noRd
// [[Rcpp::export]]
Rcpp::List fit_multinomial_1d(const Eigen::VectorXd& z, const Rcpp::IntegerVector& cls, int K,
                              const Eigen::MatrixXd& start, double ridge = 0.1,
                              int maxit = 30, double tol = 1e-6) {
  const int n = z.size();
  if (cls.size() != n) Rcpp::stop("`z` and `cls` must have the same length.");
  if (K < 2) Rcpp::stop("At least two classes are needed.");
  if (start.rows() != 2 || start.cols() != K) Rcpp::stop("`start` must be a 2 x K matrix.");
  for (int i = 0; i < n; i++)
    if (cls[i] < 0 || cls[i] >= K) Rcpp::stop("`cls` must take values in 0..K-1.");

  const int p = 2 * (K - 1);
  MatrixXd coef = start;
  coef.col(0).setZero();
  MatrixXd P(n, K);
  double ll = penalized_loglik(z, cls, coef, ridge);
  bool converged = false;
  int it = 0;

  for (; it < maxit; it++) {
    class_probs(z, coef, P);
    VectorXd g = VectorXd::Zero(p);
    MatrixXd A = MatrixXd::Zero(p, p);
    for (int i = 0; i < n; i++) {
      const double zi = z[i], zi2 = zi * zi;
      for (int k = 1; k < K; k++) {
        const double r = (cls[i] == k ? 1.0 : 0.0) - P(i, k);
        g[2 * (k - 1)] += r;
        g[2 * (k - 1) + 1] += r * zi;
        for (int l = k; l < K; l++) {
          const double w = (k == l ? P(i, k) : 0.0) - P(i, k) * P(i, l);
          A(2 * (k - 1), 2 * (l - 1)) += w;
          A(2 * (k - 1), 2 * (l - 1) + 1) += w * zi;
          A(2 * (k - 1) + 1, 2 * (l - 1)) += w * zi;
          A(2 * (k - 1) + 1, 2 * (l - 1) + 1) += w * zi2;
        }
      }
    }
    // A is only filled for l >= k; symmetrize through the blocks
    for (int k = 1; k < K; k++)
      for (int l = k + 1; l < K; l++)
        A.block(2 * (l - 1), 2 * (k - 1), 2, 2) = A.block(2 * (k - 1), 2 * (l - 1), 2, 2).transpose();
    for (int k = 1; k < K; k++) {
      g[2 * (k - 1)] -= ridge * coef(0, k);
      g[2 * (k - 1) + 1] -= ridge * coef(1, k);
    }
    A.diagonal().array() += ridge;

    VectorXd step = A.ldlt().solve(g);
    if (!step.allFinite()) break;

    double t = 1.0;
    MatrixXd cand = coef;
    double ll_new = ll;
    for (int ls = 0; ls < 30; ls++) {
      for (int k = 1; k < K; k++) {
        cand(0, k) = coef(0, k) + t * step[2 * (k - 1)];
        cand(1, k) = coef(1, k) + t * step[2 * (k - 1) + 1];
      }
      ll_new = penalized_loglik(z, cls, cand, ridge);
      if (ll_new >= ll - 1e-12) break;
      t *= 0.5;
    }
    if (ll_new < ll - 1e-12) break;  // no improving step: stop at the current point
    coef = cand;
    const double change = t * step.cwiseAbs().maxCoeff();
    ll = ll_new;
    if (change < tol) {
      converged = true;
      it++;
      break;
    }
  }

  return Rcpp::List::create(Rcpp::Named("coef") = coef, Rcpp::Named("loglik") = ll,
                            Rcpp::Named("iter") = it, Rcpp::Named("converged") = converged);
}

//' Class probabilities of the one-covariate multinomial logit (internal).
//' @noRd
// [[Rcpp::export]]
Eigen::MatrixXd multinomial_1d_probs(const Eigen::VectorXd& z, const Eigen::MatrixXd& coef) {
  MatrixXd P(z.size(), coef.cols());
  class_probs(z, coef, P);
  return P;
}
