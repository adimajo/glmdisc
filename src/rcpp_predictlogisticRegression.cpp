// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppNumerical)]]
#include <RcppNumerical.h>
using namespace Numer;

//' Predicting using a logistic regression fitted with RCpp::fast_LR.
//' 
//' This function returns a numeric vector containing the probability of each observation of being of class 1 given a vector of logistic
//' regression parameters (usually estimated through RCpp::fast_LR).
//'
//' @param test A matrix containing test data
//' @param parameters A vector containing the logistic regression parameters
//' @export
// [[Rcpp::export]]
Eigen::VectorXd predictlogisticRegression(const Eigen::MatrixXd &test,
                                          const Eigen::VectorXd &parameters) {
     if (test.cols() != parameters.size()) {
          Rcpp::stop("Non-conformable arguments: `test` has %d columns but `parameters` has length %d.",
                     static_cast<int>(test.cols()), static_cast<int>(parameters.size()));
     }

     const int n = test.rows();
     Eigen::VectorXd prob(n);
     Eigen::VectorXd xbeta = test * parameters;

     for (int i = 0; i < n; i++)
          prob[i] = R::log1pexp(xbeta[i]);

     prob = (xbeta - prob).array().exp();
     return prob;
}
