// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppNumerical)]]
#include <RcppNumerical.h>
using namespace Numer;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

//' Leading NA
//' 
//' This function returns a logical vector identifying if 
//' there are leading NA, marking the leadings NA as TRUE and
//' everything else as FALSE.
//'
//' @param test A matrix containing test data
//' @param parameters A vector containing the logistic regression parameters
//' @export
// [[Rcpp::export]]
Eigen::VectorXd predictlogisticRegression(const Eigen::MatrixXd test,
                                              const Eigen::VectorXd parameters) {
     
     const int n = test.rows();
     Eigen::VectorXd prob(n);
     Eigen::VectorXd xbeta = test*parameters;
     
     for(int i = 0; i < n; i++)
          prob[i] = R::log1pexp(xbeta[i]);
     
     prob = (xbeta - prob).array().exp();
     return prob;
}