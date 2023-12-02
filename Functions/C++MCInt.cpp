#include <Rcpp.h>
using namespace Rcpp;

// Define the integration function in C++
NumericVector mcIntNDimCpp(Function f, NumericVector lower, NumericVector upper,
                           NumericVector muVector, NumericMatrix covMatrix,
                           CharacterVector RNG, int nValues,
                           int numCores, int chunkSize) {
  
  int nDim = lower.size();
  NumericVector sequentialEstimateVector(nValues);
  
  // Implement your integration logic here using C++
  // ...
  
  return sequentialEstimateVector;
}

// Define the comparison function in C++
DataFrame compareMCIntegrationMetricsCpp(Function f, NumericVector lower, NumericVector upper,
                                         NumericVector muVector, NumericMatrix covMatrix,
                                         int nValues, int df) {
  
  // Implement your comparison logic here using C++
  // ...
  
  return DataFrame::create(_["Method"] = CharacterVector::create("Sobol", "Halton", "Pseudo"),
                           _["Estimate"] = NumericVector::create(estimateVector[0], estimateVector[1], estimateVector[2]),
                           _["Variance"] = NumericVector::create(varianceVector[0], varianceVector[1], varianceVector[2]),
                           _["MSE"] = NumericVector::create(mseVector[0], mseVector[1], mseVector[2]),
                           _["CalcTime"] = NumericVector::create(calcTime[0], calcTime[1], calcTime[2]),
                           _["StdEstimate"] = NumericVector::create(stdEstimateVector[0], stdEstimateVector[1], stdEstimateVector[2]),
                           _["TrueValue"] = NumericVector::create(trueValue, trueValue, trueValue));
}

// R interface for mcIntNDimCpp
// [[Rcpp::export]]
NumericVector mcIntNDimSequentialRcpp(Function f, NumericVector lower, NumericVector upper,
                                      NumericVector muVector, NumericMatrix covMatrix,
                                      CharacterVector RNG, int nValues,
                                      int numCores, int chunkSize) {
  return mcIntNDimCpp(f, lower, upper, muVector, covMatrix, RNG, nValues, numCores, chunkSize);
}

// R interface for compareMCIntegrationMetricsCpp
// [[Rcpp::export]]
DataFrame compareMCIntegrationMetricsRcpp(Function f, NumericVector lower, NumericVector upper,
                                          NumericVector muVector, NumericMatrix covMatrix,
                                          int nValues, int df) {
  return compareMCIntegrationMetricsCpp(f, lower, upper, muVector, covMatrix, nValues, df);
}
