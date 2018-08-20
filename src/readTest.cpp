#include "MGDReader.h"
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


//' @export
// [[Rcpp::export]]
void testRead(const std::string& fileName){
  
  Rcpp::Rcout << "Input file is: " << fileName << std::endl;
  
  MGDReader fileReader(fileName, fileName);
  
}