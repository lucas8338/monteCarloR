// this file contains c++ versions of distributions (PDF).

#include <Rcpp.h>
#include <cmath>
#include <string>

using namespace std;
using namespace Rcpp;

//' @title Rcpp_pdf_normal
//' @description normal distribution PDF from: "https://web.stanford.edu/class/archive/cs/cs109/cs109.1178/lectureHandouts/110-normal-distribution.pdf".
//' @param x the target probability
//' @param mean the mean of the distribution
//' @param stddev the standard deviation
//' @return a double with PDF
//' @export
//[[Rcpp::export]]
NumericVector pdf_normal(double x,double mean,double stddev){
    // variable storing euler's number
    double e=exp(1.0);
    // variable from the library "cmath" containing the value of pi.
    double pi=M_PI;

    double part1=1.0/(stddev*sqrt(2.0*pi));
    double part2=-(1.0/2.0)*pow((x-mean)/stddev,2.0);
    part2= pow(e,part2);
    double result= part1*part2;
    NumericVector resultVector= NumericVector::create(result);
    resultVector.attr("class")="normalLike";
    return resultVector;
}