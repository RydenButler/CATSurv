#include "Cat.h"
using namespace Rcpp;

double likelihood(Cat & cat, double theta, std::vector<int> items) {
  if (cat.poly) {
    double L = 1.0;
    for (unsigned int i = 0; i < items.size(); ++i) {
      int question = items[i];
      std::vector<double> question_cdf;
      question_cdf.push_back(1.0);
      probability(cat, theta, question, question_cdf);
      question_cdf.push_back(0.0);
      
      std::vector<double> question_pdf;
      for (unsigned int j = 0; j < question_cdf.size() - 1; ++j) {
        question_pdf.push_back(question_cdf[j] - question_cdf[j + 1]);
      }
      L *= question_pdf[cat.answers[question] - 1];
    }
    return L;
  } else { 
    double L = 1.0;
    for(unsigned int i = 0; i < items.size(); ++i){
      int question = items[i];
      double prob = probability(cat, theta, question);
      int this_answer = cat.answers[question]; 
      double l_temp = pow(prob, this_answer) * pow(1-prob, 1-this_answer);
      L *= l_temp;
    }
    return L;
  }
}


//' Likelihood of having offered the specific response profile given theta
//' 
//' This function returns the value of likelihood of a person with ability parameter \eqn{\theta} having offered the specific response profile stored in answers conditional on the item-level parameters. 
//' 
//' @param cat_df An object of \code{Cat} class
//' @param t A numeric for possible value of theta (position on the latent scale of interest)
//' @param items A vector containing the index of the question items we want to include in the likelihood calculations
//' 
//' @return A vector value of likelihood of
//' 
//' @details 
//'  
//' @export
// [[Rcpp::export]]
double likelihood(S4 cat_df, NumericVector t){
  Cat cat = constructCppCat(cat_df);
  double theta = as<std::vector<double> >(t)[0];
  return likelihood(cat, theta, cat.applicable_rows);
}
