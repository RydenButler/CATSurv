#include "Cat.h"
#include <math.h>
using namespace Rcpp;


void probability(Cat &cat, double theta, int question, std::vector<double> &ret_prob) {
	size_t diff_size = cat.poly_difficulty[question].size();
	double discrimination = cat.discrimination[question];
	double guessing = cat.guessing[question];

	for (size_t i = 0; i < diff_size; ++i) {
		double exp_prob = exp(cat.D * discrimination * (theta - cat.poly_difficulty[question][i]));
		ret_prob.push_back(guessing + (1 - guessing) * (exp_prob) / (1 + exp_prob));
	}
}


/* Overloaded since non-poly case needs to just return one double value,
 * rather than a vector of doubles.
 */
double probability(Cat &cat, double theta, int question) {
	double D = cat.D;
  
	//double discrimination = cat.discrimination[question];
	//double difficulty = cat.nonpoly_difficulty[question];
	//double guessing = cat.guessing[question];
	
	double discrimination = 2;
	double difficulty = 2;
	double guessing = .5;
	double exp_prob = exp(D * discrimination * (theta - difficulty));
	return guessing + (1 - guessing) * (exp_prob / (1 + exp_prob));
}

//' Probabilities of the responses to a question given theta 
//' 
//' This function calculates the probabilities of a specific set of responses to a specific question for a specific value of \eqn{\theta}.
//' 
//' @param cat_df An object of \code{Cat} class
//' @param t A double indicating the potential value for \eqn{\theta_j}
//' @param q An integer indicating the index of the question
//' @param ret_prob (For polytonomous implementation only) A double-vector where the calculations carried out by this function will be stored.
//' 
//' @return A vector consisting of the probability of a correct response for each respondent on item \eqn{i}.
//' 
//' @details The probability of a correct response for respondent \eqn{j} on item \eqn{i} is ....
//' where \eqn{\theta_j} is respondent \eqn{j}'s position on the latent scale of interest, \eqn{a_i} is item \eqn{i}'s discrimination parameter,
//'  \eqn{b_i} is item i's difficulty parameter, and \eqn{c_i} is item \eqn{i}'s guessing parameter.
//'  
//'  Note: this function is overloaded, due to different output types of binary vs polytomous implementations (outputs single value for binary implementation,
//'  vector of values for polytomous implementation)
//'  
//'  Note: the function for polytomous implementation does not return values, but rather alters the object ret_prob in memory
//'  
//'  Note: I'm not sure whether the above two notes are true...
//'  
//' @export
// [[Rcpp::export]]
List probability(S4 cat_df, NumericVector t, IntegerVector q) {
  // Rcout << "print" ;
	// convert R inputs
	Cat cat = constructCppCat(cat_df);

	// Todo: Determine whether or not these casts are necessary.
	double theta = Rcpp::as<std::vector<double> >(t)[0];
	int question = Rcpp::as<std::vector<int> >(q)[0];
	
	std::vector<double> probs;

	if (cat.poly) {
		probability(cat, theta, question, probs);
	}
	else {
	  probs.push_back(probability(cat, theta, question));
	}

	DataFrame question_probs = DataFrame::create(Named("probabilities") = probs);
	return List::create(Named("all.probabilities") = question_probs);
}

