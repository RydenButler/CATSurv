#pragma once

#include <vector>
#include "Cat.h"
#include "probability.h" // should we include this?

double likelihood(Cat & cat, double theta, std::vector<int> items);

double likelihood(S4 cat_df, NumericVector t);