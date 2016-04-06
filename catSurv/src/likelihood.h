#pragma once

#include <vector>
#include "Cat.h"

double likelihood(Cat & cat, double theta, std::vector<int> items);

double likelihood(S4 cat_df, NumericVector t);