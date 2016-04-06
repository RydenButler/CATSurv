## Load libraries and set working directory
library(devtools)
library(roxygen2)
library(Rcpp)
library(testthat)
#setwd("/Users/iramalis/Desktop/gitstuff/CATSurv")
setwd("/Users/erinrossiter/Dropbox/Spring2016/Rclass/CATSurv")

current.code <- as.package("catSurv")
load_all(current.code)
document(current.code)

## Trying to write a test for binary probability
## Creating a cat object and filling in needed slots
cat <- new("Cat")
cat@discrimination <- c(2)
cat@difficulty <- c(2)
cat@guessing <- c(0)
## Getting answer from probability
probability(cat, t=1, q=1)

## Writing a function in R to test the C++ function
## Tried to copy format as C++ version
probability_test <- function(cat, theta, question){
  D = cat@D
	discrimination = cat@discrimination[question]
	difficulty = cat@difficulty[question]
	guessing = cat@guessing[question]
	exp_prob = exp(D * discrimination * (theta - difficulty))
	probability <- guessing + (1-guessing) * (exp_prob / (1 + exp_prob))
	return(probability)
}
probability_test(cat, 1, 1)

## Trying to write it another way
probability_test <- function(cat, theta, question){
  c <- cat@guessing
  D <- cat@D
  a <- cat@discrimination
  b <- cat@difficulty
  x <- (D*a)*(theta - b)
  probability <- c + ((1-c) * (exp(x)/(1+exp(x))))
  return(probability)
}
probability_test(cat, 1, 1)






## Checking documentation
?probability
probability


## Eventually we will want to run our tests
test_package("catSurv")



## Install the package
install(pkg=current.code, local=TRUE, args="--no-multiarch")

## For Testing
setwd("C:/Users/Haley/Documents/PDSL/CAT_Survey/CAT-Survey")
library(rjson)
library(CATPack)

json_cat <- fromJSON(file="eqModel.txt") ## for poly
json_cat <- fromJSON(file="narcModel.txt") ## for binary

cat <- new("Cat")
cat@guessing <- json_cat$guessing
cat@discrimination <- unlist(json_cat$discrimination)
cat@answers <- as.numeric(json_cat$answers)
cat@priorName <- json_cat$priorName
cat@priorParams <- json_cat$priorParams
cat@lowerBound <- json_cat$lowerBound
cat@upperBound <- json_cat$upperBound
cat@quadPoints <- json_cat$quadPoints
cat@D <- json_cat$D
cat@X <- json_cat$X

## for poly
cat@difficulty <- lapply(json_cat$difficulty, unlist)
cat@poly <- TRUE

## for binary
cat@difficulty <- unlist(json_cat$difficulty)
cat@poly <- FALSE

## Change cat@selection to change nextItem algorithm
nextItem(cat) 

## Run this if you change code and need to resource the cpp file
sourceCpp("./catSurv/src/epv.cpp")

## Build a version of the package to share manually
build(current.code, path=getwd())