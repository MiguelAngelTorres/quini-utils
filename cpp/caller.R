library(Rcpp)
sourceCpp('cpp/get_voted_prob.cpp')
source('R/get_matches.R')

matches <- get_random_matches()


get_voted_prob_test(matches$real_1, matches$real_x, matches$real_2, 1, 4)