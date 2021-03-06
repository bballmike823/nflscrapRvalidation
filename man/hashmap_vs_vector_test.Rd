% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities_and_tests.R
\name{hashmap_vs_vector_test}
\alias{hashmap_vs_vector_test}
\title{test the speed of the hashmap and vector methods of looking up stats
several possible syntax options for the vector lookup are tested
run a benchmark that compares the speed and accuracy of hashmap and named vectors for the needs of nflscrapR
nfl_stat_map is used once, in a situation where it only looks up scalar values that are always hits
in nflscrapR, the stats are force cast to character beforehand so included a test that shows the cost of doing so
as well as a test where the cost of doing so is not included
the == notation matches functionality only under those conditions, while match should
perform in the event of a non match or you want to look up a vector of stat ids.}
\usage{
hashmap_vs_vector_test(number_of_trials = 10000)
}
\arguments{
\item{number_of_trials}{number of microbenchmark trials to run. 10000 is the default}
}
\description{
test the speed of the hashmap and vector methods of looking up stats
several possible syntax options for the vector lookup are tested
run a benchmark that compares the speed and accuracy of hashmap and named vectors for the needs of nflscrapR
nfl_stat_map is used once, in a situation where it only looks up scalar values that are always hits
in nflscrapR, the stats are force cast to character beforehand so included a test that shows the cost of doing so
as well as a test where the cost of doing so is not included
the == notation matches functionality only under those conditions, while match should
perform in the event of a non match or you want to look up a vector of stat ids.
}
