% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stats.R
\name{get_played_porcentage}
\alias{get_played_porcentage}
\title{Calculate the played percentage for each match given a set of bets}
\usage{
get_played_porcentage(selected)
}
\arguments{
\item{selected}{(data.table): a table with a column named sign, that
contains a string of 14 characters with the voted signs for each match.}
}
\value{
stats (data.table): A table with 3 columns, the percentage
played for each sign on each match.
}
\description{
This is a useful function to measure the trend in a bet method.
If most of the bets in the set have the same result in any match, the
function will return a number close to 1 in the voted sign and a number close
to 0 in the other two signs.
}
\examples{
library(data.table)
selected <- data.table(sign = c('11111111111111', 'x1111111111111', '21111111111111'))
get_played_porcentage(selected)

}
