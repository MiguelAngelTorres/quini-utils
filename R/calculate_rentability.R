
#' Calculate rentability for a given prob-voted table
#'
#' The rentability is calculated as the real probability divided for the voted probability.
#' The rentability is used as a metric to order the bets. It is much faster than the em
#' but less exact, as it doesn't take into account that there are many bets that will have
#' 10, 11, 12 and 13 signs right.
#'
#' As the raw rentability gives too much importance to the voted probability (that will
#' end with a low real probability bet selection), a normalization parameter called importance was added.
#' Importance = 1 is the raw rentability.
#'
#' @param prob_voted_table (data.table): the prob_voted table to work with. Watch calculate_probabilities
#' function for further information.
#' @param importance (numeric): the importance given to the probability.
#' The recommended values are between 1 and 2. Default importance = 1
#'
#' @return prob_voted_table (data.table): The same prob_voted_table with rentability columns added.
#'
#' @export
#' @examples
#' \dontrun{
#' library(data.table)
#' matches <- get_random_matches()
#' prob_voted_table <- calculate_probabilities(matches)
#' get_rentability(prob_voted_table, 1.5)
#' }
#'
get_rentability <- function(prob_voted_table, importance = 1.0){

  prob_voted_table[,':='(rent_14 = (prob_14 ^ importance) / voted_14,
                         rent_13 = (prob_13 ^ importance) / voted_13,
                         rent_12 = (prob_12 ^ importance) / voted_12,
                         rent_11 = (prob_11 ^ importance) / voted_11,
                         rent_10 = (prob_10 ^ importance) / voted_10)]

  prob_voted_table[,':='(rent = rent_14 * (0.16^importance) +
    rent_13 * (0.075^importance) +
    rent_12 * (0.075^importance) +
    rent_11 * (0.075^importance) +
    rent_10 * (0.09^importance)
  )]

  return(prob_voted_table)
  
}


