
#' Fix matches real probability with voted probability
#'
#' Some times the voted probability seams more reasonable than
#' real probability given by betting houses. To fix that you can
#' use this function.
#'
#' @param matches (data.table): a table with 6 expected columns:
#' real_1, real_x, real_2, voted_1, voted_x and voted_2.
#' @param index (integer list): a list with 1 to 14 integers
#' that represent the indexes of the matches to be change.
#'
#' @return matches (data.table): the table in the input with the
#' real_1, real_x and real_2 changed with voted columns.
#'
#' @examples
#' increase_real_by_voted(matches, c(4,7))
#'
#' @export
#' @import data.table
#'
increase_real_by_voted <- function(matches, index){

  matches[index, ':='(
    real_1 = (0.85*real_1 + 0.15*voted_1),
    real_x = (0.85*real_x + 0.15*voted_x),
    real_2 = (0.85*real_2 + 0.15*voted_2)
  )]

  return(matches)
}


#' Fix matches voted probability when voting too early
#'
#' Some times you cant vote the last day so voted probability
#' may change within the time. In order to try to reduce the impact
#' you can use this function. Note that this is a simple normalization,
#' it has no prediction component.
#'
#' @param matches (data.table): a table with 6 expected columns:
#' real_1, real_x, real_2, voted_1, voted_x and voted_2.
#'
#' @return matches (data.table): the table in the input with the
#' voted columns normalized
#'
#' @examples
#' normalized_voted(matches)
#'
#' @export
#' @import data.table
#'
normalize_voted <- function(matches){

  matches[,':='(
    voted_1 = (ifelse(voted_1>33, 33 + ((voted_1-33)*64/67), 3 + (voted_1*30/33)))/100,
    voted_x = (ifelse(voted_x>33, 33 + ((voted_x-33)*64/67), 3 + (voted_x*30/33)))/100,
    voted_2 = (ifelse(voted_2>33, 33 + ((voted_2-33)*64/67), 3 + (voted_2*30/33)))/100,
    real_1 = real_1/100, real_x = real_x/100, real_2 = real_2/100
  )]

  return(matches)
}