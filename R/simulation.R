
#' Function to get random matches result
#'
#' The function generates results for the 14 matches
#'
#' @param matches (data.table): A data.table with expected columns real_1, real_x and real_2 where
#' each one represents the probability (over 100) of the result to happen.
#' Note that real_1 + real_x + real_2 = 100
#'
#' @return result (list of characters): A list with 14 characters with the matches results,
#' '1','x' or '2' for each element.
#'
#' @examples
#' get_random_matches()
#'
#' @export
#' @import data.table
#'
random_result <- function(matches = data.table()){

  result_prob = runif(14)*100

  if(nrow(matches > 0)){
    result <- matches[,.(result = ifelse(result_prob <= real_1,"1", ifelse(result_prob <= real_x + real_1, "x", "2")))]$result
  }else{
    result <- ifelse(result_prob <= (100/3),"1", ifelse(result_prob <= (200/3), "x", "2"))
  }

  return(result)
}
