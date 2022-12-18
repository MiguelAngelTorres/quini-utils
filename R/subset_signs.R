
#' Calculate the signs that have n different matches with other given sign
#'
#' This is an internal function useful for other complex calculations. It returns
#' all the signs which distance to other is n. For example, The signs that have
#' distance 1 with the sign '111' are
#' 'x11','211','1x1','121','11x' and '112'
#' 
#' Note that sign must be of length 14
#'
#' @param out_sign (data.table): Ignored param, just used for recursive call in internal
#' logic.
#' @param out_fail (data.table): Ignored param, just used for recursive call in internal
#' logic.
#' @param this_id (integer): Ignored param, just used for recursive call in internal
#' logic.
#' @param dist (integer): The number of different sign between mysign and output.
#' @param allow_lower_fails (boolean): If true, returns the matches with lower different signs than dist.
#' Default set to false.
#' @param mysign (string): A string with 14 characters, '1', '2' or 'x' representing the sign
#' to calculate.
#'
#' @return return (data.table): A table with one column named sign with the signs that
#' have dist matches different.
#'
#' @examples
#' signs_with_distance(dist = 3, mysign='11111111111111')
#'
#' @export
#' @import data.table
#'
signs_with_distance <- function(out_sign, out_fail, this_id = 1, dist, allow_lower_fails=FALSE, mysign){

  next_id <- this_id+1

  this_sign <- unlist(strsplit(mysign, ""))[this_id]
  signs_not <- c('1','x','2')
  signs_not <- signs_not[!signs_not %in% this_sign]

  if(this_id != 1){

    mask = out_fail < dist
    out_allowed_sign <- out_sign[mask]
    out_allowed_fails <- out_fail[mask]

    if(length(out_allowed_sign) > 0){

      out_sign <- c(paste0(out_sign, this_sign),
                    paste0(out_allowed_sign,signs_not[1]),
                    paste0(out_allowed_sign,signs_not[2]))

      out_fail <- c(out_fail,
                    out_allowed_fails + 1,
                    out_allowed_fails + 1)

    }else{

      out_sign <- paste0(out_sign, this_sign)

    }

    if(this_id != 14){
      return <- signs_with_distance(out_sign = out_sign, out_fail = out_fail, this_id = next_id, dist = dist, allow_lower_fails, mysign)
      return(return)

    }else{

      if(allow_lower_fails){
        mask = out_fail <= dist
        return_sign <- out_sign[mask]
        return_fail <- out_fail[mask]
      }else{
        mask = out_fail == dist
        return_sign <- out_sign[mask]
        return_fail <- out_fail[mask]
      }

      return(data.table(sign = return_sign, fails = return_fail))

    }

  }

  if(this_id == 1){

    if(dist == 0){

      out_sign <- this_sign
      out_fail <- 0

      return <- signs_with_distance(out_sign = out_sign, out_fail=out_fail, this_id = next_id, dist = dist, allow_lower_fails,  mysign)

    } else {

      out_sign <- c(this_sign, signs_not)
      out_fail <- c(0, 1, 1)

      return <- signs_with_distance(out_sign = out_sign, out_fail=out_fail, this_id = next_id, dist = dist, allow_lower_fails,  mysign)

    }

    return(return)

  }

}

#' Filter the prob_voted_table with possible final results
#'
#' Given a set of matches which result is already known, it returns
#' the filtered prob_voted_table with only the final results that
#' are possible.
#'
#'
#' @param prob_voted_table (data.table): a table with a column sign that represents all bets to filter.
#' @param results (list of characters): A list with 14 characters with the known results,
#' '1','x' or '2' for each element. The unknown results must be represented with ''.
#'
#' @return possible_results (data.table): The prob_voted_table filtered with the final possible results
#'
#' @examples
#' library(data.table)
#' my_table <- data.table(sign = c('11111111111111', '1111111111111x'))
#' results <- c('','','','','', '', '', '','','','','','','x')
#' possible_results <- filter_possible_results(my_table, results)
#'
#' @export
#' @import data.table
#'
filter_possible_results <- function(prob_voted_table, results) {

  all_table <- prob_voted_table

  for (i in c(1 : 14)) {
    if (results[i] != "") {
      all_table <- all_table[substr(sign,i,i) == results[i]]
    }
  }

  return(all_table)
}
