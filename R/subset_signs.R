
#' Calculate the signs that have n different matches with other given sign
#'
#' This is an internal function useful for other complex calculations. It returns
#' all the signs which distance to other is n. For example, The signs that have
#' distance 1 with the sign '111' are
#' 'x11','211','1x1','121','11x' and '112'
#' 
#' Note that sign must be of length 14
#'
#' @param out (data.table): Ignored param, just used for recursive call in internal
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
signs_with_distance <- function(out, this_id, dist, allow_lower_fails=FALSE, mysign){

  next_id <- this_id+1

  this_sign <- unlist(strsplit(mysign, ""))[this_id]
  signs_not <- c('1','x','2')
  signs_not <- signs_not[!signs_not %in% this_sign]

  if(this_id != 1 & this_id != 14){

    out_allowed_fails <- out[fails < dist]

    if(nrow(out_allowed_fails) > 0){
      out <- rbind(out[,.(sign = paste(sign, this_sign, sep=''), fails)],
                   out_allowed_fails[,.(sign = paste(sign, signs_not[1], sep=''), fails = fails + 1)],
                   out_allowed_fails[,.(sign = paste(sign, signs_not[2], sep=''), fails = fails + 1)])
    }

    return <- signs_with_distance(out = out, this_id = next_id, dist = dist, allow_lower_fails, mysign)

    return(return)

  }

  if(this_id == 14){

    out_allowed_fails <- out[fails < dist]

    if(nrow(out_allowed_fails) > 0){
      out <- rbind(out[,.(sign = paste(sign, this_sign, sep=''), fails)],
                   out_allowed_fails[,.(sign = paste(sign, signs_not[1], sep=''), fails = fails + 1)],
                   out_allowed_fails[,.(sign = paste(sign, signs_not[2], sep=''), fails = fails + 1)])
    }

    if(allow_lower_fails){
      return(out[fails <= dist])
    }else{
      return(out[fails == dist])
    }
  }

  if(this_id == 1){

    if(dist == 0){

      out <- rbind(data.table(sign = this_sign, fails= 0))

      return <- signs_with_distance(out = out, this_id = this_id + 1, dist = dist, allow_lower_fails,  mysign)

    } else {

      out <- rbind(data.table(sign = this_sign, fails= 0),
                   data.table(sign = signs_not[1], fails = 1),
                   data.table(sign = signs_not[2], fails = 1))

      return <- signs_with_distance(out = out, this_id = next_id, dist = dist, allow_lower_fails,  mysign)

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

  all_table <- copy(prob_voted_table)

  for (i in c(1 : 14)) {
    if (results[i] != "") {
      all_table <- all_table[substr(sign,i,i) == results[i]]
    }
  }

  return(all_table)
}
