
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

  next_id <- this_id+2

  this_sign_1 <- unlist(strsplit(mysign, ""))[this_id]
  this_sign_2 <- unlist(strsplit(mysign, ""))[this_id+1]
  signs_not <- c('1','x','2')
  signs_not_1 <- signs_not[!signs_not %in% this_sign_1]
  signs_not_2 <- signs_not[!signs_not %in% this_sign_2]

  if(this_id != 1){

    mask_1_fail = out_fail < dist
    mask_2_fail = out_fail < dist - 1
    out_allowed_1_sign <- out_sign[mask_1_fail]
    out_allowed_1_fails <- out_fail[mask_1_fail]
    out_allowed_2_sign <- out_sign[mask_2_fail]
    out_allowed_2_fails <- out_fail[mask_2_fail]

    out_sign <- paste0(out_sign, this_sign_1, this_sign_2)

    if(length(out_allowed_1_sign) > 0){

      out_sign <- c(out_sign,
                    paste0(out_allowed_1_sign,signs_not_1[1], this_sign_2),
                    paste0(out_allowed_1_sign,signs_not_1[2], this_sign_2),
                    paste0(out_allowed_1_sign,this_sign_1, signs_not_2[1]),
                    paste0(out_allowed_1_sign,this_sign_1, signs_not_2[2]))

      out_fail <- c(out_fail,
                    out_allowed_1_fails + 1,
                    out_allowed_1_fails + 1,
                    out_allowed_1_fails + 1,
                    out_allowed_1_fails + 1)

    }

    if(length(out_allowed_2_sign) > 0){

      out_sign <- c(out_sign,
                    paste0(out_allowed_2_sign,signs_not_1[1], signs_not_2[1]),
                    paste0(out_allowed_2_sign,signs_not_1[2], signs_not_2[1]),
                    paste0(out_allowed_2_sign,signs_not_1[1], signs_not_2[2]),
                    paste0(out_allowed_2_sign,signs_not_1[2], signs_not_2[2]))

      out_fail <- c(out_fail,
                    out_allowed_2_fails + 2,
                    out_allowed_2_fails + 2,
                    out_allowed_2_fails + 2,
                    out_allowed_2_fails + 2)

    }

    if(this_id == 13){
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
    }else{
      return(signs_with_distance(out_sign = out_sign, out_fail = out_fail, this_id = next_id, dist = dist, allow_lower_fails, mysign))
    }

  }else{

    out_sign <- paste0(this_sign_1, this_sign_2)
    out_fail <- 0

    if(dist >= 1) {

      out_sign <- c(out_sign,
                    paste0(this_sign_1, signs_not_2[1]),
                    paste0(this_sign_1, signs_not_2[2]),
                    paste0(signs_not_1[1], this_sign_2),
                    paste0(signs_not_1[2], this_sign_2))

      out_fail <- c(out_fail, 1, 1, 1, 1)

    }
    if(dist >= 2){
      out_sign <- c(out_sign,
                    paste0(signs_not_1[1], signs_not_2[1]),
                    paste0(signs_not_1[2], signs_not_2[1]),
                    paste0(signs_not_1[1], signs_not_2[2]),
                    paste0(signs_not_1[2], signs_not_2[2]))

      out_fail <- c(out_fail, 2, 2, 2, 2)
    }

    return(signs_with_distance(out_sign = out_sign, out_fail = out_fail, this_id = next_id, dist = dist, allow_lower_fails, mysign))
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



#' Filter the prob_voted_table with specified results in matches
#'
#' This function can be used in two ways:
#' - giving a set of matches and the results that must have at each match (length(matches) == length(result))
#' - giving a set of matches where a sign must appear n times with lower_bound <= n <= upper_bound (length(result) == 1)
#'
#'
#' @param prob_voted_table (data.table): a table with a column sign that represents all bets to filter.
#' @param matches (list of integers): A list with n integer numbers where the results are applied.
#' @param result (list of characters): A list with n signs to filter. A sign could be composed by one or two
#' characters, for example, '1x' mean that sign is 1 or x.
#' @param lower_bound (integer): Minimum number of appearances of result within the matches.
#' @param upper_bound (integer):  Maximum number of appearances of result within the matches.
#'
#' @return possible_results (data.table): The prob_voted_table filtered with the specified results in matches
#'
#' @examples
#' library(data.table)
#' my_table <- data.table(sign = c('x1111111111111', '11111111111111'))
#' filtered_table <- filter_table_prob(my_table, c(1, 4), c('1x', '1')))
#'
#' @export
#' @import data.table
#'
filter_table_prob <- function(prob_voted_table, matches, result = c(), lower_bound = as.numeric(NA), upper_bound = as.numeric(NA)){

  tab = prob_voted_table

  if(is.na(lower_bound) & is.na(upper_bound)){
    for(i in c(1:length(matches))){
      eval(parse(text=paste0("tab = tab[ ",paste0("(substr(sign, matches[i], matches[i]) %in% c('",
                                                  paste0(unlist(strsplit(result[i], split = "")), collapse = "','"),"')"), ")]")))
    }
  }else{
      if(!is.na(lower_bound)){
        eval(parse(text=paste0("tab = tab[",paste0("(substr(sign,", matches,",", matches, ") %in% c('",
                                                   paste0(unlist(strsplit(result, split = "")),collapse="','"),"'))" ,
                             collapse = "+"), " >= ", lower_bound,"]")))
      }
      if(!is.na(upper_bound)){
        eval(parse(text=paste0("tab = tab[",paste0("(substr(sign,", matches,",", matches, ") %in% c('",
                                                   paste0(unlist(strsplit(result, split = "")),collapse="','"),"'))" ,
                             collapse = "+"), " <= ", upper_bound,"]")))
      }
  }

  return(tab)
}
