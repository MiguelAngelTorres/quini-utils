
#' Calculate the prob_voted table
#'
#' The prob_voted table is a big table that contains 4.7M rows with the possible
#' results and, for each row: 
#' - The probability of someone else to have voted with 14,13,12,11 and 10 matches right 
#' if that row is the final result.
#' - The probability of having 14,13,12,11 and 10 matches right if you vote this row.
#' 
#' For example, if the output of a row is voted_13 = 0.2 and prob_13 = 0.3, it means that: 
#' - For 10 external bets (other players bets), 2 of them will get 13 matches right.
#' - You have 0.3 probabilities of having 13 matches right voting this row.
#' 
#'
#' @param matches (data.table): The real probability of each sign and the voted
#' percentage of each sign. Expected, at least, 6 columns named voted_1, voted_x,
#' voted_2, real_1, real_x and real_2.
#'
#' @return prob_voted_table (data.table): the table with prob and voted columns.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' matches <- get_random_matches()
#' calculate_probabilities(matches)
#' }
#'
#' @export
#' @import data.table
#'
calculate_probabilities <- function(matches){

  matches[,':='(
      voted_1 = voted_1/100, voted_x = voted_x/100, voted_2 = voted_2/100,
      real_1 = real_1/100, real_x = real_x/100, real_2 = real_2/100
  )]


  matches[,':='(
    voted_no_1 = 1 - voted_1, voted_no_x = 1 - voted_x, voted_no_2 = 1 - voted_2,
    real_no_1 = 1 - real_1, real_no_x = 1 - real_x, real_no_2 = 1 - real_2
  )]


  prob_voted_table_splited = get_voted_prob(matches)
  
  gc()

  prob_voted_table <- get_prob_with_fails(prob_voted_table_splited)

  return(prob_voted_table)

}



#' Calculate the prob_voted table splited by rows
#'
#' The prob_voted table splited by rows is similar to the prob_voted table but,
#' instead of having columns for 14,13,12,11 and 10 matches right, you only have 
#' four columns: the matches results, the prob, the voted and the fails you expect.
#' 
#' That way, you will have 4.7M * (n+1) rows, 
#' with n the number of allow_fails (typically 4)
#' 
#'
#' @param matches (data.table): The matches to calculated the probabilities.
#' @param out (data.table): Ignored param, just used for recursive call in internal
#' logic.
#' @param this_id (integer): Ignored param, just used for recursive call in internal
#' logic.
#' @param allow_fails (integer): The number of max fails you want to work with. For example,
#' a value of 2 will return the only the prob and voted of 0,1 and 2 wrong matches.
#' Default value is 4
#'
#' @return prob_voted_table_splited (data.table): the prob_voted table splited by rows.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' matches <- get_random_matches()
#' matches[,':='(
#'   voted_no_1 = 1 - voted_1, voted_no_x = 1 - voted_x, voted_no_2 = 1 - voted_2,
#'   real_no_1 = 1 - real_1, real_no_x = 1 - real_x, real_no_2 = 1 - real_2
#' )]
#' get_voted_prob(matches)
#' }
#'
#' @export
#' @import data.table
#'
get_voted_prob <- function(matches, out = data.table(), this_id = 1, allow_fails = 4){
  
  start_id = 1
  end_id = 14
  next_id = this_id+1
  
  this_match <- matches[this_id]
  
  # Normal iteration
  if(this_id != start_id & this_id != end_id){
    
    out_allowed_fails <- out[fails < allow_fails]
    
    out <- rbind(out[,.(sign = paste(sign, '1', sep=''), prob = prob * this_match$real_1, 
                                     voted  = voted * this_match$voted_1, fails)], 
                        out[,.(sign = paste(sign, 'x', sep=''), prob = prob * this_match$real_x, 
                                     voted = voted * this_match$voted_x, fails)],
                        out[,.(sign = paste(sign, '2', sep=''), prob = prob * this_match$real_2, 
                                     voted = voted * this_match$voted_2, fails)],
                        out_allowed_fails[,.(sign = paste(sign, '1', sep=''), prob = prob * this_match$real_no_1, 
                                                    voted = voted * this_match$voted_no_1, fails = fails + 1)], 
                        out_allowed_fails[,.(sign = paste(sign, 'x', sep=''), prob = prob * this_match$real_no_x, 
                                                    voted = voted * this_match$voted_no_x, fails = fails + 1)],
                        out_allowed_fails[,.(sign = paste(sign, '2', sep=''), prob = prob * this_match$real_no_2, 
                                                    voted = voted * this_match$voted_no_2, fails = fails + 1)])
    
   out <-out[,.(prob = sum(prob), voted = sum(voted)), by=.(fails, sign)]
   return <- get_voted_prob(matches, out = out, this_id = next_id, allow_fails = allow_fails)
    
    return(return)
    
  }
  
  
  # Last iteration
  if(this_id == end_id){
    
    out_allowed_fails <- out[fails < allow_fails]
    
    out <- rbind(out[,.(sign = paste(sign, '1', sep=''), prob = prob * this_match$real_1, 
                                     voted = voted * this_match$voted_1, fails)], 
                        out[,.(sign = paste(sign, 'x', sep=''), prob = prob * this_match$real_x,
                                     voted = voted * this_match$voted_x, fails)],
                        out[,.(sign = paste(sign, '2', sep=''), prob = prob * this_match$real_2, 
                                     voted = voted * this_match$voted_2, fails)],
                        out_allowed_fails[,.(sign = paste(sign, '1', sep=''), prob = prob * this_match$real_no_1, 
                                                    voted = voted * this_match$voted_no_1, fails = fails + 1)], 
                        out_allowed_fails[,.(sign = paste(sign, 'x', sep=''), prob = prob * this_match$real_no_x, 
                                                    voted = voted * this_match$voted_no_x, fails = fails + 1)],
                        out_allowed_fails[,.(sign = paste(sign, '2', sep=''), prob = prob * this_match$real_no_2, 
                                                    voted = voted * this_match$voted_no_2, fails = fails + 1)])

    out <- out[,.(prob = sum(prob), voted = sum(voted)), by=.(fails, sign)]
    return(out)
    
  }
  
  
  # First iteration
  if(this_id == start_id){
    
    if(allow_fails == 0){
      
      out <- rbind(data.table(sign = '1', prob = this_match$real_1,
                                     voted = this_match$voted_1, fails = 0), 
                          data.table(sign = 'x', prob = this_match$real_x,
                                     voted = this_match$voted_x, fails = 0),
                          data.table(sign = '2', prob = this_match$real_2, 
                                     voted = this_match$voted_2, fails = 0))
      
      out <- out[,.(prob = sum(prob), voted = sum(voted)), by=.(fails, sign)]
      return <- get_voted_prob(matches, out = out, this_id = next_id, allow_fails = allow_fails)
    
    } else {
      
      out <- rbind(data.table(sign = '1', prob = this_match$real_1,
                                     voted = this_match$voted_1, fails = 0),
                          data.table(sign = 'x', prob = this_match$real_x,
                                     voted = this_match$voted_x, fails = 0),
                          data.table(sign = '2', prob = this_match$real_2, 
                                     voted = this_match$voted_2, fails = 0),
                          data.table(sign = '1', prob = this_match$real_no_1,
                                     voted = this_match$voted_no_1, fails = 1), 
                          data.table(sign = 'x', prob = this_match$real_no_x,
                                     voted = this_match$voted_no_x, fails = 1),
                          data.table(sign = '2', prob = this_match$real_no_2, 
                                     voted = this_match$voted_no_2, fails = 1))

      out <- out[,.(prob = sum(prob), voted = sum(voted)), by=.(fails, sign)]
      return <- get_voted_prob(matches, out = out, this_id = next_id, allow_fails = allow_fails)
      
    }
    
    return(return)
    
  }
  
}


#' Transforms the prob_voted table splited by rows to the normal prob_voted table.
#'
#' Watch get_probabilities and get_voted_prob functions if need further information
#' about the prob_voted table.
#' 
#'
#' @param prob_voted_table_splited (data.table): the prob_voted table splited by rows returned by 
#' get_voted_prob function.
#' 
#' @return prob_voted_table (data.table): the joined prob_voted table.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' matches <- get_random_matches()
#' matches[,':='(
#'   voted_no_1 = 1 - voted_1, voted_no_x = 1 - voted_x, voted_no_2 = 1 - voted_2,
#'   real_no_1 = 1 - real_1, real_no_x = 1 - real_x, real_no_2 = 1 - real_2
#' )]
#' prob_voted_table_splited = get_voted_prob(matches)
#' get_prob_with_fails(prob_voted_table_splited)
#' }
#'
#' @export
#'
get_prob_with_fails <- function(prob_voted_table_splited){
  
  prob_voted_table_splited <- dcast(prob_voted_table_splited, sign ~ fails, value.var=c("prob", "voted"))
  
  setnames(prob_voted_table_splited,
           c('prob_0', 'prob_1', 'prob_2', 'prob_3', 'prob_4', 'voted_0', 'voted_1', 'voted_2', 'voted_3', 'voted_4'),
           c('prob_14', 'prob_13', 'prob_12', 'prob_11', 'prob_10', 'voted_14', 'voted_13', 'voted_12', 'voted_11', 'voted_10')
           )
  
  return(prob_voted_table_splited)
  
}





