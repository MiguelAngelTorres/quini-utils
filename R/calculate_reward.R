
#' Calculate the expected reward for a given prob-voted table
#'
#' The expected reward is the money you will get if you get an specific number 
#' of right matches and the final result is the row sign. For example, if the row has
#' the sign 111...111 and reward_11 is 20, it means that if the final result is 1 
#' in all matches and you have 11 matches with 1 you will earn 20 euros.
#'
#' @param prob_voted_table (data.table): the prob_voted table to work with. Watch calculate_probabilites 
#' function for further information.
#' @param money (numeric): the total money played by all the bets
#'
#' @return prob_voted_table (data.table): The same prob_voted_table with reward columns added.
#'
#' @export
#' @examples
#' calculate_all_reward(prob_voted_table, 2600000)
#'
calculate_all_reward <- function(prob_voted_table, money){

  prob_voted_table[ , ':='(reward_14 = (money*0.16) / (1+(voted_14*(money/0.75))),
                           reward_13 = (money*0.075) / (1+(voted_13*(money/0.75))),
                           reward_12 = (money*0.075) / (1+(voted_12*(money/0.75))),
                           reward_11 = (money*0.075) / (1+(voted_11*(money/0.75))),
                           reward_10 = (money*0.09) / (1+(voted_10*(money/0.75)))
  )]

  return(prob_voted_table)

}


#' Calculate the min and max reward for a given partial result and a set of bets
#'
#' Given a set of matches which result is already known and a set of bets,
#' it returns the max and min money you can earn. The max and min reward depends
#' on the result of the matches which result is unknown.
#'
#' @param prob_voted_table (data.table): the prob_voted table with the expected reward
#' calculated
#' @param played (data.table): A column named sign with the character result is expected
#' @param results (list of characters): A list with 14 characters with the known results,
#' '1','x' or '2' for each element. The unknown results must be represented with ''.
#'
#' @return played (data.table): The same played table with min and max reward columns added.
#'
#' @export
#' @examples
#' results <- c('x','1','x','2','', '', '', '','x','2','1','','','')
#' min_max_rewards <- calculate_min_max_reward(prob_voted_table, selected, results)
#'
calculate_min_max_reward <- function(prob_voted_table, played, results){

  str_eval = 'TRUE'

  for(i in c(1:14)){
    if(results[i] != ''){

      eval(parse(text = paste0("prob_voted_table[,':='(sign_" ,i, " = substr(sign,",i,",",i,"))]")))
      str_eval = paste(str_eval, paste0('sign_',i,' == results[',i,']'), sep = ' & ')

    }
  }

  possible_results = eval(parse(text=paste0("prob_voted_table[",str_eval,"]")))

  for(i in c(1:nrow(played))){
    for(fails in c(0:4)){
      possible_wins <- possible_results[sign %in% signs_with_distance(this_id = 1, dist = fails, mysign = played[i]$sign)$sign]

      num <- (14-fails)
      eval(parse(text=paste0(" played[",i,",':='(max_reward_",num, " = max(possible_wins$reward_",num,", na.rm = TRUE), ",
                             "min_reward_",num, " = min(possible_wins$reward_",num,", na.rm = TRUE))]")))
    }
  }

  invisible(lapply(names(played),function(.name) set(played, which(is.infinite(played[[.name]])), j = .name,value =0)))

  # Delete auxiliar columns
  columns_signs = c('sign_1','sign_2','sign_3','sign_4','sign_5','sign_6','sign_7',
                    'sign_8','sign_9','sign_10','sign_11','sign_12','sign_13','sign_14')
  eval(parse(text=paste0("prob_voted_table[,':='(", columns_signs," = NULL)]")))


  return_cols <- names(played)[names(played) %like% 'min_reward|max_reward|sign']

  return(played[,..return_cols])

}


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
#' @param mysign (string): A string with 14 characters, '1', '2' or 'x' representing the sign
#' to calculate.
#'
#' @return return (data.table): A table with one column named sign with the signs that
#' have dist matches different.
#'
#' @export
#' @examples
#' signs_with_distance(dist = 3, mysign='11111111111111')
#'
signs_with_distance <- function(out, this_id, dist, mysign){

  next_id <- this_id+1

  this_sign <- unlist(strsplit(mysign, ""))[this_id]
  signs_not <- c('1','x','2')
  signs_not <- signs_not[!signs_not %in% this_sign]

  if(this_id != 1 & this_id != 14){

    out_allowed_fails <- out[fails < dist]

    out <- rbind(out[,.(sign = paste(sign, this_sign, sep=''), fails)],
                 out_allowed_fails[,.(sign = paste(sign, signs_not[1], sep=''), fails = fails + 1)],
                 out_allowed_fails[,.(sign = paste(sign, signs_not[2], sep=''), fails = fails + 1)])

    return <- signs_with_distance(out = out, this_id = next_id, dist = dist, mysign)

    return(return)

  }

  if(this_id == 14){

    out_allowed_fails <- out[fails < dist]

    out <- rbind(out[,.(sign = paste(sign, this_sign, sep=''), fails)],
                 out_allowed_fails[,.(sign = paste(sign, signs_not[1], sep=''), fails = fails + 1)],
                 out_allowed_fails[,.(sign = paste(sign, signs_not[2], sep=''), fails = fails + 1)])

    return(out[fails == dist])

  }

  if(this_id == 1){

    if(dist == 0){

      out <- rbind(data.table(sign = this_sign, fails= 0))

      return <- signs_with_distance(out = out, this_id = this_id + 1, dist = dist, mysign)

    } else {

      out <- rbind(data.table(sign = this_sign, fails= 0),
                   data.table(sign = signs_not[1], fails = 1),
                   data.table(sign = signs_not[2], fails = 1))

      return <- signs_with_distance(out = out, this_id = next_id, dist = dist, mysign)

    }

    return(return)

  }

}
