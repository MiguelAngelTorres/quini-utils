
#' Calculate the expected reward for a given prob-voted table
#'
#' The expected reward is the money you will get if you get an specific number 
#' of right matches and the final result is the row sign. For example, if the row has
#' the sign 111...111 and reward_11 is 20, it means that if the final result is 1 
#' in all matches and you have 11 matches with 1 you will earn 20 euros.
#'
#' @param prob_voted_table (data.table): the prob_voted table to work with. Watch calculate_probabilities
#' function for further information.
#' @param money (numeric): the total money played by all the bets
#'
#' @return prob_voted_table (data.table): The same prob_voted_table with reward columns added.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' matches <- get_random_matches()
#' prob_voted_table <- calculate_probabilities(matches)
#' calculate_all_reward(prob_voted_table, 2600000)
#' }
#'
#' @export
#' @import data.table
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
#' calculated. Watch calculate_all_reward function for more information about the reward column.
#' @param played (data.table): A table with a column named sign with the character result is expected
#' @param results (list of characters): A list with 14 characters with the known results,
#' '1','x' or '2' for each element. The unknown results must be represented with ''.
#'
#' @return played (data.table): The same played table with min and max reward columns added.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' matches <- get_random_matches()
#' prob_voted_table <- calculate_probabilities(matches)
#' prob_voted_table <- calculate_all_reward(prob_voted_table, 2600000)
#' selected = data.table(sign = c('11111111111111'))
#' results <- c('1','1','1','1','','','','','','','','','','')
#' min_max_rewards <- calculate_min_max_reward(prob_voted_table, selected, results)
#' }
#'
#' @export
#' @import data.table
#'
calculate_min_max_reward <- function(prob_voted_table, played, results){

  possible_results <- filter_possible_results(prob_voted_table, results)

  for(i in c(1:nrow(played))){
    for(fails in c(0:4)){
      possible_wins <- possible_results[sign %in% signs_with_distance(dist = fails, mysign = played[i]$sign)$sign]

      num <- (14-fails)
      eval(parse(text=paste0(" played[",i,",':='(max_reward_",num, " = max(possible_wins$reward_",num,", na.rm = TRUE), ",
                             "min_reward_",num, " = min(possible_wins$reward_",num,", na.rm = TRUE))]")))
    }
  }

  invisible(lapply(names(played),function(.name) set(played, which(is.infinite(played[[.name]])), j = .name,value =0)))

  return_cols <- names(played)[names(played) %like% 'min_reward|max_reward|sign']

  return(played[,..return_cols])

}


#' Calculate the probability of been rewarded with more than an amount
#'
#' Given a set of matches which result is already known, a set of bets
#' and a list of amounts, the function returns the probability of been
#' rewarded with more than every amount.
#'
#'
#' @param prob_voted_table (data.table): the prob_voted table. Watch calculate_probabilities
#' function for further information.
#' @param played (data.table): A table with a column named sign with the character result is expected
#' @param results (list of characters): A list with 14 characters with the known results,
#' '1','x' or '2' for each element. The unknown results must be represented with ''.
#' @param rewards (list of numeric): The list with the rewards whose probability is going to
#' be calculated. Default value c(10,50,100,500,1000,5000).
#' @param money (numeric): the total money played by all the bets
#'
#' @return prob_of_reward (data.table): A table with two columns, the rewards and the probability
#' of earning more of that rewards.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' matches <- get_random_matches()
#' prob_voted_table <- calculate_probabilities(matches)
#' played <- data.table(sign = c('11111111111111', '11211111111111'))
#' results <- c('1','1','1','1','', '', '', '','','','','','','')
#' calculate_probability_of_reward(prob_voted_table, played, results, rewards = c(10,20), 2600000)
#' }
#'
#' @export
#' @import data.table
#'
calculate_probability_of_reward <- function(prob_voted_table, played, results, rewards = c(10,50,100,500,1000,5000), money){

  possible_results <- filter_possible_results(prob_voted_table, results)
  possible_results[,':='(num_prizes_14 = 0, num_prizes_13 = 0, num_prizes_12 = 0, num_prizes_11 = 0, num_prizes_10 = 0)]

  total_prizes <- data.table()
  dim_played <- nrow(played)

  for(i in c(1:dim_played)){
    prizes <- signs_with_distance(this_id = 1, dist = 4, allow_lower_fails = TRUE, mysign = played[i]$sign)[,.(sign, fails, conteo = 1)]
    total_prizes <- rbindlist(list(prizes, total_prizes))[,.(conteo = sum(conteo)),by=.(sign,fails)]
  }

  total_prizes <- dcast(total_prizes, sign ~ fails, value.var=c("conteo"))

  possible_results[total_prizes, on=.(sign), ':='(num_prizes_14 = i.0, num_prizes_13 = i.1, num_prizes_12 = i.2,
                                                  num_prizes_11 = i.3, num_prizes_10 = i.4)]
  possible_results[is.na(possible_results)] <- 0

  possible_results[ , ':='(reward_14 = (money*0.16) / (num_prizes_14+(voted_14*(money/0.75))),
                           reward_13 = (money*0.075) / (num_prizes_13+(voted_13*(money/0.75))),
                           reward_12 = (money*0.075) / (num_prizes_12+(voted_12*(money/0.75))),
                           reward_11 = (money*0.075) / (num_prizes_11+(voted_11*(money/0.75))),
                           reward_10 = (money*0.09) / (num_prizes_10+(voted_10*(money/0.75)))
  )]

  possible_results[,':='(total_reward = num_prizes_14 * reward_14 + num_prizes_13 * reward_13 +
                                        num_prizes_12 * reward_12 + num_prizes_11 * reward_11 +
                                        num_prizes_10 * reward_10)]

  possible_results[,':='(num_prizes_14 = NULL, num_prizes_13 = NULL, num_prizes_12 = NULL,
                         num_prizes_11 = NULL, num_prizes_10 = NULL)]

  total_prob = sum(possible_results$prob_14)
  filter_rewards <- function(reward) {
    sum(possible_results[,.(prob = prob_14/total_prob, total_reward)][total_reward>reward]$prob)
  }

  probs <- lapply(rewards, FUN = filter_rewards)

  return(data.table(prob_reward = as.numeric(probs), reward = rewards))

}


#' Calculate the probability of been rewarded with more than the invested amount
#'
#' Given a set of matches which result is already known, a set of bets,
#' the function returns the probability of been
#' rewarded with more than the invested amount.
#'
#'
#' @param prob_voted_table (data.table): the prob_voted table to work with. Watch calculate_probabilities
#' function for further information.
#' @param played (data.table): A column named sign with the character result is expected
#' @param results (list of characters): A list with 14 characters with the known results,
#' '1','x' or '2' for each element. The unknown results must be represented with ''.
#' @param money (numeric): the total money played by all the bets
#'
#' @return prob_of_reward (data.table): A table with two columns, the rewards and the probability
#' of earning more of the invested amount (number of bets * 0.75).
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' matches <- get_random_matches()
#' prob_voted_table <- calculate_probabilities(matches)
#' played <- data.table(sign = c('11111111111111', '11211111111111'))
#' results <- c('x','1','x','2','', '', '', '','x','2','1','','','')
#' money <- 1000000
#' prob_of_reward <- calculate_probability_of_roi(prob_voted_table, played, results, money)
#' }
#'
#' @export
#' @import data.table
#'
calculate_probability_of_roi <- function(prob_voted_table, played, results, money){

  return(calculate_probability_of_reward(prob_voted_table, played, results, rewards = (nrow(played)*0.75), money))

}
