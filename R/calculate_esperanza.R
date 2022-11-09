
#' Calculate EM for a given prob-voted table
#'
#' EM is calculated as the money you would earn with an event multiplied by
#' the probability of that event to happen. The param level controls the
#' events to be calculated
#'
#' @param prob_voted_table (data.table): the prob_voted table to work with. Watch calculate_probabilities
#' function for further information.
#' @param money (numeric): the total money played by all the bets
#' @param level (integer): The minimum EM to be calculted. For example,
#' with 'level = 12', EM14, EM13 and EM12 will be calculated. That is the
#' EM of getting 14, 13 and 12 right matches. Note that level must be an integer
#' between 10 and 14.
#' @param optimization (boolean): True to enable optimization. Optimization has the same output,
#' but decrease the time it takes to calculate by using more RAM. Note that if the process requires
#' more RAM than the available, the program may crush.
#'
#' @return prob_voted_table (data.table): The same prob_voted_table with em columns added.
#'
#' @examples
#' library(data.table)
#' matches <- get_random_matches()
#' prob_voted_table <- calculate_probabilities(matches)
#' get_em(prob_voted_table, 2600000, 13)
#'
#' @export
#' @import data.table
#'

get_em <- function(prob_voted_table, money, level = 10, optimization = FALSE){
  
  get_em_10 <- level <= 10
  get_em_11 <- level <= 11
  get_em_12 <- level <= 12
  get_em_13 <- level <= 13
  
  if(get_em_13){

    if(optimization){
      singles_signs = c('sign_1_','sign_2_','sign_3_','sign_4_','sign_5_','sign_6_','sign_7_',
                     'sign_8_','sign_9_','sign_10_','sign_11_','sign_12_','sign_13_','sign_14_')
      column_signs = c('sign_1_2_','sign_2_3_','sign_3_4_','sign_4_5_','sign_5_6_','sign_6_7_','sign_7_8_',
                     'sign_8_9_','sign_9_10_','sign_10_11_','sign_11_12_','sign_12_13_','sign_13_14_',
                     'sign_1_','sign_2_','sign_3_','sign_4_','sign_5_','sign_6_','sign_7_',
                     'sign_8_','sign_9_','sign_10_','sign_11_','sign_12_','sign_13_','sign_14_'
                     )
    }else{
      singles_signs = c('sign_1_','sign_2_','sign_3_','sign_4_','sign_5_','sign_6_','sign_7_',
                     'sign_8_','sign_9_','sign_10_','sign_11_','sign_12_','sign_13_','sign_14_')
      column_signs = c('sign_1_','sign_2_','sign_3_','sign_4_','sign_5_','sign_6_','sign_7_',
                     'sign_8_','sign_9_','sign_10_','sign_11_','sign_12_','sign_13_','sign_14_'
                     )
    }

    prob_voted_table[,':='(sum_prob_13 = 0, sum_voted_13 = 0, aux_mult_13 = prob_14 * voted_13,
                     sum_prob_12 = 0, sum_voted_12 = 0, aux_mult_12 = prob_14 * voted_12,
                     sum_prob_11 = 0, sum_voted_11 = 0, aux_mult_11 = prob_14 * voted_11,
                     sum_prob_10 = 0, sum_voted_10 = 0, aux_mult_10 = prob_14 * voted_10)]
  }
  
  
  if(get_em_13){
    for(i in c(1:14)){
      eval(parse(text = paste0("prob_voted_table[,':='(sign_" ,i, "_ = substr(sign,",i,",",i,"))]")))
      if(i != 14){
        eval(parse(text = paste0("prob_voted_table[,':='(sign_" ,i, "_", i+1, "_ = substr(sign,",i,",",i+1,"))]")))
      }
    }
    
    group_column <- get_group_cols(singles_signs, column_signs, c())
    
    eval(parse(text=paste0("prob_voted_table[,':='(prob_i_j_h_k = sum(prob_14), aux_10_i_j_h_k = sum(aux_mult_10), aux_11_i_j_h = sum(aux_mult_11), aux_12_i_j = sum(aux_mult_12), aux_13_i = sum(aux_mult_13)),by=.(",
                           paste0(group_column,collapse=','),")]", collapse='')))
    
    for(i in c(1:14)){
      group_column <- get_group_cols(singles_signs, column_signs, c(i))
      
      eval(parse(text=paste0("prob_voted_table[,':='(prob_j_h_k = sum(prob_14), aux_10_j_h_k = sum(aux_mult_11), aux_11_j_h = sum(aux_mult_11), aux_12_j = sum(aux_mult_12), aux_13= sum(aux_mult_13)),by=.(",
                             paste0(group_column,collapse=','),")]", collapse='')))
      
      if(get_em_12 && i < 14){
        for(j in c((i+1):14)){
          group_column <- get_group_cols(singles_signs, column_signs, c(i, j))
          
          eval(parse(text=paste0("prob_voted_table[,':='(prob_h_k = sum(prob_14), aux_10_h_k = sum(aux_mult_10), aux_11_h = sum(aux_mult_11), aux_12 = sum(aux_mult_12)),by=.(",
                                 paste0(group_column,collapse=','),")]", collapse='')))
          
          if(get_em_11 && j < 14){
            for(h in c((j+1):14)){
              group_column <- get_group_cols(singles_signs, column_signs, c(i, j, h))
              
              eval(parse(text=paste0("prob_voted_table[,':='(prob_k = sum(prob_14), aux_10_k = sum(aux_mult_10), aux_11 = sum(aux_mult_11)),by=.(",
                                     paste0(group_column,collapse=','),")]", collapse='')))
              
              if(get_em_10 && h < 14){
                for(k in c((h+1):14)){
                  group_column <- get_group_cols(singles_signs, column_signs, c(i, j, h, k))
                  
                  eval(parse(text=paste0("prob_voted_table[,':='(prob = sum(prob_14), aux_10 = sum(aux_mult_10)),by=.(",
                                         paste0(group_column,collapse=','),")]", collapse='')))
  
                  
                  prob_voted_table[,':='(sum_prob_10 = sum_prob_10 + prob + prob_i_j_h_k,
                                   sum_voted_10 = sum_voted_10 + aux_10 + aux_10_i_j_h_k)]
                }
              }
              
              prob_voted_table[,':='(sum_prob_10 = sum_prob_10 - 11 * prob_k,
                               sum_voted_10 = sum_voted_10 - 11 * aux_10_k)]
              
              prob_voted_table[,':='(sum_prob_11 = sum_prob_11 + prob_k - prob_i_j_h_k,
                               sum_voted_11 = sum_voted_11 + aux_11 - aux_11_i_j_h)]
            }
          }
          
          prob_voted_table[,':='(sum_prob_10 = sum_prob_10 + 66 * prob_h_k,
                           sum_prob_11 = sum_prob_11 - 12 * prob_h_k,
                           sum_voted_10 = sum_voted_10 + 66 * aux_10_h_k,
                           sum_voted_11 = sum_voted_11 - 12 * aux_11_h)]
          
          prob_voted_table[,':='(sum_prob_12 = sum_prob_12 + prob_h_k + prob_i_j_h_k,
                           sum_voted_12 = sum_voted_12 + aux_12 + aux_12_i_j)]
        }
      }
      
      prob_voted_table[,':='(sum_voted_10 = sum_voted_10 - 286 * aux_10_j_h_k,
                       sum_voted_11 = sum_voted_11 + 78 * aux_11_j_h,
                       sum_voted_12 = sum_voted_12 - 13 * aux_12_j,
                       sum_prob_10 = sum_prob_10 - 286 * prob_j_h_k,
                       sum_prob_11 = sum_prob_11 + 78 * prob_j_h_k,
                       sum_prob_12 = sum_prob_12 - 13 * prob_j_h_k)]
      
      prob_voted_table[,':='(sum_prob_13 = sum_prob_13 + prob_j_h_k - prob_i_j_h_k,
                       sum_voted_13 = sum_voted_13 + aux_13 - aux_13_i)]
      
    }
  }
  
  prob_voted_table[,':='(em14 = prob_14 * (money*0.16) /(1 + (voted_14*(money/0.75))))]
  
  if(get_em_10){
    
    prob_voted_table[,':='(em10 = prob_10 * (money*0.075) / (1+((sum_voted_10 / sum_prob_10)*(money/0.75))),
                     prob = NULL,
                     aux_10 = NULL)]
  }
  
  if(get_em_11){
    
    prob_voted_table[,':='(em11 = prob_11 * (money*0.075) / (1+((sum_voted_11 / sum_prob_11)*(money/0.75))),
                     prob_k = NULL,
                     aux_11 = NULL,
                     aux_10_k = NULL)]
  }
  
  if(get_em_12){
    
    prob_voted_table[,':='(em12 = prob_12 * (money*0.075) / (1+((sum_voted_12 / sum_prob_12)*(money/0.75))),
                     prob_h_k = NULL,
                     aux_12 = NULL,
                     aux_11_h = NULL,
                     aux_10_h_k = NULL)]
  }
  
  if(get_em_13){
    
    eval(parse(text=paste0("prob_voted_table[,':='(",
                           column_signs," = NULL)]")))
    
    prob_voted_table[,':='(em13 = prob_13 * (money*0.075) / (1+((sum_voted_13 / sum_prob_13)*(money/0.75))),
                     sum_voted_13 = NULL, sum_prob_13 = NULL, aux_mult_13 = NULL,
                     sum_voted_12 = NULL, sum_prob_12 = NULL, aux_mult_12 = NULL,
                     sum_voted_11 = NULL, sum_prob_11 = NULL, aux_mult_11 = NULL,
                     sum_voted_10 = NULL, sum_prob_10 = NULL, aux_mult_10 = NULL,
                     prob_j_h_k = NULL, prob_i_j_h_k = NULL,
                     aux_13 = NULL, aux_13_i = NULL,
                     aux_12_j = NULL, aux_12_i_j = NULL,
                     aux_11_j_h = NULL, aux_11_i_j_h = NULL,
                     aux_10_j_h_k = NULL, aux_10_i_j_h_k = NULL)]
  }
  

  return(prob_voted_table)
  
}



#' Calculate the group by columns for the get_EM function
#'
#' This is a function used inside the get_EM function. It is not
#' useful for other purposes
#'
#' @param singles_signs (list string): the columns valid for group by process with just one sign
#' @param column_signs (list string): The columns valid for group by process
#' @param out_columns (list integer): The columns to exclude
#'
#' @return get_col (list string): The columns to perform the group by
#'
#' @export
#'
get_group_cols <- function(singles_signs, column_signs, out_columns){

  out_merged = paste0(paste0('_', out_columns, '_', sep=''), collapse = '|')

  column_signs <- column_signs[!column_signs %like% out_merged]
  single_to_take <- singles_signs[!singles_signs %like% out_merged]
  single_to_take <- paste0('_',unlist(regmatches(single_to_take, gregexpr("[[:digit:]]+", single_to_take))),'_', sep='')

  get_col <- c()
  for(sin in single_to_take){
    if(!any(get_col %like% sin)){
      possible_get <- column_signs[column_signs %like% sin]
      in_get <- unlist(regmatches(get_col, gregexpr("[[:digit:]]+", get_col)))
      get_col <- c(get_col,
                   possible_get[!possible_get %like% paste0(paste0('_', in_get, '_', sep=''), collapse = '|')][1])
    }
  }

  return(get_col)
}

