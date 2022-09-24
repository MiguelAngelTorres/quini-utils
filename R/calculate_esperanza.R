
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
#'
#' @return prob_voted_table (data.table): The same prob_voted_table with em columns added.
#'
#' @examples
#' get_em(prob_voted_table, 2600000, 10)
#'
#' @export
#' @import data.table
#'

get_em <- function(prob_voted_table, money, level = 10){
  
  get_em_10 <- level <= 10
  get_em_11 <- level <= 11
  get_em_12 <- level <= 12
  get_em_13 <- level <= 13
  
  if(get_em_13){
    
    prob_voted_table[,':='(sum_prob_13 = 0, sum_voted_13 = 0, aux_mult_13 = prob_14 * voted_13,
                     sum_prob_12 = 0, sum_voted_12 = 0, aux_mult_12 = prob_14 * voted_12,
                     sum_prob_11 = 0, sum_voted_11 = 0, aux_mult_11 = prob_14 * voted_11,
                     sum_prob_10 = 0, sum_voted_10 = 0, aux_mult_10 = prob_14 * voted_10)]
  }
  
  
  if(get_em_13){
    for(i in c(1:14)){
      eval(parse(text = paste0("prob_voted_table[,':='(sign_" ,i, " = substr(sign,",i,",",i,"))]")))
    }
    
    column_signs = c('sign_1','sign_2','sign_3','sign_4','sign_5','sign_6','sign_7',
                        'sign_8','sign_9','sign_10','sign_11','sign_12','sign_13','sign_14')
    
    group_column <- paste0(column_signs, collapse = ',')
    
    eval(parse(text=paste0("prob_voted_table[,':='(prob_i_j_h_k = sum(prob_14), aux_10_i_j_h_k = sum(aux_mult_10), aux_11_i_j_h = sum(aux_mult_11), aux_12_i_j = sum(aux_mult_12), aux_13_i = sum(aux_mult_13)),by=.(",
                           group_column,")]", collapse='')))
    
    for(i in c(1:14)){
      out_column_i = c(paste0('sign_', i))
      group_column <- paste0(column_signs[!column_signs %in% c(out_column_i)], collapse = ',')
      
      eval(parse(text=paste0("prob_voted_table[,':='(prob_j_h_k = sum(prob_14), aux_10_j_h_k = sum(aux_mult_11), aux_11_j_h = sum(aux_mult_11), aux_12_j = sum(aux_mult_12), aux_13= sum(aux_mult_13)),by=.(",
                             group_column,")]", collapse='')))
      
      if(get_em_12 && i < 14){
        for(j in c((i+1):14)){
          out_column_j = c(paste0('sign_', j))
          group_column <- paste0(column_signs[!column_signs %in% c(out_column_i, out_column_j)], collapse = ',')
          
          eval(parse(text=paste0("prob_voted_table[,':='(prob_h_k = sum(prob_14), aux_10_h_k = sum(aux_mult_10), aux_11_h = sum(aux_mult_11), aux_12 = sum(aux_mult_12)),by=.(",
                                 group_column,")]", collapse='')))
          
          if(get_em_11 && j < 14){
            for(h in c((j+1):14)){
              out_column_h = c(paste0('sign_', h))
              group_column <- paste0(column_signs[!column_signs %in% c(out_column_i, out_column_j, out_column_h)],
                                     collapse = ',')
              
              eval(parse(text=paste0("prob_voted_table[,':='(prob_k = sum(prob_14), aux_10_k = sum(aux_mult_10), aux_11 = sum(aux_mult_11)),by=.(",
                                     group_column,")]", collapse='')))
              
              if(get_em_10 && h < 14){
                for(k in c((h+1):14)){
                  out_column_k = c(paste0('sign_', k))
                  
                  group_column <- paste0(column_signs[!column_signs %in% c(out_column_i, out_column_j, out_column_h, out_column_k)],
                                         collapse = ',')
                  
                  eval(parse(text=paste0("prob_voted_table[,':='(prob = sum(prob_14), aux_10 = sum(aux_mult_10)),by=.(",
                                         group_column,")]", collapse='')))
  
                  
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


