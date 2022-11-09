
#' Function to check if a sign has some distance conditions
#'
#' The function returns true if the new sign has the next two conditions:
#' - At least a distance of min_diff with every sign in selected.
#' - Joining with selected, at least there are total_diff matches with not all signs equal.
#'
#' @param data (data.table): A table with expected column 'sign'
#' @param selected (list of integer): A list with the position in data of selected bets
#' @param new (integer): the position in data of the new bet
#' @param min_diff (integer): the min distance to check, sign by sign, between selected and new.
#' @param total_diff (integer): the min matches with different results when joined seleted and new.
#'
#' @return is_diff (boolean): Wheter if new has the two conditions or not
#'
#' @examples
#' library(data.table)
#' data <- data.table(sign = c('11111111111111', '11111111112222', '11122222111111'))
#' is_diff_sign(data, c(1), c(2), 5, 4)
#' is_diff_sign(data, c(1), c(2), 4, 4)
#'
#' @export
#' @import data.table
#'
is_diff_sign <- function(data, selected, new, min_diff, total_diff){
  
  diffs = rep(FALSE, 14)
  
  vectornew <- unlist(strsplit(data[new]$sign, split=""))
  
  for(i in 1:length(selected)){
    vectorTested <- unlist(strsplit(data[selected[i]]$sign, split=""))
    this_diff = (vectorTested != vectornew)
    
    if(sum(this_diff) < min_diff){
      return(FALSE)
    }
    
    diffs = Reduce("|", list(diffs, this_diff))
  }
  
  return(sum(diffs) >= total_diff)
}


#' Function that selects a set of bets splitting the selection
#'
#' Normally, if you select the bets by probability, rentability or em,
#' consecutive bets have just 1 different sign. In order to avoid taking
#' n very similar bets. This function provides a method to take splited bets
#' with different signs in different matches.
#'
#' @param data (data.table): A table with expected column sign ordered by
#' priority of taking (typically em, probability or rentability)
#' @param nsigns (integer): number of bets you want to select
#' @param diff (integer): min number of signs different between every 2
#' selected bets.
#'
#' @return selected (data.table): Final bets selected
#'
#' @examples
#' library(data.table)
#' data <- data.table(sign = c('11111111111111', '11111111112222', '11122222111111'))
#' best_n_with_diff(data, 2, 5)
#'
#' @export
#' @import data.table
#' @import stats
#'
best_n_with_diff <- function(data, nsigns, diff){
  
  
  sp_fun <- splinefun(x = c(1,nsigns/5, nsigns*4/5, nsigns), y = c(diff,diff+1,min(nsigns+2, 13),min(nsigns+2, 13)),
                      method = c( "monoH.FC"))
  
  selected <- c(1)
  
  for(i in 1:nrow(data)){
    
    take = TRUE
    take = take & (is_diff_sign(data = data, selected = selected, new = i, min_diff = diff, total_diff = floor(sp_fun(length(selected)))))

    if(take){
      selected <- c(selected, i)
      print(i)
    }
    
    if(length(selected) == nsigns){
      break()
    }
  }
  
  return(data[selected])
}




