
library(stats)

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




