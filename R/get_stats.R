
#' Calculate the played percentage for each match given a set of bets
#'
#' This is a useful function to measure the trend in a bet method.
#' If most of the bets in the set have the same result in any match, the
#' function will return a number close to 1 in the voted sign and a number close
#' to 0 in the other two signs.
#'
#' @param selected (data.table): a table with a column named sign, that
#' contains a string of 14 characters with the voted signs for each match.
#'
#' @return stats (data.table): A table with 3 columns, the percentage
#' played for each sign on each match.
#'
#' @export
#' @examples
#' get_played_porcentage(selected)
#'
get_played_porcentage <- function(selected){
  
  plays <- transpose(selected[,strsplit(sign, split="")])
  stats <- data.table()
  
  for(i in c(1:14)){
    str_eval <- paste0(
      "stats <- rbind(stats,
          data.table('%1' = sum(plays$V",i," == '1')/nrow(plays),
                     '%x' = sum(plays$V",i," == 'x')/nrow(plays),
                     '%2' = sum(plays$V",i," == '2')/nrow(plays))
          )"
    )
    
    eval(parse(text = str_eval))
  }
  
  return(stats)
}
