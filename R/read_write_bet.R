
#' Read a file with bets
#'
#' The file must be a .txt file with one bet per row.
#' Every bet must be composed by 14 characters with 1,2 or x
#' and 2 more with the 15th match, indicating 0,1,2 or M
#'
#' A valid example of file.txt with 3 bets would be:
#' 1111111111111102
#' 1211111111111102
#' 11111111111X11M2
#'
#' @param path (string): The path where the file is located
#'
#' @return bets (data.table): A table with 1 column named sign
#' that contains the bets.
#'
#' @examples
#' \dontrun{
#' fileConn<-file("file.txt")
#' writeLines(c('1111111111111102', '11111111111X11M2'), fileConn)
#' close(fileConn)
#' read_bet("file.txt")
#' }
#'
#'
#' @export
#' @import data.table
#'
read_bet <- function(path){
  bets <- fread(path, header = F)

  setnames(bets, 'V1', 'sign')
  bets <- bets[sign != ""]

  return(bets)
}



#' Write a file with bets
#'
#' The file will be a .txt file with one bet per row.
#'
#' A valid example of file.txt with 3 bets would be:
#' 1111111111111102
#' 1211111111111102
#' 11111111111X11M2
#'
#' @param bet (vector of strings): The list of bets to write
#' @param path (string): The path where the file will be located
#'
#'
#' @examples
#' \dontrun{
#' bet = c('1111111111111102', '11111111111X11M2')
#' write_bet(bet, "file.txt")
#' }
#'
#'
#' @export
#' @import data.table
#'
write_bet <- function(bet, path){
  fileConn<-file(path)
  writeLines(bet, fileConn)
  close(fileConn)
}