
library(rvest)

#' Scrapper function to get matches data
#'
#' The function reads the html placed in the param url
#' and returns the real and voted info for each match.
#'
#' Note that the scrapper is only expecting the 'https://quinielaticas.com/' or
#' 'https://quinielaticas.com/analisis-de-la-proxima-jornada-de-la-quiniela/' html
#' depends on if you prefer this week or next week matches.
#'
#' @param url (string): The url where the html is located
#'
#' @return matches (data.table): A table with the matches information
#'
#' @export
#' @examples
#' get_data('https://quinielaticas.com/')
#'
get_data <- function(url){
  
  # get url with table
  html <- read_html(url)
  iframes <- html %>% html_nodes('iframe') %>% html_attr("src")
  table_web <- iframes[iframes %like% 'spreadsheets']
  
  html <- read_html(table_web)
  
  table_info <- (html %>% html_nodes('table'))[5]
  table <- table_info %>% html_table()
  table = as.data.table(as.data.frame(table))  

  real <- table[2:15, .(partido = Var.1 - 1, 
                        real_1 = as.numeric(str_replace(Var.2, ',', '.')), 
                        real_x = as.numeric(str_replace(Var.3, ',', '.')), 
                        real_2 = as.numeric(str_replace(Var.4, ',', '.'))
                        )
                ]
  
  voted <- table[25:38, .(voted_1 = as.numeric(str_replace(Var.2, ',', '.')), 
                          voted_x = as.numeric(str_replace(Var.3, ',', '.')), 
                          voted_2 = as.numeric(str_replace(Var.4, ',', '.'))
                         )
                ]

  matches <- cbind(real, voted)
  
  return(matches)
  
}
