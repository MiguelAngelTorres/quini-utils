
#' Scrapper function to get matches data from quinielaticas's web
#'
#' The function reads the html placed in the param url
#' and returns the real and voted info for each match.
#'
#' Note that the scrapper is only expecting the 'https://quinielaticas.com/' or
#' 'https://quinielaticas.com/analisis-de-la-proxima-jornada-de-la-quiniela/' html
#' depends on if you prefer this week or next week matches. If you are interested
#' in older information, use get_data_eduardolosilla function instead.
#'
#' @param url (string): The url where the html is located
#'
#' @return matches (data.table): A table with the matches information
#'
#' @examples
#' \dontrun{
#' get_data_quinielaticas('https://quinielaticas.com/')
#' }
#'
#' @export
#' @import data.table
#' @import rvest
#' @import stringr
#'
get_data_quinielaticas <- function(url){
  
  # get url with table
  html <- read_html(url)
  iframes <- html %>% html_nodes('iframe') %>% html_attr("src")
  table_web <- iframes[iframes %like% 'spreadsheets'][1]
  
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



#' Scrapper function to get matches data from eduardolosilla's web
#'
#' The function sends a request to eduardolosilla's web, parses the
#' response and returns the matches real and voted probabilities.
#'
#'
#' @param temporada (string): The season's bet. The season is always encoded with the bigger year, that means that
#' the season 2022/2023 is requested with temporada = 2023
#' @param jornada (string): The jornada's bet.
#'
#' @return matches (data.table): A table with the matches information
#'
#' @examples
#' \dontrun{
#' get_data_eduardolosilla('2023', '33')
#' }
#'
#' @export
#' @import data.table
#' @import xml2
#' @import XML
#'
get_data_eduardolosilla <- function(temporada, jornada){

  my_xml <- read_xml(paste0("https://api.eduardolosilla.es/servicios/v1/probabilidad_real?jornada=",jornada,"&temporada=",temporada))
  my_xml <- xmlParse(my_xml)
  partidos <- getNodeSet(my_xml, "//partido")
  real_1 <- as.numeric(unlist(lapply(partidos, xmlGetAttr, "porcDec_1")))
  real_x <- as.numeric(unlist(lapply(partidos, xmlGetAttr, "porcDec_X")))
  real_2 <- as.numeric(unlist(lapply(partidos, xmlGetAttr, "porcDec_2")))

  my_xml <- read_xml(paste0("https://api.eduardolosilla.es/servicios/v1/porcentajes_lae?jornada=",jornada,"&temporada=",temporada))
  my_xml <- xmlParse(my_xml)
  partidos <- getNodeSet(my_xml, "//partido")
  lae_1 <- as.numeric(unlist(lapply(partidos, xmlGetAttr, "porc_1")))
  lae_x <- as.numeric(unlist(lapply(partidos, xmlGetAttr, "porc_X")))
  lae_2 <- as.numeric(unlist(lapply(partidos, xmlGetAttr, "porc_2")))

  my_xml <- read_xml(paste0("https://api.eduardolosilla.es/servicios/v1/porcentajes_quinielista?jornada=",jornada,"&temporada=",temporada))
  my_xml <- xmlParse(my_xml)
  partidos <- getNodeSet(my_xml, "//partido")
  quinielista_1 <- as.numeric(unlist(lapply(partidos, xmlGetAttr, "porc_1")))
  quinielista_x <- as.numeric(unlist(lapply(partidos, xmlGetAttr, "porc_X")))
  quinielista_2 <- as.numeric(unlist(lapply(partidos, xmlGetAttr, "porc_2")))

  voted_1 = (quinielista_1 + lae_1) * 0.5
  voted_x = (quinielista_x + lae_x) * 0.5
  voted_2 = (quinielista_2 + lae_2) * 0.5

  matches <- data.table(partido = 1:14, real_1 = real_1, real_x = real_x, real_2 = real_2, voted_1 = voted_1, voted_x = voted_x, voted_2 = voted_2)

  return(matches)

}



#' Function to get random matches data
#'
#' The function generates real and voted random probabilities
#'
#' @param uniform (boolean): If TRUE, all real and voted probabilities returned are
#' equal (0.33). Default value is FALSE.
#'
#' @return matches (data.table): A table with the random matches information
#'
#' @examples
#' get_random_matches()
#'
#' @export
#' @import data.table
#'
get_random_matches <- function(uniform = FALSE){

  if(uniform){

    matches <- data.table(partido = 1:14,
                          real_1 = rep(1/3, 14),
                          real_x = rep(1/3, 14),
                          real_2 = rep(1/3, 14),
                          voted_1 = rep(1/3, 14),
                          voted_x = rep(1/3, 14),
                          voted_2 = rep(1/3, 14)
    )

  }else{

    matches <- data.table(partido = 1:14,
                          real_1 = runif(14, 5, 80),
                          voted_1 = runif(14, 5, 80)
    )
    matches[,':='(real_x = runif(1, 5, 90 - real_1),
                  voted_x = runif(1, 5, 90 - voted_1)), by=partido]
    matches[,':='(real_2 = 100 - real_1 - real_x,
                  voted_2 = 100 - voted_1 - voted_x)]

  }

  return(matches[,.(real_1, real_x, real_2, voted_1, voted_x, voted_2)])

}
