# quini-utils

R library to perform better on quiniela bets.

## Install
Install last version from github
> install_github("MiguelAngelTorres/quini-utils", auth_token = read.csv('git_token.txt', header = FALSE)$V1, force = TRUE)



## Examples
Case of use: selected 10 best bets order by EM12

> library(quiniutils)

Get matches information from quinielaticas website

> matches <- get_data('https://quinielaticas.com/')

Calculate the real and voted probabilities
> table_prob <- calculate_probabilities(matches)

Calculate the EM of each bet
> table_prob <- get_em(table_prob, money = money, level = 12, optimization = TRUE)

Pick the best 10 bets
> selected <- table_prob[order(-em_12)][1:10]

Get the output to vote
> fileConn<-file("selected.txt")
> 
> writeLines(c(selected$sign), fileConn)
> 
> close(fileConn)

