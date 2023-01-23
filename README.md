# quini-utils

R library to perform better on quiniela bets.

## How this works

Standard quiniela's players think that the best way to earn money in quiniela is to be right in as many matches as you can.
So, in order to achieve that, they usually bet that the team with more probabilities of winning will be the winner of the match.

However, the prize in quiniela is not fixed, it is calculated as the collected money in bets at this week minus 40% of government taxes.
Besides, the prize is splitted between the right bets. That means that to win quiniela when so many others fail is better than win quiniela
when many others do. But, as expected, if you select rare bets with low probability you will never win the game.
The point is to balance between bets low voted and probably bets.

The government updates the % of people that are voting every result at every match. You can't know exactly what combinations were done in the
14 matches, but you can make a good estimation. In the other hand, there are several betting houses publishing the probability of winning
for every match. In a graph would be:

![explain](https://user-images.githubusercontent.com/15888562/214118440-b0097847-c166-4e5a-b125-de07d9b51603.PNG)

Now, the quini-utils library comes. It allows you to download this information and calculate the probability of being right in x matches and the 
probability of having other people with x matches right for every single combination. Many people bet by dividing probably by voted percentages, which
is called profitability, and ordering that to select the most profitability combinations.

However, you can also calculate the mathematical expectation (EM), which calculus is a bit complex.
It will take a while to calculate (about 30 minutes for EM10, EM11, EM12, EM13 and EM14 all together, 10 min for EM12, EM13 and EM14) 
but it is worth it.


## Install
Install last version from github
> install_github("MiguelAngelTorres/quini-utils")


## Examples
How to select 10 best bets ordered by EM12

> library(quiniutils)

Get matches information from EduardoLosilla website

> matches <- get_data_eduardolosilla('2023', '35')

![Eduardo](https://user-images.githubusercontent.com/15888562/214114011-616a6c6b-c3d1-45e1-bb02-60994cca98e2.PNG)

Calculate the real and voted probabilities
> table_prob <- calculate_probabilities(matches)

![prob_voted](https://user-images.githubusercontent.com/15888562/214114854-451d26f9-98ec-4aa1-a77e-0b38436c5fd9.PNG)

Calculate the EM of each bet
> table_prob <- get_em(table_prob, money = money, level = 12, optimization = TRUE)

![EM](https://user-images.githubusercontent.com/15888562/214117699-b400c856-9a9d-4652-956d-5e06dee11346.PNG)

Pick the best 10 bets
> selected <- table_prob[order(-em_12)][1:10]

Get the output to vote
> write_bet("selected.txt", selected)


