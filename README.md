
## Predicting the winner of the 2018 FIFA World Cup
# Method:
Use data from Kaggle to model the outcome of certain pairings between teams, given their rank, points, and the weighted point difference with the opponent.
Used Support vector Machine Algorithm to predict the qualifiers for Round of 16 , Quarter and Semis

## Datasets
We have used two datsets :-  
FIFA rankings from 1993 to 2018 June – It consists of the points , change in rank , current rank and weighted average point difference b/w opponents.
International Soccer matches from 1872 to 2018. This is used to check how teams have played against each other. 

All the programming is done in R. R code file is attached below.
Approach:-  We have calculated the winning probability of all 16 teams. Below are the 16 teams in order of winning probability from highest to lowest.
  

France
Spain
Brazil
Denmark
England
Uruguay
Mexico
Japan
Belgium
Russia
Sweden
Colombia
Argentina
Portugal
Crotia
Switzerland

We checked which team will be playing against which . The team having highest probability will move to quarter finals.
Below are 8 Team that will qualify to Quarters 
Uruguay
France
Brazil
Belgium
Spain
Denmark
Sweden
England

To predict who will win the quarter finals we have taken into account two things-
Who has high probability of winning as per our model.
Who has won more matches against each other as Uruguay will be playing against France , we have given preference to the team who has won more b/w the two. This is also calculated using R code . The R code for the whole prediction is attached below along with the comments.
For e.g. Uruguay has probability of winning any match is 1.07 and France is having 1.15 (more than actual) .  But France has won only 3 games against  Uruguay where as Uruguay has won 18 against France .  As probabilities are almost same , Uruguay seems to be the clear winner.
## Semi Finalist
Spain 
England
Uruguay 
Brazil

## Finals         England Vs Brazil
As per our model Brazil has higher probability of winning and won more matches against England , Brazil will win the FIFA 2018.
