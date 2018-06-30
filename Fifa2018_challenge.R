library(gdata)
library(dplyr)
library(caTools)
library(kernlab)
library(readr)
library(caret)
fifa_rank<-read.csv('fifa_ranking.csv')
View(fifa_rank)
results<-read.csv('results.csv')
View(results)


results <- subset(results,home_team=="Argentina" | home_team=="France" | home_team=="Portugal" | home_team=="Japan" | home_team=="Spain" |
                    home_team=="Belgium" | home_team=="Uruguay" | home_team=="Russia" | home_team=="Denmark" | home_team=="Croatia" | 
                    home_team=="Mexico" | home_team=="Brazil" | home_team=="Sweden" | home_team=="Switzerland" |
                    home_team=="Colombia" | home_team=="England" | away_team=="Argentina" | away_team=="France" | away_team=="Portugal" | away_team=="Japan" | away_team=="Spain" |
                    away_team=="Belgium" | away_team=="Uruguay" | away_team=="Russia" | away_team=="Denmark" | away_team=="Croatia" | 
                    away_team=="Mexico" | away_team=="Brazil" | away_team=="Sweden" | away_team=="Switzerland" |
                    away_team=="Colombia" | away_team=="England")

nrow(results)

fifa_rank <- subset(fifa_rank,country_full=="Argentina" | country_full=="France" | country_full=="Portugal" | country_full=="Japan" | country_full=="Spain" |
                      country_full=="Belgium" | country_full=="Uruguay" | country_full=="Russia" | country_full=="Denmark" | country_full=="Croatia" | 
                      country_full=="Mexico" | country_full=="Brazil" | country_full=="Sweden" | country_full=="Switzerland" |
                      country_full=="Colombia" | country_full=="England")

#Data Cleaning 
View(results)
View(fifa_rank)

#Checking Missing Values
sapply(results,function(x) length(which(x==""))) #No Missing Value found
sapply(fifa_rank,function(x) length(which(x==""))) #No Missing Value found

#Check for NA Values
sum(is.na(results))   # No NA Values
sum(is.na(fifa_rank)) # No NA Values

#Checking for factor variable having uppercase or lowercase issue
#No uppercase/lower case issue found

#removing rows where home goal=away goal
results<-results[!(results$home_score==results$away_score),]
View(results)
#defing winning and losing team based on goals 
results$WinningTeam<-ifelse(results$home_score>results$away_score,
                           as.character(results$home_team),as.character(results$away_team))
results$LosingTeam<-ifelse(results$home_score>results$away_score,
                            as.character(results$away_team),as.character(results$home_team))
#creating derived metrics fr goal difference

results$GoalDiff<-abs(results$away_score-results$home_score)
results_final<-results[,c('WinningTeam','LosingTeam','tournament','GoalDiff')]

#converting to only two factors
results_final$tournament<-ifelse(results_final$tournament=='FIFA World Cup',
                                     'FIFA World Cup' ,'OtherTournament')
View(results_final)

####################################################
summary(fifa_rank)
str(fifa_rank)
View(fifa_rank)
#converting to date format
fifa_rank$rank_date<-as.Date(fifa_rank$rank_date,format("%Y-%m-%d"))
#taking last 2 year data only
fifa_rank<-fifa_rank[fifa_rank$rank_date>'2016-12-31',]
nrow(fifa_rank)
# derived metrics for weighted avg based on last year, previous year
fifa_rank$avg_weighted<-(fifa_rank$cur_year_avg_weighted*0.4)+
                       (fifa_rank$last_year_avg_weighted*0.3)+
                       (fifa_rank$two_year_ago_weighted*0.2)+
                       (fifa_rank$three_year_ago_weighted*0.1)

fifa_rank_final<-fifa_rank[,c('rank','country_full','total_points','previous_points',
                              'rank_change','avg_weighted','confederation')]
View(fifa_rank_final)
fifa2018<- merge(results_final,fifa_rank_final,
                by.x="WinningTeam", by.y="country_full",all = F)
View(fifa2018)
#taking losing team only these 16 teams
fifa2018<-subset(fifa2018,LosingTeam=="Argentina" | LosingTeam=="France" | LosingTeam=="Portugal" | LosingTeam=="Japan" | LosingTeam=="Spain" |
                   LosingTeam=="Belgium" | LosingTeam=="Uruguay" | LosingTeam=="Russia" | LosingTeam=="Denmark" | LosingTeam=="Croatia" | 
                   LosingTeam=="Mexico" | LosingTeam=="Brazil" | LosingTeam=="Sweden" | LosingTeam=="Switzerland" |
                   LosingTeam=="Colombia" | LosingTeam=="England")
View(fifa2018)
str(fifa2018$tournament)
#####################################################################
#data cleaning on final data set
#outlier treatment
sapply(fifa2018[,c('rank','total_points','previous_points',
                   'rank_change','avg_weighted','GoalDiff')],
                   function(x) quantile(x,seq(0,1,0.01),na.rm='T'))

#no outliers present
#checking for missing values
sapply(fifa2018,function(x) sum(is.na(x))) #no NA present

#feature standardisation(noramalising continous features)
fifa2018$rank<-scale(fifa2018$rank)
fifa2018$total_points<-scale(fifa2018$total_points)
fifa2018$previous_points<-scale(fifa2018$previous_points)
fifa2018$rank_change<-scale(fifa2018$rank_change)
fifa2018$avg_weighted<-scale(fifa2018$avg_weighted)
fifa2018$GoalDiff<-scale(fifa2018$GoalDiff)

str(as.factor(fifa2018$tournament))
str(as.factor(fifa2018$confederation))
unique(fifa2018$tournament)
View(fifa2018)
#creating data frame for categorical variables
fifa2018_chr<-fifa2018[,c('LosingTeam','tournament','confederation')]
View(fifa2018_chr)
#converting them to factors
fifa2018_fact<-data.frame(sapply(fifa2018_chr,function(x) factor(x)))
View(fifa2018_fact)
#taking'fifa as one factor and all as other tournament'

#creating dummy variables
dummies<-data.frame(sapply(fifa2018_fact,
                           function(x) data.frame(model.matrix(~x-1,data=fifa2018_fact))[,-1]))
str(dummies)

#assigning a number to each team
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Argentina']<-1
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='France']<-2
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Portugal']<-3
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Japan']<-4
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Spain']<-5
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Belgium']<-6
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Uruguay']<-7
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Russia']<-8
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Denmark']<-9
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Croatia']<-10
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Mexico']<-11
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Brazil']<-12
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Sweden']<-13
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Switzerland']<-14
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='Colombia']<-15
fifa2018$WinningTeamClass[fifa2018$WinningTeam=='England']<-116

fifa2018$WinningTeamClass<-factor(fifa2018$WinningTeamClass)
#final dataset
fifa2018_final<-cbind(fifa2018[,c('WinningTeamClass','GoalDiff','rank','total_points',
                          'previous_points','rank_change','avg_weighted')],dummies)
View(fifa2018_final)
str(fifa2018_final)

### splitting data into train and test
set.seed(100)
indices<-sample.split(fifa2018_final$WinningTeamClass,SplitRatio=0.6)
train<-fifa2018_final[indices,]
test<-fifa2018_final[!(indices),]
View(train)
View(test)



#########creating model###############

#Using Linear Kernel
Model_linear <- ksvm(WinningTeamClass~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)
test$test_pred<-Eval_linear
confusionMatrix(test$test_pred,test$WinningTeamClass)

#finding probability of each team using the above prediction
x<-aggregate(test$test_pred,by=list(test$test_pred),FUN=length)
y<-aggregate(test$WinningTeamClass,by=list(test$WinningTeamClass),FUN=length)
colnames(x)<-c('TeamNo','PredictedCount')
colnames(y)<-c('TeamNo','ActualCount')
Prob_df<-merge(x,y,by='TeamNo')
Prob_df$WinProb<-Prob_df$PredictedCount/Prob_df$ActualCount
View(Prob_df)
# winning team and prob 
#2 --France(Prob:1.1555),5 --spain(Prob:1.112),12 --brazil(prob(1.096)),
#9--Denmark(1.041),116--England(1.035),7--Uruguay(1.0037), 11--Mexico(1), 
#4--Japan(1), 6--Belgium (1),8-Russia(1), 13--Sweden(0.99), 15--Columbia(0.988), 
#1--Argentina(0.90) ,#3--Portugal(0.83), 10--croatia(0.683), 14--switzerland(0.63)



# we know that first match is between Uruguay and Portugal
#uruguay winning prob:1.0037  #portugal Winning Prob:0.83
#match 1 winner: uruguay

# we know that second match is between france and argentina
#France winning prob:1.155 #argentina Winning Prob:0.90
#match 2 winner: France

# we know that third match is between Brazil and mexico
#Brazil winning prob:1.096 #mexico Winning Prob:1
#match 3 winner: Brazil


# we know that fourth match is between Belgium and japan
#belgium winning prob:1 #japan Winning Prob:1
#match 4:
#Tie :we need to check how many times japan has beaten belgium
japanwin<-subset(test,test$WinningTeamClass==4 & test$LosingTeam.xBelgium==1 & test$test_pred==4)
nrow(japanwin) #12 times japan won against belgium
#check how many times belgium won against japan
Belgiumwin<-subset(test,test$WinningTeamClass==6 & test$LosingTeam.xJapan==1 & test$test_pred==6)
nrow(Belgiumwin) #8 times belgium won against japan

#match4: japan is the winner



# we know that fifth match is between spain and Russia
#spain winning prob:1 #1.112 Winning Prob:1
#match 5:spain is the winner

# we know that sixth match is between crotia and denmark
#crotia winning prob:1 #0.683  denmark Winning Prob:1.041
#match 6:denmark is the winner


# we know that seventh match is between sweden and switzerland
#sweden winning prob:1 #0.99  switzerland Winning Prob:0.63
#match 7:sweden is the winner


# we know that eighth match is between columbia and england
#columbia winning prob:1 #0.988  england Winning Prob:1.035
#match 8:England is the winner

###################QUARTER FINALS############################################

#we have following teams in quarter finals
#Uruguay, France, Brazil, japan
#spain denmark sweden england

#Matches in quarter finals are as follow
#match1 :Uruguay vs France
#uruguay prob:1.0037  France Prob:1.155
#checking how many times uruguay wins against France
uruguawin<-subset(test,test$WinningTeamClass==7 & test$LosingTeam.xFrance==1 & test$test_pred==7)
nrow(uruguawin) #18 times uruguay won against france
#check how many times france won against uruguay
Francewin<-subset(test,test$WinningTeamClass==2 & test$LosingTeam.xUruguay==1 & test$test_pred==2)
nrow(Francewin) #3 times france won against uruguay

#Here although france has high winning rate but Uruguay win more matches than Francewin
#QUARTER FINAL MATCH 1  winner: URUGUAY


#match2 :Brazil vs Japan
#brazil prob:1.096  japan Prob:1
#checking how many times brazil wins against japan
brazilwin<-subset(test,test$WinningTeamClass==12 & test$LosingTeam.xJapan==1 & test$test_pred==12)
nrow(brazilwin) #70 times uruguay won against france
#check how many times japan won against brazil
japanwin_quat<-subset(test,test$WinningTeamClass==4 & test$LosingTeam.xBrazil==1 & test$test_pred==4)
nrow(japanwin_quat) #0 times japan won against brazil

#QUARTER FINAL MATCH 2 Winner: BRAZIL

#match3 :spain vs denmark
#spain prob:1.112  denmark Prob:1.041
#checking how many times denmark wins against spain
denmarkwin<-subset(test,test$WinningTeamClass==9 & test$LosingTeam.xSpain==1 & test$test_pred==9)
nrow(denmarkwin) #16 times denmark won against spain
#check how many times spain won against denmark
spainwin<-subset(test,test$WinningTeamClass==5 & test$LosingTeam.xDenmark==1 & test$test_pred==5)
nrow(spainwin) #63 times japan won against brazil

#QUARTER FINAL MATCH 3 Winner: SPAIN

#match4 :sweden vs england
#sweden prob:0.99  england Prob:1.035
#checking how many times sweden wins against england
swedenwin<-subset(test,test$WinningTeamClass==13 & test$LosingTeam.xEngland==1 & test$test_pred==13)
nrow(swedenwin) #47 times sweden won against england

#check how many times england won against sweden
englandwin<-subset(test,test$WinningTeamClass==116 & test$LosingTeam.xSweden==1 & test$test_pred==116)
nrow(englandwin) #52 times japan won against brazil

#QUARTER FINAL MATCH 4 Winner: ENGLAND

##########  SEMI FINALS #######################
#four semifinalists are URUGUAY, BRAZIL, SPAIN, ENGLAND

#semifinal match 1:uruguay and brazil
#uruguay prob:1.0037  brazil Prob:1.096
#checking how many times uruguay wins against brazil
uruguyawin_semi<-subset(test,test$WinningTeamClass==7 & test$LosingTeam.xBrazil==1 & test$test_pred==7)
nrow(uruguyawin_semi) #142 times uruguya won against brazil
#check how many times brazil won against uruguay
brazilwin_semi<-subset(test,test$WinningTeamClass==12 & test$LosingTeam.xUruguay==1 & test$test_pred==12)
nrow(brazilwin_semi) #208 times brazil won against uruguay

#SEMIFINAL1 WINNER: BRAZIL



#semifinal match 2:spain and england
#spain prob:1.112  england Prob:1.035
#checking how many times spain wins against england
spainwin_semi<-subset(test,test$WinningTeamClass==5 & test$LosingTeam.xEngland==1 & test$test_pred==5)
nrow(spainwin_semi) #34 times spain won against england
#check how many times england won against spain
englandwin_semi<-subset(test,test$WinningTeamClass==116 & test$LosingTeam.xSpain==1 & test$test_pred==116)
nrow(englandwin_semi) #83 times england won against spain

#SEMIFINAL2 WINNER: ENGLAND


#######################FINALS####################################################
#final match is between brazil and england

#Brazil prob:1,096  England Prob:1.035
#checking how many times brazil wins against england
brazilwin_final<-subset(test,test$WinningTeamClass==12 & test$LosingTeam.xEngland==1 & test$test_pred==12)
nrow(brazilwin_final) #74 times brazil won against england
#check how many times england won against brazil
englandwin_final<-subset(test,test$WinningTeamClass==116 & test$LosingTeam.xBrazil==1 & test$test_pred==116)
nrow(englandwin_final) #24 times england won against brazil

###### WINNER OF FIFA 2018 is BRAZIL ################################







                    

