#Import the dataset

dataset<- read.csv('IDS.csv')

#----PRE PROCESSING START------

#Remove unnecessary variables

dataset<-dataset[,-c(1,4,15)]

#Encoding Categorical Data

dataset$toss_decision = factor(dataset$toss_decision,
                               levels = c('field', 'bat'),
                               labels = c(1, 2))

dataset$result = factor(dataset$result,
                        levels = c('normal', 'tie','no result'),
                        labels = c(1, 2, 3))

#Season wise dataset

#install.package('dplyr')

library(dplyr)

dataset_2017<-filter(dataset,season==2017)

dataset_2016<-filter(dataset,season==2016)

dataset_2015<-filter(dataset,season==2015)

dataset_2014<-filter(dataset,season==2014)

dataset_2013<-filter(dataset,season==2013)

dataset_2012<-filter(dataset,season==2012)

dataset_2011<-filter(dataset,season==2011)

dataset_2010<-filter(dataset,season==2010)

dataset_2009<-filter(dataset,season==2009)

dataset_2008<-filter(dataset,season==2008)

#Missing value prediction
#No umpire is assigned to a match played between royal challenger banglore and delhi daredevil in IPL 2017
#First we will assign NA to missing valule then
#We will assign those two umpires which are featured most in the IPL 2017

#NA to missing values
library(zoo)
dataset_2017$umpire1[dataset_2017$umpire1 == ""] <- NA
dataset_2017$umpire2[dataset_2017$umpire2 == ""] <- NA

#finding most featured umpires
umpire1_appearance<- aggregate(data.frame(No_of_match = dataset_2017$umpire1), list(Umpire1 = dataset_2017$umpire1), length)
max_match<-which.max(umpire1_appearance$No_of_match)
umpire1<-umpire1_appearance[4,1]


umpire2_appearance<- aggregate(data.frame(No_of_match = dataset_2017$umpire2), list(Umpire2 = dataset_2017$umpire2), length)
max_match2<-which.max(umpire2_appearance$No_of_match)
umpire2<-umpire2_appearance[10,1]

#Replacing NA with most featured umpires(i.e umpire1 and umpire2)
dataset_2017$umpire1[is.na(dataset_2017$umpire1)]<-umpire1

dataset_2017$umpire2[is.na(dataset_2017$umpire2)]<-umpire2


#--------PRE-PROCESSING END--------

#INFERENCES from a particular season of IPL 2013 dataset

#1 Total number of matches won by a team in entire season
matches_won_by_a_team<- aggregate(data.frame(Matches_Won = dataset_2013$winner), list(Team = dataset_2013$winner), length)
matches_won_by_a_team<-matches_won_by_a_team[rev(order(matches_won_by_a_team$Matches_Won)),]
matches_won_by_a_team

#2 Total number of Toss win by a team in entire season
toss_won_by_a_team<- aggregate(data.frame(Toss_Won = dataset_2013$toss_winner), list(Team = dataset_2013$toss_winner), length)
toss_won_by_a_team<-toss_won_by_a_team[rev(order(toss_won_by_a_team$Toss_Won)),]
toss_won_by_a_team

#3 Number of normal and Tie matches
Normal_and_tie<- aggregate(data.frame(Number_of_matches = dataset_2013$result), list(Result = dataset_2013$result), length)
Normal_and_tie


#4 Number of matches affected by weather
dl_applied<- aggregate(data.frame(Number_of_matches = dataset_2013$dl_applied), list(dl_applied = dataset_2013$dl_applied), length)
dl_applied


#5 Most number of Player of the match
Man_of_the_match_won_by_a_player<- aggregate(data.frame(MOM_won = dataset_2013$player_of_match), list(Player = dataset_2013$player_of_match), length)
Man_of_the_match_won_by_a_player<-Man_of_the_match_won_by_a_player[rev(order(Man_of_the_match_won_by_a_player$MOM_won)),]
Man_of_the_match_won_by_a_player

#6 Number of matches won batting first
Matches_won_batting_first <- as.data.frame(length(which(dataset_2013$win_by_runs != 0)))
colnames(Matches_won_batting_first)[1]<-"Matches_Won_Batting_First"
Matches_won_batting_first

#7 Number of matches won batting Second
Matches_won_batting_Second <- as.data.frame(length(which(dataset_2013$win_by_wickets != 0)))
colnames(Matches_won_batting_Second)[1]<-"Matches_Won_Batting_Second"

Batting_First_Second<-merge(Matches_won_batting_first,Matches_won_batting_Second)
Batting_First_Second$Tied_Match<-2
Matches_won_batting_Second

#8 Winner of IPL 2013
Winning_Team<-as.data.frame(matches_won_by_a_team[which.max(matches_won_by_a_team$Matches_Won),1])
colnames(Winning_Team)[1]<-"Winner_IPL"
Winning_Team

#Visualisation for IPL 2013

library(ggplot2)
plot1<-ggplot(data=matches_won_by_a_team, aes(x=Team, y=Matches_Won,color=Team,fill=Team,labels=Matches_Won)) +
  geom_col()+
  ggtitle("Performance of Teams in IPL 2013")+
  xlab("Teams") + ylab("Number of Matches Won") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.2))+
  geom_text(aes(x = Team, 
                y = Matches_Won + 0.5, 
                label = round(Matches_Won, 2)))+
  coord_flip()

plot1

library(ggplot2)
plot2<-ggplot(data=toss_won_by_a_team, aes(x=Team, y=Toss_Won,color=Team,fill=Team,labels=Toss_Won)) +
  geom_col()+
  ggtitle("Teams that have been lucky in IPL 2013")+
  xlab("Teams") + ylab("Number of Toss Won") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.2))+
  geom_text(aes(x = Team, 
                y = Toss_Won + 0.5, 
                label = round(Toss_Won, 2)))+
  coord_flip()

plot2


library(ggplot2)
library(ggrepel)

plot3<-ggplot(Man_of_the_match_won_by_a_player, aes(x=Player, y=MOM_won)) +
  geom_point()+
  geom_text_repel(aes(Player, MOM_won, label = Player), size = 3)

plot3




piec<-as.numeric(Batting_First_Second[1,])
lbls1 <- c("Batting_First", "Batting_Second", "NoResult")
pct <- floor((piec/sum(piec)*100))
lbls1 <- paste(lbls1, pct) # add percents to labels 
lbls1 <- paste(lbls1,"%",sep="") # ad % to labels
plot4<-pie(piec,
           labels=lbls1,
           main="Number of Matches won Batting First and Second",
           col=c("red","orange","yellow"),
           border="brown",
           clockwise=TRUE
)

plot4


a = data.frame(win="Runs", value = dataset_2013$win_by_runs)
b = data.frame(win = "Wickets", value = dataset_2013$win_by_wickets)
plot.data = rbind(a,b)

plot5<-ggplot(plot.data, aes(x=win, y=value, fill=win)) +  
  geom_boxplot()+
  ggtitle("Box Plot Showing Win by Runs and Win by Wickets Statistics in IPL 2013")

plot5

summary(dataset_2013$win_by_runs)
summary(dataset_2013$win_by_wickets)


#Inferences from all seasons of IPL.

#1. Most consistent Team in the history of IPL
most_consistent_team<- aggregate(data.frame(Matches_Won = dataset$winner), list(Team = dataset$winner), length)
most_match<-which.max(most_consistent_team$Matches_Won)
Team<-as.data.frame(most_consistent_team[9,1])
colnames(Team)[1]<-"Most_Consistent_Team"
most_consistent_team<-most_consistent_team[-1,]
most_consistent_team<-most_consistent_team[rev(order(most_consistent_team$Matches_Won)),]
most_consistent_team

#2. Most consistent Player in the history of IPL
most_consistent_player<- aggregate(data.frame(MOM_Won = dataset$player_of_match), list(Player = dataset$player_of_match), length)
most_mom<-which.max(most_consistent_player$MOM_Won)
Player<-as.data.frame(most_consistent_player[33,1])
colnames(Player)[1]<-"Most_Consistent_Player"
most_consistent_player<-most_consistent_player[-1,]
most_consistent_player<-most_consistent_player[rev(order(most_consistent_player$MOM_Won)),]
most_consistent_player

#3. AVERAGE WINNING RUNS in all the seasons of IPL
avg_winning_runs<-mean(dataset$win_by_runs)
avg_winning_runs<-round(avg_winning_runs)
avg_winning_runs

#4. AVERAGE WINNING WICKETS in all the seasons of IPL
avg_winning_wkts<-mean(dataset$win_by_wickets)
avg_winning_wkts<-round(avg_winning_wkts)
avg_winning_wkts

#5. Number of normal,tie and no result matches in history of IPL
Normal_and_tie_and_noresult<- aggregate(data.frame(Number_of_matches = dataset$result), list(Result = dataset$result), length)
Normal_and_tie_and_noresult


#Visualisation for IPL History.


library(ggplot2)
plot6<-ggplot(data=most_consistent_team, aes(x=Team, y=Matches_Won,color=Team,fill=Team,labels=Matches_Won)) +
  geom_col()+
  ggtitle("Performance of Teams in the history of IPL")+
  xlab("Teams") + ylab("Number of Matches Won") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(x = Team, 
                y = Matches_Won + 1.5, 
                label = round(Matches_Won, 2)))+
  coord_flip()

plot6


library(ggplot2)
library(ggrepel)
plot7<-ggplot(most_consistent_player, aes(x=Player, y=MOM_Won)) +
  geom_point()+
  geom_text_repel(aes(Player, MOM_Won, label = Player), size = 3)
 
plot7


aa = data.frame(win="Runs", value = dataset$win_by_runs)
bb = data.frame(win = "Wickets", value = dataset$win_by_wickets)
plot.data = rbind(aa,bb)

plot8<-ggplot(plot.data, aes(x=win, y=value, fill=win)) +  
  geom_boxplot()
  
  ggtitle("Box Plot Showing Win by Runs and Win by Wickets Statistics in the history of IPL")

plot8

summary(dataset$win_by_runs)
summary(dataset$win_by_wickets)




piec1<-as.numeric(unlist(Normal_and_tie_and_noresult[1:3,2]))
lbls <- c("Normal", "tie", "NoResult")
plot9<-pie(piec1,
           labels=lbls,
           main="Number of Normal/tie/noresult matches in the history of IPL",
           col=c("red","orange","yellow"),
           border="brown",
           clockwise=TRUE,
           radius = 1,
           cex=0.8
)

plot9