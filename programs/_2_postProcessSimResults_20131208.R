# post processing: takes the list "results" from the setup file as input and creates a couple of
# dataframes used for graphing in the following program

rm(list=ls())  # clean up environment

# load important libraries
library(vioplot)
library(car)
library(lattice) 
library(ggplot2)
library(plyr)

# set working environment
setwd("~/GoogleDrive/WORKSPACE/travaille/dataScience/1205_euroCup/")

#save originalPlottingParameters, to reset par, just type par(resetToOriginalPlottingPar)
resetToOriginalPlottingPar<-par()

# load results of the EC simulation
load("./data/results.rda")


# create some vectors needed for labeling and looping below
countries    <- c("Pol", "Gre", "Rus", "Cze", "Net", "Den", "Ger","Por", 
                  "Esp", "Ita", "Ire", "Cro","Ukr", "Swe", "Fra", "Eng")
revCountries <- rev(countries) #reverse order of country names

# store results list as a dataframe
results.data.frame<-as.data.frame(results)


#determine how many interations were run for the simulation
numOfSimIterations <- length(results.data.frame$Pol.gpoints)


# calculate the total number of points accumulated during euro cup by each national team
total.gpoints           <- (rowSums(results.data.frame[,seq(from=1,to=80,by=5)], na.rm = FALSE)*2)
total.eliminationpoints <- (rowSums(results.data.frame[,seq(from=4,to=80,by=5)], na.rm = FALSE)*2)
total.points            <- total.gpoints + total.eliminationpoints

# create data frame that indicate if a country makes it out of the group (0/1)
outOfGroup.matrix       <- results.data.frame[,seq(from=2,to=80,by=5)]

for(i in 1:length(outOfGroup.matrix)) {
  outOfGroup.matrix[[i]][outOfGroup.matrix[[i]] %in% c(3,4)]   <- 0
  outOfGroup.matrix[[i]][outOfGroup.matrix[[i]] %in% c(1,2)]   <- 1
}
# label data.frame columns
names(outOfGroup.matrix)<-c("Pol.elStage", "Gre.elStage", "Rus.elStage",  "Cze.elStage",
                            "Net.elStage", "Den.elStage", "Ger.elStage",	"Por.elStage", "Esp.elStage",	
                            "Ita.elStage", "Ire.elStage", "Cro.elStage",	"Ukr.elStage", "Swe.elStage",	
                            "Fra.elStage", "Eng.elStage")

# calculate some additional vectors
dollars.per.point <- 1000/total.points
dollars.per.point <- mean(dollars.per.point)

# calculate the total points per national team (group+elimination stage)
attach(results.data.frame)
Pol.points <-Pol.gpoints + Pol.eliminationpoints
Gre.points <-Gre.gpoints + Gre.eliminationpoints
Rus.points <-Rus.gpoints + Rus.eliminationpoints
Cze.points <-Cze.gpoints + Cze.eliminationpoints
Net.points <-Net.gpoints + Net.eliminationpoints
Den.points <-Den.gpoints + Den.eliminationpoints
Ger.points <-Ger.gpoints + Ger.eliminationpoints
Por.points <-Por.gpoints + Por.eliminationpoints
Esp.points <-Esp.gpoints + Esp.eliminationpoints
Ita.points <-Ita.gpoints + Ita.eliminationpoints
Ire.points <-Ire.gpoints + Ire.eliminationpoints
Cro.points <-Cro.gpoints + Cro.eliminationpoints
Ukr.points <-Ukr.gpoints + Ukr.eliminationpoints
Swe.points <-Swe.gpoints + Swe.eliminationpoints
Fra.points <-Fra.gpoints + Fra.eliminationpoints
Eng.points <-Eng.gpoints + Eng.eliminationpoints
detach(results.data.frame)


# combine the total points of all 16 national teams in one data.frame
totalPoints.frame <- as.data.frame(cbind(Pol.points,  Gre.points,  Rus.points,  Cze.points,
                            Net.points,  Den.points,	Ger.points,	Por.points,	Esp.points,	
                            Ita.points,	Ire.points,	Cro.points,	Ukr.points,	Swe.points,	
                            Fra.points,	Eng.points))

# combine all kinds of other results of the simulation in a data.frame
results.data.frame <- as.data.frame(cbind(results.data.frame,totalPoints.frame, 
                                          total.gpoints, total.eliminationpoints, total.points, 
                                          outOfGroup.matrix))

# save dataframe so that I can reopen it in Latex knitr file
save(results.data.frame, file="./data/resultsDataFrame.rda")


##################################################################
# summarize the results of the simulation iterations and create a national team level results table (each national team 1 obs)

# chances making it out of the group
outOfGroup   <- round((colMeans(results.data.frame[,seq(from=100,to=115,by=1)]))*100,digits=1)

# chances of winning the EC
finalWinner  <- round((colMeans(results.data.frame[,seq(from=5,to=80,by=5)]))*100,digits=1)

# calculate some additional statistics
meanPoints   <- sapply(results.data.frame[,seq(from=81, to=96, by=1)], mean,     na.rm=TRUE)
sdPoints     <- sapply(results.data.frame[,seq(from=81, to=96, by=1)], sd,       na.rm=TRUE)
quantile     <- sapply(results.data.frame[,seq(from=81, to=96, by=1)], quantile, probs=c(0.10,0.25, 0.75,0.90), na.rm=TRUE)
medianPoints <- sapply(results.data.frame[,seq(from=81, to=96, by=1)], quantile, probs=c(0.5), na.rm=TRUE)

idealValue   <- round((medianPoints * 1.95),digits=1)
trueValue    <- medianPoints * dollars.per.point


# combine the results calculated above and combine them in a results table
temp.result.table <- rbind(idealValue, trueValue, medianPoints, quantile,outOfGroup,finalWinner,sdPoints)
result.table      <- t(temp.result.table)

# change row names of results table
rownames(result.table) <- countries

# convert results table to a data frame
result.table <- as.data.frame(result.table)

# save dataframe so that I can reopen it in Latex Sweave file
save(result.table, file="./data/resultTable.rda")


##############################################################
# create buying chart that indicates for what price each team is a good buy

off50 <- trueValue/1.5
off40 <- trueValue/1.4
off30 <- trueValue/1.3
off20 <- trueValue/1.2
off10 <- trueValue/1.1

buying.frame            <- as.data.frame(cbind(idealValue, off50, off40, off30, off20, off10, trueValue))
row.names(buying.frame) <- countries
save(buying.frame, file="./data/buyingFrame.rda")




##############################################################
# explore some key results graphically

###########################
# rank graph / histogram

graph.ranks <- results.data.frame[seq(from=2,to=80,by=5)]
par(mar=c(1,1,1,1))
par(mfrow=c(4,4))
for (i in 1:16){
  title <- countries[[i]]
hist(graph.ranks[[i]],prob=TRUE,breaks=0:4,main=title,
     xlab='Leaves')
}



#########################################
# total points per team boxplot graph

#stack points for each iteration for each country on top of each other
pointsVector<-c(results.data.frame[[81]], results.data.frame[[82]],results.data.frame[[83]], 
                results.data.frame[[84]], results.data.frame[[85]], results.data.frame[[86]],
                results.data.frame[[87]], results.data.frame[[88]], results.data.frame[[89]],
                results.data.frame[[90]], results.data.frame[[91]], results.data.frame[[92]],
                results.data.frame[[93]], results.data.frame[[94]], results.data.frame[[95]],
                results.data.frame[[96]])


# create a name vector variable with the country names in it
nameVector<-as.factor(rep(1:16, each=numOfSimIterations))
nameVector<-factor(nameVector,
                   levels=c("16","15","14","13","12","11","10","9","8","7","6","5","4","3","2","1"),
                   labels=revCountries)


#combine dataVector with the stacked country points and the name vector and save it as a data frame
graphingData <- as.data.frame(cbind(nameVector,pointsVector))
save(graphingData, file="./data/graphingData.rda")


# boxplots for each combination of two factors 
bwplot(nameVector~pointsVector,
            ylab="Countries", xlab="Points During Euro Cup", 
            main="Average Points Over 10,000 Simulations")



#######################################################################
# experimenting with pdf graphs

pdf(file="./output/testMygraphic.pdf",width=10,height=4)
density.totalPoints<-density(total.points)
plot(density.totalPoints,ylim=c(0,0.05),xlim=c(270,370))
text(2,3, "the text is CENTERED around (x,y) = (6,2) by default",     cex = 1)
dev.off()


#######################################################################
# auction team results

# combine national team final results in data matrix

finalPointsByNat<-cbind(results.data.frame[81], results.data.frame[82],results.data.frame[83], 
                        results.data.frame[84], results.data.frame[85], results.data.frame[86],
                        results.data.frame[87], results.data.frame[88], results.data.frame[89],
                        results.data.frame[90], results.data.frame[91], results.data.frame[92],
                        results.data.frame[93], results.data.frame[94], results.data.frame[95],
                        results.data.frame[96])

###############################################
## analyze data by auction teams
# calculate auction team results

team1  <- (rowSums(finalPointsByNat[,c( 2,  5)], na.rm = FALSE))
team2  <- (rowSums(finalPointsByNat[,c( 5,  6, 10)], na.rm = FALSE))
team3  <- (rowSums(finalPointsByNat[,c( 1, 13,  9)], na.rm = FALSE))
team4  <- (rowSums(finalPointsByNat[,c( 4, 15, 15)], na.rm = FALSE))
team5  <- (rowSums(finalPointsByNat[,c( 1,  7, 12)], na.rm = FALSE))
team6  <- (rowSums(finalPointsByNat[,c( 3, 10, 14)], na.rm = FALSE))
team7  <- (rowSums(finalPointsByNat[,c( 2,  9, 11)], na.rm = FALSE))
team8  <- (rowSums(finalPointsByNat[,c(3,  6,  8, 13, 16)], na.rm = FALSE))
team9  <- (rowSums(finalPointsByNat[,c(4,  7, 12)], na.rm = FALSE))
team10 <- (rowSums(finalPointsByNat[,c(8, 14, 11, 16)], na.rm = FALSE))



# combine auction team results and transpose

teamResults <- cbind(team1, team2, team3, team4, team5, team6, team7, team8, team9, team10)
teamResultsTransposed <- t(teamResults)


#vector to store team rank results

team1.rank <-vector()
team2.rank <-vector()
team3.rank <-vector()
team4.rank <-vector()
team5.rank <-vector()
team6.rank <-vector()
team7.rank <-vector()
team8.rank <-vector()
team9.rank <-vector()
team10.rank <-vector()

rank <-(1:10)
auctionTeam <-(1:10)

# rank order teams by points and random number for each iteration (random is tie-breaker)

# start loop here for each iteration
for (i in 1:numOfSimIterations){
  iterationResult <- as.data.frame(cbind(auctionTeam, teamResultsTransposed[,i],runif(10)))
  names(iterationResult) <- c("auctionTeam", "gpoints", "random")
  
  
  auctionTeamsResult <- as.data.frame(cbind(iterationResult[order(-iterationResult$gpoints,iterationResult$random),],rank))
  auctionTeamsResult
  # add points to auction teams vectors
  
  team1.rank[i] <- auctionTeamsResult$rank[auctionTeamsResult$auctionTeam==1]
  team2.rank[i] <- auctionTeamsResult$rank[auctionTeamsResult$auctionTeam==2]
  team3.rank[i] <- auctionTeamsResult$rank[auctionTeamsResult$auctionTeam==3]
  team4.rank[i] <- auctionTeamsResult$rank[auctionTeamsResult$auctionTeam==4]
  team5.rank[i] <- auctionTeamsResult$rank[auctionTeamsResult$auctionTeam==5]
  team6.rank[i] <- auctionTeamsResult$rank[auctionTeamsResult$auctionTeam==6]
  team7.rank[i] <- auctionTeamsResult$rank[auctionTeamsResult$auctionTeam==7]
  team8.rank[i] <- auctionTeamsResult$rank[auctionTeamsResult$auctionTeam==8]
  team9.rank[i] <- auctionTeamsResult$rank[auctionTeamsResult$auctionTeam==9]
  team10.rank[i] <- auctionTeamsResult$rank[auctionTeamsResult$auctionTeam==10]
}

# generate variable to record the winning team
winningTeam <- rep(99,numOfSimIterations)


teamResults <- as.data.frame(cbind(teamResults,
                                   team1.rank, team2.rank, team3.rank, team4.rank, team5.rank,
                                   team6.rank, team7.rank, team8.rank, team9.rank, team10.rank,winningTeam))

# which Euro cup auction team wins the euro cup the most often in the auction
# recode variable winning team

teamResults$winningTeam[teamResults$team1.rank==1] <- 1
teamResults$winningTeam[teamResults$team2.rank==1] <- 2
teamResults$winningTeam[teamResults$team3.rank==1] <- 3
teamResults$winningTeam[teamResults$team4.rank==1] <- 4
teamResults$winningTeam[teamResults$team5.rank==1] <- 5
teamResults$winningTeam[teamResults$team6.rank==1] <- 6
teamResults$winningTeam[teamResults$team7.rank==1] <- 7
teamResults$winningTeam[teamResults$team8.rank==1] <- 8
teamResults$winningTeam[teamResults$team9.rank==1] <- 9
teamResults$winningTeam[teamResults$team10.rank==1] <- 10

# table with the winning team
table(teamResults$winningTeam)

# bar graph

par(mfrow=c(1,1))

percentWinning <- prop.table(table(teamResults$winningTeam))*100


barplot(percentWinning,main="Likelyhood of Winning the EC Auction by Teams", 
        ylab="Predicted Probability of Winning", ylim=c(0,35),
        xlab="Teams",
        names.arg=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0"))

# save teamresults, delete all data but 

save(teamResults, file="./data/teamResults.rda")
rm(list=setdiff(ls(), "numOfSimIterations"))


load("./data/teamResults.rda")
############################################################
# post process auction team data (team results) for a "heat graph" showing which place they finish in most often
# distributional graph for rank and teams 
#http://www.r-bloggers.com/visualizing-car-brand-choices-in-ggplot2/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29

attach(teamResults)

# create a name vector variable with the country names in it
groupNumberVector <- (rep(1:10, each=numOfSimIterations))

rankStacked <- c(team1.rank, team2.rank, team3.rank, team4.rank, team5.rank, team6.rank, 
                 team7.rank, team8.rank, team9.rank, team10.rank)

detach(teamResults)

ggplotGraphingData <- as.data.frame(cbind(groupNumberVector,rankStacked))

#cross-tab team/rank
crossTab <- (table(ggplotGraphingData$groupNumberVector,ggplotGraphingData$rankStacked)/numOfSimIterations)*100

# transform the data for plotting

group <- as.factor(ggplotGraphingData$groupNumberVector)
rank  <- as.factor(ggplotGraphingData$rankStacked)
ggplotinput <- ddply(ggplotGraphingData, .(group, rank), .fun = nrow)
names(ggplotinput)  <- c("Team", "Rank", "Probability")


# convert counts to %
ggplotinput$Probability <- (ggplotinput$Probability/numOfSimIterations)*100

save(ggplotinput, file="./data/ggplotinput.rda")

# graph cross tab

ggplot() +
  geom_point(data = ggplotinput,
             aes(x = Rank, y = Team, colour = Probability),
             shape=15, size = 10) +
               scale_colour_gradient(limits=c(0,36), low="aliceblue", high="blue") +
               opts(panel.background = theme_blank(),
                    #legend.position = "none",
                    #axis.title.x = theme_blank(),
                    #axis.title.y = theme_blank(),
                    axis.text.x = theme_text(colour = "black"), 
                    axis.text.y = theme_text(colour = "black")) + ylab("Teams") + xlab("Rank of Team")



# explore histogram

par(mfrow=c(5,2))
for (i in 11:20){
  teamNumber <- i-10
  title <- paste("Team", teamNumber )
  hist(teamResults[[i]],prob=TRUE,breaks=0:10,main=title,
       xlab='Rank', ylab='Proportion')
}

par(mfrow=c(1,1))
  hist(teamResults[[20]],prob=TRUE,breaks=0:10,main="title",
       xlab='xlab')




