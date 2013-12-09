# this program simulates the outcomes of the euro cup 2012 and applies the point system of the 
# euro cup game Xtian came up with. The goal of this simulation is to determine the optimal
# price of each team, give the number of points the team will likely accumulate. This should
# allow us to determine for what price we should buy a team. Since there is a fixed amount of
# money in the pool, the assumption is that some teams pay more for a team than the team is worth.
# Therefore, some teams will be sold under value and we hope to identify these teams and buy them.

library(Hmisc) # used for increment function inc(x)

rm(list = ls())

setwd("~/GoogleDrive/WORKSPACE/travaille/dataScience/1205_euroCup")

##############################################################################################
# SETUP FOR SIMULATION STARTS HERE #
##############################################################################################

# number of iterations for simulation
numOfSimIterations <- 10000

# setup points for making it out of the group stage
advancing.points<-c(3,2,0,0) # winner receives 3 points runner-up 2

#  vector for countries and name of vector elements to store results
countries<-c("Pol", "Gre", "Rus", "Cze", "Net", "Den", "Ger","Por", "Esp", "Ita", "Ire", "Cro","Ukr", "Swe", "Fra", "Eng")
result.types = c("gpoints", "rank", "ecpoints", "eliminationpoints","finalWinner") #  gpoints are points earned in the group stage, ecpoints euro cup points earned in elimination stage
  
# create empty lists to store country specific results 
results = list()
for (country in countries) {
    results[[country]] <- list()
    for (resType in result.types) {
        results[[country]][[resType]] <- vector() 
    }
}

#specify vectors with group assignment
groupAcountries <-c("Pol", "Gre", "Rus", "Cze")
groupBcountries <-c("Net", "Den", "Ger", "Por")
groupCcountries <-c("Esp", "Ita", "Ire", "Cro")
groupDcountries <-c("Ukr", "Swe", "Fra", "Eng")

# read in group games
groupA <-read.table('./data/groupA.csv', header=T, sep=",")
groupB <-read.table('./data/groupB.csv', header=T, sep=",")
groupC <-read.table('./data/groupC.csv', header=T, sep=",")
groupD <-read.table('./data/groupD.csv', header=T, sep=",")

# combine groups to a tournament dataset
gamesDatasets<-c("groupA","groupB","groupC","groupD")

# read group result table (empty)
resultsGroupA<-read.table('./data/resultsGroupA.csv', header=T, sep=",")
resultsGroupB<-read.table('./data/resultsGroupB.csv', header=T, sep=",")
resultsGroupC<-read.table('./data/resultsGroupC.csv', header=T, sep=",")
resultsGroupD<-read.table('./data/resultsGroupD.csv', header=T, sep=",")

# nested list for datasets

groupAlist<-list(groupA, resultsGroupA,groupAcountries)
groupBlist<-list(groupB, resultsGroupB,groupBcountries)
groupClist<-list(groupC, resultsGroupC,groupCcountries)
groupDlist<-list(groupD, resultsGroupD,groupDcountries)

datasetLists<-list(groupAlist, groupBlist, groupClist, groupDlist)



#  list to store the result tables after the group stage
groupFinalResultTables.List<-list()

# strength table that will be used to determine the outcome of the elimination stage based on current odds ratios 
# (for next version: read directly real time tables from betting sites)

countryStrength<-list()
countryStrength[["Rus"]]<-3.2
countryStrength[["Cze"]]<-2
countryStrength[["Gre"]]<-1.4
countryStrength[["Pol"]]<-2.2


countryStrength[["Ger"]]<-6
countryStrength[["Net"]]<-5.7
countryStrength[["Por"]]<-3.2
countryStrength[["Den"]]<-1.0


countryStrength[["Esp"]]<-6.5
countryStrength[["Ita"]]<-4
countryStrength[["Cro"]]<-2.2
countryStrength[["Ire"]]<-1.2

countryStrength[["Fra"]]<-4.2
countryStrength[["Eng"]]<-5
countryStrength[["Ukr"]]<-2.4
countryStrength[["Swe"]]<-1.5

# temp iteration variables to test loop "by hand"
it<-1
group<-1

##############################################################################################
# SIMULATION STARTS HERE #
##############################################################################################

# loop for one realization of the tournament
for(it in 1:numOfSimIterations){
    
    # loop through 4 groups in group stage
    for (group in 1:4) {
        
        # setup dataframes that makes it easier to simulate group outcome
        gamesDataset   <-as.data.frame(datasetLists[[group]][1])
        resultsTable   <-as.data.frame(datasetLists[[group]][2])
        groupCountries <-unlist(datasetLists[[group]][3])
                
        # makes it easier to access variables in group games file
        
        groupMatchFile<-gamesDataset
        
        # setup dataset to create cutoff 1 and 2 between win home/draw/win away
        # create inverse and probability of win, draw and loss in each group match
        
        invOddsH       <-1/groupMatchFile$OddsH
        invOddsD       <-1/groupMatchFile$OddsD
        invOddsA       <-1/groupMatchFile$OddsA
        totalInvOdds   <-invOddsH+invOddsD+invOddsA
        ppH            <-invOddsH/totalInvOdds
        ppD            <-invOddsD/totalInvOdds
        ppA            <-invOddsA/totalInvOdds
        Group          <- groupMatchFile$Group
        Game           <- groupMatchFile$Game
        Home           <- groupMatchFile$Home
        Away           <- groupMatchFile$Away
        
        groupMatchFile2<-data.frame(Group,Game,Home,Away,ppH,ppD,ppA)
        groupMatchFile2
        
        # cleaning up
        rm(groupMatchFile)
        
                
        # makes it easier to access variables in group results table
        results.frame<-resultsTable
        
        
        # counter will loop through all 6 games in the respective group
        
        i<-1  # choose one iteration "manually" for testing purposes        
        for(i in 1:6){
            
            homeTeam <-as.vector(groupMatchFile2[["Home"]][i])
            awayTeam <-as.vector(groupMatchFile2[["Away"]][i])
                        
            # set cutoff points to separate win home, draw and win away for specific match i
            cutoff1<-groupMatchFile2[["ppH"]][i]
            cutoff2<-1-groupMatchFile2[["ppA"]][i]
          
            # outcome of the game as a uniform random draw over probability distribution
            # based on the cutoff scores, this determines the outcome of a match and
            # the number of points that get assigned to each team
            randomResult <- runif(1)
            
            # add points for  to the table
            # condition 1: home team wins
            if (randomResult<cutoff1){
                inc(results.frame[["wins"]][results.frame[["teams"]] == homeTeam]) <- 1     # home team wins
            # condition 2: away team wins
            }else if (randomResult>cutoff2) {
                inc(results.frame[["wins"]][results.frame[["teams"]] == awayTeam]) <- 1     # away team wins
            # condition 3: draw
            }else{
                inc(results.frame[["draws"]][results.frame[["teams"]] == homeTeam]) <- 1    # draw
                inc(results.frame[["draws"]][results.frame[["teams"]] == awayTeam]) <- 1
            }                   
        results.frame            
            
        } # end loop of the 6 group games
        
        
        # calculate point vectors for game points and euro cup points
        
        ec.points   <- results.frame[["wins"]] * 3 + results.frame[["draws"]] * 1
        game.points <- results.frame[["wins"]] * results.frame[["win.factor"]] + results.frame[["draws"]] * results.frame[["draw.factor"]]
        random.rank <- runif(4) # used as a tie-breaker when ranks are tied (-> given match outcome, goals are treated as random error)
        
        # create final group ranking: combine point vectors to sort by random rank
        # the random rank sorting decides which team makes it out of the group if they have the same # of ec points
        tableGroup<-results.frame$tableGroup
        teams     <-results.frame$teams
        toSort<-data.frame(tableGroup,teams,ec.points,game.points,random.rank)
        toSort
        sorted<-toSort[order(-ec.points,random.rank),]
        sorted
        
        # in our euro cup game, a team gets additional game.points for making it out of the group stage
        # add the points for making it out of the group stage (rank 1 and 2) here 
        sorted$game.points<-sorted$game.points + advancing.points
        rank<-1:4
        final.stage<-c(1,1,0,0)
        finalresults.frame<-data.frame(sorted,rank, final.stage)
        finalresults.frame <- subset(finalresults.frame, select = - c(random.rank)) # drop Variable random.rank
        finalresults.frame

        
        # save final result frame for elimination stage
        groupFinalResultTables.List[[group]]<-finalresults.frame 
        
        # save results in result list
        for (country in groupCountries) {
            results[[country]][["gpoints"]]  [it] <- finalresults.frame[["game.points"]][finalresults.frame[["teams"]] == country] # assign appropriate value here
            results[[country]][["rank"]]     [it] <- finalresults.frame[["rank"]]       [finalresults.frame[["teams"]] == country] # assign appropriate value here
            results[[country]][["ecpoints"]] [it] <- finalresults.frame[["ec.points"]]  [finalresults.frame[["teams"]] == country] # assign appropriate value here
            results[[country]][["eliminationpoints"]] [it] <- 0 # this is 0 for now, will be replaced during elination stage
            results[[country]][["finalWinner"]] [it] <- 0 # this is 0 for now, will be replaced during elination stage
        }
               

        
    }
    
       
    # cleaning up
        # rm(finalresults.frame)
        # rm(results.frame)
        # rm(toSort, sorted) 
        # rm(ec.points,final.stage,game.points,rank)
        # rm(cutoff1,cutoff2,invOddsA,invOddsD,invOddsH,totalInvOdds,ppA,ppD,ppH)
    
       
    
    #################################################################################
    # elimination stage
     
    groupFinalResultTables.List
    
    results[["Pol"]][["gpoints"]]
    results[["Pol"]][["ecpoints"]]
    results[["Pol"]][["rank"]]

    
    
    
    ##############################################################
    # quarter finals [need to program this more elegantly]
    
    semiFinalists<-vector() #setup vector to hold the semi finalists
    
    
    #pick teams playing the quarter finals (numbering of the quarter finals by date)
    
    q_homeTeams<-vector()
    q_homeTeams[1]<-as.vector(groupFinalResultTables.List[[1]][["teams"]][[1]]) # qfinal 1, home team
    q_homeTeams[2]<-as.vector(groupFinalResultTables.List[[2]][["teams"]][[1]]) # qfinal 2
    q_homeTeams[3]<-as.vector(groupFinalResultTables.List[[3]][["teams"]][[1]]) # ...
    q_homeTeams[4]<-as.vector(groupFinalResultTables.List[[4]][["teams"]][[1]])
    
    q_awayTeams<-vector()
    q_awayTeams[1]<-as.vector(groupFinalResultTables.List[[2]][["teams"]][[2]])
    q_awayTeams[2]<-as.vector(groupFinalResultTables.List[[1]][["teams"]][[2]])
    q_awayTeams[3]<-as.vector(groupFinalResultTables.List[[4]][["teams"]][[2]])
    q_awayTeams[4]<-as.vector(groupFinalResultTables.List[[3]][["teams"]][[2]])
    
    
    # loop through 4 quarterfinals
    
    for (qfinal in 1:4){
        
        home <- q_homeTeams[qfinal]
        away <- q_awayTeams[qfinal]
        
        home.strength <- countryStrength[[home]]
        away.strength <- countryStrength[[away]]
        
        # determine the cutoff between home team and away team winning, home advantage is based on the assumption that the group winner
        # has some type of momentum and is playing a stronger tournament than the opponent        
        cutoff <- 1.5*home.strength/(1.5*home.strength+away.strength)

        # outcome of the game        
        randomResult <- runif(1)
        
        # assign points depending on outcome of the game          
        if (randomResult<cutoff){
            results[[home]][["eliminationpoints"]] [it] <- 3     #home team wins
            semiFinalists[qfinal]                       <- home
        }else{
            results[[away]][["eliminationpoints"]] [it] <- 3
            semiFinalists[qfinal]                       <- away
        }
        
    } # quarter final loop ends here
    
    semiFinalists
    
    #####################################################################################
    #####################################################################################
    #semi final
    
    finalists<-vector() # holds the country names of the finalists
    
    s_homeTeams<-vector()
    s_homeTeams[1]<-semiFinalists[1]
    s_homeTeams[2]<-semiFinalists[2]
    
    s_awayTeams<-vector()
    s_awayTeams[1]<-semiFinalists[3]
    s_awayTeams[2]<-semiFinalists[4]
    

    # semi-final loop    
    for (sfinal in 1:2){
       
        home <-s_homeTeams[sfinal]
        away <-s_awayTeams[sfinal]

        home.strength <-countryStrength[[home]]
        away.strength <-countryStrength[[away]]
        
        cutoff <-home.strength/(home.strength+away.strength)
        
        #outcome of the game
        
        randomResult <- runif(1)
        
        if (randomResult<cutoff){
            results[[home]][["eliminationpoints"]] [it] <- results[[home]][["eliminationpoints"]] [it]+ 3     #home team wins
            finalists[sfinal]                           <-home
        }else{
            results[[away]][["eliminationpoints"]] [it] <- results[[away]][["eliminationpoints"]] [it] + 3
            finalists[sfinal]                           <-away
        }        
    }

    finalists
    
    #####################################################################################
    #####################################################################################
    #final

    home <- finalists[1]
    away <- finalists[2]
    
    home.strength <-countryStrength[[home]]
    away.strength <-countryStrength[[away]]

    cutoff <-home.strength/(home.strength+away.strength)

    # outcome of the game    
    randomResult <- runif(1)

    
    # assign points for outcome of the semi final
    
    if (randomResult<cutoff){
        results[[home]][["eliminationpoints"]] [it] <- results[[home]][["eliminationpoints"]] [it] + 9     #home team wins
        results[[home]][["finalWinner"]]       [it] <- 1
    }else{
        results[[away]][["eliminationpoints"]] [it] <- results[[away]][["eliminationpoints"]] [it] + 9
        results[[home]][["finalWinner"]]       [it] <- 1
    }
    
    ##################################### final end here
    
    
} #end iteration loop here

############################################


# check out some results
results[["Ger"]][["eliminationpoints"]][1:10]
results[["Ger"]][["finalWinner"]][1:10]
results[["Ger"]][["gpoints"]][1:10]
results[["Ger"]][["ecpoints"]][1:10]
results[["Ger"]][["rank"]][1:10]




#remove all but list results
rm(list=setdiff(ls(), "results"))
save(results,file="./data/results.rda")


