library(igraph)
library(shiny)
library(pROC)
library(sqldf)

createPerfectSeasons=function(numSeasons){
  #Create a shell schedule based on NFL 2013 season
  games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Ranking/NFL_Games.csv")
  games=games[games$WEEK<18 & games$SEAS==2013,]
  games=games[c("GID", "SEAS", "WEEK", "V", "H")]
  history=NULL
  for(season in 1:numSeasons){
    #Assign strengths to each of the teams and add to games
    teams=data.frame(name=unique(games$H), Strength=rnorm(32,0,1))
    games$hStrength=teams$Strength[match(games$H,teams$name)]
    games$vStrength=teams$Strength[match(games$V,teams$name)]
    games$home_win=ifelse(games$hStrength>games$vStrength, 1, 0) #Assign Winners Based on Strength
    games$ptDiff=games$hStrength-games$vStrength
    games$SEAS=season
    history=rbind(history,games)
  }
  history$Winner=ifelse(history$home_win==1,history$H,history$V)
  history$Loser=ifelse(history$home_win==0,history$H,history$V)
  return(history)
}



createRandomSeasons=function(numSeasons,  home_adv=0,  variance=1){
  games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Ranking/NFL_Games.csv")
  games=games[games$WEEK<18 & games$SEAS==2013,]
  games=games[c("GID", "SEAS", "WEEK", "V", "H")]
  history=NULL
  len<-nrow(games)
  for(season in 1:numSeasons){
    #Assign strengths to each of the teams and add to games
    teams=data.frame(name=unique(games$H), Strength=rnorm(32,0,1))
    games$hStrength=teams$Strength[match(games$H,teams$name)]
    games$vStrength=teams$Strength[match(games$V,teams$name)]
    games$hRand=rnorm(len,games$hStrength, rep(variance,len))
    games$vRand=rnorm(len,games$vStrength, rep(variance,len))
    games$home_win=ifelse(games$hRand+home_adv>games$vRand, 1, 0)   #Create winners adding an elemnt of randomness and homefield advantage
    games$ptDiff=games$hRand-games$vRand
    games$Win_Margin=abs(games$ptDiff)
    games$SEAS=season
    history=rbind(history,games)
  }
  history$Winner=ifelse(history$home_win==1,history$H,history$V)
  history$Loser=ifelse(history$home_win==0,history$H,history$V)
  return(history) 
}
  

createClassSeasons=function(numSeasons=5,  home_adv=0,  variance=1, ClassEffect=1){
  games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Games.csv")
  games=games[games$WEEK<18 & games$SEAS==2013,]
  games=games[c("GID", "SEAS", "WEEK", "V", "H")]
  classMatrix=rbind(cbind(1,1,0), cbind(1,2,ClassEffect), cbind(1,3,-ClassEffect), cbind(2,1,-ClassEffect), cbind(2,2,0), 
                    cbind(2,3,ClassEffect), cbind(3,1,ClassEffect), cbind(3,2,-ClassEffect), cbind(3,3,0))
  colnames(classMatrix)=c("H","V","ClassEffect")
  classMatrix=data.frame(classMatrix)
  history=NULL
  for(season in 1:numSeasons){
    #Assign strengths to each of the teams and add to games
    teams=data.frame(name=unique(games$H), Strength=rnorm(32,0,1))
    games$hStrength=teams$Strength[match(games$H,teams$name)]
    games$vStrength=teams$Strength[match(games$V,teams$name)]
    
    #Create alternate winners adding 3 classes with a rock-paper-scissors effect
    teams$class=sample(c(1,2,3), 32, replace=TRUE)
    games$hclass=teams$class[match(games$H,teams$name)]
    games$vclass=teams$class[match(games$V,teams$name)]
    
    class_effect=sqldf("Select ClassEffect from classMatrix join games 
                             on classMatrix.H=games.hclass and classMatrix.V=games.vclass order by GID")
    games$class_effect=unlist(class_effect)    
    games$hRand=rnorm(32,games$hStrength, rep(variance,32))
    games$vRand=rnorm(32,games$vStrength, rep(variance,32))
    #Create winners adding in the class effect
    games$home_win=ifelse(games$hRand+home_adv+games$class_effect>games$vRand, 1, 0)   
    games$SEAS=season
    games$Winner=ifelse(games$home_win==1,games$H,games$V)
    games$Loser=ifelse(games$home_win==0,games$H,games$V)
    history=rbind(history,games)
  }
  return(history) 
}

games<-createPerfectSeasons(1)
games<-createRandomSeasons(10)
games<-createClassSeasons(1, 0, .1, 3)


games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Games.csv")
games$Winner=ifelse(games$home_win==1,games$H,games$V)
games$Loser=ifelse(games$home_win==0,games$H,games$V)
ranking<-addMeasures(games, GLM=FALSE)

summary(ranking)
ranking$cdiff=ranking$hColab-ranking$vColab

colab=roc(formula=home_win~cdiff, data=ranking)
plot.roc(colab)

winp=roc(formula=home_win~windiff, data=ranking)
plot.roc(winp, add=TRUE, col="red")

par(mfrow=c(1,2))
plotWeeks(ranking)
plotAllROCs(ranking)
par(mfrow=c(1,1))


plotAllROCs=function(history){
  #full.roc=roc(formula=home_win~fullPred, data=history)
  #interact.roc=roc(formula=home_win~interactPred, data=history)
  power.roc=roc(formula=home_win~powdiff, data=history)
  elo.roc=roc(formula=home_win~elodiff, data=history)
  winp.roc=roc(formula=home_win~windiff, data=history)
  adj.roc=roc(formula=home_win~adjdiff, data=history)
  exp.roc=roc(formula=home_win~expdiff, data=history)
  #dom.roc=roc(formula=home_win~domdiff, data=history)
  
  plot.roc(winp.roc)
  #plot.roc(full.roc, add=TRUE, col="grey")
  plot.roc(power.roc, add=TRUE, col="red")
  plot.roc(elo.roc, add=TRUE, col="yellow")
  plot.roc(adj.roc, add=TRUE, col="orange")
  #plot.roc(dom.roc, add=TRUE, col="light blue")
  plot.roc(exp.roc, add=TRUE, col="blue")
}

plotWeeks=function(ranking){
  #Correctness Measures for each
  ranking$eloRight=ifelse(ifelse(ranking$hElo>=ranking$vElo,1,0) == (ranking$home_win), 1,0)
  ranking$powRight=ifelse(ifelse(ranking$hPower>=ranking$vPower,1,0) == (ranking$home_win), 1,0)
  ranking$winpRight=ifelse(ifelse(ranking$hWinP>=ranking$vWinP,1,0) == (ranking$home_win), 1,0)
  ranking$domRight=ifelse(ifelse(ranking$domdiff>=0,1,0) == (ranking$home_win), 1,0)
  ranking$expRight=ifelse(ifelse(ranking$expdiff>=0,1,0) == (ranking$home_win), 1,0)
  ranking$adjRight=ifelse(ifelse(ranking$adjdiff>=0,1,0) == (ranking$home_win), 1,0)
  d=sqldf("Select WEEK, avg(eloRight) elo, avg(powRight) pow, avg(winpRight) win, avg(domRight) dom, avg(expRight) exp, avg(adjRight) adj
          from ranking group by WEEK")
  plot(d$WEEK, d$win, type="l", ylim=c(0.3,1), lwd=2)
  lines(d$WEEK, d$pow, col="red", lwd=2)
  lines(d$WEEK, d$elo, col="yellow", lwd=2)  
  lines(d$WEEK, d$dom, col="light blue", lwd=2)
  lines(d$WEEK, d$adj, col="orange", lwd=2)
  lines(d$WEEK, d$exp, col="blue", lwd=2) 
  abline(h=.5, lty=5)
}



plotFit=function(predictor, response){
  model=glm(predictor~response, family="binomial")
  plot(predictor, model$fitted, type="l")
}

