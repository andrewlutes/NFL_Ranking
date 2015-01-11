library(igraph)


teams<-data.frame(unique(cbind(games$H,games$hclass)))
teams[order(teams$X2),]

games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Games.csv")
games$Winner=ifelse(games$home_win==1,games$H,games$V)
games$Loser=ifelse(games$home_win==0,games$H,games$V)
games=games[games$SEAS==2013,]



games<-createClassSeasons(1, 0, .1, 3)
train=games[games$WEEK<17,]
test=games[games$WEEK==17,]

collaborativeFilter(adj)


g1 <- graph.data.frame(data.frame(from=train$Loser, to=train$Winner))
adj=get.adjacency(g1, sparse=FALSE) 

powerRanking(adj, schedWeight=.5, Iterations=10)





attachPredictions<-function(test, predictionMatrix){
  test$hColab=0
  test$vColab=0
  
  #Insert results into each game
  for(i in 1:nrow(test)){   
    row=test[i,]
    
    if(row$H %in% c(train$H,train$V) && row$V %in% c(train$H,train$V)){
      test$hColab[i]=  predictionMatrix[row$H,row$V]
      test$vColab[i]=  predictionMatrix[row$V,row$H]
    }else{
      test$hColab[i]=0
      test$vColab[i]=0
    }
  }
  return(test)
}

           
