options(stringsAsFactors=FALSE)
games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Ranking/NFL_Games.csv")
games=games[games$WEEK<18 & games$SEAS==2013,]
ptDiffs=games$PTSH-games$PTSV
games$PTD<-ptDiffs

require(sqldf)


optimize(games, pvar=10, h=2)

optimize<-function(games, pvar, h){
  games$PTD=games$PTSH-games$PTSV
  
  #Starting point for team strength based on point differences
  team=data.frame(name=unique(c(games$H,games$V)))
  team=sqldf("Select name, avg(case when name=g.H then g.PTD else -g.PTD end) Mu 
               from team join games g on g.H=name or g.V=name group by name")
  
  
  x<-team$Mu
  names(x)<-team$name
    
  likelihood<-function(x){
    Game_p=attach_Pred_Rank(games,x,"Mu")$Mudiff
    R=sum(dnorm(games$PTD, Game_p-h, sd=pvar, log=TRUE))
    return(R)
  }
   
  o<-optim(par=x, likelihood)
  return(o$par)
}


plot(x, o$par, pch=16)
text(x, o$par, label=names(x))

