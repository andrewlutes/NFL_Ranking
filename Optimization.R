options(stringsAsFactors=FALSE)
games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Games.csv")
games=games[games$WEEK<18 & games$SEAS==2013,]
ptDiffs=games$PTSH-games$PTSV

require(sqldf)
team=data.frame(name=unique(c(games$H,games$V)))
team=sqldf("Select name, avg(case when name=g.H then g.PTD else -g.PTD end) Mu 
             from team join games g on g.H=name or g.V=name group by name")


pvar<- 10
h<- 2
x<-team$Mu
names(x)<-team$name
  
likelihood<-function(x){
  Game_p=attach_Pred_Rank(games,x,"Mu")$Mudiff
  R=sum(dnorm(ptDiffs, Game_p-h, sd=pvar, log=TRUE))
  return(R)
}


o<-optim(par=x, likelihood)
plot(x, o$par, pch=16)
text(x, o$par, label=names(x))

