library(sqldf)
games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Games.csv")

game<-sqldf("Select H Team, home_win win, SEAS, WEEK, hwinP winP, vwinP owinP from games UNION
              Select V Team, 1-home_win win, SEAS, WEEK,vwinP winP, hwinP owinP from games")

addlag=function(x){
  lab<-paste("lag",x,sep="")
  lag<-sqldf(paste("select g2.win
                from game g1
                left join game g2
                on g1.Team=g2.Team and g1.SEAS=g2.SEAS
                and g2.WEEK=g1.WEEK-",x,sep=""))
  return(cbind(game,lag))
}
library(sqldf)
x<-data.frame(x=seq(0,1,1/10))
df<-sqldf("Select a.x r, b.x g, c.x b from x a join x b join x c")
r<-rep(seq(0,1,.1),10)


for(x in 1:10){
  game<-addlag(x)
}
colnames(game)[7:16]<-paste("lag",1:10,sep="")


full.gl<-glm(win~winP+owinP+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10, data=game)
base.gl<-glm(win~winP+owinP, data=game)
part.gl<-glm(win~winP+owinP+lag1, data=game)



summary(full.gl)
summary(part.gl)

game[,"lag2"]<-0
addlag(2)

game[game$Team=="ARI" & game$SEAS==2000,]


cor(game$win, game$lag1, use="complete.obs")
tab<-table(game$win, game$lag1)


summary(glm(game$win~game$lag1))
prop.test(c(tab[1,1], tab[1,2]), c(sum(tab[,1]),sum(tab[,2])))


library(igraph)
games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Games.csv")
nfcw<-c("ARI","SEA","SF","STL")
games<-games[games$SEAS==2013 & games$WEEK<18 & games$H %in% nfcw & games$V %in% nfcw,]
g1<-graph.data.frame(data.frame(from=games$Loser, to=games$Winner, weight=games$Win_Margin))
tkplot(g1)

g1<-graph.data.frame(data.frame(from=games$Loser, to=train$Winner))
tkplot(g1)

setwd("C:\\Users\\andrew.lutes\\Desktop\\Project\\Football")
write.csv(data.frame(from=games$Loser, to=games$Winner, weight=games$Win_Margin), "sample.csv")






games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Games.csv")

predF=function(adj){expWin(adj, schedDep=0)}


addPred(games, predF, timeDep=0.05, ptWeight=TRUE, homeAdv=0.1)




