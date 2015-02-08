games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Games.csv")
games=games[games$WEEK<18 & games$SEAS==2013,]

sos<-SOS(games)
sos[[2]]

SOS<-function(games){
  teams<-sqldf("Select distinct H Team from games")
  teams<- sqldf("Select Team, sum(case when Team=games.H then home_win else (1-home_win) end)/cast(count(*) as double) Winp
                     from teams join games on teams.Team=games.H or teams.Team=games.V
                     group by Team")
  teams<- sqldf("Select t.Team, max(t.WinP) WinP, avg(Op.winp) OpR
                    from teams t 
                    join games on t.Team=games.H or t.Team=games.V
                    join teams Op on t.Team<>Op.Team and (Op.Team=games.H or Op.Team=games.V)
                     group by t.Team")
  
  teams<- sqldf("Select t.Team, max(t.WinP) WinP, max(t.OpR) OpR, avg(Op.OpR) OOpR
                    from teams t 
                    join games on t.Team=games.H or t.Team=games.V
                    join teams Op on t.Team<>Op.Team and (Op.Team=games.H or Op.Team=games.V)
                     group by t.Team")
  teams$SOS<-(teams$WinP+2*teams$OpR+teams$OOpR)/3
  
  SOS<-teams$SOS; winp<-teams$WinP
  names(SOS)<-teams$Team; names(winp)<-teams$Team
  return(list(winp,SOS))
}

SOS<-function(adj){
  degree<-apply(adj,1,function(x){sum(!x==0)})+apply(adj,2,function(x){sum(!x==0)})
  
  adj2<- adj %*% adj
  
  teams<-sqldf("Select distinct H Team from games")
  teams<- sqldf("Select Team, sum(case when Team=games.H then home_win else (1-home_win) end)/cast(count(*) as double) Winp
                     from teams join games on teams.Team=games.H or teams.Team=games.V
                     group by Team")
  teams<- sqldf("Select t.Team, max(t.WinP) WinP, avg(Op.winp) OpR
                    from teams t 
                    join games on t.Team=games.H or t.Team=games.V
                    join teams Op on t.Team<>Op.Team and (Op.Team=games.H or Op.Team=games.V)
                     group by t.Team")
  
  teams<- sqldf("Select t.Team, max(t.WinP) WinP, max(t.OpR) OpR, avg(Op.OpR) OOpR
                    from teams t 
                    join games on t.Team=games.H or t.Team=games.V
                    join teams Op on t.Team<>Op.Team and (Op.Team=games.H or Op.Team=games.V)
                     group by t.Team")
  teams$SOS<-(2*teams$OpR+teams$OOpR)/3
  
  SOS<-teams$SOS; winp<-teams$WinP
  names(SOS)<-teams$Team; names(winp)<-teams$Team
  return(list(winp,SOS))
}
             
plot(teams$WinP, teams$SOS, pch=16, col=rgb(1, 0, 0, .5))
abline(lm(winP~SOS_game, data=teams), col="blue") # regression line (y~x) 
lines(lowess(teams$winP,teams$SOS_game), col="black") # lowess line (x,y)