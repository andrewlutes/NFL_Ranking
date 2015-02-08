require(sqldf)
require(ggplot2)
require(igraph)
require(expm)
require(base)
require(randomForest)
require(pROC)
require(rgl)
require(car)
options(stringsAsFactors = FALSE)


##############Traditional SOS####################
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

##############Iterative Power Rating#############
powerRanking= function(adj, schedWeight=.5, Iterations=10){
  tadj<-ifelse(adj==0&t(adj)==0, NA, adj-t(adj))
  for(i in 1:Iterations){
    #plot(avePerf, avePerfN)
    #text(avePerf, avePerfN, label=names(avePerf))
    tadj<-ifelse(adj==0&t(adj)==0, NA, adj-t(adj))
    avePerfN<-colMeans(tadj, na.rm=TRUE)
    tadj<-tadj+schedWeight*avePerfN
  }
  return(avePerfN)
}


#################ELO Rating#####################
EloRanking=function(games, F=400){
  teams<-data.frame(Team=unique(c(as.character(games$H),as.character(games$V))))
  games$Winner=ifelse(games$home_win==1,games$H,games$V)
  games$Loser=ifelse(games$home_win==0,games$H,games$V)
  teams$Rating=1450
  games$hRating=1450
  games$vRating=1450  
  
  for(i in 1:nrow(games)){ 
    #Put the ELO from the team list into each game record for the loser
    row=games[i,]
    hRating=teams$Rating[teams$Team==row$H]
    vRating=teams$Rating[teams$Team==row$V]
    
    games[i,]$hRating=hRating
    games[i,]$vRating=vRating
    
    D=abs(hRating-vRating)
    
    if(hRating<2100 || vRating<2100){
      k=32
    }else if(wRating<2400 || lRating<2400){
      k=24
    }else{
      k=16
    }
    
    if(i%%1000==0){cat(i/1000,", ")}

    #Probability of the each team winning
    p_H=1/(1+10^((hRating-vRating)/F))
    p_V=1-p_H
    outcome=row$home_win*2-1
    
    #Update Ratings
    hRating=max(hRating+outcome*k*p_H, 1015)
    vRating=max(vRating+outcome*k*p_V, 1015)
    teams[teams$Team==row$H,]$Rating=hRating
    teams[teams$Team==row$V,]$Rating=vRating
  }
  return(games)
}


##############Grab SNA metrics##################
grTab <- function(gr) {
  data.frame(        name = vertex.attributes(gr)$name,
                     closeness = centralization.closeness(gr)$res,
                     betweeness = betweenness(gr),
                     eigenvector = centralization.evcent(gr)$vector,
                     hub = hub.score(gr)$vector,
                     auth = authority.score(gr)$vector,
                     page = page.rank(gr)$vector)
}

############Collaborative Filtering############
collaborativeFilter<-function(adj){  
  #Transform the matrix such that losses are -1, wins are 1, 0 are 1 of each, NA means did not play
  tadj<-ifelse(adj==0&t(adj)==0, NA, adj-t(adj))
  
  #Optional scaling out double victories and 
  #tadj<-(tadj/ifelse(tadj==0,1,abs(tadj))+1) /2
  
  #Find Average Performance of each team
  avePerf<-colMeans(tadj, na.rm=TRUE)
  
  #Correct for the average performance of both the winner and loser to find the 'residual' for this game
  tadj<-tadj+avePerf-matrix(rep(avePerf, each=length(avePerf)), ncol=length(avePerf))
  
  
  #Find the correlations between residuals
  corM<-cor(tadj, use="pairwise.complete.obs")
  #heatmap(corM)
  
  #Insert 0's so that we can multiply the matricies
  corM<-ifelse(is.na(corM),0,corM)
  tadj<-ifelse(is.na(tadj),0,tadj)
  d<-nrow(corM)
  #Generate Predictions
  predictions<-corM%*%tadj/colSums(abs(corM), na.rm=TRUE)-avePerf+matrix(rep(avePerf, each=d), ncol=d)
  #heatmap(predictions)
  
  return(predictions)
}

##########Exponential Win Percentage###########
expWin <- function(adj, schedDep=1){
  library(expm)
  #Exponential Adjacency matrix 
  expAdj=expm(schedDep*adj)-diag(dim(adj)[1])

  #Exponential based win pct
  expAdj=expAdj/(expAdj+t(expAdj)) #Scale so that each cell represents a unit probability
  
  #Find the exponential win and loss record for each team
  Exp=data.frame(name=row.names(adj), win=colSums(expAdj, na.rm=TRUE), loss=rowSums(expAdj, na.rm=TRUE)) 
  #Scale to find exponential win percentage
  Exp=as.numeric(Exp$win/(Exp$win+Exp$loss))
  names(Exp)<-row.names(adj)
  
  return(list(expAdj, Exp))
}



##########Optimal Explanation of Point Differences###########
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


##########Page Rank Bitches!!!###########
pageW<-function(adj){
  g<-graph.adjacency(adj)
  win<-page.rank(g)$vector
  return(win)
}

pageL<-function(adj){
  lg<-graph.adjacency(t(adj))
  lose<-page.rank(lg)$vector
  return(lose)
}


#########Attach predictions to games###########
attach_Pred_Matrix<- function(test, predMatrix, name){
  H=as.character(test$H)
  V=as.character(test$V)    
  for(j in 1:nrow(test)){
    if(H[j] %in% row.names(predMatrix) && V[j] %in% row.names(predMatrix)){
      test[j,paste("v",name,sep="")]=predMatrix[H[j], V[j]]
      test[j,paste("h",name,sep="")]=predMatrix[V[j],H[j]]
    }
  }
  test[,paste(name,"diff",sep="")]= test[,paste("h",name,sep="")]-test[,paste("v",name,sep="")]
  return(test)
}


games[games$SEAS==2005 & games$WEEK==10,]

attach_Pred_Rank<- function(test, predRanking, name){
  fill<-mean(predRanking, na.rm=TRUE)
  predRanking[is.na(predRanking)] <- fill
  
  test[,paste("h",name,sep="")]=ifelse(test$H %in% names(predRanking), predRanking[test$H], fill)
  test[,paste("v",name,sep="")]=ifelse(test$V %in% names(predRanking), predRanking[test$V], fill)
  
  test[,paste(name,"diff",sep="")]= test[,paste("h",name,sep="")]-test[,paste("v",name,sep="")]
  return(test)
}


#########Create The Adjacency Matrix#########
createAdj<-function(train, homeAdv, ptWeight){
  train$Weight=1
  
  if(ptWeight){
    train$Weight=train$Win_Margin
  }
  
  train$Weight= train$Weight-((train$home_win*2-1)*homeAdv)
  train$Weight=train$Weight*(1-timeDep)^(wk-train$WEEK)
  
  #Attach Social Network Measurements
  g1 <- graph.data.frame(data.frame(from=train$Loser, to=train$Winner, weight=train$Weight))
  adj=get.adjacency(g1, sparse=FALSE, attr="weight") 
  return(adj)
}


wk<-18
sched=1

#########Attach Exp, Collab, and Power ###############
addPred=function(games, timeDep=0, ptWeight=FALSE, homeAdv=0, sched=1, startWeek=7){
  options(stringsAsFactors=FALSE)
  games$Winner=ifelse(games$home_win==1,games$H,games$V)
  games$Loser=ifelse(games$home_win==0,games$H,games$V)
  #Add winners and Losers for each game
  SEAShistory=NULL
  for(S in sort(unique(games$SEAS))){
    season=games[games$SEAS==S,]
    cat("\n",S,", ")
    
    #Split Each week to creat power rankings and page rank based on the prior week
    for(wk in sort(unique(season$WEEK))[-(1:startWeek)]){
      cat(wk,", ")
      #Split apart training and test data
      train=season[season$WEEK<wk,]
      test=season[season$WEEK==wk,]  
      
      #Create the Adjacency Matrix with appropriate games weights
      adj<-createAdj(train, homeAdv, ptWeight)
      
      #Create each measure
      #sos<-SOS(adj)
      exp<-expWin(adj, schedDep=sched)
      pow<-powerRanking(adj, schedWeight=sched*.5, Iterations=10)
      colab<-collaborativeFilter(adj)
      opt<-optimize(train, pvar=10, h=homeAdv)
      pWin<-pageW(adj)
      pLoss<-pageL(adj)
        
      #Attach Measures to games
      #test<-attach_Pred_Rank(test,sos[[1]], "winp")
      #test<-attach_Pred_Rank(test,sos[[2]], "sos")
      test<-attach_Pred_Matrix(test,exp[[1]], "adj")
      test<-attach_Pred_Rank(test,exp[[2]], "exp")
      test<-attach_Pred_Rank(test,pow, "pow")
      test<-attach_Pred_Matrix(test,colab,"colab")
      test<-attach_Pred_Rank(test,opt,"opt")
      test<-attach_Pred_Rank(test,pWin,"pageW")
      test<-attach_Pred_Rank(test,pLoss,"pageL")
      
      #Append this season
      SEAShistory=rbind(SEAShistory, test)
    }
  }
  return(SEAShistory)
}




