library(sqldf)
options(stringsAsFactors=FALSE)



# This is a simple demo of using a metropolis algorithm to get an MCMC approximation
# for the mean of a multivariate normal
metro_mu2 = function(games, k=1e2, propose=NULL, propsd=1, pvar=1){
  
  #Starting Point for estimating
  team=data.frame(name=unique(c(games$H,games$V)))
  team=sqldf("Select name, avg(case when name=g.H then g.PTD else -g.PTD end) Mu 
             from team join games g on g.H=name or g.V=name group by name")
  tNames<-team$name
  team<-team$Mu
  #team<-rep(0,32)
  ptDiffs<-games$PTD
  h<-mean(ptDiffs)
  n = nrow(games); m = length(team)
  samples = matrix(0, k, m) 
  samples[1,] = team
  colnames(samples) = tNames
  h<-mean(ptDiffs)
  
  if(is.null(propose)){
    propose=function(last){ #random walk proposal
      rnorm(m,last,propsd) 
    }
  }  
   
  U = runif(k)
  allR<-rep(0,k)
  accept=1
  i=2
  for(i in 2:k){ 
    theta   = samples[i-1,]
    theta_p = propose(theta)  
    names(theta)=tNames
    names(theta_p)=tNames
    
    lGame=attach_Pred_Rank(games,theta,"Mu")$Mudiff
    Game_p=attach_Pred_Rank(games,theta_p,"Mu")$Mudiff
    
    #cbind(lGame, Game_p, ptDiffs)
    
    R=exp(sum(dnorm(ptDiffs, Game_p-h, sd=pvar, log=TRUE))  - sum(dnorm(ptDiffs, lGame-h, sd=pvar, log=TRUE)))
    allR[i]<-R 
    #Choose whether to accept the new theta
    if(U[i]<R){
      result = theta_p
      accept=accept+1 
    }else { 
    result=theta
    }
    samples[i,] = result    
  }
  
  list(samples=samples, accept=accept/k, R=allR)
}



###Read in real games
games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Games.csv")
games=games[games$WEEK<17 & games$SEAS==2013,]
games$PTD=games$PTSH-games$PTSV

###Create Fake Games
games<-createPerfectSeasons(1)
games<-createRandomSeasons(1,  home_adv=0,  variance=.5)

#Attach Strength Measures
games$PTD=games$hStrength-games$vStrength
team<-data.frame(name=unique(games$H), mu=unique(games$hStrength))
team=sqldf("Select name, avg(case when name=g.H then g.PTD else -g.PTD end) Mu_, mu 
             from team join games g on g.H=name or g.V=name group by name")
tNames<-team$name
#team<-team$mu
#names(team)<-tNames
#text(team$mu,team$Mu_, label=tNames)


###Run Metro Mu
test = metro_mu2(games, k=1e4, propsd=2.15, pvar=sd(games$PTD))

###Results and diagnostics
plot(1:1e3,test$R)

test$accept

#Normalize Results
test$samples<-normalize(test$samples)
team[,2]<-normalize(team[,2])
team[,3]<-normalize(team[,3])

plot(normalize(colMeans(test$samples)), normalize(colMeans(test$samples))-normalize(team[,3]), 
     pch=19, xlab="Estimate", ylab="Error")

plotAll()
plotTeam(2)

cbind(colMeans(test$samples), apply(test$samples, 2, sd))


par(mfrow=c(1,1))
####Create Diagnostic plots
plotAll<-function(){
  plot(normalize(colMeans(test$samples)), normalize(team[,2]), pch=19, xlab="Estimate", ylab="Actual")
  points(normalize(team[,2]), normalize(team[,3]), pch=16, col=rgb(0,0,1,.3), cex=.8)
}

plotTeam<-function(x, k=1000){
  par(mfrow=c(2,1))
  plot(test$samples[1:k,x], type='l')
  abline(h=team[x,2:3], col=c("grey","black"))
  plot(density(test$samples[,x]))
  abline(v=team[x, 2:3], col=c("grey","black"))
  par(mfrow=c(1,1))
}

###Normalize
normalize=function(x){
  x<-(x-min(x))/(max(x)-min(x))
  return(x)
}


