library(dplyr)
library(igraph)
library(expm)
library(scales)
library(sqldf)

#Create one of these data sets to work with
games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Ranking/College_Games.csv")
games$WEEK<-games$WEEK-34
games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Ranking/NFL_Games.csv")
games<-games %>% filter(SEAS == 2013) 


#################          Method 1         ####################################
#
#   Involves measuring the distribution of win percentages at the end of a season
#   a league with perfectly transitive wins will have a uniform distribution of
#   records while a random league will have a binomial distribution.
#
#################################################################################
recordDist<-function(games){
  teams<-sqldf("Select distinct H Team from games")
  teams<- sqldf("Select Team, 
              sum(case when Team=games.H then home_win else (1-home_win) end)/cast(count(*) as double) Winp
                     from teams join games on teams.Team=games.H or teams.Team=games.V
                     group by Team")
  return(teams)
}

gamesp<-createPerfectSeasons(1)
gamesr<-createRandomSeasons(1,  home_adv=0,  variance=3)

plot(density(recordDist(gamesr)$Winp))

for(i in 1:20){
  gamesp<-createPerfectSeasons(1)
  lines(density(recordDist(gamesp)$Winp), col="blue")
}


for(i in 1:20){
  gamesr<-createRandomSeasons(1,  home_adv=0,  variance=1)
  lines(density(recordDist(gamesr)$Winp), col="red")
}

for(i in 2001:2013){
  seas<-games %>% filter(SEAS == i) 
  lines(density(recordDist(seas)$Winp), col="black", lty=2)
}

br<-seq(0,1,.1)
h<-hist(recordDist(gamesp)$Winp, breaks=br)

x<-recordDist(gamesp)$Winp
shapiro.test(x)
binom.test(x=x*16,n=(1-x)*16)


n<-16
q<-qqplot(x, rbinom(10000,n,.5)/n, ylim=c(0,1), type="l")
bi<-sum((q$x-q$y)^2)
q<-qqplot(x ,runif(10000), ylim=c(0,1), type="l")
un<-sum((q$x-q$y)^2)



#################          Method 2         ####################################
#
#   Involves measuring the decay of paths of increasing length. The Idea is that
#   a league with perfectly transitive wins will have fewer paths of high length
#   than a random league.
#
#################################################################################

#Gives you the number of paths of lengths 1:n for adjacency matrix A
pow2<-function(n, A, A1){
  dir<-rep(0,length(n))
  undir<-rep(0,length(n))
  for (i in 1:length(n)){
    dir[i]<-sum(A %^% n[i])
    undir[i]<-sum(A1 %^% n[i])
  }
  return(cbind(dir,undir))
}

#Create the season and the relevant matricies
gamesp<-createPerfectSeasons(1)
perfSeas<-plotGrowth(gamesp)
lines(perfSeas$scale*perfSeas$dir/perfSeas$undir, type="l")

for(i in 1:50){
  gamesr<-createRandomSeasons(1,  home_adv=0,  variance=3)
  randomSeas<-plotGrowth(gamesr)
  lines(randomSeas$scale*randomSeas$dir/randomSeas$undir, col=alpha("blue",.2))
}

for(i in 2001:2013){
  games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Ranking/NFL_Games.csv")
  seas<-games %>% filter(SEAS == i) 
  Seas<-plotGrowth(seas)
  lines(Seas$scale*Seas$dir/Seas$undir, col="red")
}

plotGrowth=function(games){
  g <- graph.data.frame(data.frame(from=games$Loser, to=games$Winner), directed=TRUE)
  g1 <- graph.data.frame(data.frame(from=games$Loser, to=games$Winner), directed=FALSE)
  A=get.adjacency(g, sparse=FALSE) 
  A1=get.adjacency(g1, sparse=FALSE) 
  ####Examine relative growth fuctions
  x<-1:20
  df<-data.frame(x, pow2(x, A, A1), scale=2^x)
  #lines(df$scale*df$dir/df$undir, type="l")
  return(df)
}
