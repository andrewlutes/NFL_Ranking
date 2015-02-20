library(dplyr)
library(igraph)
library(expm)
library(sqldf)

############Running the Algorithms#################
#Create one of these data sets to work with
games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Ranking/College_Games.csv")
games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Ranking/NFL_Games.csv")
games=createRandomSeasons(2,  home_adv=0,  variance=.01)

#Add necessaries
games$ptDiff=games$PTSH-games$PTSV
games$WEEK<-games$WEEK-34
games$winpdiff<-games$hWinP-games$vWinP

###Add the measures
games=addPred(games, timeDep=0, ptWeight=TRUE, homeAdv=2, sched=1, startWeek=8)

#For SPM
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y, use="complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

names(games)

pairs(games[,c("ptDiff","winpdiff","optdiff","adjdiff","expdiff","powdiff","pageWdiff", "pageLdiff")], pch=16, 
      upper.panel=panel.cor,
      col=rgb(games$home_win,1-games$home_win,0,.5))


######Are these models any better than raw win%? Yes.#########
modelp<-lm(ptDiff~expdiff+powdiff, data=games)
full.modelp<-lm(ptDiff~expdiff+powdiff+winpdiff+optdiff+adjdiff+pageWdiff+pageLdiff, data=games)
null.modelp<-lm(ptDiff~winpdiff, data=games)
step<-stepAIC(full.model)
summary(step)

hist()

model<-glm(home_win~expdiff+powdiff, data=games)
null.model<-glm(home_win~winpdiff, data=games)
full.model<-glm(home_win~expdiff+powdiff+winpdiff+optdiff+adjdiff+pageWdiff+pageLdiff, data=games)

summary(null.model)


install.packages("DAAG")
library(DAAG)
cv<-cv.lm(df=games, modelp, m=10)
cv<-cv.lm(df=games, null.modelp, m=10)
cv<-cv.lm(df=games, full.modelp, m=10)


cv<-CVbinary(model, nfolds=10) 
cv.null<-CVbinary(null.model, nfolds=10)
cv.full<-CVbinary(full.model, nfolds=10)

names(cv)
cv$cvhat

######Which variables actually matter?########
library(Boruta)
library(elasticnet)
names(games)

train<-games[!is.na(games$adj),]
net<-enet(x=as.matrix(train[,c("expdiff","powdiff","winpdiff","optdiff","adjdiff","pageWdiff","pageLdiff")]), 
        y=train$ptDiff, lambda=0)
plot(net)
summary(net)


bor<-Boruta(x=as.matrix(train[,c(17,18,20:46)]), 
       y=train$ptDiff)
names(bor)


#Maybe this makes more of a difference for the playoffs
playoff<-games[games$WEEK>17,]
nrow(playoff)

model<-lm(ptDiff~expdiff+powdiff, data=playoff)
null.model<-lm(ptDiff~winpdiff, data=playoff)

model<-glm(home_win~expdiff+powdiff, data=playoff)
null.model<-glm(home_win~winpdiff, data=playoff)
summary(model)
summary(null.model)


#############Individual ROC Curves################
####Examine ROC Curves
exp=roc(formula=home_win~expdiff, data=games)
sos=roc(formula=home_win~sosdiff, data=games)
win=roc(formula=home_win~winpdiff, data=games)
opt=roc(formula=home_win~optdiff, data=games)
adj=roc(formula=home_win~adjdiff, data=games)
pow=roc(formula=home_win~powdiff, data=games)
colab=roc(formula=home_win~colabdiff, data=games)
page=roc(formula=home_win~(pageWdiff-pageLdiff), data=games)
##Plot ROC Curves
plot.roc(win)
plot.roc(opt, col="grey", add=TRUE)
plot.roc(exp, add=TRUE, col="blue")
plot.roc(adj, add=TRUE, col="red")
plot.roc(colab, add=TRUE, col="purple")
plot.roc(pow, add=TRUE, col="green")
plot.roc(page, add=TRUE, col="pink")







##########Play with Principal Components#########
library(scales)
library(rgl)
load("College_Ranking.RData")

predMat<-games[!is.na(predMat$vadj),]
predMat.pc<-princomp(predMat[,c("hWinP","vWinP","vadj","hadj","hexp","vexp","hpow","vpow",
                                "hopt","vopt","hpageW","vpageW","hpageL","vpageL")], 
                     cor=TRUE)

predMat.pc<-princomp(predMat[,c("winpdiff","adjdiff","expdiff","powdiff",
                                "optdiff","pageWdiff","pageLdiff")], 
                     cor=TRUE)

names(predMat)
names(predMat.pc)
predMat.pc$loadings
predMat.pc$scores
plot(predMat.pc$scores, pch=16, col=alpha(rainbow(2)[predMat$home_win+1],.2), ylim=c(-2,1))


scores<-cbind(predMat.pc$scores,predMat$home_win)
scores<-scores[scores[,2]<.5 & scores[,2]>-.5,]
scores<-scores[scores[,3]<2 & scores[,3]>-2,]
plot3d(scores, pch=16, col=rainbow(2)[scores[,8]+1], size=5)
summary(scores)

biplot(predMat.pc)
loadings(predMat.pc)





#######Test out differences##########
games=games[games$WEEK<18 & games$SEAS==2013,]
h<-NULL
adj=createAdj(games, .2, FALSE)
exp<-expWin(adj, schedDep=1)[[2]]
h<-cbind(h,exp)
train<-attach_Pred_Matrix(test,exp[[1]], "adj")
test<-attach_Pred_Rank(test,exp[[2]], "exp")    
games=games[games$WEEK<8 & games$SEAS==2000,]
adj<-createAdj(games, .1, FALSE)
collaborativeFilter(adj)
pow<-powerRanking(adj, schedWeight=.5, Iterations=10)
h<-mean((games$home_win*2-1)*games$Win_Margin)
