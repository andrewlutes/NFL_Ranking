library(pROC)


x<-rep(0,10)
y<-rep(0,10)
x[1]<-.1
x[2]<-.9
for(i in 1:10){
  if(i>2){
    x[i]<-x[i-1]+y[i-1]*(x[i-1]-x[i-2])/(y[i-1]-y[i-2])
  }
  cat("\n",i,", ", x[i],"   ")
  y[i]<-f(x[i])
}



f=function(x){
  history<-addPred(timeDep=0.01, ptWeight=FALSE, homeAdv=.1, sched=x)
  exp.roc=roc(formula=home_win~expdiff, data=history)
  p<-plot.roc(exp.roc)
  auc<-p$auc[1]
  return(auc)
}

history<-addPred()

exp.roc=roc(formula=home_win~expdiff, data=history)
win.roc=roc(formula=home_win~winp, data=history)
adj.roc=roc(formula=home_win~adjdiff, data=history)

plot.roc(win.roc, add=TRUE, col="red")
plot.roc(adj.roc, add=TRUE, col="blue")

names(p)
