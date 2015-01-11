#Attach Social Network Measurements
g1 <- graph.data.frame(games[, c('Loser', 'Winner')])
page= data.frame(name = vertex.attributes(g1)$name,
                 page = page.rank(g1, damping=.95)$vector)

plot(density(page.rank(g1, damping=.99)$vector))
plot(g1)

teams$hPage=page[match(test$H,page$name),]$page
teams$vPage=page[match(test$V,page$name),]$page

summary(test)

#Examine Adjacency matrix between each
adjacency=get.adjacency(g1, sparse=FALSE)
adj=adjacency #%*% adjacency
adj2=adj %*% adjacency
adj3=adj2 %*% adjacency

#Metrics for parity in the system
sum(adj)
sum(diag(adj2))/sum(adj2)
sum(diag(adj3))/sum(adj3)

sum(adj*t(adj)>0)
sum(adj>1)

library(igraph)
games<- read.csv("C:/Users/andrew.lutes/Desktop/Project/Football/NFL_Games.csv")
games=games[games$WEEK<18 & games$SEAS==2004 & 
              games$H %in% c("SEA","SF", "ARI", "STL") & games$V %in% c("SEA","SF", "ARI", "STL"),]
#Attach Social Network Measurements
g1 <- graph.data.frame(games[, c('Loser', 'Winner')])
page= data.frame(name = vertex.attributes(g1)$name,
                 page = page.rank(g1, damping=.95)$vector)

tkplot(g1)


#Examine Adjacency matrix between each
adjacency=get.adjacency(g1, sparse=FALSE)
adj=adjacency #%*% adjacency
adj2=adj %*% adjacency
adj3=adj2 %*% adjacency

#Metrics for parity in the system
sum(adj)
sum(adj2)
sum(adj3)