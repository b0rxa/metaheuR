## ----,message=FALSE------------------------------------------------------
library("metaheuR")
n <- 50
rnd.matrix <- matrix(runif(n^2) , ncol = n)
rnd.tsp <- tsp.problem(cmatrix = rnd.matrix)
names(rnd.tsp)

## ------------------------------------------------------------------------
rnd.solution <- random.permutation(length = n)
rnd.tsp$evaluate(rnd.solution)

## ------------------------------------------------------------------------

cost.matrix <- tsplib.parser(file = "http://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/XML-TSPLIB/instances/brazil58.xml.zip")
tsp.brazil <- tsp.problem(cost.matrix)

## ------------------------------------------------------------------------
n <- dim(cost.matrix)[1]
csol <- identity.permutation(n)
tsp.brazil$evaluate(csol)

## ------------------------------------------------------------------------
n <- 10
weight <- runif(n)
value <- weight + 0.2*runif(n)
capacity <- sum(sample(weight , size = 0.25*n , replace = FALSE))
ksp <- knapsack.problem(weight = weight , value = value , limit = capacity)
names(ksp)

## ------------------------------------------------------------------------
all.in.solution <- rep(TRUE , n)
ksp$is.valid(all.in.solution)
corrected.solution <- ksp$correct(all.in.solution)
ksp$is.valid(corrected.solution)

## ------------------------------------------------------------------------
set.seed(1)
library(igraph)
n <- 100
rnd.graph <- random.graph.game (n = n , p.or.m = 0.05)
misp <- mis.problem(graph = rnd.graph , penalization = 0)
misp.penalized <- mis.problem(graph = rnd.graph , penalization = 0.5)

## ------------------------------------------------------------------------
rnd.solution <- runif(n) > 0.4
misp$is.valid(rnd.solution)
misp$evaluate(rnd.solution)
misp.penalized$evaluate(rnd.solution)

## ------------------------------------------------------------------------
misp$evaluate

## ------------------------------------------------------------------------
valid.solution <- misp$correct (rnd.solution)
misp$evaluate(valid.solution)
misp.penalized$evaluate(valid.solution)

## ------------------------------------------------------------------------
misp$plot(rnd.solution , node.size = 5)
misp$plot(valid.solution)

