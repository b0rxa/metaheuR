## ----message=FALSE-------------------------------------------------------
library("metaheuR")
library("igraph")
set.seed(1)
num.nodes <- 25
rnd.graph <- random.graph.game(n=num.nodes, p.or.m=0.15)

## ----message=FALSE-------------------------------------------------------
gcp <- graphColoringProblem (graph=rnd.graph)
names(gcp)

## ----message=FALSE-------------------------------------------------------
solution <- factor(rep("C1", num.nodes), levels=paste("C", 1:num.nodes, sep=""))
solution
gcp$valid(solution)
gcp$evaluate(solution)
gcp$plot(solution, node.size=20, label.cex=1)

## ----message=FALSE-------------------------------------------------------
corrected.solution <- gcp$correct(solution)
corrected.solution
gcp$valid(corrected.solution)
gcp$evaluate(corrected.solution)
gcp$plot(corrected.solution, node.size=20, label.cex=1)

## ----message=FALSE-------------------------------------------------------
trivial.solution <- factor(paste("C", 1:num.nodes, sep=""),
                           levels=paste("C", 1:num.nodes, sep=""))
trivial.solution
gcp$valid(trivial.solution)
gcp$evaluate(trivial.solution)

## ----message=FALSE,fig.width=10, fig.height=5----------------------------
getEvaluation(bls)
getSolution(bls)
plotProgress(bls) + labs(y="Number of colors", 
                         x="Number of solutions evaluated")
gcp$plot(getSolution(bls), node.size=20, label.cex=1)

