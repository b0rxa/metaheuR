## ----,message=FALSE------------------------------------------------------
library("metaheuR")
library("igraph")
set.seed(1)
num.nodes <- 25
rnd.graph <- random.graph.game(n = num.nodes , p.or.m = 0.15)

## ----,message=FALSE------------------------------------------------------
gcp <- graph.coloring.problem (graph = rnd.graph)
names(gcp)

## ----,message=FALSE------------------------------------------------------
solution <- factor(rep("C1",num.nodes) , levels=paste("C",1:num.nodes,sep=""))
solution
gcp$is.valid(solution)
gcp$evaluate(solution)
gcp$plot(solution , node.size=10 , label.cex = 1)

## ----,message=FALSE------------------------------------------------------
corrected.solution <- gcp$correct(solution)
corrected.solution
gcp$is.valid(corrected.solution)
gcp$evaluate(corrected.solution)
gcp$plot(corrected.solution , node.size=10 , label.cex = 1)

## ----,message=FALSE------------------------------------------------------
trivial.solution <- factor(paste("C",1:num.nodes,sep="") , 
                   levels=paste("C",1:num.nodes,sep=""))
trivial.solution
gcp$is.valid(trivial.solution)
gcp$evaluate(trivial.solution)

## ----,message=FALSE,cache=TRUE-------------------------------------------
hamm.ngh <- hammingNeighborhood(base = trivial.solution)
bls <- basic.local.search(evaluate = gcp$evaluate , 
                           non.valid = 'discard' ,
                           valid = gcp$is.valid ,
                           initial.solution = trivial.solution , 
                           neighborhood = hamm.ngh , 
                           selector = greedy.selector)

## ----,message=FALSE,fig.width=10, fig.height=5---------------------------
evaluation(bls)
optima(bls)[[1]]
plot.progress(bls) + labs(y="Number of colors" , x="Number of solutions evaluated")

