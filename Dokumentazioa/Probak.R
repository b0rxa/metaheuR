library(metaheuR)


## Examples of TSP. Size n, random cost matrix
n<-50
cost<-matrix(runif(n^2),n)

## Objective function
tsp.rnd.50<-tsp.problem(cost)
## Neighborhood
swp.ngh<-swapNeighborhood(base = identity.permutation(n),random = FALSE)

## Basic local search with no limits. Starting from identity permutation and greedy selector
bls.result<-basic.local.search(evaluate = tsp.rnd.50$evaluate , initial.solution = identity.permutation(n),
                               neighborhood = swp.ngh , selector = greedy.selector)
  
## Visualization of the results
bls.result
plot.progress(bls.result)


## Generator of random permutations of size n (for the multistart approach)
rnd.permutation.generator<-function(n){
  function() random.permutation(n)
}

## Multistart from random permutations, limited resources and first improvement selector
restarts <- 10
resources <- cresource(time = 60 , evaluations = 10 * n^2 , iterations = 10 * n) 
msls<-multistart.local.search(evaluate = tsp.rnd.50$evaluate , verbose = TRUE , do.log = TRUE , generate.solution = rnd.permutation.generator(n),
                                       num.restarts = restarts , neighborhood = swp.ngh , selector = first.improvement.selector, resources = resources)

msls
## Additional parameters can be passed to the plot, including the x axis and geom_line parameters such as col, size or linetype
plot.progress(msls , vs = 'time' , col="blue" , size=1.1)


## Perturbation function generator, for the ILS
perturb.func.generator<-function(ratio){
  function(solution) shuffle(permutation = solution, ratio = ratio)
}

## ILS limited by the number of evaluations and time
restarts <- 10
resources <- cresource(evaluations = 10 * n^2 , time = 5)
perturbation.ratio <- 0.1
## Example of execution with no feedback
ils<-iterated.local.search(evaluate = tsp.rnd.50$evaluate , verbose = T , do.log = T , initial.solution = identity.permutation(n) , 
                           accept = threshold.accept , perturb = perturb.func.generator(perturbation.ratio) , num.restarts = restarts , 
                           neighborhood = swp.ngh , selector = greedy.selector, resources = resources , KK = 0)


plot.progress(ils , col="blue" , size=1.1)


## Example knapsack problem with random weights, values and capacity limit

n<-100
w<-runif(n)
v<-runif(n)
l<-sum(w[runif(n)>0.5])

## Define the evaluation function, the validity checker and the correction function
knp <- knapsack.problem(w,v,l)
bngh<- binaryNeighborhood(rep(T,n) , T)

start.sol <- knp$correct(runif(n)>0.5)

## Basic local search discarding the non-valid solutions
knp.bls <- basic.local.search(evaluate = knp$evaluate , initial.solution =  start.sol, neighborhood = bngh , selector = greedy.selector , non.valid = 'discard' , valid = knp$is.valid)
plot.progress(knp.bls)

## Perturbation function for the ILS algorithm
binary.perturb.generator<-function (ratio){
  function(solution){
    n<-length(solution)
    id<-sample(1:n,size = ratio*n,replace = F)
    solution[id]<-!solution[id]
    solution
  }
}

ratio<-1/3
perturb<-binary.perturb.generator(ratio)
resources <- cresource(time = 30 , evaluations = 2*n^2 , iterations = 2*n)
initial.solution <- knp$correct(runif(n)>0.5)

## ILS correcting non valid solution and using a first improvement approach and limited resources
knp.ils <- iterated.local.search(evaluate = knp$evaluate , initial.solution = initial.solution , 
                                 neighborhood = bngh , selector = first.improvement.selector , 
                                 perturb = perturb  , non.valid = 'correct' , correct = knp$correct, 
                                 resources = resources)

plot.progress(knp.ils , size=1.1) 




# Graph coloring ----------------------------------------------------------

n <- 30
rnd.graph <- random.graph.game(n , p.or.m = 0.5)
rnd.sol <- factor (paste("c",sample(1:5 , size = n , replace = T) , sep="") , 
                   levels = paste("c" , 1:n , sep=""))
gcol.problem <- graph.coloring.problem (rnd.graph)
gcol.problem$is.valid(rnd.sol)
corrected.sol <- gcol.problem$correct(rnd.sol)
gcol.problem$is.valid(corrected.sol)
gcol.problem$plot(rnd.sol,node.size=10)
gcol.problem$plot(corrected.sol)
