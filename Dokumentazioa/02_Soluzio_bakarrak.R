


## ----GC_1 , prompt=TRUE, echo=-1 , message=FALSE-------------------------
set.seed(1)
library(metaheuR)
n <- 10
rnd.value <- runif(n) * 100
rnd.weight <- rnd.value + runif(n) * 50
max.weight <- sum(sample(rnd.weight , size = n/2 , replace = FALSE))
knp <- knapsack.problem(weight = rnd.weight , value = rnd.value , limit = max.weight)
rnd.sol <- runif(n) < 1/3


## ----GC_2 , prompt=TRUE--------------------------------------------------
rnd.sol <- knp$correct(rnd.sol)
which(rnd.sol)
flip.ngh <- flipNeighborhood(base = rnd.sol , random = FALSE)
while(has.more.neighbors(flip.ngh)){
  ngh <- next.neighbor(flip.ngh)
  isvalid <- ifelse (knp$is.valid(ngh) , "bideragarria" , "bideraezina")
  mssg <- paste("Inguruneko soluzio ",isvalid,": " , 
                paste(which(ngh),collapse = ",") , sep="")
  cat (mssg , "\n")
}


## ----Neighborhoods , prompt=TRUE-----------------------------------------
n <- 10
rnd.sol <- random.permutation(length = n)

swp.ngh <- swapNeighborhood(base = rnd.sol)
exchange.count <- 0
swap.count <- 0
while(has.more.neighbors (swp.ngh)){
  swap.count <- swap.count + 1
  next.neighbor(swp.ngh)
}

ex.ngh <- exchangeNeighborhood(base = rnd.sol)
exchange.count <- 0
while(has.more.neighbors (ex.ngh)){
  exchange.count <- exchange.count + 1
  next.neighbor(ex.ngh)
}

swap.count
exchange.count


## ----GC_1, prompt=TRUE , echo=-1-----------------------------------------
set.seed(1)
n <- 25
rnd.graph <- random.graph.game(n = n , p.or.m = 0.25)
gcp <- graph.coloring.problem (graph = rnd.graph)


## ----GC_2, prompt=TRUE, cache=TRUE , fig.path='./Irudiak/' , fig.keep='all' , fig.show='hide' , fig.width=5 , fig.height=5----
colors <- paste("C",1:n,sep="")
initial.solution <- factor (colors, levels = colors)
h.ngh <- hammingNeighborhood(base = initial.solution)


## ----GC_3, prompt=TRUE---------------------------------------------------
resources <- cresource(time = 10 , evaluations = 100*n^2 , iterations = 100*n)


## ----GC_4, prompt=TRUE , cache=TRUE , fig.path='./Irudiak/' , fig.keep='all' , fig.show='hide' , fig.width=6 , fig.height=6----
bls <- basic.local.search(evaluate = gcp$evaluate , valid = gcp$is.valid , 
                          correct = gcp$correct, initial.solution = initial.solution , 
                          neighborhood = h.ngh , selector = first.improvement.selector , 
                          non.valid = 'correct' , resources = resources)

bls
final.solution <- optima(bls)[[1]]
as.character(unique(final.solution))
plot.gpc.solution <- gcp$plot
plot.gpc.solution(solution = final.solution , node.size = 15 , label.cex = 0.8)


## ----GC_5, prompt=TRUE , cache=TRUE , fig.path='./Irudiak/' , fig.keep='all' , fig.show='hide' , fig.width=10 , fig.height=5----
plot.progress(bls , size=1.1 ) + labs(y="Evaluation")


## ----Order_1, prompt=TRUE , echo=-1 , message=FALSE , tidy=TRUE----------
set.seed(1)
url <- system.file("bays29.xml.zip" , package = "metaheuR")
cost.matrix <- tsplib.parser(url)
n <- dim(cost.matrix)[1]
tsp.babaria <- tsp.problem(cost.matrix)
csol <- identity.permutation(n)
eval <- tsp.babaria$evaluate(csol)


## ----Order_2, prompt=TRUE------------------------------------------------
ex.nonrandom <- exchangeNeighborhood(base = csol , random = TRUE)
ex.random <- exchangeNeighborhood(base = csol , random = FALSE)


## ----Order_3, prompt=TRUE , cache=TRUE-----------------------------------
first.improvement.selector(neighborhood = ex.nonrandom , evaluate = tsp.babaria$evaluate , 
                           initial.solution = csol , initial.evaluation = eval)$evaluation
first.improvement.selector(neighborhood = ex.random , evaluate = tsp.babaria$evaluate , 
                           initial.solution = csol , initial.evaluation = eval)$evaluation


## ----Order_4, prompt=TRUE,cache=TRUE-------------------------------------
greedy.selector(neighborhood = ex.nonrandom , evaluate = tsp.babaria$evaluate ,
                initial.solution = csol , initial.evaluation = eval)$evaluation
greedy.selector(neighborhood = ex.random , evaluate = tsp.babaria$evaluate , 
                initial.solution = csol , initial.evaluation = eval)$evaluation


## ----Babaria_1, prompt=TRUE,cache=TRUE,echo=-1---------------------------
set.seed(1)
rnd.sol <- random.permutation(n)
greedy.sol <- tsp.greedy(cmatrix = cost.matrix)
tsp.babaria$evaluate(rnd.sol)
tsp.babaria$evaluate(greedy.sol)


## ----Babaria_2, prompt=TRUE,cache=TRUE-----------------------------------
eval <- tsp.babaria$evaluate
swp.ngh.rnd <- swapNeighborhood (base = rnd.sol)
swp.ngh.greedy <- swapNeighborhood (base = greedy.sol)
swap.greedy.rnd.sol <- basic.local.search(evaluate = eval , initial.solution = rnd.sol , 
                                          neighborhood = swp.ngh.rnd , 
                                          selector = greedy.selector , verbose = FALSE)

swap.fi.rnd.sol <- basic.local.search(evaluate = eval , initial.solution = rnd.sol ,
                                      neighborhood = swp.ngh.rnd , verbose = FALSE ,
                                      selector = first.improvement.selector)

swap.greedy.greedy.sol <- basic.local.search(evaluate = eval , verbose = FALSE , 
                                             initial.solution = greedy.sol ,
                                             neighborhood = swp.ngh.greedy , 
                                             selector = greedy.selector)

swap.fi.greedy.sol <- basic.local.search(evaluate = eval , initial.solution = greedy.sol ,
                                         neighborhood = swp.ngh.greedy , verbose = FALSE , 
                                         selector = first.improvement.selector )


## ----Babaria_3, prompt=TRUE,cache=TRUE-----------------------------------
tsp.babaria$evaluate(rnd.sol) - evaluation(swap.greedy.rnd.sol)
tsp.babaria$evaluate(rnd.sol) - evaluation(swap.fi.rnd.sol)


## ----Babaria_4, prompt=TRUE,cache=TRUE-----------------------------------
consumed.evaluations(resources(swap.greedy.rnd.sol))
consumed.evaluations(resources(swap.fi.rnd.sol))


## ----Babaria_5, prompt=TRUE,cache=TRUE-----------------------------------
tsp.babaria$evaluate(greedy.sol) - evaluation(swap.greedy.greedy.sol)


## ----Babaria_6, prompt=TRUE,cache=TRUE-----------------------------------
ex.ngh.rnd <- exchangeNeighborhood (base = rnd.sol)
ex.greedy.rnd.sol <- basic.local.search(evaluate = eval , initial.solution = rnd.sol ,
                                        neighborhood = ex.ngh.rnd , 
                                        selector = greedy.selector , verbose = FALSE)

tsp.babaria$evaluate(rnd.sol) - evaluation(ex.greedy.rnd.sol)
consumed.evaluations(resources(ex.greedy.rnd.sol))


## ----Burma_1, prompt=TRUE,cache=TRUE-------------------------------------
f <- paste("http://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95"
         ,"/XML-TSPLIB/instances/burma14.xml.zip",sep="")
burma.mat <- tsplib.parser(f)

n <- ncol(burma.mat)
burma.tsp <- tsp.problem(burma.mat)

init.sol <- random.permutation(n)
ngh <- exchangeNeighborhood(init.sol)
sel <- first.improvement.selector
th.accpet <- threshold.accept
th <- 0


## ----Burma_2, echo=-1 , prompt=TRUE,cache=TRUE , results='hide', fig.path='./Irudiak/' , fig.keep='all' , fig.show='hide' , fig.width=14 , fig.height=6----
set.seed(1)
perturb.2opt <- function(solution , ratio , ...)
  shuffle(permutation = solution , ratio = ratio)

r <- 5
ratio <- 0.01
ils.1 <- iterated.local.search(evaluate = burma.tsp$evaluate , 
                               initial.solution = init.sol ,
                               neighborhood = ngh , selector = sel , 
                               perturb = perturb.2opt ,  ratio = ratio ,
                               accept = th.accpet , th = th , 
                               num.restarts = r)
ratio <- 0.25
ils.25 <- iterated.local.search(evaluate = burma.tsp$evaluate , 
                               initial.solution = init.sol ,
                               neighborhood = ngh , selector = sel , 
                               perturb = perturb.2opt ,  ratio = ratio ,
                               accept = th.accpet , th = th , 
                               num.restarts = r)
ratio <- 1
ils.100 <- iterated.local.search(evaluate = burma.tsp$evaluate , 
                               initial.solution = init.sol ,
                               neighborhood = ngh , selector = sel , 
                               perturb = perturb.2opt ,  ratio = ratio ,
                               accept = th.accpet , th = th , 
                               num.restarts = r)

plot.progress(result = list(ILS1 = ils.1 , ILS33 = ils.25 , ILS100 = ils.100)) + 
  facet_grid(. ~ Group , scales = 'free_x') + labs(y="Evaluation")


## ----Knap_GRASP, prompt = TRUE , echo = FALSE----------------------------
knap.GRASP <- function (weight , value , limit , cl.size = 0.25){
  size <- length(weight)
  ratio <- value / weight
  solution <- rep(FALSE , size)
  finished = FALSE
  while (!finished){
    non.selected <- which(!solution)
    cl.n <- round(length(non.selected)*cl.size)
    cl <- sort(ratio[non.selected] , decreasing=TRUE)[1:cl.n]
    selected <- sample(cl,1)    
    aux <- solution
    aux[ratio == selected] <- TRUE
    if (sum(weight[aux])<limit){
      solution <- aux
    }else{
      finished <- TRUE
    }
  }
  solution
}










## ----GRASP_vs_rndstart_1, echo=-1 , prompt=TRUE , cache=TRUE-------------
set.seed(5)
n <- 50
values  <- c(runif(n/2) * 10,  runif(n/2)*25)
weights <- values * rnorm(n,1,0.05)
limit   <- sum(sample(weights , n/5))


## ----GRASP_vs_rndstart_2, echo=-1 , prompt=TRUE , cache=TRUE-------------
args <- list()
knp.problem             <- knapsack.problem(weights , values , limit)

args$evaluate           <- knp.problem$evaluate
args$valid              <- knp.problem$is.valid
args$correct            <- knp.problem$correct
args$non.valid          <- 'discard'

args$neighborhood       <- flipNeighborhood(base = rep(FALSE , n))
args$selector           <- greedy.selector

args$generate.solution  <- knap.GRASP
args$num.restarts       <- 25
args$weight             <- weights
args$value              <- values
args$limit              <- limit


## ----GRASP_vs_rndstart_3, echo=-1 , prompt=TRUE , cache=TRUE , results='hide'----
args$cl.size <- 1
rnd.ls <- do.call (multistart.local.search , args)

args$cl.size <- 0.50
GRASP.50 <- do.call (multistart.local.search , args)

args$cl.size <- 0.1
GRASP.10 <- do.call (multistart.local.search , args)


## ----GRASP_vs_rndstart_4, echo=FALSE , prompt=TRUE , cache=TRUE , fig.path='./Irudiak/' , fig.keep='all' , fig.show='hide' , fig.width=8 , fig.height=4----
plot.progress(list(GRASP50 = GRASP.50 , GRASP10 = GRASP.10 , RND = rnd.ls) ) + labs(y="Current solution") + scale_color_discrete("Strategy")
plot.progress(list(GRASP50 = GRASP.50 , GRASP10 = GRASP.10 , RND = rnd.ls) , y = 'best') + labs(y="Best solution") + scale_color_discrete("Strategy")


