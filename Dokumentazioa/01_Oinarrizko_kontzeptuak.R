


## ----TSP_1 , prompt=TRUE , message=FALSE---------------------------------
library("metaheuR")
cost.matrix <- matrix(c(0 , 20.4 , 40.9 , 28.4 , 70.7 ,
                        20.4 , 0 , 26.9 , 36.7 , 54 , 
                        40.9 , 26.9 , 0 , 23.5 , 41.4 , 
                        28.4 , 36.7 , 23.5 , 0 , 51.8 ,
                        70.7 , 54 , 41.4 , 51.8 , 0) , nrow=5)
city.names <- c("Donostia","Zarautz","Azpeitia","Tolosa","Arrasate")
colnames(cost.matrix) <- city.names
rownames(cost.matrix) <- city.names
cost.matrix
tsp.example <- tsp.problem(cmatrix = cost.matrix)


## ----TSP_2 , prompt=TRUE-------------------------------------------------
solution <- permutation(c(5,2,4,3,1))
tsp.example$evaluate (solution)


## ----Graph_coloring_1 , prompt=TRUE--------------------------------------
library("igraph")
set.seed(1623)
n <- 15
rnd.graph <- random.graph.game(n , p.or.m = 0.2)
gcol.problem <- graph.coloring.problem (rnd.graph)


## ----Graph_coloring_2 , prompt=TRUE , fig.path="./Irudiak/" , fig.keep='all' , fig.show='hide'----
rnd.sol <- factor (paste("c",sample(1:3 , size = n , replace = T) , sep="") , 
                   levels = paste("c" , 1:n , sep=""))
rnd.sol
gcol.problem$is.valid(rnd.sol)
gcol.problem$plot(rnd.sol , node.size = 15 , label.cex = 1.5)


## ----Graph_coloring_3 , prompt=TRUE , fig.path="./Irudiak/" , fig.keep='all' , fig.show='hide'----
trivial.sol <- factor (paste("c" , 1:n , sep="") , 
                   levels = paste("c" , 1:n , sep=""))
trivial.sol
gcol.problem$is.valid(trivial.sol)
gcol.problem$plot(trivial.sol , node.size = 15 , label.cex = 1.5)


## ----TSP_greedy , prompt=TRUE , echo=FALSE-------------------------------
tsp.constructive <- function (cmatrix){
  diag(cmatrix) <- NA
  best.pair <- which(cmatrix == min(cmatrix,na.rm=T), arr.ind=TRUE)
  solution <- c(best.pair[1,1],best.pair[1,2])
  cmatrix[best.pair[1,1],]<-NA
  cmatrix[ , best.pair[1,]]<-NA
  for (i in 3:nrow(cmatrix)){
    next.city <- which.min (cmatrix[solution[i-1],])
    solution <- append(solution , next.city)
    cmatrix[solution[i-1],] <- NA
    cmatrix[ , next.city] <- NA
  }
  names(solution)<-NULL
  permutation(vector = solution)
}










## ----TSP_greedy_2 , prompt=TRUE------------------------------------------
greedy.solution <- tsp.constructive(cost.matrix)
tsp.example$evaluate(greedy.solution)
colnames(cost.matrix)[as.numeric(greedy.solution)]


## ----Knapsack_problem_1, prompt=TRUE-------------------------------------
is.valid <- function(solution, weight, limit){
    sum(weight[solution]) <= limit
}


## ----Knapsack_problem_2, prompt=TRUE-------------------------------------
correct <- function(solution , weight , value , limit){
  wv_ratio<-weight/value
  while(!is.valid(solution , weight , limit)){
    max.in <- max(wv_ratio[solution])
    id <- which(wv_ratio==max.in & solution)[1]
    solution[id] <- FALSE
  }
  solution
}


## ----Knapsack_problem_3, prompt=TRUE-------------------------------------
P <- c(2 , 6 , 3 , 6 , 3)
W <- c(1 , 3 , 1 , 10 , 2)
c_m <- 5
solution <- rep(TRUE, times = 5)
is.valid(solution = solution , weight = W , limit = c_m)
W/P


## ----Knapsack_problem_4, prompt=TRUE-------------------------------------
solution
is.valid(solution = solution , weight = W , limit = c_m)
corrected.solution <- correct (solution = solution , weight = W , value = P , limit = c_m)

corrected.solution
is.valid(solution = corrected.solution , weight = W , limit = c_m)


