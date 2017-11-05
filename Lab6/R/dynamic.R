
#' Dynamic programming 
#'
#' Knapsack problem is swoved using dynamic programming.
#'
#' @param x is a dataframe containing weights and values of elements.
#' @param W is the highest weight capacity
#'
#' @return The output is the maximum value knapsack can hold for the given weight capacity
#' @export
#'
#' @examples knapsack_dynamic(x=knapsack_objects[1:1000,], W = 100)
#' 
#' @references \href{https://en.wikipedia.org/wiki/Knapsack_problem}{Dynamic Algorithm}
#' 













knapsack_dynamic<-function(x,W){
  stopifnot(is.data.frame(x)== TRUE && is.numeric(W)== TRUE)  
  stopifnot(x[[1]]>0 && x[[2]]>0 && W>0)
  
  values<-as.vector(x[[2]])
  weights<-as.vector(x[[1]])
  n<- nrow(x)
  m<-W
  it<-c()
  sol.matrix<-matrix(0,nrow = n+1,ncol = m+1)
  
  for(i in 2:n){
    for(j in 1:m){
      if(weights[i]>j)
        sol.matrix[i,j]<-sol.matrix[i-1,j]
      else
        sol.matrix[i,j]<-max( sol.matrix[i-1,j], sol.matrix[i-1,j-weights[i]]+values[i])
    }
  }
  
  knapsack<-round(sol.matrix[n,m])
  
  
  
  while(m>0 && n-1>0){
    
    if(sol.matrix[n,m]!=sol.matrix[n-1,m]){
      it<-append(it,n)
      m<-m-weights[n]
    }
    n<-n-1
  }
  return(list(value=knapsack,elements=sort(it)))
  
  
  
}
set.seed(42)

n <- 2000

knapsack_objects <- data.frame(
  
  w=sample(1:4000, size = n, replace = TRUE),
  
  v=runif(n = n, 0, 10000)
  
)
knapsack_dynamic(x = knapsack_objects[1:1200,], W = 3500)
