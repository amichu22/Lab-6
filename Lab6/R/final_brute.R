library(Rcpp)



#' Brute Force Algorithm in R

#'

#' Knapsack problem solution using Brute Force Algorithm in R.

#' @param x Data.frame containing values and weights

#' @param W Highest capacity

#' @param fast logical argument for running Rcpp code.

#'

#' @return Returns maximum knapsack value and respective elements

#' @export

#'

#' @examples brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

#'           brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500,fast=TRUE)

#' @references \href{https://en.wikipedia.org/wiki/Knapsack_problem}{Dynamic Algorithm}

#'  

brute_force_knapsack <- function(x,W,fast=FALSE){
  
  
  
  #whether the inputs are non-negative interger values.
  
  
  
  if(is.data.frame(x) && is.numeric(W)) {
    
    if(x[[1]]>0 && x[[2]]>0 && W>0){
      
      n<-nrow(x)
      
      weights<-x[[1]]
      
      values<-x[[2]]
      
      Capacity<-W
      
      bestValue<- 0
      
      bestChoice<-c()
      
      bestWeight<-0
      
      A<- rep(0,n)
      
      
      
      for (i in 1:(2^n))
        
      {
        
        
        
        j<-n
        
        tempWeight<-0
        
        tempValue<-0
        
        while(A[j]!= 0 && j>0){
          
          A[j]<-0
          
          j<- j-1
          
        }
        
        
        
        A[j]<- 1
        
        
        
        for (k in 1:n){
          
          if(A[k] == 1) 
            
          {
            
            tempWeight<-tempWeight+ weights[k]
            
            tempValue<-tempValue+values[k]
            
          }
          
        }
        
        
        
        if (fast==FALSE){
          
          if((tempValue > bestValue) && (tempWeight<= Capacity)){
            
            bestValue<-tempValue
            
            bestWeight<-tempWeight
            
            bestChoice<-which(A==1)   
            
          }
          
        }
        
        else
          
        {
          
          library(Rcpp)
          
          Rcpp::cppFunction(' int abc(int tempValue, int bestValue, int tempWeight,
                            
                            int Capacity, int bestWeight){
                            
                            
                            
                            if((tempValue > bestValue) && (tempWeight<= Capacity)){
                            
                            bestValue=tempValue;
                            
                            bestWeight=tempWeight;
                            
                            
                            
                            
                            
                            }
                            
                            return bestValue;
                            
                            
                            
                            } ')

          bestValue<-abc(tempValue,bestValue,tempWeight,
                         
                         Capacity,bestWeight)
          
        }
        
        
        
      }
      
      
      
      
      
      
      
      return(list(value=round(bestValue), elements=bestChoice))
      
      
      
  }
    
    else stop("Data is not positive ! \n",call. = FALSE)
    
}
  
  else
    
    stop("Data is not numeric or not dataframe ! \n",call. = FALSE)
  
}

