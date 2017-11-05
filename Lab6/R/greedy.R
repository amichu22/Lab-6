#' Greedy Algorithm in R

#'

#' Knapsack problem solution using Greedy Algorithm in R.

#' @param x Data.frame containing values and weights.

#' @param W Highest capacity

#'

#' @return Returns maximum knapsack value and respective elements

#' @export

#'

#' @examples greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

#' 

#' @references \href{https://en.wikipedia.org/wiki/Knapsack_problem}{Dynamic Algorithm}

#'



greedy_knapsack<-function(x,W){
  
  
  
  #whether the inputs are non-negative interger values. 
  
  if(is.data.frame(x) && is.numeric(W)) {
    
    if(x[[1]]>0 && x[[2]]>0 && W>0){
      
      n<-nrow(x)
      
      weights<-x[[1]]
      
      values<-x[[2]]
      
      Capacity<-W
      
      ratio<-c()
      
      knapsack<-0
      
      item<-c()
      
      
      
      ratio<- values/weights
      
      
      
      
      
      yy<-sort(ratio,index.return=TRUE,decreasing = TRUE)
      
      values_updated <- values[yy$ix]
      
      weights_updated<-weights[yy$ix]
      
      
      
      k<-Capacity
      
      
      
      j=1
      
      while(k>=weights_updated[j]){
        
        k<-k-weights_updated[j]
        
        item<-append(item,yy$ix[j])
        
        knapsack<-knapsack+values_updated[j]
        
        j=j+1
        
        
        
      }
      
      
      
      
      
      return(list(value=round(knapsack),elements=item))
      
    }
    
    else stop("Data is not positive ! \n",call. = FALSE)
    
  }
  
  else
    
    stop("Data is not numeric or not dataframe ! \n",call. = FALSE)
  
}
