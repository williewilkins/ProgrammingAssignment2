## Put comments here that give an overall description of what your
## functions do
## This fuction for caching the inverse a matrix

makeCacheMatrix <- function(x = matrix()) {

 inverse_matrix <- NULL                  ## initialize inverse_matrix   
 set <- function(y) {                    ## define the set function   
        x <<- y                          ## value of matrix in parent environment  
        inverse_matrix <<- NULL          ## reset inverse_matrix to NULL  
 }  
 get <- function() x                     ## define the get fucntion - returns value of the matrix argument  
      
 setinverse <- function(inverse) inverse_matrix <<- inverse  ## value of inverse_matrix  
 getinverse <- function() inverse_matrix                     ## value of inverse_matrix  
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer   
 
}

## This function computes the inverse of the matrix returned by makeCacheMatrix  

cacheSolve <- function(x, ...) {
   inverse_matrix <- x$getinverse()  
   if(!is.null(inverse_matrix)) {  
       message("getting cached data")  
       return(inverse_matrix)  
   }  
   dataset <- x$get()  
   inverse_matrix <- solve(dataset, ...)  
   x$setinverse(inverse_matrix)  
   inverse_matrix  
}
