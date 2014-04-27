#----------R programmign Week 2--------------
#Authors Joe Wehmeyer & Brian Hart
#Date: 04/23/2014
#--------------------------------------------
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
  
  cacheInv <- NULL
  
  set <- function(y) {
    
    matrix <<- y
    
    cacheInv <<- NULL
    
  }
  
  get <- function() matrix
  
  setInverse <- function(inverse) cacheInv <<- inverse
  
  getInverse <- function() cacheInv
  
  list(set = set, get = get,
       
       setInverse = setInverse,
       
       getInverse = getInverse)
  
}




#Pass a cached list of functions into cacheSolve
#This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then  cacheSolve  should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  inverse <- x$getInv()
  
  if(!is.null(inverse)) {
    
    message(c("Retrieving cached Inverse"))
    
    return(inverse)
    
  }
  
  data <- x$get()
  
  cacheInv <- solve(data, ...)
  
  x$setInverse(cacheInv)
  
  cacheInv
  
}