
##Assignment: Caching the Inverse of a Matrix


## A pair of functions that cache the inverse of a matrix
## Creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  
  k <- NULL
  
  ## Method to set the matrix
  
  set <- function(y){
    x <<- y
    k <<- NULL
  }
  
  ## Method the get the matrix
  
  get <- function(){
    
    x
  }
  ## Method to set the inverse of the matrix
  
  setInverse <- function(inverse) {
    
    k <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  
  getInverse <- function(){
    
    k 
  }
  ## Return a list of the methods
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not changed)
## then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  k <- x$getInverse()
  
  ## Just return the inverse if its alreaady set
  
  if(!is.null(k)){
    message("getting cached data")
    return(k)
  }
  
  ## Get the matrix from our object
  
  data <- x$get()
  
  
  ## Calculate the inverse using matrix multiplication
  
  k <- solve(data, ...)
  
  ##Set the inverse to the object
  
  x$setInverse(k)
  
  ##Return the matrix
  k
}
