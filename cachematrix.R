## These functions allow to avoid repetitive computations of the inverse of a matrx. 
## This is done by storing the matrix and its inverse in cache, and accessing them
## when needed.


## Takes a matrix X and creates a 'special' matrix and its inverse
## in the environment where the function is defined.
## Returns a list of four functions: set, get, setinverse and get inverse
## which can be used to interact with them. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## sets the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## returns the matrix
  get <- function() x
  
  ## sets the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## returns the inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes as input a list of four functions created by makeCacheMatrix.
## Returns the inverse of the matrix accessible by those functions.
## If the inverse has been computed, then it retrieves it from cache
## otherwise it computes it and saves it to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## if the inverse is already computed, take it from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if the inverse is not in cache, compute it and save it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
