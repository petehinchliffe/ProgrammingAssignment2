## functions to create a cacheing version of a matrix and its inverse,
## and to check for and calculate/set the inverse
## example usage:
## mat <- matrix(c(-3,3, 3,3), nrow = 2, ncol = 2)
## cacheMx <- makeCacheMatrix(mat)
## inverseMx <- cacheSolve(cacheMx)

## Given a matrix, return a vector containing functions to store and retrieve the matrix value and its inverse value 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## create a function which will store the matrix value and clear the inverse value
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    ## create a function which will retrieve the matrix value
    get <- function() x
    ## create a function which will store the inverse value
    setInverse <- function(inv) inverse <<- inv
    ## create a function which will retrieve the inverse value
    getInverse <- function() inverse
    ## return the functions in a list
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return the cached inverse if there is one.
## Otherwise calculate the inverse, set the cached value and return it. 
cacheSolve <- function(x, ...) {
  ## try to get the cached inverse
  inverse <- x$getInverse()
  ## if a non-null value was retreived, then return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## calculate the inverse of the original matrix
  data <- x$get()  
  inverse <- solve(data, ...)
  ## set the result as the cached inverse value
  x$setInverse(inverse)
  ## and return the value
  inverse
}
