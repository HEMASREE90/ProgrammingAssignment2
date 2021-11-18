

## This function create a matrix and that can hide its inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y){
    x <- y
    invs <- NULL
  }
get <- function()x
setinverse <- function(inverse)
  invs <- inverse
gertinverse <- function()invs
list(set = set, get = get,
     setinverse = setinverse, 
     getinverse = getinverse)
}


## This function calculates the inverse of matrix or if it is already calculated and hidden, it can retrieve the inverse

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)){
    message("getting cached data")
    return (invs)
  }
        ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  invs <- solve(data, ...)
  x$setinverse(invs)
  invs
}
