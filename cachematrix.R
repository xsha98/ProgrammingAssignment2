## Two functions that can cache the inverse of a matrix

## Create a matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<-function(matrix){
    x <<- matrix
    inv <<- NULL
  }
  get<- function(){x}
  setInverse <-function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the above matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getInverse()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data) %*% data
  x$setInverse(xinv)
  xinv
}
