## These functions make a matrix 'wrapper' that can store the matrix's 
## own inverse so it can save computation time on future calls to 
## calculate an inverse

## Constructor for the CacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat){
    x <<- mat
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## If the inverse is already calculated, just return it
## otherwise calculate, store and return the inverse. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
