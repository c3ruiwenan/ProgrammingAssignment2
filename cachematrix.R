## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse # set the value of the inverse of the matrix
  getinverse <- function() i                    # get the value of the inverse of the matrix
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  # if inverse exists (cached), get cached data, return inverse matrix of x
  if(!is.null(i)) { 
    message("getting cached data")
    return(i)
  }
  # if inverse is not cached, compute and return inverse of x
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
