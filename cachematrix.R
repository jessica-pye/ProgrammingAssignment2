## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
## x is any square invertible matrix
## this function will return a list that is used as the input to cacheSolve()
  inv <- NULL
  set <- function(y) {
## set the matrix
    x <<- y
## using the <<- operator to assign a value to an object in an environment that is different from the current environment
    inv <<- NULL
  }
## get the matrix
  get <- function() x
## set the inverse
  setinverse <- function(inverse) inv <<- inverse
## get the inverse
  getinverse <- function() inv
## returns list of set, get, setinverse, getinverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## assuming the matrix supplied is always invertible
cacheSolve <- function(x, ...) {
## uses the output from the makeCacheMatrix function
## this function will return the inverse matrix
  inv <- x$getinverse()
  if(!is.null(inv)) {
## if the invervse has been calculated then skip and get from cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
## calculate inverse
  inv <- solve(data, ...)
  x$setinverse(inv)
## returns the inverse of 'x' 
  inv
}

