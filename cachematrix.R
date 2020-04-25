## Put comments here that give an overall description of what your
## functions do

## function to cache the matrix if it already exists

makeCacheMatrix <- function(x = matrix()) {

  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse)inv<<-inverse
  getInverse<-function()inv
  list(set = set,get = get, setInverse = setInverse, getInverse = getInverse)
}


## function to compute the inverse of the given matrix

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  z <- x$get()
  m <- solve(z) %*% z
  x$setInverse(m)
  m
}
