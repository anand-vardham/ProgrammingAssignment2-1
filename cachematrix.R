## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(y) {
    mat  <<- y
    inverse <<- NULL
  }
  get <- function() mat
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(matix) {
  inverse <- matix$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- matix$get()
  # calculate inverse
  inverse  <- solve(data)
  matix$setinverse(inverse)
  inverse
}