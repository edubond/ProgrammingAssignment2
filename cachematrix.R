## The first function make a matrix and and the second function calculate if necessary 
## the inverse of the matrix and store it 

## The function has four function inside: the first set the matrix, the second get the matrix 
## value, the third set the inverse value of the matrix and the last get the inverse value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function calculate and set the value of x's inverse in the previous function only if
## the inverse value of x is NULL, otherwise return the current cache inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setInverse(i)
  i
}
