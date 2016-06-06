## The following two functions allow for the creation and storage of the inverse
## of a user define matrix.

## This first function makeCacheMatrix creates a special "matrix", which is really a list
## containing a set of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse value of the matrix
## 4. get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with
## the above function. However, it first checks to see if the inverse has already been
## created. If so, it gets the inverse from teh cache and skips the computation.
## Otherwise it calculates the inverse of the matrix and set the value of the inverse
## in the chache via the setInverse function.

## Note define a square matrix to the variable y before running cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matr <- x$get()
  if(length(matr) == 1) {
    x$set(y)
    matr <- x$get()
  }
  m <- solve(matr)
  x$setinverse(m)
  m
}
