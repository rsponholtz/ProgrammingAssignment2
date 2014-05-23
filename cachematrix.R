## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix which will cache the inverse(if calculated)
##   Parameters
##    matrixvalue - this is the value of the matrix to be turned into a cached matrix
##   
##   It stores the value of the matrix in matrixvalue
##   and the value of the inverse in cachedinverse

makeCacheMatrix <- function(matrixvalue = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    matrixvalue <<- y
    cachedInverse <<- NULL
  }
  get <- function() matrixvalue
  setinverse <- function(inver) cachedInverse <<- inver
  getinverse <- function() cachedInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse for a cached matrix
## Paramters
##    x - the matrix in question
##  Return
##    the inverse of the matrix x
##
##  side effect, the inverse of the matrix is stored in the X list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() #get the cached version
  if(!is.null(inv)) {   # if it has a value, return it
    message("getting cached data")
    return(inv)  
  }
  data <- x$get() #otherwise, get the matrix
  inv <- solve(data, ...) #solve the matrix
  x$setinverse(inv)   #store the calculated inverse in the matrix
  inv                 #return the matrix to callers
}
