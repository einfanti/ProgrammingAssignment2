## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
iv<-NULLset<-function(y){
    x  <<- y
    iv <<- NULL
}
get <- function() x
setinverse <- function(solve) iv <<- solve
getinverse <- function() iv
list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by the function makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv <- x$getinverse()
        if(!is.null(iv)){
          message("getting chached data")
          return(iv)
        }
        data <- x$get()
        iv<-solve(data,...)
        x$setinverse(iv)
        iv
}
