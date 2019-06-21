## `makeCacheMatrix` and `cacheSolve` compute the inverse of a matrix. If this is already stored in the cache,
## the functions do not compute the inverse again, but retrieve it from the cache.
#
# e.g. call
# myMat <-makeCacheMatrix(matrix(data=runif(25),nrow=5,ncol=5))
# cacheSolve(myMat)

## `makeCacheMatrix` creates a list of functions to set/get a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    # define the functions to get/set matrix and get/set its inverse
    # note that the function set is not used in `cacheSolve`
    set         <- function(y){
        x  <<- y
        iv <<- NULL
    }
    get         <- function() x
    setinverse  <- function(solve) iv <<- solve
    getinverse  <- function() iv
    
    # this is what the function returns: a list with all the defined functions
    list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)
}

## `cacheSolve` computes the inverse of the special "matrix" returned by the function 'makeCacheMatrix'.
## If the inverse has already been calculated (i.e. it is not NULL), it is retrieved from the cache
cacheSolve <- function(x, ...) {
    ## Get the inverse. If it is not NULL, retrieve the value from cache. A message in displayed in the console
    iv  <- x$getinverse()
    if(!is.null(iv)){
        message("getting chached data")
        return(iv)
    }
    ## If the inverse is NULL, get the matrix and compute the inverse. Once computed, the inverse is stored in
    ## the cache memory using the function setinverse
    data <- x$get()
    iv   <- solve(data,...)
    x$setinverse(iv)
    
    # this is what the function returns: the inverse of the matrix
    iv
}
