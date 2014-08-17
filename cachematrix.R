## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function makeCacheMatrix creates a Matrix object and different methods
## to work with it. The purprose is uesed as interface to get/set the values 
## of a matrix and get/ set the matrix inverse once its calcualte by the function
## cacheSolve

## the method setInverse will overwrite the value of the inverse in the cache and 
## it will not possible to get it. It will necessary make a calculation from the begining
## uploading the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) inv <<- Inverse 
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## The following funciton store the inverse matrix
## from a given one in the cache. So, if many times the inverse is need it
## is not necessary to compute again due to is store in the cache. The command
## solve is used in R to calculate the matrix inverse


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
