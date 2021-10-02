## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL     #configuring inverse as NULL
        set <- function (y){
                x <<- y
                inv <<- NULL   
        }
        
        get <- function () {x}  #this function is supposed to get the matrix x
        setInverse <- function (inverse) {inv <<- inverse}
        getInverse <- function () {inv} #to get the inverse of the matrix
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
                message ("processing cached data")
                return (inv)
                }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
}
