##this program is created to make a matrix that will also cache the inverse
makeCacheMatrix <- function(a = matrix()) {
  I <- NULL
  set <- function (matrix){ 
    a <<- matrix
    I <<- NULL
  }
  
  get <- function() {a} 
  setInverse <- function (inverse) {I <<- inverse}
  getinverse <- function() {I}
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#This is for get the inverse from cache
cacheSolve <- function(x, ...) {
  a <- x$getInverse()
  if (!is.null(a)){
    message ("processing cached information")
    return(a)
  }
  
  data <- x$get()
  a <- solve(data) %% data ##solve inverse with mat. multi.
  x$setInverse(a)
  a ##return matrix
}

