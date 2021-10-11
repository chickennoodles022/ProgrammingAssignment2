#ASSIGNMENT FOR WEEK 3 PROG. ASS. 2
#CREATED by TRISHIA PALOMARES
#this program is created to cached the inverse of a matrix

#---------------------------------------------------------------------------------------------------------------------------------
#For the first part, we will create a special matrix that we will use to cached its matrix
makeCacheMatrix <- function(MTX = matrix()) {
  reciprocal <- NULL #Y is for inverse because I want to
  
  set <- function (matrix){ 
    MTX <<- matrix #assigning values
    reciprocal <<- NULL #assigning values }
  
  setInverse <- function (inverse) reciprocal <<- inverse
  get <- function() MTX
  getinverse <- function() reciprocal

  list(getInverse = getInverse, setInverse = setInverse, get = get, set = set) }}

#---------------------------------------------------------------------------------------------------------------------------------
#For the second part, we are going to use cacheSolve to compute for the inverse of the marix returned by the first function above.
cacheSolve <- function(MTX, ...) {
  reciprocal <- x$getInverse()
  if (!is.null(reciprocal)){
    message ("processing cached info") #display to inform them b whatchu doin
    return(reciprocal) } }
  
  data <- x$get()
  reciprocal <- solve(data) %% data ##solve inverse with mat. multi.
  x$setInverse(reciprocal) reciprocal ##return matrix } %the end adios
