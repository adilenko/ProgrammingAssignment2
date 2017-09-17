
## The function compare two matrix. It returns TRUE if matrix are equal and FALSE if not.
matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

## The function  create a special "matrix" object which contains the following function
## set the value of the matrix
## get the value of the matrix
## set the value of the invrted matrix
## get the value of the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  ismatrixchanged <- NULL
  set <- function(y) {
  ismatrixchanged <<- !matequal(x,y)
    x <<- y
    if(ismatrixchanged)
    sol <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) sol <<- solve
  getsolve<- function() sol
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## The function get an object created using makeCacheMatrix() .
#if If the inverse has already been calculated and the matrix has not changed
#then the function returns a value from cache, otherwise it performs inverse operation
# and return a result of the operation

cacheSolve <- function(x, ...) {
  
  sol <- x$getsolve()
  data<-x$get()
  if(!is.null(sol)) {
    message("getting cached data")
    return(sol)
  }
 
  sol <- solve(data)
  x$setsolve(sol)
  sol
       
}




