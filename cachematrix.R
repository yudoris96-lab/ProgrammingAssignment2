## Computing the inverse of a square matrix can be done
## with the solve function in R.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(solve)inv<<-solve
  getinverse<-function()inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Example test
A <- diag(c(2, 5, 7))
V<-makeCacheMatrix(A)
cacheSolve(V)