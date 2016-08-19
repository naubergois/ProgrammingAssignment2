

## Function that cache a matrix

makeCacheMatrix <- function(x=matrix ) {
  
  inv <-NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Inverse the matrix if the value is difference of null 

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
 
  if (!is.null(inv)){
    return(inv)
  }
  
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}
