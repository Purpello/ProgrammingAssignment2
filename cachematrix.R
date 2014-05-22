## Two functions are presented below which cache values from resource intensive calculations so that 
## values that might be needed again can be looked up rather than recomputed.

## makeCacheMatrix creates a matrix object that can be cached using the <<- assignment
## operator to keep a copy of the matrix in the parent environment.

makeCacheMatrix <- function(matrx = matrix()) {
  matrxInv<-NULL                     #set the local value of the matrix inverse to NULL
  
  set <- function(mtrx) {          
    matrx <<- mtrx                   #cache the value of the original matrix
    matrxInv <<- NULL                #set the cached value of the matrix inverse to NULL
  }
  get <- function() matrx
  setInv <- function(Inv) matrxInv <<- Inv 
  getInv <- function() matrxInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve takes the object created by makeCacheMatrix and determines if there already
## exists a matrix inverse.  If so, it returns the cached value.  If not, it computes a 
## new value, using solve() to create the inverse.  The assumption is that the inverse exists.
## The function does not check for that.

cacheSolve <- function(matrx, ...) {
  Inv <- matrx$getInv()
  if(!is.null(Inv)) {
    message("getting cached matrix inverse")
    return(Inv)
  }
  data <- matrx$get()
  Inv <- solve(data, ...)
  matrx$setInv(Inv)
  Inv
}
