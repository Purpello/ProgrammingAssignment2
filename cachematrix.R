## Two functions are presented below which cache values from resource intensive calculations so that 
## values that might be needed again can be looked up rather than recomputed.

## makeCacheMatrix creates a matrix object that can be cached using the <<- assignment
## operator to keep a copy of the matrix and its inverse in the parent environment.
## The next function, cacheSolve, checks to see if cached values exist.  If so, cached values are used.  If not, values are computed.

makeCacheMatrix <- function(matrx = matrix()) {
  matrxInv<-NULL                           #set the "local" value of the matrix inverse to NULL
  
  set <- function(mtrx) {          
    matrx <<- mtrx                         #cache the value of the original matrix
    matrxInv <<- NULL                      #set the cached value of the matrix inverse to NULL
  }
  get <- function() matrx                  #get the cached value of the original matrix
  setInv <- function(Inv) matrxInv <<- Inv #cache the inverse
  getInv <- function() matrxInv            #get the cached value of the matrix inverse
  list(set = set, get = get,               #return a list object with the various set and get values
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve takes the object created by makeCacheMatrix and determines if there already
## exists a matrix inverse.  If so, it returns the cached value.  If not, it computes a 
## new value, using solve() to create the inverse.  The assumption is that the inverse exists
## The function does not check for that.

cacheSolve <- function(matrx, ...) {
  Inv <- matrx$getInv()                   #get the cached value of the inverse of the matrix object
  if(!is.null(Inv)) {                     #if the inverse has been cached already, just report that cached data 
    message("getting cached matrix inverse")
    return(Inv)                           #return the value of the cached inverse
  }
  data <- matrx$get()                     #if an inverse was not cached, get the original matrix
  Inv <- solve(data, ...)                 #and calculate its inverse
  matrx$setInv(Inv)                       #then cache the calculated inverse
  Inv                                     #return the value of the inverse just calculated
}
