## This pair of functions performs matrix inversion in a
## less time-consuming and costly way by first caching 
## matrix objects that can then be inversed without being 
## re-computed each time.

## The function makeCacheMatrix outputs a matrix object
## that can be accessed by future functions without being 
## recalculated.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## The cacheSolve function accesses the cached matrix objects
## created by the above function and solves for their inverse.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("retrieving cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
