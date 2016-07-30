## Cache the inverse of a matrix
## set the value of the matrix
## get the value of the matrix
## set teh value of the inverse matrix
## get the value of the inverse matrix

## m<-makeCacheMatrix(matrix(c(2,0,0,2),c(2,2)))
##  m$get()           [,1] [,2]
##              [1,]    2    0
##              [2,]    0    2
##cacheSolve(m)      inverse of matrix
##                    [,1] [,2]
##              [1,]  0.5  0.0
##              [2,]  0.0  0.5

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inv) i<<-inv
  getinverse<-function()i
  list(set= set, get = get, 
       setinverse =setinverse,
       getinverse =getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  m<-x$get()
  i<-solve(m,...)
  i
}
