## 10-10-18
## R_course assignment 2
## Generate functions that cache the inverse of a matrix
## creates a matrix object and illustrates lexical scoping

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  seti<-function(inv)i<<-inv
  geti<-function()m
  list (set=set, get=get, seti=seti, geti=geti)
}

## function generates the inverse of the matrix cached by makeCacheMatrix

cacheSolve <- function(x, ...) {
  i<-x$geti()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$seti(i)
  i      
}
