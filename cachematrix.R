makeCacheMatrix<-function(x=matrix()){
  i <- NULL
  set<-function(y){ x<<-y
  i<<-NULL
  }
  get<-function() x
  setinverse<-function(inver) i<<-solve
  getinverse<-function() i
  list(set=set, get=get, setinverse = setinverse, getinverse= getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
