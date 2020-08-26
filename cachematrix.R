## These functions combined form an object that stores a matrix and cache's its inverse 

## This function creates a special matrix that can store the inverse of the matrix in its cache

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set = set, get = get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## This function calculates the inverse of the special matrix created above. However, if the inverse has already been calculated and cached, it simply retrieves the inverse, saving comptational time.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    retun(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
