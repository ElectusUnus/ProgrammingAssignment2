#These functions are intended to cache the inverse via the cacheSolve function
#of a special matrix created by the makeCacheMatrix function.


#Creates a "special matrix" which is capable of storing its inverse
#within cache so it does not have to be re computed.
makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(inver) inv <<- inver
  getinv<-function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#Checks to see if the inverse is already within memory and if not
#it will compute the inverse, set it to the object, and then return the
#value.
cacheSolve<-function(x,...){
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}