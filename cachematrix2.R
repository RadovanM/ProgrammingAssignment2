#Inversing matrices may be very costly. It is often preferableto cache a resolt of a 
#matrix inverse in order to save ressources.This is what the following functions
#accomplish.

#The first function, makeMatrix creates a special list containing a function to
#
#   set the value of the matrix
#   get the value of the matrix
#   set the value of the inverse
#   get the value of the inverse

makeMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The cachematrix function first checks whether an inverse has already been stored in the cache.
#If so, it pulls the inverse from cache, otherwise it calculates the inverse of the matrix
#and sets the new inverse value in the cache via the setmean function.

cachematix <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
