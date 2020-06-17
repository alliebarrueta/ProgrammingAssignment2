## This function will create a special object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {##set the matrix
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  
  get <- function() x ##we get the value of the matrix
  setinverse <- function(inverse) i <<- inverse ## we set the inverse
  getinverse <- function() i ##and get the value of the inverse
  list(set = set,
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message ("getting cached data") ## if inverse has already been calculated it is retrieved from cache
    return(i)}
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
} ## Return a matrix that is the inverse of 'x'
