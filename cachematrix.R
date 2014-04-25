## These functions used in conjunction with each other can 
## be used to save processing time.  Instead of repeatedly 
## calculating the inverse of a matrix, it is stored in a 
## retrievable cache.  The cache changes only if the matrix 
## called changes...

## The makeCacheMatrix function is a constructor function
## that creates functions to set and get the matrix and 
## the inverse of the matrix...

makeCacheMatrix <- function(x = matrix())  {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setInverse = setinverse,
       getInverse = getinverse)
}

## The cacheSolve function calls the functions created in the makeCacheMatrix to 
## either retrieve or save the inverse of a matrix.  If the matrix has not changed 
## and the inverse has already been calculated, it retrieves the matrix inverse from
## the cache.  Otherwise, it calculates the matrix inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}

