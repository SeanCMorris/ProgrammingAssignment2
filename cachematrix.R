## The following two functions create a square, invertible matrix
## and then caches this inverse in the cache environment


##  The first function creates a matrix 
## 1. set the matix value
## 2. get the matrix value
## 3. set the matrix inverse value
## 4. get the matrix inverse value
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                     # setting to NULL
  
  
  set <- function(y) {            # creating matrix in environment 
    x <<- y
    inv <<- NULL
  }
  
  
  get <- function() x             # get matrix value
  setinverse <- function(inverse) inv <<- inverse   # invert/store matrix in cache
  getinverse <- function() inv # retrieve from cache
  
  # return functions to environment
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function computes/retrieves the inverse of the matrix created above.
## If the inverse has not been calculated, it will do so 
## If the inverse has been calculated, then it will retrieve from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse() # attempt to retrieve inverse from cache
  
  
  if(!is.null(inv)) {
    message("getting cached data.") # retrieves from cache if it exists
   
     return(inv) # returns cached inverse
  }
  
  # creates matrix in environment if not cached
  matrix1 <- x$get()
  inv <- solve(matrix1)
  x$setinverse(inv)
  inv
}