## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){ #set matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x #get matrix
  setinverse <- function(inverse) i <<- inverse #set inverse of a matrix
  getinverse <- function()i #get inverse of a matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
   i <- x$getinverse()
   if(!is.null(i)){ #if is has been inversed before
     message("getting cached inverse matrix")
     return(i)
   }
   data <- x$get() #if not, get this matrix first
   i <- solve(data)%%data #then inverse
   x$setinverse(i) #then cache it
   i
}
