## English isn't my mother language, so please be compressive
## Write a short comment describing this function
# This function creates a new object "makeCacheMatrix", a matrix that can store its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL  # It sets the inverse as a NULL
  set <- function(y) {
    x <<- y    # 
    i <<- NULL # When a new matrix is set, it its inverse to NULL
  }
  get <- function(){ x  } # It returns the original matrix
  setinverse <- function(inv)   { i <<- inv} # It sets the inverse of the matrix
  getinverse <- function(){ i} # It returns the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #List of the functions 
}


## This function uses a makeCacheMatrix object and returns its inverse. 
# If the inverse isn't set, it calculates its inverse and then returns it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
