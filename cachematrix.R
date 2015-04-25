makeCacheMatrix <- function(x) {
  y <- NULL
  metrixInverse <- NULL
  set <- function(x) {
    y <<- x
    }
  get <- function() y
  setinverse <- function(inversesMatrix) metrixInverse <<- inversesMatrix
  getinverse <- function() metrixInverse
  ## list function provides Object of makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

matequal <- function(x, y){
  if(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y))
    return(TRUE)
  else 
    return(FALSE)
}

cacheSolve <- function(data,x1) {
   m <- x1$getinverse()
  if(matequal(data,x1$get()) && !is.null(m)) 
    {
     message("getting cached data")
     return(m)
    }
  x1$set(data)
  m <- solve(data)
  x1$setinverse(m)
  m
}