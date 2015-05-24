## cachematrix.R is a script that takes a square matrix as 
## an input, finds the inverse and caches it. If the inverse was
## previously computed, the script can find the cached inverse matrix


## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL # initialized cache to NULL
  # stores the matrix from input
  setMatrix <- function(x){
    x <<- y
    cache <<- NULL # clear cache
  }
  # returns newly created matrix
  getMatrix <- function(){
    x
  }
  # sets the arguement to be cached
  setInverse <- function(solve){
    cache <<- solve
  }
  # retuns cache
  getInverse <- function(){
    cache
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse )
}

## This function complutes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, then cacheSolve should retrieve the solution from the cache

cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # return inverse
  inverse
}