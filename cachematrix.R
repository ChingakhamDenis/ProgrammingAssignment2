## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
  {                    ## @x: a square invertible matrix
                       ## return: a list containing functions to
                       ## 1. set the matrix
                       ## 2. get the matrix
                       ## 3. set the inverse
                       ## 4. get the inverse
                       ## this list is used as the input to cacheSolve()
    inv <- NULL
    set <- function(y)   # use `<<-` to assign a value to an object in an environment 
                         # different from the current environment. 
    {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }

## Write a short comment describing this function
## Data Caching

cacheSolve <- function(x, ...) ## @x: output of makeCacheMatrix()
                               ## return: inverse of the original matrix input to makeCacheMatrix()
{
  inv <- x$getInverse()
  if (!is.null(inv))    # if the inverse has already been calculated
  {                     # get it from the cache and skips the computation.
    message("Data is Cached")
    return(inv)
  }
mat <- x$get()          # else, calculates the inverse
inv <- solve(mat, ...) 
x$setInverse(inv)       # sets the value of the inverse in the cache via the setinv function.
inv
}

