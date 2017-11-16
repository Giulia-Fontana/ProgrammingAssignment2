##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { 
    ## initialize matrix inverse
    mat_inv <- NULL  
    ##define the set function for the parent environment 
    set <- function(y){
       x <<- y
       mat_inv <<- NULL
    }
    ##define the get function
    get <- function() x
    ## set inverse variable in parent environment
    setinverse <- function(inverse) mat_inv <<- inverse
    getinverse <- function() mat_inv
    list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
} 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) { 
    mat_inv <- x$getinverse()
    if(!is.null(mat_inv)) {
      message("getting cached data")
      return(mat_inv)
    }
    data <- x$get()
    mat_inv <- solve(data, ...)
    x$setinverse(mat_inv)
    mat_inv
} 
