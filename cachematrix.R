## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The proposed solution is to cache the inverse of a matrix in order to compute
## the inverse only once and then cache the value and reuse it.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

		## create a inverse matrix (initialize the variable)
		inverse_matrix <- NULL
		
		## define the set mothod
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
		## define the get mothod
        get <- function() x
		
		## define the setinverse mothod
        setinverse <- function(solve) inverse_matrix <<- solve
		
		## define the getinverse mothod
        getinverse <- function() inverse_matrix
		
		## define the list of available methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
			 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		## get the inverse of the matrix
		inverse <- x$getinverse()
		
		## if the response is not null then the inverse has been computed before this call.
		## (there was another call of the method cacheSolve
        if(!is.null(inverse)) {
				## Inform the user that the response is from cache and not computed
                message("getting cached data")
				##return the actual value
                return(inverse)
        }
		## the inverse is null so it has to be computed
		
		## get the original matrix
        data <- x$get()
		##compute the inverse
        inverse <- solve(data)
		##set the inverse value (cache it)
        x$setinverse(inverse)
		##return the inverse matrix
        inverse		
}
