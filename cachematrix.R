## Calculate and cache the inverse of a matrix with 2 functions

makeCacheMatrix <- function(x = matrix()) {
        # create a special "matrix" object that can cache its inverse.
        
        mati <- NULL
        
        set <- function(y) {
                x <<- y
                mati <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) mati <<- solve
        getsolve <- function() mati
        
        list(set = set,
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


# Computing the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
        mati <- x$getsolve()
        
        if(!is.null(mati)) {
                message("getting cached data")
                return(mati)
        }
        
        data <- x$get()
        mati <- solve(data, ...)
        x$setsolve(mati)
        
        mati
}
