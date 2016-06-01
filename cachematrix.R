## The first function write a matrix into cache.
## The second function inverts the matrix, or gets the previously inverted matrix from cache if it exists.


# Usage
# 1)    Cache a Matrix by using:  > cache <- makeCacheMatrix(myMatrix)
#       where myMatrix is an invertible matrix.
# 2)    Invert the matrix by calling:  > cacheSolve(cache) 
# 3)    Run it again to see that the value comes from the cache the second time. (Getting cached data message appears)

# This function creates a special matrix object that can cache a matrix and its inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) m <<- solve
        getinvert <- function() m
        list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


#This function returns the inverse of a matrix. 
# It assumes that the matrix is inversible since there is no conditions for preventing errors.
# If the inverse is already found in cache it will use that value. Otherwise it will calculate it.

cacheSolve <- function(x, ...) {
        m <- x$getinvert()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinvert(m)
        m
}

