## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## There are couple of functions below
## that cache first and than compute the 
## inverse of a matrix.

## First, function creates a special object, let call it "matrix"

makeCacheMatrix <- function(mtx = matrix()) {
        inverse <- NULL
        set <- function(x) {
                mtx <<- x;
                inverse <<- NULL;
        }
        get <- function() return(mtx);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function

## Now it is possible to compute the
## "matrix" returned by `makeCacheMatrix` above. 
## For inverse already been computed ( matrix has no changes)
## `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
        inverse <- mtx$getinv()
        if(!is.null(inverse)) {
                message("Getting cached data...")
                return(inverse)
        }
        data <- mtx$get()
        invserse <- solve(data, ...)
        mtx$setinv(inverse)
        return(inverse)
}