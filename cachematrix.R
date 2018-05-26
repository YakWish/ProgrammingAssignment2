## These functions turn a matrix into a cachable version of a matrix. 
## Then, the inverse is calculated for the cachable matrix and stored to memory so it can be quickly located

## This function makes a cachable version of a matrix
makeCacheMatrix <- function(x = matrix()){
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## This function saves the inverse of the matrix next to the matrix, so it can be found quickly

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}