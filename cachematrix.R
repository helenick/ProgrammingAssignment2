
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
## Define set operation for matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
## Define get operation for matrix
        get <- function() x
		
##  Define get and set operation's for cached matrix
        setinv <- function(inv) m <<- inv
        getinv <- function() m
## Compose object structure
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
## Check if matrix already computed and cashed
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
##set value for cashed matrix
        x$setSolve(m)
        m
}

        
