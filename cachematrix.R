## The makeCacheMatrix Funktion creates or prepares a List of funktions 
## get/ set/ setinverse/ getinverse in a seperate environment 
## which can be untilised by the cacheSolve funktion. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, 
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve funktion will look-up if an 
## inverse Matrix is already stored and retrieves 
## the data if possible, otherwise it will calculate the solution 
## and save it in the makeCacheMatrix environment

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
