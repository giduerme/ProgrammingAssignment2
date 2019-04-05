## ProgrammingAssignment: Caching the inverse of a matrix
## makeCacheMatrix - function that generates a matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve - function that computes the inverse of the returned matrix of the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        ## Return a matrix which is the inverse of 'x'
        if(!is.null(inv)){
                message("getting cached data...")
                return(inv)
        }
        
        matdata <- x$get()
        inv <- solve(matdata, ...)
        x$setinverse(inv)
        inv
}
