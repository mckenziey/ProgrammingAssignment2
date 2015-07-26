## The following two functions provide a quick way to compute the inverse of a matrix
## makeCacheMatrix set the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
      
      #set value of maxtrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get value of matrix
        get <- function() x
        #set inverse of matrix
        setinverse <- function(solve) m <<- solve
        #get inverse of matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve prints the inverse of the matrix from the first function (either by pull it from the first function, or calculating it)

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}