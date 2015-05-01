       makeCacheMatrix <- function(x = matrix())
                {    
                MATRIX <- NULL
                set <- function(y)
                        {
                        x <<- y  
                        MATRIX <<- NULL
                        }
                get <- function() x
                setInverse <- function(solve) MATRIX<<- solve
                getInverse <- function() MATRIX
                list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
        }

cacheSolve <- function(x, ...) 
        {
      
        MATRIX <- x$getInverse()
        if(!is.null(MATRIX)){
                message("getting cached data")
                return(MATRIX)  
                }
        data <- x$get()
        MATRIX <- solve(data, ...)
        x$setInverse(MATRIX)      
}
