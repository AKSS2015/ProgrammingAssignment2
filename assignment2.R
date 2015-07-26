makeCacheMatrix <- function(x = matrix())
        { # This function creates a special object that stores a "matrix"
        m <- NULL
                set <- function(y) {
                x <<- y
                m <<- NULL
                }
                get <- function() x
                setinverse <- function (solve) m <<- solve(x)
                getinverse <- function() m
                list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse) #the special object created by 
                # makeCacheMatrix is a list containing a function to set
                # and get the value of the matrix and set and get the 
                # value of its inverse
        }

cacheSolve <- function(x, ...) 
        { # This function calculates the inverse of the
        # special "matrix" object created with makeCacheMatrix.
        m <- x$getinverse()
        if(!is.null(m)) # Checks if the inverse's already been calculated
                { 
                message("getting cached inverse")
                return(m) # Returns the cached inverse
                }
        data <- x$get()
        m <- solve(data, ...)# calculates the inverse
        x$setinverse(m) # Sets the value of the inverse in the cache via
        # the setinverse function
        m
        }
