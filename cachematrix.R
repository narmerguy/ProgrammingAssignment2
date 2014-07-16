## These are two functions. The first takes an input matrix and
## creates a special list that can be used (for example by the
## second function) to access/calculate/manipulate the inverse
## of that matrix using the solve() function.

## This function creates a special "matrix" list that can be used
## to store or change the inverse or the underlying matrix itself.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # Initialize inverse as null
        set <- function(y) {
                inv <- NULL
                x <<- y
        }
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        matrixvector = list(get = get, set = set, getInverse = getInverse, setInverse = setInverse) # Return list of functions that can be used to access or manipulate stored inverse/data
}


## This is a function which accesses the list created from makeCacheMatrix
## to either retrieve the stored matrix or calculate it here

cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse() # Acess stored inverse
        if (is.null(inverse)) { # If inverse hasn't been calculated, calculate it
                print("Calculating Matrix Inverse")
                Matrix <- x$get()
                inverse <- solve(Matrix)
                x$setInverse(inverse)
        } else { # If inverse has been calculated, keep the original obtained value
                print("Using Cached Inverse")
        }
        inverse # Return final inverse value
}