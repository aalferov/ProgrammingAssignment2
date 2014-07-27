# The two functions below helps to cache the matrix inversion operation

#This function creates a special "matrix" object that can cache its inverse
#It returns a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # inv_matrix is for storing the cached object
    inv_matrix <- NULL
    
    # Set function for the object
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    # Get function for the object
    get <- function() x
    
    # Set function for the inverse matrix
    set_inverse <- function(solve) inv_matrix <<- solve
    
    # Get function for the inverse matrix
    get_inverse <- function() inv_matrix
    
    # Return the list with all defined functions
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of 'x'
    inv_matrix <- x$get_inverse()
    
    # If the inverse is already calculated, return it
    if (!is.null(inv_matrix)) {
        message("get cached matrix")
        return(inv_matrix)
    }
    
    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache the inverse
    x$set_inverse(inv_matrix)
    
    # Return cached matrix
    inv_matrix
}
