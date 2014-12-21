## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" object
#   that can cache its inverse. This means the variable
# x of the data type matrix that is given to makeCacheMatrix 
# will be saved as a list. The list contains the elements:

# getinverse: 
#				When getinverse is called the inverse of the matrix i is
#				returned.
# setinverse: 
#				the inverse of the matrix is saved with the <<- operator
#			  	to the variable i.
#
# set:			Is a function that stores the matrix x in a variable y and 
#				resets the inverse of the matrix i to NULL.
#
# get:			A function that returns the matrix x 

makeCacheMatrix <- function(x = matrix()) {

	i 	<- NULL
    set <- function(y) {
				x <<- y
                i <<- NULL
            }
			
    get <- function() x
            
	setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
            
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	
	
    }


## Write a short comment describing this function

# The cacheSolve function does the following:

#At first it is tested if the special matrix (which is a list) contains already
#a calculated mean. If there is already a calculated mean, which would then be found
#in x$getinverse a message and the already calculated and srored inverse matrix is returned.

#If there is no inverse matrix saved in the special matrix: x$getinverse is empty and the
#matrix is saved into "data" and the inverse matrix is computed by solve(data)



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		   
            i <- x$getinverse()
			
            if(!is.null(i)) {
                    message("getting cached data")
                    return(i)
            }
			
			else {
			
            data <- x$get()
            i <- solve(data, ...)
            x$setinverse(i)
            i
			
			}
    }

