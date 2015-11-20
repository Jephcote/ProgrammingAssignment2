## R functions for caching the potentially time-consuming computation, Inverse matrix
## Calvin Jephcote (2015)


## Background: '2x2 Matrix Inverse' example calculation

# Swap the positions of a and d, put negatives in front of b and c, 
# and divide everything by the determinant (ad-bc)

#	Input	             Determinant		Output
#	[a] [b]		---> 1/(ad-bc)  --->  	        [d] [-b]
#	[c] [d]					        [-c] [a]

# r <- c(1, 2, 3, 4)
# m <- matrix(r, nrow=2, ncol=2)
# cmat <- makeCacheMatrix(m)
# inv_m <- cacheSolve(cmat)

#	Matrix: m		Matrix: inv_m
#	[1] [3]			[-2] [1.5]
#	[2] [4]			[1] [-0.5]



makeCacheMatrix <- function(x = matrix()) {
	## This function creates a special "matrix" object that can cache its inverse.
	## Required Input: (x) A square invertible matrix
	
	## Step 1: Set the matrix
        Inv <- NULL
        set <- function(y) {
			## Operator '<<-' assigns a value to an object in an environment, different from the current environment
                x <<- y
                Inv <<- NULL
        }
		
        ## Step 2: Get the matrix
		get <- function() x
		
        ## Step 3: Set the inverse
		setInv <- function(inverse) Inv <<- inverse
        
	## Step 4: Get the inverse
		getInv <- function() Inv
		list(set = set, get = get, setInv = setInv, getInv = getInv)
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the original matrix sent to makeCacheMatrix()
	## solve(data) : Inverse of 'data' where 'data' is a square matrix. 
		
	Inv <- x$getInv()
		
	# If the inverse has already been calculated, get from cache and skip computation
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        
	## Otherwise calculate the inverse
	data <- x$get()
        Inv <- solve(data, ...)
		
	## Set the value of the inverse in the cache
        x$setInv(Inv)
		
	return(Inv)
}




## Test for the potentially time-consuming computation, inverse matrix
	
# r <- rnorm(10000)
# test_matrix <- matrix(r, nrow=100, ncol=100)
# testInv(test_matrix)


testInv <- function(test_matrix, runs=10){
    ## Function to test the effect of caching the potentially
    ## time-consuming computation, inverse matrix. 
        
    temp <- makeCacheMatrix(test_matrix)
       
    for (i in 1:runs) {
	    start.time <- Sys.time()
	    cacheSolve(temp)
	    dur <- Sys.time() - start.time
	    print(dur)
	    }
}
