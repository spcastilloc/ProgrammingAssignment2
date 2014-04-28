#makeCacheMatrix <- function(x = matrix()) { ##creation of special matrix object
	inv <- NULL   ##inicialization of inverse matrix
	set <- function(y) {  ##function of set matrix's values.##if matrix changed, then its inverse is initialized again 
		x <<- y
		inv <<- NULL   
	}
	get <- function() x	 ##return matrix
	setinv <- function(inverse) inv <<- inverse  ##set matrix inverse
	getinv <- function() inv ##return inverse of matrix, if matrix inverse hasn't calculated yet then return null
	list(set = set,get = get, ##list of functions
		setinv = setinv,
		getinv = getinv)
}

cacheSolve <- function(x) { ##function that calcule and return the inverse of special matrix 'x'
	inv <- x$getinv() ## first get the inverse of matrix
        if(!is.null(inv)) { ## if inverse has already calculated then return it
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ##if not, then get data of x 
        inv <- solve(data) ##calcule inverse of adquired data
        x$setinv(inv) ##set the inverse of object x
        inv	##return the inverse
