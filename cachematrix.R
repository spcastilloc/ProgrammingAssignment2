#makeCacheMatrix <- function(w = matrix()) { ##creation of special matrix object
	inv <- NULL   ##inicialization of inverse matrix
	set <- function(z) {  ##function of set matrix's values.##if matrix changed, then its inverse is initialized again 
		w <<- z
		inv <<- NULL   
	}
	get <- function() w	 ##return matrix
	setinv <- function(inverse) inv <<- inverse  ##set matrix inverse
	getinv <- function() inv ##return inverse of matrix, if matrix inverse hasn't calculated yet then return null
	list(set = set,get = get, ##list of functions
		setinv = setinv,
		getinv = getinv)
}

cacheSolve <- function(w) { ##function that calcule and return the inverse of special matrix 'x'
	inv <- w$getinv() ## first get the inverse of matrix
        if(!is.null(inv)) { ## if inverse has already calculated then return it
                message("getting cached data")
                return(inv)
        }
        data <- w$get() ##if not, then get data of w 
        inv <- solve(data) ##calcule inverse of adquired data
        w$setinv(inv) ##set the inverse of object w
        inv	##return the inverse
}
