##el script permite crear un tipo especial de matriz con la función makeCacheMatrix(), este objeto puede recordar, configurar y obtener sus atributos
##Adicionalmente permite calcular sus inversa a través de la función cacheSolve si es nnecesario

makeCacheMatrix <- function(x = matrix()) { ##creación del objeto especial
inv <- NULL ##inicialización de la inversa
set <- function(y) { ##función para asignar valores a la matrix, inicializa de nuevo ma inversa
x <<- y
inv <<- NULL
}
get <- function() x ##retorna la matriz
setinv <- function(inverse) inv <<- inverse ##asigna valor a la inversa
getinv <- function() inv ##retorna la inversa de la matriz.
list(set = set,get = get, ##lista de funciones
setinv = setinv,
getinv = getinv)
}

cacheSolve <- function(x) { ##funcion que calcula y retorna la inversa de la matriz especial x 
inv <- x$getinv() ## primero obtiene la inversa de la matrix
        if(!is.null(inv)) { ## si la inversa ha sido calculada, la retorna
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ##si no, obtiene la matriz
        inv <- solve(data) ##y calcula la inversa de los datos obtenidos
        x$setinv(inv) ##configura el valor de la inversa
        inv ##retorna la inversa
}
