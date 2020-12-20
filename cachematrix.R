 #andres felipe montoya morales
 # estadistico 
 # universidad nacional de colombia
 #coursera 

# i Write the following functions:
  
# firts i do the function called 
#makeCacheMatrix: 


####

#first function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 
# la siguiente función calcula la inversa de una (matriz) especial que devuelve la anterior funcion
# makeCacheMatrix .
## Si la inversa ya se ha calculado (y la matriz no ha cambiado),
## entonces la cachesolve debería recuperar la inversa de la caché.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}




## TEST
M <- matrix(c(1,2,3,4),2,2)
M1 <- makeCacheMatrix(M)
cacheSolve(M1)


