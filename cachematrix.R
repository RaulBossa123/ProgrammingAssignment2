## Put comments here that give an overall description of what your
## functions do

## cacheSolve es una función la cual calcula el inverso de la matriz dada por la funcion
##  makeCacheMatrix. 

makecachematrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversa <- function(inverse) m <<- inverse
        getinversa <- function() m
        list(set = set, get = get,
             setinversa = setinversa,
             getinversa = getinversa)
}


## Esta función calcula el inverso de la matriz que fue creada por la funcion
## makeCacheMatrix. cahe solve devuelve el inverso de la matriz.

cacheSolve <- function(x, ...) {
        cachesolve <- function(x, ...) {
        m <- x$getinversa()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversa(m)
        m
}
        ## Return a matrix that is the inverse of 'x'
}


#probando

my_matrix <- makecachematrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getinversa()
cachesolve(my_matrix)
