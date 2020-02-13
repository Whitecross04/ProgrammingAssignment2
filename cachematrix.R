## The makeCacheMatrix is a function made to take in a matrix object
## Its also a list that contains functions to set the vlue of th matrix,
## get the value of the matrix, set the inverse of the matrix and also
## get the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
        p <- NULL
        set <- function(y){
                x <<- y
                p <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) p <<- inverse
        getInverse <- function() p 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## The function cacheSolve calculates the inverese of the matrix which was 
## created with the makeCacheMatrix function above.
##it first checks to see if the inverse has  been obtained. If so, 
## it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the inverse in 
## the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## This function will Return a matrix wich is an inverse of 'x'
        p <- x$getInverse()
        if(!is.null(p)){
                message("obtaining cached data")
                return(p)
        }
        
        mat <- x$get()
        p <- solve(mat,...)
        x$setInverse(p)
        p
}

#Testing the functions
Mattt<- matrix(rnorm(9, mean = 3, sd = 1.25), 3,3)
Amatrix<- makeCacheMatrix(Mattt)
cacheSolve(Amatrix)
