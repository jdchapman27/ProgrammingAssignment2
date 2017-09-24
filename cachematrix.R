##Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    inverse <-NULL
    
    m <- x
    
    #gets the matrix data that is stored
    getMatrix <- function()  m
    
    #sets the matrix data and NULLS the inverse
    setMatrix <- function(d){
        m <<- d
        inverse <<- NULL
    }
    
    #sets the inverse
    setInverse <- function(inv){ 
        inverse <<- inv
    }
    
    #gets the inverse
    getInverse <- function() inverse
    
    list(
        getMatrix = getMatrix,
        setMatrix = setMatrix,
        setInverse = setInverse,
        getInverse = getInverse
   )
   
}


##computes the inverse of a special matrix
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if( !is.null(i) )
    {
        message("getting cached inverse.")
        return(i)
    }
    
    matrix <- x$getMatrix()
    
    i <- solve(matrix)
    
    x$setInverse( i )
    
    i	
}
