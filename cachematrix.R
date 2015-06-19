#-------------------------------------------
#       Programming Assigment 2
#           Mila Lewerenz
#-------------------------------------------

#---------Comments--------------------------
# The idea is to write two functions that check if an inversed matrix has already been computed and cached.
# !!NB!! 'For this assignment, assume that the matrix supplied is always invertible.' (Coursera) 
#           => no need to prove / test it every time.

#--- F1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
#--- F2: cacheSolve: This function computes the inverse of the special "matrix" returned by 
#           makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
#           then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
      
}

# use the "solve" function. if X matrix, then solve(X) gives its inverse.

#--------Function 2----------------------
#---- Return a matrix that is the inverse of 'x'----

cacheSolve <- function(x, ...){
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}

# For a walk-through explanation on how the example worked:
#     https://github.com/DanieleP/PA2-clarifying_instructions
# My code is pretty much the same except that I changed the names: getmean -> getInverse
# A good matrix for trials would be: 
#     m <- matrix(c(4,3,3,2),2,2)
#     MC <- makeCacheMatrix(m)
#     cacheSolve(MC)          # once to see the result of [-2,3;3,-4]
#     cacheSolve(MC)          # second time to see "getting cached data" + result

# Have fun evaluating! :)
