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
      set <- function(y) {          # defines the "set" function. m remains unspecified, x gets appointed to y.
            x <<- y
            m <<- NULL
      }
      get <- function() x     # "get" simply gives you x as the output. If you use 'mat' as I defined below, 
                              #     you'll get a 2x2 matrix containing [4,3; 3,2] / you'll get mat back.
      setInverse <- function(solve) m <<- solve    # Inverse is calculated by using the solve-function on a matrix. Let 'm' be the result.
      getInverse <- function() m                   # getInverse does almost the same thing as get: it gives you your 'm'. 
      list(set = set, get = get,                   # prints a list containing four functions defined in this piece of code.
           setInverse = setInverse,                # if one testes to see the environment, one can see that there are 4 functions and 2 element.
           getInverse = getInverse)
      
}

# use the "solve" function. if X matrix, then solve(X) gives its inverse.

#--------Function 2----------------------
#---- Return a matrix that is the inverse of 'x'----

cacheSolve <- function(x, ...){
      m <- x$getInverse()      # using the list from function 1, m takes only the 4th function.
      # test if there is already a cached inverse matrix
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      # if not use the 'get' part of function 1, therefore the initial matrix we put and invert it with 'solve?
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)   # for future reference: save the value of the inverse in the above list. 
      m
}

# For a walk-through explanation on how the example with makeVector worked:
#     https://github.com/DanieleP/PA2-clarifying_instructions
#
# My code is pretty much the same except that I changed the names: getmean -> getInverse
# A good matrix for trials would be: 
#     mat <- matrix(c(4,3,3,2),2,2)
#     MC <- makeCacheMatrix(mat)
#     cacheSolve(MC)          # once to see the result of [-2,3;3,-4]
#     cacheSolve(MC)          # second time to see "getting cached data" + result

# Have fun evaluating! :)



