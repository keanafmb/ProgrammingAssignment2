## These functions can cache the inverse of a matrix

## The first function, makeCacheMatrix, can create a special "matrix" object that
#can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
    set_mx <- function(y){. #used to reset the matrix. 
        x <<- y               
        mx <<- NULL
    }
    get <- function() x
    setInv_mx <- function (Inv_mx) mx <<- Inv_mx
    getInv_mx <- function () mx
    list (set_mx = set_mx, get = get,
          setInv_mx = setInv_mx,
          getInv_mx = getInv_mx)
    
}


## cacheSolve can return the inverse of the matrix 'x' created by 
# makeCacheMatrix function

cacheSolve <- function(x, ...) {
       mx <- x$getInv_mx ()
       if (!is.null(mx)) {  # if inverse of that matrix has already been calculated,
           return(mx)       # return old matrix
       }
       data <- x$get()      # if not calculated, get the matrix
       mx <- solve(data)    # calculate the inverse using solve()
       x$setInv_mx(mx)      # reassign the new inverse matrix to mx
       mx                   # print the inverse matrix
}
