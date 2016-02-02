
## Create a list containg each matrix(set, get, setinverse, getinverse)
makeCacheMatrix <- function(x = matrix()) {
        inv_mat = NULL
        set = function(y) {
                x = y
                inv_mat = NULL
        }
        get = function() x
        setinverse = function(inverse) inv_mat <<- inverse
        getinverse = function() inv_mat
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## Skip the computation if the inverse has already been computed 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv_mat = x$getinverse()
        if(!is.null(inv_mat)) {
                message("data from cache.")
                return(inv_mat)
        }
        data = x$get()
        inv_mat = solve(data)
        x$setinverse(inv_mat)
        inv_mat
}

