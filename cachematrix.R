## @author mikolaan
## @date 06-AUG-2016

## Function makeCacheMatrix consists of four subfunctions
## @return list of function values

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## setter
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        ## getter
        get <- function(){
                return(x)
        }
        setInversedMatrix <- function(inversedMatrix){
                m <<- inversedMatrix
        }
        
        getInversedMatrix <- function(){
                return(m)
        }
        list(set = set, 
                get = get,
                setInversedMatrix = setInversedMatrix,
                getInversedMatrix = getInversedMatrix)
}

## Function responsible for inversing the matrix

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getInversedMatrix()
        if(!is.null(mat)){
                print("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...) ##inverses the matrix
        x$setInversedMatrix(mat)
        mat
}