# Matrix inversion is often a costly computation and there can be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly. This
# script includes a pair of functions that cache the inverse of a matrix.


#' Creates a special "matrix" (really just a list containing functions) that
#' allows you to get and set the value of the matrix, and to get and set the
#' inverse of the matrix. The inverse is cached to allow for speedy reuse.
#' 
#' @param x A matrix
#' @return A list of functions
makeCacheMatrix <- function(x = matrix()) {
  inverse.cache <- NULL

  set.matrix <- function(m) {
    x <<- m
    inverse.cache <<- NULL
  }

  get.matrix <- function() x

  set.inverse.cache <- function(inverse) {
    inverse.cache <<- inverse
    inverse  # Return the inverse as a courtesy.
  }

  get.inverse.cache <- function() inverse.cache

  list(
    set.matrix = set.matrix,
    get.matrix = get.matrix,
    set.inverse.cache = set.inverse.cache,
    get.inverse.cache = get.inverse.cache
  )
}

#' Compute the inverse of a square \code{makeCacheMatrix} list and cache the
#' result.
#' 
#' @param x A \code{makeCacheMatrix} list
#' @return The inverse of the cached matrix
#' @examples
#' m <- matrix(rnorm(250000000), nrow=5000, ncol=5000)
#' cm <- makeCacheMatrix(m)
#' all(solve(m) == cacheSolve(cm))
#' cacheSolve(cm)  # Much faster the second time.
cacheSolve <- function(x) {
  inverse <- x$get.inverse.cache()
  if (!is.null(inverse)) {
    message("Using cached data.")
    return(inverse)
  }
  x$set.inverse.cache(solve(x$get.matrix()))
}
