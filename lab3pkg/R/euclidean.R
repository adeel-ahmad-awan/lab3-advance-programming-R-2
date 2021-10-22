#' Euclid's algorithm, is an efficient method for computing the greatest common divisor (GCD) of two integers (numbers)
#' https://en.wikipedia.org/wiki/Euclidean_algorithm 
#' @export
#' @param a numeric
#' @param b numeric
#' @return The greatest common divisor
#' @export
euclidean <-
function(a,b) {
    remainder <- integer(0);
    while ((a %% b) > 0)  {
        remainder = a %% b
        a = b
        b = remainder
    }
    return(b)
}
