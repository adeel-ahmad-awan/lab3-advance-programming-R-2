#' euclidean
#' @param a numeric
#' @param b numeric
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
