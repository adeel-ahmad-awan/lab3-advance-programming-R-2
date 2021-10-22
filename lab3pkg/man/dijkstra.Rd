\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dijkstra
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, init_node) 
{
    len <- length(unique(graph$v1))
    vectors <- unique(graph$v1)
    visited <- rep(FALSE, len)
    parent <- rep("-1", len)
    value <- rep(Inf, len)
    names(visited) <- vectors
    names(parent) <- vectors
    names(value) <- vectors
    currentNode <- init_node
    value[currentNode] <- 0
    while (!is.na(as.logical(match(F, visited)))) {
        tempGraph <- graph[graph$v1 == currentNode, ]
        visitedIndex <- which(visited == T)
        if (length(visitedIndex) > 0) {
            visitedVector <- vectors[visitedIndex]
            tempGraph <- tempGraph[tempGraph$v2 \%in\% nonVisitedIndex, 
                ]
        }
        visited[currentNode] <- T
        if (nrow(tempGraph) > 0) {
            for (i in 1:nrow(tempGraph)) {
                tempValue = as.numeric(value[currentNode]) + 
                  as.numeric(tempGraph[i, 3])
                if (tempValue < value[tempGraph[i, 2]]) {
                  value[tempGraph[i, 2]] = tempValue
                  parent[tempGraph[i, 2]] = currentNode
                }
            }
        }
        nonVisitedIndex <- which(visited == F)
        if (length(nonVisitedIndex) == 0) {
            break
        }
        minValue <- min(value[nonVisitedIndex])
        currentNode <- vectors[which(value == minValue)]
        if (length(currentNode) > 1) {
            for (k in currentNode) {
                if (visited[k] == F) {
                  currentNode <- k
                  break
                }
            }
        }
    }
    return(as.numeric(value))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
