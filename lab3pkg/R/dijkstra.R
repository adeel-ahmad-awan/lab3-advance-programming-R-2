#' Dijkstra's algorithm is an algorithm for finding the shortest paths between nodes in a graph.
#' https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm 
#' @param graph data.frame
#' @param init_node numeric
#' @return The vector should returning the shortest path to every other node from the starting node
#' @export
dijkstra <-
function(graph, init_node) {
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
    
    
    while (!is.na(as.logical( match(F, visited)))) {
        tempGraph <- graph[graph$v1 == currentNode, ]
        
        visitedIndex <- which(visited == T)
        if (length(visitedIndex)  > 0) {
            visitedVector <- vectors[visitedIndex]
            tempGraph <- tempGraph[tempGraph$v2  %in% nonVisitedIndex, ]
        }
        
        
        visited[currentNode] <- T
        if (nrow(tempGraph) > 0) {
            for (i in 1:nrow(tempGraph)) {
                tempValue = as.numeric(value[currentNode]) + as.numeric(tempGraph[i,3])  
                if ( tempValue <  value[tempGraph[i,2]]) {
                    value[tempGraph[i,2]] = tempValue
                    parent[tempGraph[i,2]] = currentNode
                }
            }
            
        }
        
        nonVisitedIndex <- which(visited == F)
        if (length(nonVisitedIndex) == 0) {
            break
        }
        minValue <- min(value[nonVisitedIndex])
        currentNode <- vectors[which(value == minValue) ]
        
        if (length(currentNode) > 1) {
            for (k in currentNode) {
                if (visited[k] == F) {
                    currentNode <- k
                    break
                }
            }
        }
    }
    
    return(as.numeric(value) )
}
