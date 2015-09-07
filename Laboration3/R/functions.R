
#' The euclidean algorithm
#' 
#' @param x A number. x must be a numeric and if its not an integer the results may be wrong.
#' @param y A number. y must be a numeric and if its not an integer the results may be wrong.
#' @return The greatest common divisor of \code{x} and \code{y}.
#' The euclidean algorithm calculates the greatest common divisor of its two input variables. It is explained at the Wikipedia page \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}.
#' @examples
#' euclidean(123612, 13892347912)
#' # [1] 4
#' euclidean(100, 1000)
#' # [1] 100
#' @seealso \code{\link{dijkstra}} for our other function.

euclidean <- function(x,y){
  stopifnot(is.numeric(x) & is.numeric(y))
  while(y != 0){
    temp <- y
    y <- x %% y
    x <- temp
  }
  return(x)
}

#' Dijkstra's algorithm
#' 
#' @param graph A graph that is defined by a data frame containing the three columns \code{v1}, \code{v2} and \code{w}. \code{v1} and \code{v2} are the names of the vertices in the graph and \code{w} is the weight (distance) of the path from \code{v1} to \code{v2}.
#' @param init_node The node from which the distances are to be calculated. \code{init_node} must be in the set of vertices defined by \code{v1} and \code{v2}.
#' @return A vector containing the least weight (shortest distance) from \code{init_node} to all other vertices.
#' The Dijkstra's algorithm calculates the shortest distance from a given vertex in a graph and all other vertices. It is explained at the Wikipedia page \url{https://en.wikipedia.org/wiki/Dijkstra's_algorithm}.
#' @examples
#' dijkstra(wiki_graph, 1)
#' # [1]  0  7  9 20 20 11
#' dijkstra(wiki_graph, 3)
#' # [1]  9 10  0 11 11  2
#' @seealso \code{\link{euclidean}} for our other function.
#' \code{\link{wiki_graph}} for the graph in the example.

dijkstra <- function(graph, init_node){
  stopifnot(is.numeric(init_node) & length(init_node)==1)
  stopifnot(is.data.frame(graph))
  stopifnot(ncol(graph) == 3)
  stopifnot(colnames(graph)==c("v1", "v2", "w"))
  stopifnot(init_node %in% graph$v1 )
  
  nodes <- unique(unlist(graph[, 1:2]))
  unvisited <- rep(TRUE, length(nodes))
  distance <- rep(Inf, length(nodes))
#   names(distance) <- nodes
  distance[nodes == init_node] <- 0
  
  while(sum(unvisited) != 0){
    
    current_node <- nodes[unvisited][which.min(distance[unvisited])]
    neighbours <- graph[graph[, 1] == current_node, ]
    neighbours <- neighbours[neighbours[, 2] %in% nodes[unvisited], ]
    if(nrow(neighbours) != 0){
      
      for(neighbour in neighbours[, 2]){
        new_dist <- distance[nodes == current_node] + neighbours[which(neighbours[, 2] == neighbour), 3]
        if(new_dist < distance[nodes == neighbour]) distance[nodes == neighbour] <- new_dist
      }
      
    }
    unvisited[nodes == current_node] <- FALSE
    
  }
  
  return(distance)
  
}

# ## Another example
# ## http://www.geeksforgeeks.org/greedy-algorithms-set-6-dijkstras-shortest-path-algorithm/
# geek_graph <- data.frame(
#   v1 = c(0, 0, 1,  1, 2, 2, 2, 3,  3,  4, 5, 6, 6, 7, 1, 7, 2,  7, 3, 5, 8, 4,  5,  5, 6, 7, 8, 8),
#   v2 = c(1, 7, 2,  7, 3, 5, 8, 4,  5,  5, 6, 7, 8, 8, 0, 0, 1,  1, 2, 2, 2, 3,  3,  4, 5, 6, 6, 7),
#   w  = c(4, 8, 8, 11, 7, 4, 2, 9, 14, 10, 2, 1, 6, 7, 4, 8, 8, 11, 7, 4, 2, 9, 14, 10, 2, 1, 6, 7))

# dijkstra(geek_graph, 0)
# # [1]  0  4 12 19 21 11  9  8 14







#' An example graph from Wikipedia.
#'
#' A dataset containing egde from a start node to it's neighbour nodes, from a wikipedia page (\url{https://en.wikipedia.org/wiki/Dijkstra's_algorithm}) as an example of how Dijkstra's algorithm works.
#' 
#' @format A data frame with 18 rows and 3 variables v1, v2 and w:
#' \describe{
#'   \item{v1}{containing a vector of start nodes}
#'   \item{v2}{containing a vector of end nodes}
#'   \item{w}{containing the weight of the edges between \code{v1} and \code{v2}}
#' }
#' @source \url{https://upload.wikimedia.org/wikipedia/commons/5/57/Dijkstra_Animation.gif}


wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                         v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                         w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))




