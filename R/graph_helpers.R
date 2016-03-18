# Description:
#   Helper functions for working with the graph package.


#' Get a Subgraph
#'
#' Get the subgraph of a graph. Behavior is similar to subGraph from the graph
#' package, but the data defaults for the graph are preserved.
#'
#' @param graph a graph
#' @param nodes the nodes to keep
#' @export
subgraph = function(graph, nodes) {
  subgraph = subGraph(nodes, graph)
  nodeDataDefaults(subgraph) = nodeDataDefaults(graph)
  edgeDataDefaults(subgraph) = edgeDataDefaults(graph)

  return(subgraph)
}


#' Extract and Simplify a Node Attribute
#'
#' Extract the specified node attribute for all nodes, and simplify the
#' resulting list to a vector whenever possible.
#'
#' @param graph a graph
#' @param attr the name of a node attribute
#' @export
node_attr = function(graph, attr) {
  simplify2array(nodeData(graph, attr = attr))
}


#' Extract and Simplify an Edge Attribute
#'
#' Extract the specified edge attribute for all edges, and simplify the
#' resulting list to a vector whenever possible.
#'
#' @param graph a graph
#' @param attr the name of a edge attribute
#' @export
edge_attr = function(graph, attr) {
  simplify2array(edgeData(graph, attr = attr))
}
