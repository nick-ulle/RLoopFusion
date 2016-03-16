# Graph-building Algorithm:
#
# Walk the code looking for a loop. Upon finding one:
#
#   1. [x] Create a new node for that loop.
#   2. [x] Assess whether the loop is parallel or sequential (how do we
#      determine this?).
#   3. [x] Assess whether the loop depends on any variables from earlier loops.
#      So here we need to check what variables are read from the loop.
#   4. [x] List what variables are written to by the loop.
#   5. [x] Update the dependency graph based on (3) and (4).


fusion_graph = function(x) {
  UseMethod("fusion_graph")
}


`fusion_graph.{` = function(x) {
  graph = new_fusion_graph()

  # Break code into nodes (blocks and for-loops).
  nodes = blockify(x[-1], as_block = "for")

  for (i in seq_along(nodes)) {
    # Update node with dependency information.
    node = collect_deps(nodes[[i]])
    nodes[[i]] = node

    node_name = paste0(substr(node$loop_type, 1, 1), i)
    names(nodes)[[i]] = node_name

    # Create node in graph.
    graph = addNode(node_name, graph)
    nodeData(graph, node_name, "type") = node$loop_type

    # Add edges.
    if (i != 1)
      graph = add_fusion_edges(graph, i, nodes)
  }

  list(graph = graph, nodes = nodes)
} # end fusion_graph.{


fusion_graph.call = function(x) {
  # function [parameters] [body] [srcref]
  name = as.character(x[[1]])

  if (name == "function") {
    if (class(x[[3]]) != "{")
      x[[3]] = call("{", x[[3]])
    .fuse_loops(x[[3]])

  } else {
    .fuse_loops.default(x)
  }
}


fusion_graph.default = function(x) {
  stop("Class %s is not supported for loop fusion.", class(x))
}


#' Add Edges To The Fusion Graph
#'
#' @param g graph
#' @param i index of current block
#' @param nodes list of nodes
add_fusion_edges = function(graph, i, nodes) {
  node = nodes[[i]]
  node_name = names(nodes)[[i]]

  is_for = (class(node$code) == "for")
  reads = node$reads
  writes = union(node$writes, node$conditional_writes)

  for (j in seq.int(i - 1, 1, -1)) {
    ancestor = nodes[[j]]
    ancestor_name = names(nodes)[[j]]

    no_edge = TRUE

    # Fusion-Preventing Edges
    # =======================
    if (is_for && class(ancestor$code) == "for") {
      if (
        # FPE between incompatible for-loops.
        !header_equal(node$code, ancestor$code) ||
        #
        # FIXME: The antidependence case should be per-variable, meaning we
        # can fuse as long as the variable with an antidependence in one loop
        # doesn't appear in the other.
        #
        # FPE if either for-loop has antidependence.
        ("sequential" %in% c(node$loop_type, ancestor$loop_type)) ||
        # FPE if fusion would introduce antidependence.
        # NOTE: This is the scalar (R, W) case. Since
        #   (RBW, W) => (R, W)
        #   (R, RBW) => (R, W)
        # these are also handled.
        any(writes %in% ancestor$reads)
      ) {
        graph = add_fp_edge(ancestor_name, node_name, graph)

        no_edge = FALSE
      } 
    }

    # Dependence Edges
    # ================
    # FIXME: Technically, these should be FPEs if the current node is not a
    # loop, but this doesn't seem to be necessary since the fusion algorithm
    # only deals with one node type at a time anyways. It also doesn't matter
    # where non-loops are placed, as long as dependence order is respected.
    if (length(reads) != 0) {
      ancestor_writes = c(ancestor$writes, ancestor$conditional_writes)

      if (no_edge && any(reads %in% ancestor_writes))
        # E on (W, R).
        graph = addEdge(ancestor_name, node_name, graph)

      # Remove dependences that have been satisfied.
      reads = setdiff(reads, ancestor_writes)
    }
  } # end for

  return(graph)
}


#' Create a New Fusion Graph
#'
#' Create an empty directed graph with default node and edge data suitable for
#' use in loop fusion.
#'
#' @export
new_fusion_graph = function() {
  graph = graphNEL(edgemode = "directed")
  nodeDataDefaults(graph, "type") = "block"
  edgeDataDefaults(graph, "prevent_fusion") = FALSE

  return(graph)
}


#' Add Fusion-Preventing Edge
#'
#' Add a fusion-preventing edge to a fusion graph.
#'
#' @export
add_fp_edge = function(from, to, graph, ...) {
  graph = addEdge(from, to, graph, ...)
  edgeData(graph, from, to, "prevent_fusion") = TRUE

  return(graph)
}


#' Check whether two for-loops have equal headers.
#'
#' @export
header_equal = function(x, y) {
  # FIXME: We really only care about whether the sequence is equal. The
  # inductive variable is just a dummy.
  (x[[2]] == y[[2]]) && (x[[3]] == y[[3]])
}


#' Group a list of code objects into blocks, while keeping the specified
#' classes as independent blocks.
#'
#' @export
blockify = function(x, as_block) {
  if (class(x) == "{")
    x = x[-1]

  x = as.list(x)

  j = 0
  blocks = list()
  line_idx = 0

  for (obj in x) {

    if (class(obj) %in% as_block) {
      # Keep as an independent block.
      j = j + 1
      blocks[[j]] = obj
      line_idx = 0

    } else if (line_idx > 0) {
      # Append to the current block.
      line_idx = line_idx + 1
      blocks[[j]][[line_idx]] = obj

    } else {
      # Place in a new block.
      j = j + 1
      blocks[[j]] = call("{", obj)
      line_idx = 2
    }
  }

  return(blocks)
}
