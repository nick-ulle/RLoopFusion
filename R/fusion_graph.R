# Graph-building Algorithm:
#
# Walk the code looking for a loop. Upon finding one:
#
#   1. [x] Create a new node for that loop.
#   2. [x] Assess whether the loop is parallel or serial (how do we
#      determine this?).
#   3. [x] Assess whether the loop depends on any variables from earlier loops.
#      So here we need to check what variables are read from the loop.
#   4. [x] List what variables are written to by the loop.
#   5. [x] Update the dependency graph based on (3) and (4).


#' Generate a Fusion Graph
#'
#' This function generates the fusion graph for the specified block of code.
#'
#' @param x a function or code block
#' @export
fusion_graph = function(x) {
  UseMethod("fusion_graph")
}


#' @export
`fusion_graph.{` = function(x) {
  graph = new_fusion_graph()

  # Break code into nodes (blocks and for-loops).
  code = blockify(x[-1], as_block = "for")
  nodes = vector("list", length(code))

  for (i in seq_along(code)) {
    # Update node with dependency information.
    node = collect_deps(code[[i]])
    nodes[[i]] = node

    node_name = paste0(substr(node$loop_type, 1, 1), i)
    names(nodes)[[i]] = names(code)[[i]] = node_name

    # Create node in graph.
    graph = addNode(node_name, graph)
    nodeData(graph, node_name, "type") = node$loop_type

    # Add edges.
    if (i != 1)
      graph = add_fusion_edges(graph, i, nodes)
  }

  list(graph = graph, code = code)
} # end fusion_graph.{


#' @export
fusion_graph.call = function(x) {
  # function [parameters] [body] [srcref]
  name = as.character(x[[1]])

  if (name == "function") {
    if (class(x[[3]]) != "{")
      x[[3]] = call("{", x[[3]])
    fusion_graph(x[[3]])

  } else {
    fusion_graph.default(x)
  }
}


#' @export
fusion_graph.default = function(x) {
  stop("Class %s is not supported for loop fusion.", class(x))
}


#' Add Edges To The Fusion Graph
#'
#' @param graph a fusion graph
#' @param i index of the current block
#' @param nodes a list of dependency information for the nodes
add_fusion_edges = function(graph, i, nodes) {
  node      = nodes[[i]]
  node_name = names(nodes)[[i]]
  is_for    = (class(node$code) == "for")

  vars    = node$get_vars()
  reads   = node$get_reads()
  writes  = node$get_writes()
  anti    = node$get_anti()
  i_var   = node$get_i_var()

  for (j in seq.int(i - 1, 1, -1)) {
    ancestor        = nodes[[j]]
    ancestor_name   = names(nodes)[[j]]
    ancestor_reads  = ancestor$get_reads()
    ancestor_writes = ancestor$get_writes()
    ancestor_i_var  = ancestor$get_i_var()
    is_adjacent     = FALSE

    both_for = (is_for && class(ancestor$code) == "for")

    # Ordering Edges
    # ==============
    new_reads = setdiff(reads, ancestor_writes)
    # Fix for inductive variables.
    ww = writes[writes %in% ancestor_writes]
    if (both_for && i_var == ancestor_i_var) {
      browser()
      ww = setdiff(ww, i_var)
    }
    # Add the ordering edges.
    if (
      # True (W, R)
      length(new_reads) != length(reads) ||
      # Anti (R, W) or Output (W, W)
      any(writes %in% ancestor_reads) ||
      length(ww) > 0
    ) {
      # Add an edge.
      graph = addEdge(ancestor_name, node_name, graph)
      is_adjacent = TRUE
    }
    reads = new_reads

    # Fusion-Preventing Edges
    # =======================
    if (both_for) {
      if (
        # Headers are not equivalent.
        !header_equal(node$code, ancestor$code) ||

        # FIXME: Different for subscripted variables.
        # Fusion would create a backward loop-carried dependence:
        #   1. Antidependence in one loop for a variable in both loops.
        any(ancestor$get_anti() %in% vars) ||
        any(anti %in% ancestor$get_vars()) ||
        #
        #   2. Fusing the loops creates an antidependence.
        any(writes %in% ancestor_reads)
      ) {
        if (is_adjacent)
          edgeData(graph, ancestor_name, node_name, "prevent_fusion") = TRUE
        else
          graph = add_fp_edge(ancestor_name, node_name, graph)
      }
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
#' @param from the node the edge starts at
#' @param to the node the edge ends at
#' @param graph a fusion graph
#' @param ... additional arguments to addEdge
#' @export
add_fp_edge = function(from, to, graph, ...) {
  graph = addEdge(from, to, graph, ...)
  edgeData(graph, from, to, "prevent_fusion") = TRUE

  return(graph)
}


#' Check whether two for-loops have equal headers.
#'
#' @param x the first for-loop
#' @param y the second for-loop
#' @export
header_equal = function(x, y) {
  # FIXME: We really only care about whether the sequence is equal. The
  # inductive variable is just a dummy.
  (x[[2]] == y[[2]]) && (x[[3]] == y[[3]])
}


#' Group a list of code objects into blocks, while keeping the specified
#' classes as independent blocks.
#'
#' @param x a list of code objects or a code block
#' @param as_block classes which should be treated as independent blocks
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
