# Loop Fusion Algorithm:
#
#   1. [x] Translate the graph into a component graph.
#   2. [x] Add FPEs that were not translated.
#   3. [x] Partition the component graph using the greedy algorithm.
#   4. [x] Fuse nodes in the original graph based on the partitions.


#' Loop Fusion
#'
#' This function applies the loop fusion algorithm described in (Kennedy 1993)
#' to the specified function or code block.
#'
#' @param x a function or code block
#' @export
loop_fusion = function(x) {
  fg = fusion_graph(x)

  # Compute and fuse each component graph.
  fused = fuse_component_graph(fg$graph, fg$code, "parallel")
  fused = fuse_component_graph(fused$graph, fused$code, "serial")

  # Collapse the node list in topological order.
  if (numEdges(fused$graph) > 0) 
    as_code_block(fused$code[tsort(fused$graph)])
  else 
    as_code_block(fused$code)
}


#' Compute and Fuse a Component Graph
#'
#' This function computes the requested component graph and fuses it, then
#' updates the fusion graph and list of code blocks appropriately.
#'
#' @param graph a fusion graph
#' @param nodes a list of code blocks
#' @param component a component name ("parallel" or "serial")
fuse_component_graph = function(graph, nodes, component) {
  nodes_kept = (node_attr(graph, "type") == component)

  if (any(nodes_kept)) {
    gc = subgraph(graph, nodes(graph)[nodes_kept])
    gc = necessary_edges(graph, gc, component)

    for (partition in greedy_partition(gc)) {
      if (length(partition) > 1) {
        node = paste0(partition, collapse = "")

        graph = combine_fusion_graph_nodes(graph, partition, node)

        nodes[[node]] = combine_for_loops(nodes[partition])
        nodes[partition] = NULL
      }
    }
  }

  list(graph = graph, code = nodes)
}


#' Necessary Edges Algorithm
#'
#' @param g original graph
#' @param gc component graph
#' @param type component type
necessary_edges = function(g, gc, type) {
  has_type = (node_attr(g, "type") == type)

  # Nodes need to be in topological order.
  nodes = tsort(g)
  paths = vector("list", length(nodes))
  names(paths) = nodes

  for (n in nodes) {
    if (has_type[[n]]) {
      # Add node to its own paths, then compute FPEs for incoming edges.
      paths[[n]] = n
      for (m in inEdges(n, g)[[1]]) {
        if (has_type[[m]])
          next
        ri = paths[[m]]
        if (length(ri) != 0)
          # Suppress warnings about replaced edges.
          gc = suppressWarnings(add_fp_edge(ri, n, gc))
      }
    } else {
      # Union paths for all incoming edges.
      into_n = inEdges(n, g)[[1]]
      if (length(into_n) != 0)
        paths[[n]] = do.call(union, paths[into_n])
    }
  }

  return(gc)
}


#' Greedy Partitioning Algorithm
#'
#' This function partitions a component graph so that each partition is a set
#' of fusable nodes.
#'
#' The original algorithm is described in (Allen 1986), but was modified here
#' to suit the loop fusion strategy described in (Kennedy 1993).
#'
#' @param gc component graph
greedy_partition = function(gc) {
  # FIXME: This function is embarassingly inefficient because it uses vectors
  # as sets.
  edges = edges(gc)
  partitions = list()
  i = 1

  # Set of nodes waiting to be visited.
  waiting = nodes(gc)[vapply(inEdges(gc), length, 0L) == 0]
  # Set of nodes that were already visited.
  visited = character()

  while (length(waiting) != 0) {
    # Set of nodes in this partition.
    partition = character()
    # Set of nodes unsuitable for this partition.
    not_ok = character()
    # Set of candidate nodes for this partition.
    candidates = waiting

    while (length(candidates) != 0) {
      # Add node to this partition.
      node = candidates[[1]]
      partition = union(partition, node)

      # Mark node as visited.
      waiting = setdiff(waiting, node)
      visited = union(visited, node)

      # Check if any children are not ok to be part of this partition.
      children = edges[[node]]
      if (length(children) != 0) {
        prevent_fusion =
          simplify2array(edgeData(gc, node, children, "prevent_fusion"))
        not_ok = union(not_ok, children[prevent_fusion])

        # Check if any children have no parents left to visit.
        no_parents_left = 
          vapply(children, function(child) {
            all(inEdges(child, gc)[[1]] %in% visited)
          }, TRUE)
        waiting = union(waiting, children[no_parents_left])
      }

      candidates = setdiff(waiting, not_ok)
    }

    # Add r to the partitions.
    partitions[[i]] = partition
    i = i + 1
  }

  return(partitions)
}


#' Combine Nodes in a Fusion Graph
#'
#' This function combines the specified nodes in a fusion graph, preserving any
#' fusion-preventing edges.
#'
#' @param g a fusion graph
#' @param partition a vector of nodes to combine
#' @param node a name for the combined node
combine_fusion_graph_nodes = function(
  g,
  partition,
  node = paste0(partition, collapse = "")
) {
  # Combine the nodes.
  new_g = combineNodes(partition, g, node)

  # Fix the node data.
  nodeData(new_g, node, "type") = nodeData(g, partition[[1]], "type")

  # Fix each edge going out of the new node.
  out_edges = edges(new_g, node)[[1]]
  if (length(out_edges) != 0) {
    old_out_edges = edges(g, partition)
    for (to_node in out_edges) {
      from_nodes = in_which(to_node, old_out_edges)
      values = edgeData(g, from_nodes, to_node, "prevent_fusion")
      edgeData(new_g, node, to_node, "prevent_fusion") =
        Reduce("||", values)
    }
  }

  # Fix each edge coming into the new node.
  in_edges = inEdges(node, new_g)[[1]]
  if (length(in_edges) != 0) {
    old_in_edges = inEdges(partition, g)
    for (from_node in in_edges) {
      to_nodes = in_which(from_node, old_in_edges)
      values = edgeData(g, from_node, to_nodes, "prevent_fusion")
      edgeData(new_g, from_node, node, "prevent_fusion") =
        Reduce("||", values)
    }
  }

  return(new_g)
}


#' Combine a List of for-loops
#'
#' This function combines a list of for-loops in the same order they appear in
#' the list, using the header of the first loop.
#'
#' @param loops an ordered list of for-loops
combine_for_loops = function(loops) {
  # for [variable] [range] [body]
  loop = loops[[1]][1:3]
  loop[[4]] = as_code_block(loops, function(x) x[[4]])

  return(loop)
}
