# Loop Fusion Algorithm:
#
#   1. [x] Translate the graph into a component graph.
#   2. [x] Add FPEs that were not translated.
#   3. [x] Partition the component graph using the greedy algorithm.
#   4. [ ] Fuse nodes in the original graph based on the partitions.

fuse_loops = function(x) {
  stop("This function doesn't work yet!")

  fg = fusion_graph(x)

  g = fg$graph
  nodes = fg$nodes

  # Translate to parallel component graph.
  # Need to walk through all nodes, keeping only the parallel loop nodes. Also
  # need to keep the edges between them.

  # Determine the set of nodes to keep.
  node_types = node_attr(g, "type")

  p_nodes = names(nodes)[node_types == "parallel"]
  gp = subgraph(g, p_nodes)
  gp = necessary_edges(g, gp, "parallel")
  # g = fuse(g, greedy_partition(gp))

  s_nodes = names(nodes)[node_types == "sequential"]
  gs = subgraph(g, s_nodes)
  gs = necessary_edges(g, gs, "sequential")
  # g = fuse(g, greedy_partition(gs))
}


#' Greedy Partitioning Algorithm
#'
#' @param gc component graph
#' @export
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


#' Necessary Edges Algorithm
#'
#' @param g original graph
#' @param gc component graph
#' @param type component type
#' @export
necessary_edges = function(g, gc, type) {
  node_types = node_attr(g, "type")

  nodes = nodes(g)
  paths = vector("list", length(nodes))
  names(paths) = nodes

  # g is already in preorder.
  for (n in nodes) {
    if (node_types[[n]] == type) {
      # Add node to its own paths, then compute FPEs for incoming edges.
      paths[[n]] = n
      for (m in inEdges(n, g)[[1]]) {
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
