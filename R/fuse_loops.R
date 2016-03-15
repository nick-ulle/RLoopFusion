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


fuse_loops = function(x) {
  .fuse_loops(x)
}


.fuse_loops = function(x) {
  UseMethod(".fuse_loops")
}


`.fuse_loops.{` = function(x) {
  graph = graphNEL(edgemode = "directed")
  edgeDataDefaults(graph, "prevent_fusion") = FALSE

  # Break code into blocks and for-loops, then collect dependencies for each.
  blocks = blockify(x[-1], as_block = "for")
  deps = list()

  for (i in seq_along(blocks)) {
    # Add Node
    # ========
    block = blocks[[i]]
    dep = deps[[i]] = collect_deps(block)
    is_for = class(block) == "for"

    # FIXME: Better naming scheme for nodes?
    node =
      if (is_for) "for"
      else "block"
    node = sprintf("%s%i", node, i)

    graph = addNode(node, graph)

    # Add Edges
    # =========
    if (i == 1)
      # No edges to add for first node.
      next

    reads = dep$reads
    writes = union(dep$writes, dep$conditional_writes)

    # Work backwards from current node to first node.
    for (j in seq.int(i - 1, 1, -1)) {
      parent = blocks[[j]]
      parent_dep = deps[[j]]
      parent_node = nodes(graph)[[j]]
      no_edge = TRUE

      # Fusion-Preventing Edges
      # -----------------------
      if (is_for && class(parent) == "for") {
        if (
          # FPE between incompatible for-loops.
          !header_equal(block, parent) ||
          #
          # FIXME: The antidependence case should be per-variable, meaning we
          # can fuse as long as the variable with an antidependence in one loop
          # doesn't appear in the other.
          #
          # FPE if either for-loop has antidependence.
          (dep$antidep || parent_dep$antidep) ||
          # FPE if fusion would introduce antidependence.
          # NOTE: This is the scalar (R, W) case. Since
          #   (RBW, W) => (R, W)
          #   (R, RBW) => (R, W)
          # these are also handled.
          any(writes %in% parent_dep$reads)
        ) {
          graph = addEdge(parent_node, node, graph)
          edgeData(graph, parent_node, node, "prevent_fusion") = TRUE

          no_edge = FALSE
        } 
      }

      # Block Dependence Edges
      # ----------------------
      if (length(reads) != 0) {
        parent_writes = union(parent_dep$writes, parent_dep$conditional_writes)

        if (no_edge && any(reads %in% parent_writes))
          # E on (W, R).
          graph = addEdge(parent_node, node, graph)

        reads = setdiff(reads, parent_writes)
      }
    } # end for
  } # end for

  return(graph)
}


.fuse_loops.call = function(x) {
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


.fuse_loops.default = function(x) {
  stop("Class %s is not supported for loop fusion.", class(x))
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
