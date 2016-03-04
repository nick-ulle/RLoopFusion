
# Graph-building Algorithm:
#
# Walk the code looking for a loop. Upon finding one:
#
#   1. [ ] Create a new node for that loop.
#   2. [ ] Assess whether the loop is parallel or sequential (how do we
#      determine this?).
#   3. [x] Assess whether the loop depends on any variables from earlier loops.
#      So here we need to check what variables are read from the loop.
#   4. [x] List what variables are written to by the loop.
#   5. [ ] Update the dependency graph based on (3) and (4).


walker = function(x, state) {
  UseMethod("walker")
}


walker.for = function(x, state) {
  # `for` `i` `1:10` `{}`
  # Get header from element 3.
  header = x[[3]]
  
  # Create a vertex for this loop.

  # Check variables written to by the loop.
  # get_writes(x[[4]])

  cat("Oh look! A for loop!\n")
  # Add a node to the graph.
}


walker.default = function(x, state) {
}


`walker.{` = function(x, state) {
  lapply(x, walker)

  invisible()
}

# =====

test = quote({
  a = numeric(10)

  for (i in 1:10) {
    a[i] = i
  }

  b = 4

  for (i in 1:10) {
    a[i] = a[i]^2
  }
})
