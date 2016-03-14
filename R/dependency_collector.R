# Description:
#   Functions for collecting dependencies in a block of code.


#' Collect variable dependency information for a code block.
#'
#' @export
collect_deps = function(x) {
  collector = DependencyCollector()
  .collect_deps(x, collector)

  # Check for loop-carried dependencies.
  collector$update()
  return(collector)
}


.collect_deps = function(x, state) {
  UseMethod(".collect_deps")
}


`.collect_deps.<-` = `.collect_deps.=` = function(x, state) {
  # = [left] [right]

  # First collect dependencies on right side.
  .collect_deps(x[[3]], state)

  # Now add left side to writes.
  left = x[[2]]

  if (class(left) == "call") {
    # function [write] [read 1] [read 2] ...
    if (as.character(left[[1]]) == "[")
      state$add_read(left[[2]])

    state$add_write(left[[2]])
    lapply(left[-(1:2)], .collect_deps, state)

  } else if (class(left) == "name") {
    state$add_write(left)

  } else {
    msg = sprintf("Unsupported left side '%s' in assignment.", class(left))
    stop(msg)
  }
}


# Need an unconditional write in every branch. So use a blank collector for
# every branch. The new write-set is the intersection of each branch's write
# set. Writes that aren't in the new write-set are conditional writes.
.collect_deps.if = function(x, state) {
  # if [condition] [if] [else]

  .collect_deps(x[[2]], state)

  deps_if = DependencyCollector(writes = state$writes)
  .collect_deps(x[[3]], deps_if)

  if (length(x) == 3) {
    reads = deps_if$reads
    conditional_writes = union(deps_if$writes, deps_if$conditional_writes)

  } else {
    deps_else = DependencyCollector(writes = state$writes)
    .collect_deps(x[[4]], deps_else)

    reads = union(deps_if$reads, deps_else$reads)

    # A write that appears in both branches is unconditional.
    # W1 & W2
    writes = intersect(deps_if$writes, deps_else$writes)

    # All other writes are conditional.
    # (W1 | W2 | CW1 | CW2) / W
    conditional_writes = union(
      deps_if$writes, deps_if$conditional_writes,
      deps_else$writes, deps_else$conditional_writes
    )
    conditional_writes = setdiff(conditional_writes, writes)

    state$add_write(writes)
  }

  state$add_read(reads)
  state$add_write(conditional_writes, conditional = TRUE)
}


.collect_deps.call = function(x, state) {
  # NOTE: This is separate because special handling of arrays will be needed.

  # function [arg 1] [arg 2] ...
  lapply(x[-1], .collect_deps, state)
}


.collect_deps.for = function(x, state) {
  # for [variable] [range] [body]
  state$add_write(x[[2]])
  
  lapply(x[-(1:2)], .collect_deps, state)
}


.collect_deps.while =   # while [condition] [body]
.collect_deps.repeat =  # repeat [body]
`.collect_deps.{` =     # { [line 1] [line 2] ...
  function(x, state) {
    lapply(x[-1], .collect_deps, state)
  }


.collect_deps.name = function(x, state) {
  state$add_read(x)
}


.collect_deps.default = function(x, state) {
  # Do nothing.
}


#' Collector for dependencies.
DependencyCollector =
  setRefClass("DependencyCollector",
    fields = list(
      "reads" = "character",
      "writes" = "character",
      #"conditional_reads" = "character",
      "conditional_writes" = "character",
      "is_sequential" = "logical",
      "in_conditional" = "logical"
    ),
    methods = list(
      "initialize" = function(...,
        is_sequential = FALSE,
        in_conditional = FALSE
      ) {
        callSuper(...,
          is_sequential = is_sequential,
          in_conditional = in_conditional
        )
      },

      "add_read" = function(x) {
        x = as.character(x)
        # Only track reads that happen before writes.
        reads <<- union(reads, setdiff(x, writes))
      },

      "add_write" = function(x, conditional = FALSE) {
        x = as.character(x)
        if (conditional) {
          conditional_writes <<- union(conditional_writes, x)
        } else {
          writes <<- union(writes, x)
        }
      },

      "update" = function() {
        # A loop is sequential if it contains antidependences.
        #
        #   for (...) {
        #       = a
        #     a =
        #   }
        #
        # We only track reads that happen before writes, so:
        is_sequential <<- is_sequential ||
          any(reads %in% c(writes, conditional_writes))
      }

    )
  )


union = function(...) {
  unique(do.call(c, lapply(list(...), as.vector)))
}
