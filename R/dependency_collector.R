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
    state$add_write(left[[2]])
    lapply(left[-(1:2)], .collect_deps, state)

  } else if (class(left) == "name") {
    state$add_write(left)

  } else {
    msg = sprintf("Unsupported left side '%s' in assignment.", class(left))
    stop(msg)
  }
}


.collect_deps.name = function(x, state) {
  x = as.character(x)
  # If the variable was written to in this block, then subsequent reads are not
  # external dependencies.
  if (!x %in% state$writes) state$add_read(x)
}


.collect_deps.for = function(x, state) {
  # for [variable] [range] [body]
  state$add_write(x[[2]])
  
  lapply(x[-(1:2)], .collect_deps, state)
}


.collect_deps.call = function(x, state) {
  # NOTE: This is separate because special handling of arrays will be needed.

  # function [arg 1] [arg 2] ...
  lapply(x[-1], .collect_deps, state)
}


.collect_deps.if = function(x, state) {
  # if [condition] [if] [else]
  state$in_conditional = TRUE
  lapply(x[-1], .collect_deps, state)
  state$in_conditional = FALSE
}


.collect_deps.while =   # while [condition] [body]
.collect_deps.repeat =  # repeat [body]
`.collect_deps.{` =     # { [line 1] [line 2] ...
  function(x, state) {
    lapply(x[-1], .collect_deps, state)
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
        if (!x %in% reads)
          reads <<- c(reads, x)
      },

      "add_write" = function(x) {
        x = as.character(x)
        if (in_conditional) {
          if (!x %in% conditional_writes) 
            conditional_writes <<- c(conditional_writes, x)
        } else {
          if (!x %in% writes)
            writes <<- c(writes, x)
        }
      },

      "update" = function() {
        is_sequential <<- is_sequential ||
          any(reads %in% c(writes, conditional_writes))
      }

    )
  )
