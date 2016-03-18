# Description:
#   Functions for collecting dependences in a block of code.


#' Collect variable dependence information for a code block.
#'
#' @export
collect_deps = function(x) {
  collector = DependenceCollector(code = x)
  .collect_deps(x, collector)
  collector$update_loop_type()

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
      state$add_reads(left[[2]])

    state$add_writes(left[[2]])
    lapply(left[-(1:2)], .collect_deps, state)

  } else if (class(left) == "name") {
    state$add_writes(left)

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

  if_deps = DependenceCollector()
  .collect_deps(x[[3]], if_deps)
  # FIXME: Using get_reads() ignores within-branch dependences.
  reads = if_deps$get_reads()
  if_writes = if_deps$get_writes()

  if (length(x) == 3) {
    conditional_writes = if_writes

  } else {
    else_deps = DependenceCollector()
    .collect_deps(x[[4]], else_deps)
    else_writes = else_deps$get_writes()

    reads = union(reads, else_deps$get_reads())

    # A write that appears in both branches is unconditional.
    writes = intersect(if_writes, else_writes)

    # All other writes are conditional.
    conditional_writes = setdiff(union(if_writes, else_writes), writes)

    state$add_writes(writes)
  }

  state$add_reads(reads)
  state$add_writes(conditional_writes, uncond = FALSE)
}


.collect_deps.call = function(x, state) {
  # NOTE: This is separate because special handling of arrays will be needed.

  # function [arg 1] [arg 2] ...
  lapply(x[-1], .collect_deps, state)
}


.collect_deps.for = function(x, state) {
  # for [variable] [range] [body]
  state$add_writes(x[[2]])
  
  lapply(x[-(1:2)], .collect_deps, state)
}


.collect_deps.while =   # while [condition] [body]
.collect_deps.repeat =  # repeat [body]
`.collect_deps.{` =     # { [line 1] [line 2] ...
  function(x, state) {
    lapply(x[-1], .collect_deps, state)
  }


.collect_deps.name = function(x, state) {
  state$add_reads(x)
}


.collect_deps.default = function(x, state) {
  # Do nothing.
}


#' Collector for dependences.
DependenceCollector =
  setRefClass("DependenceCollector",
    fields = list(
      "dependence"  = "character",
      ".serial" = "logical",
      "loop_type" = "character",
      "code" = "ANY",
      ".i_var" = "character"
    ),
    methods = list(
      "add_reads" = function(x) {
        x = as.character(x)
        dependence[x] <<- update_with_read(dependence[x])
      },

      "add_writes" = function(x, uncond = TRUE) {
        x = as.character(x)
        dependence[x] <<- update_with_write(dependence[x])
        .serial[x] <<- update_serial(.serial[x], dependence[x], uncond)
      },

      "get_vars" = function() {
        names(dependence)
      },
      "get_reads" = function() {
        names(dependence)[dependence %in% c("input", "anti")]
      },
      "get_writes" = function() {
        names(dependence)[dependence %in% c("output", "anti", "true")]
      },
      "get_anti" = function() {
        names(dependence)[dependence == "anti"]
      },
      "get_serial" = function() {
        update_loop_type()
        names(.serial)[.serial]
      },
      "get_i_var" = function() {
        update_loop_type()
        .i_var
      },

      "update_loop_type" = function() {
        # FIXME: Can this be cleaned up?
        serial = names(.serial)[.serial]
        deps = dependence[serial]
        parallel = names(deps)[deps == "output"]
        .serial[parallel] <<- FALSE

        # A loop is serial if it has a read before a write. That is:
        #   1. Antidependence
        #   2. True dependence where the write is conditional
        if (class(code) == "for") {
          .i_var <<- as.character(code[[2]])
          loop_type <<- 
            if (any(.serial))
              "serial"
            else
              "parallel"
        } else {
          loop_type <<- "none"
        }
      }
    ) # end methods
  ) # end setRefClass()


update_serial = function(serial, dep, uncond) {
  # Assume loops are serial...
  serial[is.na(serial)] = TRUE

  # ...unless there's an unconditional write before any reads.
  serial[uncond & (dep == "output")] = FALSE

  return(serial)
}


update_with_read = function(dep) {
  new_dep = character(length(dep))
  dep[is.na(dep)] = "none"

  new_dep[dep == "none"]    = "input"
  new_dep[dep == "input"]   = "input"
  new_dep[dep == "output"]  = "true"
  new_dep[dep == "anti"]    = "anti"  # in the RbW sense
  new_dep[dep == "true"]    = "true"

  return(new_dep)
}


update_with_write = function(dep) {
  new_dep = character(length(dep))
  dep[is.na(dep)] = "none"

  new_dep[dep == "none"]    = "output"
  new_dep[dep == "input"]   = "anti"
  new_dep[dep == "output"]  = "output" 
  new_dep[dep == "anti"]    = "anti" 
  new_dep[dep == "true"]    = "true"  # in the WbR sense

  return(new_dep)
}
