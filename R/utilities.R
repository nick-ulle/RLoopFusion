# Description:
#   Utility functions.


#' Convert To Code Block
#'
#' This function converts a list of code objects to a single code block,
#' preserving the order of the list.
#'
#' @param x a list of code objects
#' @param f a function to apply to each code block before combining
#' @export
as_code_block = function(x, f) {
  use_f = !missing(f)

  block =
    lapply(x, function(obj) {
      if (use_f)
        obj = f(obj)

      if (class(obj) == "{")
        as.list(obj[-1])
      else
        obj
    })
  block = unlist(block, recursive = FALSE)

  as.call(append(as.name("{"), block))
}


#' Which Elements Contain The Value?
#'
#' This function returns the names of all elements that contain the specified
#' value.
#'
#' @param x a value
#' @param l a list
#' @export
in_which = function(x, l) {
  names(l)[
    vapply(l, function(elt) {
      x %in% elt
    }, TRUE)
  ]
}


#' Compute Union
#'
#' Computes the union of an arbitrary number of vectors.
#'
#' @param ... vectors to be unioned
union = function(...) {
  unique(do.call(c, lapply(list(...), as.vector)))
}
