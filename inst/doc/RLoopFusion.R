## ---- results = "hide"---------------------------------------------------
library(RLoopFusion)

## ---- results = "hide"---------------------------------------------------
ex1 = quote({
  a = b = c = d = 0
  for (i in 2:10)
    a = i
  for (i in 1:10)
    b = i
  for (i in 1:10)
    c = i
  for (i in 1:10)
    d = d + 1
})

## ------------------------------------------------------------------------
loop_fusion(ex1)

## ---- results = "hide"---------------------------------------------------
graph = fusion_graph(ex1)

## ------------------------------------------------------------------------
library(graph)
library(Rgraphviz)
plot(graph$graph)

## ------------------------------------------------------------------------
edge_attr(graph$graph, "prevent_fusion")

## ------------------------------------------------------------------------
collect_deps(ex1)

## ------------------------------------------------------------------------
ex2 = quote({
  n = 42

  x = 0
  for (i in 1:n) {
    # A parallel loop.
    x = i
  }

  a = b = 1
  for (i in 1:n) {
    # A serial loop that computes Fibonacci numbers.
    fib = a + b
    a = b
    b = fib
  }

  y = 0
  for (i in 1:n) {
    # A parallel loop that depends on the Fibonacci loop.
    y = fib
  }
})

## ------------------------------------------------------------------------
loop_fusion(ex2)

## ---- echo = FALSE-------------------------------------------------------
plot(fusion_graph(ex2)$graph)

