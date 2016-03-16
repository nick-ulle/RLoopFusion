# Description:
#   Test component graph generation (mainly the necessary_edges function).

context("component graph")


test_that("correct component graph for Figure 2(a) from Kennedy", {
  graph = new_fusion_graph()
  graph = addNode(c("s1", "s2", "p3", "p4", "s5", "s6", "p7", "p8"), graph)
  nodeData(graph, c("s1", "s2", "s5", "s6"), "type") = "sequential"
  nodeData(graph, c("p3", "p4", "p7", "p8"), "type") = "parallel"

  graph = addEdge(c("s1", "s2"), "p4", graph)
  graph = add_fp_edge("p3", "s5", graph)
  graph = addEdge("p4", c("s6", "p7"), graph)
  graph = addEdge("s6", c("p7", "p8"), graph)

  result = subgraph(graph, c("p3", "p4", "p7", "p8"))
  result = necessary_edges(graph, result, "parallel")
  
  expect_true( all(isAdjacent(result, "p4", c("p7", "p8"))) )
  expect_true( all(edge_attr(result, "prevent_fusion")) )
})


test_that("correct component graph for Figure 2(d) from Kennedy", {
  # Figure 2(d) from Kennedy.
  graph = new_fusion_graph()
  graph = addNode(c("s1", "s2", "p3p4", "s5", "s6", "p7p8"), graph)
  nodeData(graph, c("s1", "s2", "s5", "s6"), "type") = "sequential"
  nodeData(graph, c("p3p4", "p7p8"), "type") = "parallel"

  graph = addEdge(c("s1", "s2"), "p3p4", graph)
  graph = add_fp_edge("p3p4", c("s5", "p7p8"), graph)
  graph = addEdge("p3p4", "s6", graph)
  graph = addEdge("s6", "p7p8", graph)

  result = subgraph(graph, c("s1", "s2", "s5", "s6"))
  result = necessary_edges(graph, result, "sequential")

  expect_true( all(isAdjacent(result, c("s1", "s2"), c("s5", "s6"))) )
  expect_true( all(edge_attr(result, "prevent_fusion")) )
})
