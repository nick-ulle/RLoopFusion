# Description:
#   Tests for the greedy partition function.

context("greedy partitions")


test_that("correct partitions for Figure 2(b) from Kennedy", {
  graph = new_fusion_graph()
  graph = addNode(c("p3", "p4", "p7", "p8"), graph)
  graph = add_fp_edge("p4", "p7", graph)
  graph = add_fp_edge("p4", "p8", graph)

  result = greedy_partition(graph)

  expect_equal_set(result[[1]], c("p3", "p4"))
  expect_equal_set(result[[2]], c("p7", "p8"))
})


test_that("correct partitions for Figure 2(e) from Kennedy", {
  graph = new_fusion_graph()
  graph = addNode(c("s1", "s2", "s5", "s6"), graph)
  graph = add_fp_edge("s1", c("s5", "s6"), graph)
  graph = add_fp_edge("s2", c("s5", "s6"), graph)

  result = greedy_partition(graph)

  expect_equal_set(result[[1]], c("s1", "s2"))
  expect_equal_set(result[[2]], c("s5", "s6"))
})
