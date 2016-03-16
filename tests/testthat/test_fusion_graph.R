# Description:
#   Test fusion graph generation.

context("fusion graph")


test_that("nodes track loop type", {
  expression = quote({
    for (i in 1:5)
      a = i
    x = 3
    for (i in 1:5)
      b = b + 1
  })

  result = fusion_graph(expression)$graph
  result = nodeData(result, attr = "type")

  expect_equal(result[[1]], "parallel")
  expect_equal(result[[2]], "none")
  expect_equal(result[[3]], "sequential")
})


test_that("incompatible loops make FPEs", {
  expression = quote({
    for (i in 1:10)
      a = i
    for (i in 1:8)
      a = i
  })

  result = fusion_graph(expression)$graph

  expect_true(edgeData(result, attr = "prevent_fusion")[["p1|p2"]])
})


test_that("existing antidependence makes FPEs", {
  # Case 1:
  expression = quote({
    for (i in 1:10)
      a = a + 1
    for (i in 1:10)
      a = i
  })

  result = fusion_graph(expression)$graph

  expect_true(edgeData(result, attr = "prevent_fusion")[["s1|p2"]])

  # Case 2:
  expression = quote({
    for (i in 1:10)
      a = i
    for (i in 1:10)
      a = a + 1
  })

  result = fusion_graph(expression)$graph

  expect_true(edgeData(result, attr = "prevent_fusion")[["p1|s2"]])
})


test_that("introduced antidependence makes FPEs", {
  expression = quote({
    for (i in 1:10)
      x = a
    z = 3
    for (i in 1:10)
      a = i
  })

  result = fusion_graph(expression)$graph

  expect_true(edgeData(result, attr = "prevent_fusion")[["p1|p3"]])
})


test_that("introduced true dependence doesn't make FPEs", {
  expression = quote({
    for (i in 1:10)
      a = i
    for (i in 1:10)
      x = a
  })

  result = fusion_graph(expression)$graph

  expect_false(edgeData(result, attr = "prevent_fusion")[["p1|p2"]])
})


test_that("introduced output dependence doesn't make FPEs", {
  expression = quote({
    for (i in 1:10)
      a = i
    for (i in 1:10)
      a = i^2
  })

  result = fusion_graph(expression)$graph

  expect_equal_set(nodes(result), c("p1", "p2"))
  expect_null(edgeData(result, attr = "prevent_fusion")[["p1|p2"]])
})


test_that("introduced input dependence doesn't make FPEs", {
  expression = quote({
    for (i in 1:10)
      x = a
    for (i in 1:10)
      y = a
  })

  result = fusion_graph(expression)$graph

  expect_equal_set(nodes(result), c("p1", "p2"))
  expect_null(edgeData(result, attr = "prevent_fusion")[["p1|p2"]])
})


test_that("", {
  expression = quote({
    a = numeric(10)

    for (i in 1:10) {
      a[i] = i
    }

    x = 3
    b = 4

    for (i in 1:10) {
      a[i] = a[i]^2
    }
  })
})
