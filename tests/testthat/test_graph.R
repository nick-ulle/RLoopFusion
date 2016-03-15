# Description:
#   Test graph generation.

context("graph generation")


test_that("incompatible loops make FPEs", {
  expression = quote({
    for (i in 1:10)
      a = i
    for (i in 1:8)
      a = i
  })

  result = fuse_loops(expression)

  expect_true(edgeData(result, attr = "prevent_fusion")[["for1|for2"]])
})


test_that("existing antidependence makes FPEs", {
  # Case 1:
  expression = quote({
    for (i in 1:10)
      a = a + 1
    for (i in 1:10)
      a = i
  })

  result = fuse_loops(expression)

  expect_true(edgeData(result, attr = "prevent_fusion")[["for1|for2"]])

  # Case 2:
  expression = quote({
    for (i in 1:10)
      a = i
    for (i in 1:10)
      a = a + 1
  })

  result = fuse_loops(expression)

  expect_true(edgeData(result, attr = "prevent_fusion")[["for1|for2"]])
})


test_that("introduced antidependence makes FPEs", {
  expression = quote({
    for (i in 1:10)
      x = a
    z = 3
    for (i in 1:10)
      a = i
  })

  result = fuse_loops(expression)

  expect_true(edgeData(result, attr = "prevent_fusion")[["for1|for3"]])
})


test_that("introduced true dependence doesn't make FPEs", {
  expression = quote({
    for (i in 1:10)
      a = i
    for (i in 1:10)
      x = a
  })

  result = fuse_loops(expression)

  expect_false(edgeData(result, attr = "prevent_fusion")[["for1|for2"]])
})


test_that("introduced output dependence doesn't make FPEs", {
  expression = quote({
    for (i in 1:10)
      a = i
    for (i in 1:10)
      a = i^2
  })

  result = fuse_loops(expression)

  expect_null(edgeData(result, attr = "prevent_fusion")[["for1|for2"]])
})


test_that("introduced input dependence doesn't make FPEs", {
  expression = quote({
    for (i in 1:10)
      x = a
    for (i in 1:10)
      y = a
  })

  result = fuse_loops(expression)

  expect_null(edgeData(result, attr = "prevent_fusion")[["for1|for2"]])
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
