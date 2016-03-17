# Description:
#   Test detection of loop-carried dependencies.

context("loop-carried deps")


test_that("true dependence implies parallel loop", {
  expression = quote(
    for (i in 1:10) {
      a = i
      b = a
    }
  )

  result = collect_deps(expression)

  expect_equal(result$loop_type, "parallel")
})


test_that("antidependence implies serial loop", {
  expression = quote(
    for (i in 1:10) {
      a = a + 1
    }
  )

  result = collect_deps(expression)

  expect_equal(result$loop_type, "serial")
})


test_that("conditional write before true dependence implies parallel loop", {
  expression = quote(
    for (i in 1:10) {
      if (i == 1)
        a = 3

      a = 5
      b = a
    }
  )

  result = collect_deps(expression)

  expect_equal(result$loop_type, "parallel")
})


test_that("true dependence in conditional implies parallel loop", {
  expression = quote(
    for (i in 1:10) {
      if (i == 3) {
        a = 5
        x = a
      }
    }
  )

  result = collect_deps(expression)

  expect_equal(result$loop_type, "parallel")
})


test_that("conditional write before read implies serial loop", {
  # Every iteration depends on the first iteration.
  expression = quote(
    for (i in 1:10) {
      if (i == 1)
        a = 42

      b = a
    }
  )

  result = collect_deps(expression)

  expect_equal(result$loop_type, "serial")
})


test_that("write in all branches before read implies parallel loop", {
  expression = quote(
    for (i in 1:n) {
      if (i == 1)
        a = 1
      else if (i == 2)
        a = 2
      else
        a = 3

      b = a
    }
  )

  result = collect_deps(expression)
  
  expect_equal(result$loop_type, "parallel")
})


test_that("true dependence with nested conditional implies parallel loop", {
  expression = quote(
    for (i in 1:n) {
      if (i == 1)
        if (j == 1) a = 1
        else a = 2
      else
        a = 3

      b = a
    }
  )

  result = collect_deps(expression)

  expect_equal(result$loop_type, "parallel")
})
