# Description:
#   Test dependency collection for control flow statements.

context("deps for control flow")


test_that("dependencies are collected for simple if", {
  expression = quote(
    if (x < 3) a1 = 3
    else a2 = 3
  )

  result = collect_deps(expression)

  expect_set_equal(result$dependence, c(
      x = "input", a1 = "output", a2 = "output"
  ))
})


test_that("dependencies are collected for blocked if-else", {
  expression = quote(
    if (x1 >= 10) {
      a1 = 2
    } else if (x2 != 20) {
      a2 = 2
    } else {
      a3 = z
    }
  )

  result = collect_deps(expression)

  expect_set_equal(result$dependence, c(
      x1 = "input", a1 = "output", x2 = "input", a2 = "output", a3 = "output",
      z = "input"
  ))
})


test_that("dependencies are collected for blocked for", {
  expression = quote(
    for (i in 1:n) {
      x = i + y
    }
  )

  result = collect_deps(expression)

  expect_set_equal(result$get_reads(), c("n", "y"))
  expect_set_equal(result$get_writes(), c("i", "x"))
})


test_that("dependencies are collected for blocked while", {
  expression = quote(
    while (x < 3) {
      x = x + 1
      y = y - 1
    }
  )

  result = collect_deps(expression)

  expect_set_equal(result$get_reads(), c("x", "y"))
  expect_set_equal(result$get_writes(), c("x", "y"))
})


test_that("dependencies are collected for blocked repeat", {
  expression = quote(
    repeat {
      x = x + 1
      if (x > 3)
        break
    }
  )

  result = collect_deps(expression)

  expect_set_equal(result$get_reads(), c("x"))
  expect_set_equal(result$get_writes(), c("x"))
})
