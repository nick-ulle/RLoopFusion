# Description:
#   Test dependency collection for arrays.

context("deps for arrays")


test_that("write dependencies are collected for arrays", {
  expression = call("=", quote(x[1]), quote(y))

  result = collect_deps(expression)

  expect_equal_set(result$reads, c("x", "y"))
  expect_equal_set(result$writes, "x")
})


test_that("read dependencies are collected for arrays", {
  expression = call("=", quote(x), quote(y[i]))

  result = collect_deps(expression)

  expect_equal_set(result$reads, c("y", "i"))
  expect_equal_set(result$writes, "x")
})

