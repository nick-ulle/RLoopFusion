# Description:
#   Test dependency collection for simple assignments.

context("deps for assignment")


test_that("simple = writes are collected", {
  expression = call("=", quote(x), 3)

  result = collect_deps(expression)

  expect_equal_set(result$get_reads(), character(0))
  expect_equal_set(result$get_writes(), "x")
})


test_that("simple <- writes are collected", {
  expression = call("<-", quote(y), "hello")

  result = collect_deps(expression)

  expect_equal_set(result$get_reads(), character(0))
  expect_equal_set(result$get_writes(), "y")
})


test_that("simple = reads are collected", {
  expression = call("=", quote(x), quote(y))

  result = collect_deps(expression)

  expect_equal_set(result$get_reads(), "y")
  expect_equal_set(result$get_writes(), "x")
})


test_that("simple <- reads are collected", {
  expression = call("=", quote(a1), quote(a2))

  result = collect_deps(expression)

  expect_equal_set(result$get_reads(), "a2")
  expect_equal_set(result$get_writes(), "a1")
})
