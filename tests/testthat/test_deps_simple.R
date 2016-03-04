# Description:
#   Test dependency collection for simple assignments.

context("deps for assignment")


test_that("simple = writes are collected", {
  expression = call("=", quote(x), 3)

  result = collect_deps(expression)

  expect_equal_set(result$reads, character(0))
  expect_equal_set(result$writes, "x")
})


test_that("simple <- writes are collected", {
  expression = call("<-", quote(y), "hello")

  result = collect_deps(expression)

  expect_equal_set(result$reads, character(0))
  expect_equal_set(result$writes, "y")
})


test_that("simple = reads are collected", {
  expression = call("=", quote(x), quote(y))

  result = collect_deps(expression)

  expect_equal_set(result$reads, "y")
  expect_equal_set(result$writes, "x")
})


test_that("simple <- reads are collected", {
  expression = call("=", quote(a1), quote(a2))

  result = collect_deps(expression)

  expect_equal_set(result$reads, "a2")
  expect_equal_set(result$writes, "a1")
})
