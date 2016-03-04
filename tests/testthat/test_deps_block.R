# Description:
#   Test dependency collection for code blocks.

context("deps for code blocks")


test_that("dependencies are collected for {}", {
  expression = 
    quote({
      x = y
      z
    })

  result = collect_deps(expression)

  expect_equal_set(result$reads, c("y", "z"))
  expect_equal_set(result$writes, "x")
})


test_that("dependencies are collected for {} with multiple assignments", {
  expression = 
    quote({
      a1 = 5
      a2
      a3 = 10
      a4 = 9
      a5
    })

  result = collect_deps(expression)

  expect_equal_set(result$reads, c("a2", "a5"))
  expect_equal_set(result$writes, c("a1", "a3", "a4"))
})


test_that("only external dependencies are collected", {
  expression = 
    quote({
      a1 = 4
      a2 = a1
    })

  result = collect_deps(expression)

  expect_equal_set(result$reads, character(0))
  expect_equal_set(result$writes, c("a1", "a2"))
})
