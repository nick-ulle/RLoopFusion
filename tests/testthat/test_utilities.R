# Description:
#   Tests for utility functions.

context("utilities")


test_that("as_code_block combines code objects correctly", {
  input = list(
    quote({
      a = 1
      b = 2
    }),
    call("=", quote(c), 3),
    quote(
      for (i in 1:10) {
        x = i
      }
    ),
    call("{")
  )

  result = as_code_block(input)

  expect_equal(result, quote({
    a = 1
    b = 2
    c = 3
    for (i in 1:10) {
      x = i
    }
  }))
})


test_that("in_which works correctly", {
  input = list(
    v = list(a = 1, b = 2),
    w = c(),
    x = c(1, 2, 3),
    y = c("a", "b"),
    z = c("1", "2")
  )

  result = in_which(1, input)

  expect_equal(result, c("v", "x", "z"))
})
