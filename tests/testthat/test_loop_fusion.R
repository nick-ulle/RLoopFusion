# Description:
#   Tests for the loop fusion function.

context("loop fusion")


test_that("", {
  expression = quote({
    for (i in 1:10) {
      a = i
    }
    for (i in 1:10) {
      b = i
    }
  })

  result = loop_fusion(expression)
  
  expect_equal(result, quote({
    for (i in 1:10) {
      a = i
      b = i
    }
  }))
})


test_that("", {
  expression = quote({
    for (i in 1:10) {
      a = i
    }
    b = 42
    for (i in 1:10) {
      b = i
    }
  })

  result = loop_fusion(expression)

  expect_equal(result, quote({
    b = 42
    for (i in 1:10) {
      a = i
      b = i
    }
  }))
})


test_that("", {
  expression = quote({
    for (i in 1:10) {
      a = i
    }
    a = 42
    for (i in 1:10) {
      b = i
    }
    for (i in 1:10) {
      x = a
    }
  })

  result = loop_fusion(expression)

  expect_equal(result, quote({
    for (i in 1:10) {
      a = i
      b = i
    }
    a = 42
    for (i in 1:10) {
      x = a
    }
  }))
})
