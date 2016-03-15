# Description:
#   Test the blockify function.

context("blockify")


test_that("blockify handles empty blocks", {
  expression = quote({})

  result = blockify(expression, "for")
  
  expect_equal(length(result), 0)
  expect_is(result, "list")
})


test_that("blockify correctly makes blocks", {
  expression = quote({
    if (x < 3) {
      x = 3
    }

    a = 6
    b = 7

    for (i in 1:10) {
      a = a + b
    }

    z = 10
  })

  result = blockify(expression, "if")

  expect_equal(length(result), 2)
  expect_equal(vapply(result, class, ""), c("if", "{"))
})
