# Description:
#   Test detection of loop-carried dependencies.

context("loop-carried deps")


test_that("simple parallel loop is parallel", {
  expression = quote(
    for (i in 1:10) {
      a = i
      b = a
    }
  )

  result = collect_deps(expression)

  expect_false(result$antidep)
})


test_that("sequential loop is sequential", {
  expression = quote(
    for (i in 1:10) {
      a = a + 1
    }
  )

  result = collect_deps(expression)

  expect_true(result$antidep)
})


test_that("parallel loop is parallel", {
  expression = quote(
    for (i in 1:10) {
      if (i == 1)
        a = 3

      a = 5
      b = a
    }
  )

  result = collect_deps(expression)

  expect_false(result$antidep)
})


# Here we fool the dependency algorithm with a write followed by a read. This
# is sequential, since the write only happens in the first iteration. How can
# we detect this in general?
#
# What matters is that the write is conditional, so it might not take place.
# So a write shouldn't "count" for future reads unless it's unconditional or in
# every branch of a conditional.
#
# We still need to keep track of conditional writes, though, because subsequent
# loops need to know about them. That is, assume they do take place in some
# iteration of the loop.
#
test_that("sequential loop with conditional is sequential", {
  # Every iteration depends on the first iteration.
  expression = quote(
    for (i in 1:10) {
      if (i == 1)
        a = 42

      b = a
    }
  )

  result = collect_deps(expression)

  expect_true(result$antidep)
})


test_that("parallel loop with conditional is parallel", {
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
  
  expect_false(result$antidep)
})


test_that("parallel loop with nested conditional is parallel", {
  expression = quote({
    if (i == 1)
      if (j == 2) a = 3
    else
      a = 4

    b = a
  })

  result = collect_deps(expression)

  expect_true(result$antidep)
})
