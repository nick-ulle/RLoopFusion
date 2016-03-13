# Description:
#   Test detection of loop-carried dependencies.

context("loop-carried deps")


test_that("loop-carried deps aren't detected in parallel loops", {
  expression = quote(
    for (i in 1:10) {
      a = i
      b = a
    }
  )

  result = collect_deps(expression)

  expect_false(result$is_sequential)
})


test_that("loop-carried deps are detected", {
  expression = quote(
    for (i in 1:10) {
      a = a + 1
    }
  )

  result = collect_deps(expression)

  expect_true(result$is_sequential)
})


# TODO: Make this test pass.
#
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
test_that("loop-carried deps in conditionals are detected", {
  # Every iteration depends on the first iteration.
  expression = quote(
    for (i in 1:10) {
      if (i == 1)
        a = 42

      b = a
    }
  )

  result = collect_deps(expression)

  expect_true(result$is_sequential)
})
