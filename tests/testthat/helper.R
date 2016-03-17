# Description:
#   Helper functions for testing.


# Test equality of two vectors as sets.
#
# This test checks that two vectors have the same length and elements, up to
# ordering.
expect_set_equal = expect_equal_set = function(x, y) {
  eval(bquote(
    expect_true(setequal(.(x), .(y)))
  ))
}
