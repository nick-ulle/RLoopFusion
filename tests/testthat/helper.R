# Description:
#   Helper functions for testing.


# Test equality of two vectors as sets.
#
# This test checks that two vectors have the same length and elements, up to
# ordering.
expect_equal_set = function(x, y) {
  eval(bquote(
    expect_equal( length(.(x)), length(.(y)) )
  ))
  eval(bquote(
    expect_true(all(.(x) %in% .(y)))
  ))
}
