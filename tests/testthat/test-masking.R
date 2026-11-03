# Loading all course packages at once means some functions share a name.
# VGAM also exports tobit() and lrtest(), which would silently replace the
# AER and lmtest versions the lecture scripts use. VGAM is therefore attached
# first in DESCRIPTION, so the others win. These tests guard that order.

fun_home <- function(f) environmentName(environment(get(f, mode = "function")))

test_that("tobit() is AER's, not VGAM's", {
  expect_identical(fun_home("tobit"), "AER")
})

test_that("lrtest() is lmtest's, not VGAM's", {
  expect_identical(fun_home("lrtest"), "lmtest")
})

test_that("glm.nb() and dispersiontest() come from MASS and AER", {
  expect_identical(fun_home("glm.nb"), "MASS")
  expect_identical(fun_home("dispersiontest"), "AER")
})
