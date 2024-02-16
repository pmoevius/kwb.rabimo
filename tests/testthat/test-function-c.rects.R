test_that("c.rects() works", {

  f <- kwb.rabimo:::c.rects

  r1 <- kwb.rabimo:::new_rects(1)
  r2 <- kwb.rabimo:::new_rects(2)

  testthat::expect_identical(f(r1, r2), rbind(r1, r2))
})
