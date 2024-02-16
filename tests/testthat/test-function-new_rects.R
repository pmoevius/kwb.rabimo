test_that("new_rects() works", {

  f <- kwb.rabimo:::new_rects

  rects <- f()

  expect_s3_class(rects, "rects")
})
