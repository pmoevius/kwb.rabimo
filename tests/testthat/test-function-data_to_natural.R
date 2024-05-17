#library(testthat)

test_that("data_to_natural() works", {

  f <- kwb.rabimo::data_to_natural

  expect_error(f())

  expect_error(
    expect_output(f(data.frame()), "make sure .*all required columns")
  )

  data <- kwb.rabimo::rabimo_inputs_2020$data %>%
    kwb.rabimo:::convert_data_types(
      data_types = kwb.rabimo:::get_expected_data_types(),
      dbg = FALSE
    )

  result <- f(data)
})
