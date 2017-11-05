context("Number of observation per month for the given year")

fpath <- system.file("extdata", package = "FARS")
setwd(fpath)

test_that("For 2015, 2014, 2013", {
  expect_equal(unname(unlist(fars_summarize_years(2015)[, 2])), c(2368, 1968, 2385, 2430, 2847, 2765, 2998, 3016, 2865, 3019, 2724, 2781))
  expect_equal(unname(unlist(fars_summarize_years(2014)[, 2])), c(2168, 1893, 2245, 2308, 2596, 2583, 2696, 2800, 2618, 2831, 2714, 2604))
  expect_equal(unname(unlist(fars_summarize_years(2013)[, 2])), c(2230, 1952, 2356, 2300, 2532, 2692, 2660, 2899, 2741, 2768, 2615, 2457))
})

