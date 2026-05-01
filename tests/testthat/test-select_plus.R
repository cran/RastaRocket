test_that("select_plus sans var_group équivaut à select", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  expect_equal(
    RastaRocket::select_plus(df, x, y), dplyr::select(df, x, y)
  )
})

test_that("select_plus avec var_group inclut la variable de groupe", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  out <- RastaRocket::select_plus(df, x, var_group = "z")
  expect_equal(names(out), c("x", "z"))
  expect_equal(out$x, 1:3)
  expect_equal(out$z, 7:9)
})
