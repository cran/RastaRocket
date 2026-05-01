test_that("riskdifference: estimate = R1 - R0", {
  res <- RastaRocket::riskdifference(a = 20, b = 10, N1 = 100, N0 = 100)
  expect_equal(res$estimate, 20/100 - 10/100)
})

test_that("riskdifference: p.value entre 0 et 1", {
  res <- RastaRocket::riskdifference(a = 10, b = 5, N1 = 100, N0 = 100)
  expect_gte(res$p.value, 0)
  expect_lte(res$p.value, 1)
})
