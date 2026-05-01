test_that("custom_round arrondit correctement avec 1 décimale par défaut", {
  expect_equal(custom_round(3.14159), "3.1")
  expect_equal(custom_round(2.5), "2.5")
  expect_equal(custom_round(0), "0.0")
})



test_that("custom_round respecte le nombre de digits", {
  expect_equal(RastaRocket::custom_round(3.14159, 0), "3")
  expect_equal(RastaRocket::custom_round(3.14159, 2), "3.14")
  expect_equal(RastaRocket::custom_round(3.14159, 3), "3.142")
  expect_equal(RastaRocket::custom_round(2, 2), "2.00")
  expect_equal(RastaRocket::custom_round(2.5, 2), "2.50")
})

