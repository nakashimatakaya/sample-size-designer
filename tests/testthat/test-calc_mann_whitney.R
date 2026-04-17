# Mann-Whitney U 検定（ARE 補正）のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_mann_whitney.R"))

test_that("ARE テーブル: 正規=3/π, 指数=3, 対数正規=1.5", {
  expect_equal(are_for_distribution("normal"),      3 / pi)
  expect_equal(are_for_distribution("lognormal"),   1.5)
  expect_equal(are_for_distribution("exponential"), 3.0)
})

test_that("normal の必要症例数 = ceiling(n_t / (3/π))", {
  ref_t <- pwr::pwr.t.test(
    d = 0.5, sig.level = 0.05, power = 0.80,
    type = "two.sample", alternative = "two.sided"
  )
  res <- calc_n_mann_whitney(1, 0, 2, distribution = "normal",
                             alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_arm_evaluable, ceiling(ref_t$n / (3 / pi)))
})

test_that("指数分布は正規より n が小さい（ARE が大きい）", {
  n_norm <- calc_n_mann_whitney(1, 0, 2, "normal",
                                alpha = 0.05)$n_per_arm_evaluable
  n_exp  <- calc_n_mann_whitney(1, 0, 2, "exponential",
                                alpha = 0.05)$n_per_arm_evaluable
  expect_gt(n_norm, n_exp)
})

test_that("未知の分布は拒否", {
  expect_error(calc_n_mann_whitney(1, 0, 2, "weird", alpha = 0.05))
})

test_that("脱落率 0.10 で n_randomized が切り上がる", {
  res <- calc_n_mann_whitney(1, 0, 2, "normal",
                             alpha = 0.05, power = 0.80, dropout = 0.10)
  expected_rand <- ceiling(res$n_per_arm_evaluable / 0.9)
  expect_equal(res$n_per_arm_randomized, expected_rand)
})
