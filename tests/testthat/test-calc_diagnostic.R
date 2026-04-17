# 診断精度（Buderer 1996）のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_diagnostic.R"))

test_that("Buderer の公式を直接評価した結果と一致", {
  Se <- 0.85; Sp <- 0.85; prev <- 0.10; E <- 0.05
  z  <- stats::qnorm(0.975)
  n_dis <- z^2 * Se * (1 - Se) / E^2
  n_non <- z^2 * Sp * (1 - Sp) / E^2
  N_exp <- max(n_dis / prev, n_non / (1 - prev))
  res <- calc_n_diagnostic(Se = Se, Sp = Sp, prev = prev,
                           half_width = E)
  expect_equal(res$n_per_arm_evaluable, ceiling(N_exp))
  expect_equal(res$n_arms, 1L)
  expect_true(is.na(res$achieved_power))
})

test_that("有病率が低いほど総例数は多い（疾患あり側が律速）", {
  lo <- calc_n_diagnostic(0.85, 0.85, prev = 0.05,
                          half_width = 0.05)$n_per_arm_evaluable
  hi <- calc_n_diagnostic(0.85, 0.85, prev = 0.30,
                          half_width = 0.05)$n_per_arm_evaluable
  expect_gt(lo, hi)
})

test_that("CI 半幅 E が小さいほど n は増える", {
  lo <- calc_n_diagnostic(0.85, 0.85, 0.10,
                          half_width = 0.10)$n_per_arm_evaluable
  hi <- calc_n_diagnostic(0.85, 0.85, 0.10,
                          half_width = 0.03)$n_per_arm_evaluable
  expect_gt(hi, lo)
})

test_that("Se=0.5 は Se=0.85 より分散が大きく n 大", {
  r50 <- calc_n_diagnostic(Se = 0.50, Sp = 0.85, prev = 0.10,
                           half_width = 0.05)$n_per_arm_evaluable
  r85 <- calc_n_diagnostic(Se = 0.85, Sp = 0.85, prev = 0.10,
                           half_width = 0.05)$n_per_arm_evaluable
  expect_gt(r50, r85)
})

test_that("prev の範囲外・Se 範囲外は拒否", {
  expect_error(calc_n_diagnostic(1.2, 0.85, 0.10, 0.05))
  expect_error(calc_n_diagnostic(0.85, 0.85, 0.0, 0.05))
  expect_error(calc_n_diagnostic(0.85, 0.85, 0.10, 0.0))
})
