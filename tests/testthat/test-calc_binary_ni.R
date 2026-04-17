# 非劣性・二値（Chow 2018 Sec 4.2 近似）のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_binary_ni.R"))

test_that("Chow 2018 Sec 4.2 の公式を直接展開した値と一致する", {
  p_A <- 0.7; p_B <- 0.7; M <- 0.1
  alpha <- 0.025; power <- 0.80
  z_a <- stats::qnorm(1 - alpha)
  z_b <- stats::qnorm(power)
  num <- (z_a + z_b)^2 * (p_A * (1 - p_A) + p_B * (1 - p_B))
  den <- (p_A - p_B + M)^2
  n_ref <- ceiling(num / den)
  res <- calc_n_binary_ni(p_A, p_B, margin = M,
                          alpha = alpha, power = power)
  expect_equal(res$n_per_arm_evaluable, n_ref)
  expect_equal(res$n_arms, 2L)
})

test_that("マージン M を大きくすると必要症例数は減る", {
  n_small <- calc_n_binary_ni(0.7, 0.7, margin = 0.05,
                              alpha = 0.025)$n_per_arm_evaluable
  n_large <- calc_n_binary_ni(0.7, 0.7, margin = 0.15,
                              alpha = 0.025)$n_per_arm_evaluable
  expect_gt(n_small, n_large)
})

test_that("脱落率を考慮した n_randomized が切り上がる", {
  res <- calc_n_binary_ni(0.7, 0.7, margin = 0.1,
                          alpha = 0.025, dropout = 0.10)
  expect_equal(res$n_per_arm_randomized,
               ceiling(res$n_per_arm_evaluable / 0.9))
})

test_that("警告メッセージがついている（Farrington-Manning 推奨）", {
  res <- calc_n_binary_ni(0.7, 0.7, margin = 0.1, alpha = 0.025)
  expect_true("warning" %in% names(res))
  expect_match(res$warning, "Farrington-Manning")
})
