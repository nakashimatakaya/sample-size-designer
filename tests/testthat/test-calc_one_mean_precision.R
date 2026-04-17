# 1 標本・平均・精度ベースのテスト。
# 公式: n = (z_{1-α/2} * SD / E)^2 を切り上げ。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_one_mean_precision.R"))

test_that("n = (z * SD / E)^2 を正しく切り上げる（sd=10, E=2, 95%）", {
  # z_{0.975} ≈ 1.959964 → n = (1.96*10/2)^2 ≈ 96.04 → 97
  res <- calc_n_one_mean_precision(sd = 10, half_width = 2,
                                   conf_level = 0.95)
  z <- stats::qnorm(0.975)
  expect_equal(res$n_per_arm_evaluable, ceiling((z * 10 / 2)^2))
  expect_equal(res$n_total_evaluable, res$n_per_arm_evaluable)  # 1 標本
  expect_equal(res$n_arms, 1L)
  expect_true(is.na(res$achieved_power))   # 検出力なし
})

test_that("信頼水準を上げると必要症例数は増える", {
  n_95 <- calc_n_one_mean_precision(sd = 10, half_width = 2,
                                    conf_level = 0.95)$n_per_arm_evaluable
  n_99 <- calc_n_one_mean_precision(sd = 10, half_width = 2,
                                    conf_level = 0.99)$n_per_arm_evaluable
  expect_gt(n_99, n_95)
})

test_that("半幅 E を小さくすると必要症例数は増える", {
  n_big  <- calc_n_one_mean_precision(sd = 10, half_width = 3)$n_per_arm_evaluable
  n_small <- calc_n_one_mean_precision(sd = 10, half_width = 1)$n_per_arm_evaluable
  expect_gt(n_small, n_big)
})

test_that("脱落率を考慮した n_randomized が切り上がる", {
  res <- calc_n_one_mean_precision(sd = 10, half_width = 2,
                                   conf_level = 0.95, dropout = 0.10)
  expect_equal(res$n_per_arm_randomized,
               ceiling(res$n_per_arm_evaluable / 0.9))
})

test_that("不正な入力は落ちる", {
  expect_error(calc_n_one_mean_precision(sd = -1, half_width = 2))
  expect_error(calc_n_one_mean_precision(sd = 10, half_width = 0))
  expect_error(calc_n_one_mean_precision(sd = 10, half_width = 2,
                                         conf_level = 1.5))
})
