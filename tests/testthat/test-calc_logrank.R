# 生存時間（log-rank）の必要イベント数・症例数のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_logrank.R"))

test_that("Schoenfeld の教科書例（HR=0.7, α=0.025 片側, β=0.20, 1:1）", {
  # 必要イベント数 D = (z_α + z_β)^2 / (0.25 * (log 0.7)^2)
  z_a <- stats::qnorm(1 - 0.025)
  z_b <- stats::qnorm(0.80)
  D_expected <- (z_a + z_b)^2 / (0.25 * (log(0.7))^2)

  res <- calc_n_logrank(median_C = 12, HR = 0.7,
                        accrual = 24, followup = 24,
                        alpha = 0.025, power = 0.80,
                        p_alloc = 0.5)
  expect_equal(res$events_required, ceiling(D_expected))
})

test_that("HR が 1 に近いほど必要イベント数は増える（単調性）", {
  res_075 <- calc_n_logrank(median_C = 12, HR = 0.75,
                            accrual = 24, followup = 24,
                            alpha = 0.025, power = 0.80)
  res_09  <- calc_n_logrank(median_C = 12, HR = 0.9,
                            accrual = 24, followup = 24,
                            alpha = 0.025, power = 0.80)
  expect_gt(res_09$events_required, res_075$events_required)
})

test_that("フォローアップが長いほど q は大きく n は少ない", {
  res_f12 <- calc_n_logrank(median_C = 12, HR = 0.7,
                            accrual = 24, followup = 12,
                            alpha = 0.025, power = 0.80)
  res_f48 <- calc_n_logrank(median_C = 12, HR = 0.7,
                            accrual = 24, followup = 48,
                            alpha = 0.025, power = 0.80)
  expect_gt(res_f12$n_total_evaluable, res_f48$n_total_evaluable)
})

test_that("HR = 1 は拒否される", {
  expect_error(calc_n_logrank(median_C = 12, HR = 1,
                              accrual = 24, followup = 24,
                              alpha = 0.025))
})

test_that("calc_power_logrank: 必要症例数で目標検出力に近い値を返す", {
  res <- calc_n_logrank(median_C = 12, HR = 0.7,
                        accrual = 24, followup = 24,
                        alpha = 0.025, power = 0.80)
  p <- calc_power_logrank(median_C = 12, HR = 0.7,
                          accrual = 24, followup = 24,
                          alpha = 0.025,
                          N_total = res$n_total_evaluable)
  expect_gt(p, 0.78)
})

test_that("脱落率 0.10 で n_randomized が切り上がる", {
  res <- calc_n_logrank(median_C = 12, HR = 0.7,
                        accrual = 24, followup = 24,
                        alpha = 0.025, power = 0.80, dropout = 0.10)
  expected_rand <- ceiling(res$n_per_arm_evaluable / 0.9)
  expect_equal(res$n_per_arm_randomized, expected_rand)
})
