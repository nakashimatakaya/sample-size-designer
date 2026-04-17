# 反復測定（longitudinal・Diggle）の必要症例数のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_longitudinal.R"))

test_that("k=1 なら通常の t 検定と一致（rho は無効）", {
  ref <- pwr::pwr.t.test(
    d = 0.5, sig.level = 0.05, power = 0.80,
    type = "two.sample", alternative = "two.sided"
  )
  res <- calc_n_longitudinal(mean_A = 1, mean_B = 0, sd_common = 2,
                             k = 1L, rho = 0.5,
                             alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_arm_evaluable, ceiling(ref$n))
})

test_that("rho = 0 なら n は通常の 1/k（k 回独立測定と同じ）", {
  # k=3, rho=0 → SD_eff = SD/sqrt(3)
  res_k1 <- calc_n_longitudinal(1, 0, 2, k = 1L, rho = 0,
                                alpha = 0.05, power = 0.80)
  res_k3 <- calc_n_longitudinal(1, 0, 2, k = 3L, rho = 0,
                                alpha = 0.05, power = 0.80)
  # おおむね 1/3（切り上げで ±1）
  expect_lt(abs(res_k3$n_per_arm_evaluable -
                ceiling(res_k1$n_per_arm_evaluable / 3)), 2)
})

test_that("rho が大きいほど n は大きい（単調性）", {
  r_lo <- calc_n_longitudinal(1, 0, 2, k = 3L, rho = 0.2,
                              alpha = 0.05)$n_per_arm_evaluable
  r_hi <- calc_n_longitudinal(1, 0, 2, k = 3L, rho = 0.8,
                              alpha = 0.05)$n_per_arm_evaluable
  expect_gt(r_hi, r_lo)
})

test_that("k が増えるほど n は小さい（同じ rho のもとで、単調性）", {
  r_k2 <- calc_n_longitudinal(1, 0, 2, k = 2L, rho = 0.5,
                              alpha = 0.05)$n_per_arm_evaluable
  r_k5 <- calc_n_longitudinal(1, 0, 2, k = 5L, rho = 0.5,
                              alpha = 0.05)$n_per_arm_evaluable
  expect_gt(r_k2, r_k5)
})

test_that("脱落率込みで n_randomized が正しく切り上がる", {
  res <- calc_n_longitudinal(1, 0, 2, k = 3L, rho = 0.5,
                             alpha = 0.05, power = 0.80, dropout = 0.2)
  expected_rand <- ceiling(res$n_per_arm_evaluable / 0.8)
  expect_equal(res$n_per_arm_randomized, expected_rand)
})
