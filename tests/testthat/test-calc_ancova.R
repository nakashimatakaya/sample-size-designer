# ANCOVA（共変量調整）の必要症例数のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_ancova.R"))

test_that("r = 0 なら通常の t 検定と一致する", {
  ref <- pwr::pwr.t.test(
    d = 0.5, sig.level = 0.05, power = 0.80,
    type = "two.sample", alternative = "two.sided"
  )
  # mean_A = 1, mean_B = 0, sd = 2 → d = 0.5
  res <- calc_n_ancova(mean_A = 1, mean_B = 0, sd_common = 2, r = 0,
                       alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_arm_evaluable, ceiling(ref$n))
})

test_that("|r| が大きいほど必要症例数は少ない（単調性）", {
  res0 <- calc_n_ancova(1, 0, 2, r = 0.0, alpha = 0.05)
  res3 <- calc_n_ancova(1, 0, 2, r = 0.3, alpha = 0.05)
  res7 <- calc_n_ancova(1, 0, 2, r = 0.7, alpha = 0.05)
  expect_gt(res0$n_per_arm_evaluable, res3$n_per_arm_evaluable)
  expect_gt(res3$n_per_arm_evaluable, res7$n_per_arm_evaluable)
})

test_that("Borm 2007 の例（d=0.5, r=0.5）: n_ancova = n_t * (1-r^2)", {
  # r=0.5 → (1 - 0.25) = 0.75 倍で済むはず
  n_t   <- calc_n_ancova(1, 0, 2, r = 0.0, alpha = 0.05)$n_per_arm_evaluable
  n_anc <- calc_n_ancova(1, 0, 2, r = 0.5, alpha = 0.05)$n_per_arm_evaluable
  # 許容: ceiling 丸めで ±2
  expect_lt(abs(n_anc - ceiling(n_t * 0.75)), 3)
})

test_that("|r| >= 1 は拒否される", {
  expect_error(calc_n_ancova(1, 0, 2, r = 1.0, alpha = 0.05))
  expect_error(calc_n_ancova(1, 0, 2, r = -1.1, alpha = 0.05))
})

test_that("mean_A == mean_B では計算が落ちる", {
  expect_error(calc_n_ancova(1, 1, 2, r = 0.5, alpha = 0.05))
})

test_that("脱落率 0.15 で n_randomized が切り上がる", {
  res <- calc_n_ancova(1, 0, 2, r = 0.5,
                       alpha = 0.05, power = 0.80, dropout = 0.15)
  expected_rand <- ceiling(res$n_per_arm_evaluable / 0.85)
  expect_equal(res$n_per_arm_randomized, expected_rand)
})
