# 非劣性・対応のある t 検定（片側）のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_paired_ni.R"))

test_that("calc_d_paired_ni: (|diff| + M) / sd_diff", {
  expect_equal(calc_d_paired_ni(diff_mean = 0, sd_diff = 4, margin = 2),
               0.5)
  expect_equal(calc_d_paired_ni(diff_mean = -1, sd_diff = 4, margin = 2),
               (1 + 2) / 4)
})

test_that("calc_n_paired_ni: pwr(paired, greater) と一致", {
  d_ni <- 0.5
  ref <- ceiling(
    pwr::pwr.t.test(
      d = d_ni, sig.level = 0.025, power = 0.80,
      type = "paired", alternative = "greater"
    )$n
  )
  res <- calc_n_paired_ni(diff_mean = 0, sd_diff = 4, margin = 2,
                          alpha = 0.025, power = 0.80)
  expect_equal(res$n_per_arm_evaluable, ref)
  expect_equal(res$n_arms, 1L)
})

test_that("脱落率を考慮した n_randomized が切り上がる", {
  res <- calc_n_paired_ni(diff_mean = 0, sd_diff = 4, margin = 2,
                          alpha = 0.025, dropout = 0.20)
  expect_equal(res$n_per_arm_randomized,
               ceiling(res$n_per_arm_evaluable / 0.8))
})
