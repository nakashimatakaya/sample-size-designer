# 対応のある t 検定（優越性・両側）のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_paired.R"))

test_that("calc_d_paired: |diff| / sd_diff を返す", {
  expect_equal(calc_d_paired(diff_mean = 2, sd_diff = 4), 0.5)
  expect_equal(calc_d_paired(diff_mean = -2, sd_diff = 4), 0.5)
})

test_that("calc_power_paired: pwr.t.test(paired) と一致", {
  ref <- pwr::pwr.t.test(
    n = 30, d = 0.5, sig.level = 0.05,
    type = "paired", alternative = "two.sided"
  )$power
  got <- calc_power_paired(diff_mean = 2, sd_diff = 4, alpha = 0.05, n = 30)
  expect_equal(got, ref, tolerance = 1e-10)
})

test_that("calc_n_paired: pwr を直接叩いた結果と一致する", {
  ref <- ceiling(
    pwr::pwr.t.test(
      d = 0.5, sig.level = 0.05, power = 0.80,
      type = "paired", alternative = "two.sided"
    )$n
  )
  res <- calc_n_paired(diff_mean = 2, sd_diff = 4,
                       alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_arm_evaluable, ref)
  expect_equal(res$n_total_evaluable,   ref)    # n_arms = 1 なので同値
  expect_equal(res$n_arms, 1L)
  expect_equal(res$backend_pkg, "pwr")
})

test_that("calc_n_paired: 脱落率を考慮した n_randomized が切り上がる", {
  ref_eval <- ceiling(
    pwr::pwr.t.test(
      d = 0.5, sig.level = 0.05, power = 0.80,
      type = "paired", alternative = "two.sided"
    )$n
  )
  ref_rand <- ceiling(ref_eval / (1 - 0.20))
  res <- calc_n_paired(diff_mean = 2, sd_diff = 4, alpha = 0.05,
                       power = 0.80, dropout = 0.20)
  expect_equal(res$n_per_arm_evaluable,  ref_eval)
  expect_equal(res$n_per_arm_randomized, ref_rand)
})

# =========================================================================
# 相関係数モード（calc_sd_diff_from_corr / calc_paired_from_corr）
# =========================================================================

test_that("calc_sd_diff_from_corr: r = 0 なら sqrt(sd1^2 + sd2^2)", {
  expect_equal(calc_sd_diff_from_corr(sd_1 = 3, sd_2 = 4, r = 0),
               sqrt(3^2 + 4^2))
  expect_equal(calc_sd_diff_from_corr(sd_1 = 5, sd_2 = 5, r = 0),
               sqrt(50))
})

test_that("calc_sd_diff_from_corr: sd_1=sd_2=4, r=0.5 → 差の SD = 4", {
  # 4 * sqrt(2 * (1 - 0.5)) = 4 * sqrt(1) = 4
  expect_equal(calc_sd_diff_from_corr(sd_1 = 4, sd_2 = 4, r = 0.5), 4)
})

test_that("calc_sd_diff_from_corr: r=1 かつ sd_1=sd_2 → 差の SD = 0 を返す（負にならない）", {
  # 数値誤差で負にならないことの確認
  expect_equal(calc_sd_diff_from_corr(sd_1 = 4, sd_2 = 4, r = 1), 0)
})

test_that("calc_sd_diff_from_corr: 不正な r / SD は落ちる", {
  expect_error(calc_sd_diff_from_corr(sd_1 = 4, sd_2 = 4, r = 1.2))
  expect_error(calc_sd_diff_from_corr(sd_1 = 4, sd_2 = 4, r = -2))
  expect_error(calc_sd_diff_from_corr(sd_1 = 0, sd_2 = 4, r = 0.5))
  expect_error(calc_sd_diff_from_corr(sd_1 = 4, sd_2 = -1, r = 0.5))
})

test_that("calc_paired_from_corr: 直接入力モードと数値が一致する（sd_1=sd_2=4, r=0.5 → sd_diff=4）", {
  # mean_1=0, mean_2=2 → diff_mean=2, sd_diff=4（上のテストで確認済み）
  res_corr <- calc_paired_from_corr(
    mean_1 = 0, mean_2 = 2, sd_1 = 4, sd_2 = 4, r = 0.5,
    alpha = 0.05, power = 0.80
  )
  res_direct <- calc_n_paired(diff_mean = 2, sd_diff = 4,
                              alpha = 0.05, power = 0.80)
  expect_equal(res_corr$n_per_arm_evaluable,  res_direct$n_per_arm_evaluable)
  expect_equal(res_corr$n_per_arm_randomized, res_direct$n_per_arm_randomized)
  expect_equal(res_corr$sd_diff,   4)
  expect_equal(res_corr$diff_mean, 2)
  # 共通 schema のフィールドはそのまま
  expect_equal(res_corr$n_arms, 1L)
  expect_equal(res_corr$backend_pkg, "pwr")
})

test_that("calc_power_paired_from_corr: 直接入力モードと検出力が一致する", {
  got <- calc_power_paired_from_corr(
    mean_1 = 0, mean_2 = 2, sd_1 = 4, sd_2 = 4, r = 0.5,
    alpha = 0.05, n = 30
  )
  ref <- calc_power_paired(diff_mean = 2, sd_diff = 4,
                           alpha = 0.05, n = 30)
  expect_equal(got, ref, tolerance = 1e-12)
})
