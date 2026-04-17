# calc_ttest_ss() / calc_*_mode1 / calc_*_mode2 のテスト。
# モード2 は今回の仕様変更で「差と群別 SD で入力する 2 標本 t 検定」
# となったため、対応のある t 検定のテストは test-calc_paired.R に移した。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_ttest.R"))

# =========================================================================
# calc_ttest_ss: Cohen (1988) の教科書例題
# =========================================================================

test_that("中程度の効果量 (d=0.5, α=0.05, power=0.80) は1群64例になる", {
  res <- calc_ttest_ss(d = 0.5, alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_group, 64)
  expect_equal(res$n_total, 128)
})

test_that("大きな効果量 (d=0.8, α=0.05, power=0.80) は1群26例になる", {
  res <- calc_ttest_ss(d = 0.8, alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_group, 26)
  expect_equal(res$n_total, 52)
})

test_that("小さな効果量 (d=0.2, α=0.05, power=0.80) は1群394例になる", {
  res <- calc_ttest_ss(d = 0.2, alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_group, 394)
  expect_equal(res$n_total, 788)
})

test_that("検出力を上げると必要症例数は増える（単調性の確認）", {
  n_80 <- calc_ttest_ss(d = 0.5, alpha = 0.05, power = 0.80)$n_per_group
  n_90 <- calc_ttest_ss(d = 0.5, alpha = 0.05, power = 0.90)$n_per_group
  expect_gt(n_90, n_80)
})

test_that("効果量を大きくすると必要症例数は減る（単調性の確認）", {
  n_small  <- calc_ttest_ss(d = 0.3, alpha = 0.05, power = 0.80)$n_per_group
  n_medium <- calc_ttest_ss(d = 0.5, alpha = 0.05, power = 0.80)$n_per_group
  n_large  <- calc_ttest_ss(d = 0.8, alpha = 0.05, power = 0.80)$n_per_group
  expect_gt(n_small, n_medium)
  expect_gt(n_medium, n_large)
})

# =========================================================================
# モード1
# =========================================================================

test_that("calc_d_mode1: プール分散ベースの d を正しく返す", {
  expect_equal(calc_d_mode1(mean_A = 10, sd_A = 4, mean_B = 8, sd_B = 4), 0.5)
  expect_equal(
    calc_d_mode1(mean_A = 10, sd_A = 3, mean_B = 8, sd_B = 5),
    2 / sqrt((3^2 + 5^2) / 2)
  )
  expect_equal(calc_d_mode1(8, 4, 10, 4), 0.5)   # 符号吸収
})

test_that("calc_power_mode1: pwr.t.test(two.sample) と一致する", {
  ref <- pwr::pwr.t.test(
    n = 50, d = 0.5, sig.level = 0.05,
    type = "two.sample", alternative = "two.sided"
  )$power
  got <- calc_power_mode1(
    mean_A = 10, sd_A = 4, mean_B = 8, sd_B = 4, alpha = 0.05, n = 50
  )
  expect_equal(got, ref, tolerance = 1e-10)
})

test_that("calc_n_mode1: Cohen 例題で 1 群 64 例、共通 schema を返す", {
  res <- calc_n_mode1(
    mean_A = 10, sd_A = 4, mean_B = 8, sd_B = 4,
    alpha = 0.05, power = 0.80
  )
  expect_equal(res$n_per_arm_evaluable,  64)
  expect_equal(res$n_per_arm_randomized, 64)
  expect_equal(res$n_total_evaluable,    128)
  expect_equal(res$n_total_randomized,   128)
  expect_equal(res$dropout, 0)
  expect_equal(res$n_arms, 2L)
  expect_equal(res$backend_pkg, "pwr")
  expect_equal(res$formula_ref, "Cohen 1988")
  expect_type(res, "list")
})

test_that("calc_n_mode1: 脱落率 0.10 で n_randomized = 72 に切り上がる", {
  res <- calc_n_mode1(
    mean_A = 10, sd_A = 4, mean_B = 8, sd_B = 4,
    alpha = 0.05, power = 0.80, dropout = 0.10
  )
  expect_equal(res$n_per_arm_evaluable,  64)
  expect_equal(res$n_per_arm_randomized, 72)     # ceiling(64 / 0.9)
  expect_equal(res$n_total_randomized,   144)
})

test_that("calc_n_mode1: 不正な dropout は stopifnot で落ちる", {
  expect_error(calc_n_mode1(10, 4, 8, 4, alpha = 0.05, dropout = -0.1))
  expect_error(calc_n_mode1(10, 4, 8, 4, alpha = 0.05, dropout = 1.0))
})

# =========================================================================
# モード2（新仕様: Δ, sd_A, sd_B で入力する 2 標本 t 検定）
# =========================================================================

test_that("calc_d_mode2: 差と群別 SD で d を返す（等分散）", {
  # 等分散 sd_A = sd_B = 4、Δ = 2 → d = 2/4 = 0.5
  expect_equal(calc_d_mode2(diff = 2, sd_A = 4, sd_B = 4), 0.5)
  # Δ の符号は |.| で吸収
  expect_equal(calc_d_mode2(diff = -2, sd_A = 4, sd_B = 4), 0.5)
  # 不等分散
  expect_equal(
    calc_d_mode2(diff = 2, sd_A = 3, sd_B = 5),
    2 / sqrt((3^2 + 5^2) / 2)
  )
})

test_that("calc_d_mode2 はモード1 と整合する（同じ入力なら同じ d）", {
  d1 <- calc_d_mode1(10, 4, 8, 4)
  d2 <- calc_d_mode2(diff = 10 - 8, sd_A = 4, sd_B = 4)
  expect_equal(d1, d2)
})

test_that("calc_power_mode2: pwr.t.test(two.sample) と一致する", {
  ref <- pwr::pwr.t.test(
    n = 50, d = 0.5, sig.level = 0.05,
    type = "two.sample", alternative = "two.sided"
  )$power
  got <- calc_power_mode2(diff = 2, sd_A = 4, sd_B = 4, alpha = 0.05, n = 50)
  expect_equal(got, ref, tolerance = 1e-10)
})

test_that("calc_n_mode2: Cohen 例題で 1 群 64 例になる", {
  res <- calc_n_mode2(diff = 2, sd_A = 4, sd_B = 4,
                      alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_arm_evaluable,  64)
  expect_equal(res$n_total_evaluable,    128)
  expect_equal(res$n_arms, 2L)
})

test_that("calc_n_mode2: 脱落率 0.20 で n_randomized が正しく切り上がる", {
  res <- calc_n_mode2(diff = 2, sd_A = 4, sd_B = 4,
                      alpha = 0.05, power = 0.80, dropout = 0.20)
  expect_equal(res$n_per_arm_evaluable,  64)
  expect_equal(res$n_per_arm_randomized, 80)   # ceiling(64 / 0.8)
  expect_equal(res$n_total_randomized,   160)
})
