# 非劣性・2 標本 t 検定（片側）のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_ttest_ni.R"))

test_that("calc_d_ttest_ni: (Δ + M) / sd_pooled を返す", {
  # sd_A = sd_B = 4, M = 2, Δ = 0 → d = (0+2)/4 = 0.5
  expect_equal(calc_d_ttest_ni(diff = 0, sd_A = 4, sd_B = 4, margin = 2),
               0.5)
  # Δ > 0（A が B より既に優れている想定）
  expect_equal(calc_d_ttest_ni(diff = 1, sd_A = 4, sd_B = 4, margin = 2),
               (1 + 2) / 4)
})

test_that("calc_n_ttest_ni: Δ=0, sd=4, M=2, α=0.025 one-sided → pwr と一致", {
  # d_ni = 0.5; 片側 α=0.025 の two.sample greater は
  # 両側 α=0.05 の two-sample と同じ n_per_arm = 64 になるはず
  ref <- ceiling(
    pwr::pwr.t.test(
      d = 0.5, sig.level = 0.025, power = 0.80,
      type = "two.sample", alternative = "greater"
    )$n
  )
  res <- calc_n_ttest_ni(diff = 0, sd_A = 4, sd_B = 4, margin = 2,
                         alpha = 0.025, power = 0.80)
  expect_equal(res$n_per_arm_evaluable, ref)
  expect_equal(res$n_arms, 2L)
})

test_that("マージン M を大きくすると必要症例数は減る（単調性）", {
  n_small <- calc_n_ttest_ni(0, 4, 4, margin = 1, alpha = 0.025)$n_per_arm_evaluable
  n_large <- calc_n_ttest_ni(0, 4, 4, margin = 3, alpha = 0.025)$n_per_arm_evaluable
  expect_gt(n_small, n_large)
})

test_that("脱落率を考慮した n_randomized が切り上がる", {
  res <- calc_n_ttest_ni(0, 4, 4, margin = 2,
                         alpha = 0.025, power = 0.80, dropout = 0.10)
  expect_equal(res$n_per_arm_randomized,
               ceiling(res$n_per_arm_evaluable / 0.9))
})
