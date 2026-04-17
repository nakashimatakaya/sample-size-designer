# 群逐次デザイン（OBF / Pocock）の必要症例数のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_group_sequential.R"))

test_that("K=1 なら固定デザインと一致", {
  ref <- pwr::pwr.t.test(
    d = 0.5, sig.level = 0.05, power = 0.80,
    type = "two.sample", alternative = "two.sided"
  )
  res <- calc_n_group_sequential(1, 0, 2, alpha = 0.05, power = 0.80,
                                 K = 1L, boundary = "OBF")
  expect_equal(res$n_per_arm_evaluable, ceiling(ref$n))
})

test_that("OBF は固定デザインより 1〜4% 多い（K=5）", {
  ref <- pwr::pwr.t.test(
    d = 0.5, sig.level = 0.05, power = 0.80,
    type = "two.sample", alternative = "two.sided"
  )
  res <- calc_n_group_sequential(1, 0, 2, alpha = 0.05, power = 0.80,
                                 K = 5L, boundary = "OBF")
  # inflation 1.028 ± ceiling 切り上げの余裕を見て 1.025 〜 1.05 とする
  ratio <- res$n_per_arm_evaluable / ref$n
  expect_gt(ratio, 1.025); expect_lt(ratio, 1.05)
})

test_that("Pocock は OBF より多い（境界緩い）", {
  n_obf <- calc_n_group_sequential(1, 0, 2, alpha = 0.05, K = 5L,
                                   boundary = "OBF")$n_per_arm_evaluable
  n_poc <- calc_n_group_sequential(1, 0, 2, alpha = 0.05, K = 5L,
                                   boundary = "Pocock")$n_per_arm_evaluable
  expect_gt(n_poc, n_obf)
})

test_that("K が増えるほど n は増える（単調性）", {
  n2 <- calc_n_group_sequential(1, 0, 2, alpha = 0.05, K = 2L,
                                boundary = "Pocock")$n_per_arm_evaluable
  n5 <- calc_n_group_sequential(1, 0, 2, alpha = 0.05, K = 5L,
                                boundary = "Pocock")$n_per_arm_evaluable
  expect_gt(n5, n2)
})

test_that("boundary 未知なら拒否", {
  expect_error(calc_n_group_sequential(1, 0, 2, alpha = 0.05,
                                       K = 3L, boundary = "foo"))
})

test_that("K 範囲外は拒否", {
  expect_error(calc_n_group_sequential(1, 0, 2, alpha = 0.05,
                                       K = 6L, boundary = "OBF"))
})
