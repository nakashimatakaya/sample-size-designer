# 二値・Fisher の正確検定（χ² 近似）のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_binary_chisq.R"))
source(testthat::test_path("..", "..", "R", "calc_binary_fisher.R"))

test_that("calc_n_binary_fisher: 数値は calc_n_binary_chisq と一致する（近似）", {
  res_f <- calc_n_binary_fisher(0.3, 0.5, alpha = 0.05, power = 0.80)
  res_c <- calc_n_binary_chisq (0.3, 0.5, alpha = 0.05, power = 0.80)
  expect_equal(res_f$n_per_arm_evaluable,  res_c$n_per_arm_evaluable)
  expect_equal(res_f$n_total_evaluable,    res_c$n_total_evaluable)
})

test_that("calc_n_binary_fisher: 警告メッセージがついている", {
  res <- calc_n_binary_fisher(0.3, 0.5, alpha = 0.05, power = 0.80)
  expect_true("warning" %in% names(res))
  expect_true(nchar(res$warning) > 0)
})

test_that("backend_fun と formula_ref が Fisher 用に書き換わっている", {
  res <- calc_n_binary_fisher(0.3, 0.5, alpha = 0.05, power = 0.80)
  expect_match(res$backend_fun, "Fisher")
  expect_match(res$formula_ref, "Fisher")
})
