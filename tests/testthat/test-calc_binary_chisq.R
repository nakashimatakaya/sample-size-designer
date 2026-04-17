# 二値・χ² 検定（優越性・両側）のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_binary_chisq.R"))

test_that("calc_h_cohen: p_A = p_B なら 0、符号が正しい", {
  expect_equal(calc_h_cohen(0.5, 0.5), 0)
  # p_A > p_B なら正
  expect_gt(calc_h_cohen(0.7, 0.5), 0)
  # p_A < p_B なら負
  expect_lt(calc_h_cohen(0.3, 0.5), 0)
})

test_that("calc_n_binary_chisq: pwr.2p.test を直接叩いた結果と一致", {
  h <- calc_h_cohen(0.3, 0.5)
  ref <- ceiling(
    pwr::pwr.2p.test(h = h, sig.level = 0.05, power = 0.80,
                     alternative = "two.sided")$n
  )
  res <- calc_n_binary_chisq(p_A = 0.3, p_B = 0.5,
                             alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_arm_evaluable, ref)
  expect_equal(res$n_arms, 2L)
  expect_equal(res$backend_pkg, "pwr")
})

test_that("calc_n_binary_chisq: 脱落率 0.10 で n_randomized が切り上がる", {
  res <- calc_n_binary_chisq(p_A = 0.3, p_B = 0.5,
                             alpha = 0.05, power = 0.80, dropout = 0.10)
  expected_rand <- ceiling(res$n_per_arm_evaluable / 0.9)
  expect_equal(res$n_per_arm_randomized, expected_rand)
})

test_that("p_A == p_B だと計算が落ちる", {
  expect_error(calc_n_binary_chisq(0.5, 0.5, alpha = 0.05))
})
