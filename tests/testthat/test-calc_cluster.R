# クラスターランダム化（連続量／二値）のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_cluster.R"))

test_that("Design Effect = 1 + (m-1)*ICC", {
  expect_equal(calc_design_effect(m = 1,   ICC = 0.05), 1)
  expect_equal(calc_design_effect(m = 10,  ICC = 0.05), 1 + 9 * 0.05)
  expect_equal(calc_design_effect(m = 100, ICC = 0.02), 1 + 99 * 0.02)
})

test_that("連続量: ICC=0, m=1 なら通常の t 検定と一致", {
  ref <- pwr::pwr.t.test(
    d = 0.5, sig.level = 0.05, power = 0.80,
    type = "two.sample", alternative = "two.sided"
  )
  res <- calc_n_cluster_continuous(1, 0, 2, m = 1, ICC = 0,
                                   alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_arm_evaluable, ceiling(ref$n))
})

test_that("連続量: ICC が大きいほど n は増える", {
  lo <- calc_n_cluster_continuous(1, 0, 2, m = 20, ICC = 0.01,
                                  alpha = 0.05)$n_per_arm_evaluable
  hi <- calc_n_cluster_continuous(1, 0, 2, m = 20, ICC = 0.10,
                                  alpha = 0.05)$n_per_arm_evaluable
  expect_gt(hi, lo)
})

test_that("連続量: クラスター数 K = ceiling(n_raw/m), n = K*m", {
  res <- calc_n_cluster_continuous(1, 0, 2, m = 15, ICC = 0.05,
                                   alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_arm_evaluable, res$K_per_arm * 15)
})

test_that("二値: ICC=0, m=1 なら通常の chisq と一致", {
  h <- 2 * asin(sqrt(0.5)) - 2 * asin(sqrt(0.3))
  ref <- pwr::pwr.2p.test(h = h, sig.level = 0.05, power = 0.80,
                          alternative = "two.sided")
  res <- calc_n_cluster_binary(p_A = 0.5, p_B = 0.3, m = 1, ICC = 0,
                               alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_arm_evaluable, ceiling(ref$n))
})

test_that("dispatch: outcome='binary' で binary が呼ばれる", {
  # make_result() は extras を c(base, extras) で展開するので
  # res$outcome_type として直接アクセスできる
  res <- calc_n_cluster(outcome = "binary",
                        p_A = 0.5, p_B = 0.3, m = 20, ICC = 0.05,
                        alpha = 0.05, power = 0.80)
  expect_equal(res$outcome_type, "binary")
})

test_that("脱落率 0.10 で n_randomized が切り上がる（連続量）", {
  res <- calc_n_cluster_continuous(1, 0, 2, m = 15, ICC = 0.05,
                                   alpha = 0.05, power = 0.80,
                                   dropout = 0.10)
  expected_rand <- ceiling(res$n_per_arm_evaluable / 0.9)
  expect_equal(res$n_per_arm_randomized, expected_rand)
})
