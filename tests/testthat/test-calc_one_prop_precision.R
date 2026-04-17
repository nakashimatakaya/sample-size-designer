# 1 標本・割合・精度ベースのテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_one_prop_precision.R"))

test_that("正規近似: n = z^2 * p(1-p) / E^2 を切り上げる", {
  res <- calc_n_one_prop_precision(
    p = 0.3, half_width = 0.05, conf_level = 0.95, method = "normal"
  )
  z <- stats::qnorm(0.975)
  expect_equal(res$n_per_arm_evaluable,
               ceiling(z^2 * 0.3 * 0.7 / 0.05^2))
  expect_equal(res$n_arms, 1L)
  expect_true(is.na(res$achieved_power))
})

test_that("wilson: 見つけた n で binom の半幅 <= E", {
  p <- 0.3; E <- 0.05; cl <- 0.95
  res <- calc_n_one_prop_precision(
    p = p, half_width = E, conf_level = cl, method = "wilson"
  )
  n <- res$n_per_arm_evaluable
  x <- round(p * n)
  ci <- binom::binom.confint(x = x, n = n, conf.level = cl,
                             methods = "wilson")
  expect_true((ci$upper - ci$lower) / 2 <= E + 1e-9)
})

test_that("wilson: n-1 では半幅 > E（最小性の確認）", {
  p <- 0.3; E <- 0.05; cl <- 0.95
  res <- calc_n_one_prop_precision(
    p = p, half_width = E, conf_level = cl, method = "wilson"
  )
  n <- res$n_per_arm_evaluable
  if (n > 2) {
    x <- round(p * (n - 1))
    ci <- binom::binom.confint(x = x, n = n - 1, conf.level = cl,
                               methods = "wilson")
    expect_true((ci$upper - ci$lower) / 2 > E - 1e-9)
  }
})

test_that("exact: 見つけた n で binom の半幅 <= E", {
  p <- 0.3; E <- 0.05; cl <- 0.95
  res <- calc_n_one_prop_precision(
    p = p, half_width = E, conf_level = cl, method = "exact"
  )
  n <- res$n_per_arm_evaluable
  x <- round(p * n)
  ci <- binom::binom.confint(x = x, n = n, conf.level = cl,
                             methods = "exact")
  expect_true((ci$upper - ci$lower) / 2 <= E + 1e-9)
})

test_that("exact は wilson よりやや大きめの n になる（典型例）", {
  p <- 0.3; E <- 0.05; cl <- 0.95
  n_w <- calc_n_one_prop_precision(p, E, cl, "wilson")$n_per_arm_evaluable
  n_e <- calc_n_one_prop_precision(p, E, cl, "exact" )$n_per_arm_evaluable
  expect_gte(n_e, n_w)
})

test_that("脱落率を考慮した n_randomized が切り上がる", {
  res <- calc_n_one_prop_precision(p = 0.3, half_width = 0.05,
                                   method = "wilson", dropout = 0.10)
  expect_equal(res$n_per_arm_randomized,
               ceiling(res$n_per_arm_evaluable / 0.9))
})
