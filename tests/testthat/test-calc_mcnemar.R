# McNemar（対応のある二値）の必要ペア数のテスト。

source(testthat::test_path("..", "..", "R", "common.R"))
source(testthat::test_path("..", "..", "R", "calc_mcnemar.R"))

test_that("calc_n_mcnemar: 公式を直接評価した結果と一致する", {
  # ψ = 0.7, p_disc = 0.3, α=0.05（両側）, power=0.80
  z_a <- stats::qnorm(0.975)
  z_b <- stats::qnorm(0.80)
  psi <- 0.7; p_disc <- 0.3
  two_psi_m1 <- 2 * psi - 1
  inner <- 1 - two_psi_m1^2 * p_disc
  expected <- ceiling((z_a + z_b * sqrt(inner))^2 /
                      (two_psi_m1^2 * p_disc))
  res <- calc_n_mcnemar(p_disc = p_disc, psi = psi,
                        alpha = 0.05, power = 0.80)
  expect_equal(res$n_per_arm_evaluable, expected)
  expect_equal(res$n_arms, 1L)
  expect_equal(res$backend_pkg, "stats")
  expect_equal(res$formula_ref, "Connor 1987")
})

test_that("ψ が 0.5 から遠いほど n は小さい", {
  res1 <- calc_n_mcnemar(p_disc = 0.3, psi = 0.55, alpha = 0.05)
  res2 <- calc_n_mcnemar(p_disc = 0.3, psi = 0.80, alpha = 0.05)
  expect_gt(res1$n_per_arm_evaluable, res2$n_per_arm_evaluable)
})

test_that("p_disc が大きいほど n は小さい", {
  res1 <- calc_n_mcnemar(p_disc = 0.1, psi = 0.70, alpha = 0.05)
  res2 <- calc_n_mcnemar(p_disc = 0.5, psi = 0.70, alpha = 0.05)
  expect_gt(res1$n_per_arm_evaluable, res2$n_per_arm_evaluable)
})

test_that("ψ = 0.5 だと計算が落ちる", {
  expect_error(calc_n_mcnemar(p_disc = 0.3, psi = 0.5, alpha = 0.05))
})

test_that("脱落率 0.10 で n_randomized が正しく切り上がる", {
  res <- calc_n_mcnemar(p_disc = 0.3, psi = 0.7,
                        alpha = 0.05, power = 0.80, dropout = 0.10)
  expected_rand <- ceiling(res$n_per_arm_evaluable / 0.9)
  expect_equal(res$n_per_arm_randomized, expected_rand)
})

test_that("calc_power_mcnemar: 必要ペア数を入れるとほぼ目標検出力を返す", {
  res <- calc_n_mcnemar(p_disc = 0.3, psi = 0.7,
                        alpha = 0.05, power = 0.80)
  p <- calc_power_mcnemar(p_disc = 0.3, psi = 0.7,
                          alpha = 0.05, n = res$n_per_arm_evaluable)
  expect_gt(p, 0.79)
})
