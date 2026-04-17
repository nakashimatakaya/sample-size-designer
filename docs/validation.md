# SampleSizeApp 検証ドキュメント

このアプリが出力する必要症例数を、**教科書の例題**および **`pwr` / `binom` / `stats`
パッケージの直接呼び出し**と突き合わせて確認する。手元で `G*Power` を使って
同じ値が得られる手順も添える。

- 参照文献は本ドキュメント末尾にまとめてある。
- 「このアプリの出力」は、それぞれの engine 関数の既定入力で実行した結果。
- 自動検証（`testthat`）の実行方法は最後のセクション参照。

---

## 1. 2 標本 t 検定・優越性（群別入力 / モード1）

engine: `R/calc_ttest.R` の `calc_n_mode1()`（`pwr::pwr.t.test`）

**入力例**

| 項目 | 値 |
|---|---|
| A 群平均 (mean_A) | 10 |
| A 群 SD (sd_A) | 4 |
| B 群平均 (mean_B) | 8 |
| B 群 SD (sd_B) | 4 |
| α | 0.05（両側） |
| 目標検出力 | 0.80 |
| 脱落率 | 0 |

効果量 d = |10 − 8| / sqrt((4² + 4²) / 2) = **0.5**

**このアプリの出力**
- 各群必要症例数（解析対象）: **64 例**
- 合計 128 例

**検証 1: Cohen (1988) 教科書**
Cohen (1988) Table 2.4.1 において、d = 0.50、α = 0.05（両側）、power = 0.80
の場合、1 群 64 例と記載。→ 一致

**検証 2: `pwr` パッケージ直接呼び出し**
```r
pwr::pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.80,
                type = "two.sample", alternative = "two.sided")
# n = 63.76561 → 切り上げて 64
```
→ 一致

**検証 3: G\*Power での再現手順**
1. Test family: **t tests**
2. Statistical test: **Means: Difference between two independent means (two groups)**
3. Type of power analysis: **A priori**
4. Tails: **Two**
5. Effect size d: **0.5**
6. α err prob: **0.05**
7. Power (1−β err prob): **0.80**
8. Allocation ratio N2/N1: **1**
→ Total sample size = 128、各群 64 と表示されるはず

---

## 2. 2 標本 t 検定・優越性（差と群別 SD / モード2）

engine: `R/calc_ttest.R` の `calc_n_mode2()`（モード1と同じ式、入力形のみ差）

**入力例**

| 項目 | 値 |
|---|---|
| 群間差 Δ | 2 |
| A 群 SD | 4 |
| B 群 SD | 4 |
| α | 0.05（両側） |
| 目標検出力 | 0.80 |

d = |2| / sqrt((4² + 4²) / 2) = **0.5**

**このアプリの出力**
- 各群必要症例数: **64 例**

**検証**
モード1 と式が一致する（入力が (mean_A, mean_B, sd_A, sd_B) か (Δ, sd_A, sd_B)
かの違いのみ）。`tests/testthat/test-calc_ttest.R` で両者が一致することを
`calc_d_mode1()` と `calc_d_mode2()` の比較で検証している。

---

## 3. 対応のある t 検定・優越性（差の SD 直接入力）

engine: `R/calc_paired.R` の `calc_n_paired()`（`pwr::pwr.t.test(type="paired")`）

**入力例**

| 項目 | 値 |
|---|---|
| 差の平均 | 2 |
| 差の SD | 4 |
| α | 0.05（両側） |
| 目標検出力 | 0.80 |

d = |2| / 4 = **0.5**

**このアプリの出力**
- 必要ペア数（解析対象）: **34 ペア**

**検証 1: Cohen (1988)**
対応あり t 検定は独立 2 群に比べて必要 n が半分前後になるのが経験則
（r = 0.5 想定のとき、ほぼ同等）。d = 0.5 で両側 α = 0.05、power = 0.80 のとき
約 34 ペア。

**検証 2: `pwr` パッケージ直接呼び出し**
```r
pwr::pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.80,
                type = "paired", alternative = "two.sided")
# n = 33.367 → 切り上げて 34
```
→ 一致

**検証 3: G\*Power**
1. Test family: **t tests**
2. Statistical test: **Means: Difference between two dependent means (matched pairs)**
3. Type of power analysis: **A priori**
4. Tails: **Two**
5. Effect size dz: **0.5**
6. α err prob: **0.05**
7. Power: **0.80**
→ Total sample size = 34 と表示されるはず

---

## 4. 対応のある t 検定・優越性（相関係数モード）

engine: `R/calc_paired.R` の `calc_paired_from_corr()`
（内部で `calc_sd_diff_from_corr()` → `calc_n_paired()`）

**入力例**

| 項目 | 値 |
|---|---|
| 治療前の平均 (mean_1) | 10 |
| 治療後の平均 (mean_2) | 12 |
| 治療前の SD (sd_1) | 4 |
| 治療後の SD (sd_2) | 4 |
| 相関係数 r | 0.5 |
| α | 0.05（両側） |
| 目標検出力 | 0.80 |

換算: sd_diff = sqrt(4² + 4² − 2 × 0.5 × 4 × 4) = sqrt(16) = **4**、
差の平均 = 12 − 10 = **2**

**このアプリの出力**
- 必要ペア数: **34 ペア**
- 換算された差の SD: 4.00

**検証 1: 直接入力モードとの一致**
上のケースは (差の平均 = 2、差の SD = 4) と等価なので、デザイン 3 の結果
（34 ペア）と**完全一致**する。`tests/testthat/test-calc_paired.R` の
"直接入力モードと数値が一致する" テストで検証済み。

**検証 2: 相関係数換算公式**
`calc_sd_diff_from_corr` の挙動を教科書公式で検証:
- r = 0: sd_diff = sqrt(sd_1² + sd_2²)（独立のときの合成 SD）
- r = 1 かつ sd_1 = sd_2: sd_diff = 0（完全一致なので差がない）
- 一般: sd_diff = sqrt(sd_1² + sd_2² − 2·r·sd_1·sd_2)（分散の差の公式）
これらはすべて `tests/testthat/test-calc_paired.R` で検証済み。

**検証 3: `pwr` パッケージ直接呼び出し**
```r
sd_diff <- sqrt(4^2 + 4^2 - 2 * 0.5 * 4 * 4)   # = 4
d <- abs(12 - 10) / sd_diff                    # = 0.5
pwr::pwr.t.test(d = d, sig.level = 0.05, power = 0.80,
                type = "paired", alternative = "two.sided")
# n = 33.367 → 34
```
→ 一致

---

## 5. 二値・χ² 検定・優越性

engine: `R/calc_binary_chisq.R` の `calc_n_binary_chisq()`（`pwr::pwr.2p.test`）

**入力例**

| 項目 | 値 |
|---|---|
| p_A | 0.30 |
| p_B | 0.50 |
| α | 0.05（両側） |
| 目標検出力 | 0.80 |

Cohen's h = 2·asin(√0.3) − 2·asin(√0.5) ≈ **−0.4115**

**このアプリの出力**
- 各群必要症例数: **93 例**

**検証 1: Cohen (1988) / Fleiss (1981)**
Cohen (1988) Table 6.3.5 / Fleiss (1981) Table A.3 で |h| ≈ 0.41、
α = 0.05 両側、power = 0.80 のとき 1 群 約 93 例と記載。

**検証 2: `pwr` パッケージ直接呼び出し**
```r
h <- 2 * asin(sqrt(0.3)) - 2 * asin(sqrt(0.5))
pwr::pwr.2p.test(h = h, sig.level = 0.05, power = 0.80,
                 alternative = "two.sided")
# n = 92.73 → 切り上げて 93
```
→ 一致

**検証 3: G\*Power**
1. Test family: **z tests**
2. Statistical test: **Proportions: Difference between two independent proportions**
3. Type of power analysis: **A priori**
4. Tails: **Two**
5. Proportion p1: **0.30**
6. Proportion p2: **0.50**
7. α err prob: **0.05**
8. Power: **0.80**
9. Allocation ratio: **1**
→ Total sample size = 186（各群 93）と表示されるはず

---

## 6. 二値・Fisher の正確検定・優越性（χ² 近似）

engine: `R/calc_binary_fisher.R` の `calc_n_binary_fisher()`
（`pwr::pwr.2p.test` による χ² 近似）

**入力例**: デザイン 5 と同じ（p_A = 0.30、p_B = 0.50、α = 0.05 両側、power = 0.80）

**このアプリの出力**
- 各群必要症例数: **93 例**（χ² と同値）

**既知の制約（重要）**
`pwr` パッケージには Fisher の正確検定の厳密な検出力計算が**実装されていない**
ため、本アプリは χ² 検定の結果を **近似として**そのまま返し、画面に以下の
警告を表示する:

> Fisher の正確検定は χ² 検定より若干保守的になります。
> 希少事象（p < 0.05 または p > 0.95）ではシミュレーションベースの
> 検出力計算を推奨します。

**検証（厳密な比較には外部ツールを併用）**
- R 単体での厳密計算には `statmod::power.fisher.test` または
  `Exact` パッケージ（`exact.test`）のシミュレーション、あるいは
  `clinfun::gsProbHyp` 等が利用できる。希少事象の試験設計時には必ず併用する。
- 中程度の頻度（p が 0.05〜0.95 の範囲内、n が十分大）では χ² 近似と
  正確検定の差はほぼ無視できる。

---

## 7. 1 標本・平均・精度ベース

engine: `R/calc_one_mean_precision.R` の `calc_n_one_mean_precision()`（`stats::qnorm`）

**入力例**

| 項目 | 値 |
|---|---|
| 予想される SD | 10 |
| 目標 CI 半幅 E | 2 |
| 信頼水準 | 0.95 |

公式: n = (z_{1−α/2} · SD / E)² を切り上げ
= (1.959964 · 10 / 2)² ≈ 96.04 → **97**

**このアプリの出力**
- 必要症例数: **97 例**
- 検出力の概念はなし（NA）

**検証 1: 標準教科書公式（例: Rosner, Fundamentals of Biostatistics）**
「母平均の推定で CI 半幅を E 以下にするために必要な n」の公式と完全一致。

**検証 2: R 直接呼び出し**
```r
z <- qnorm(0.975)
ceiling((z * 10 / 2)^2)
# [1] 97
```
→ 一致

---

## 8. 1 標本・割合・精度ベース

engine: `R/calc_one_prop_precision.R` の `calc_n_one_prop_precision()`

**入力例**

| 項目 | 値 |
|---|---|
| 予想される割合 p | 0.30 |
| 目標 CI 半幅 E | 0.05 |
| 信頼水準 | 0.95 |

**このアプリの出力（CI 法別）**

| CI 法 | 必要 n | 出典 |
|---|---:|---|
| 正規近似 (Wald) | **323** | 標準教科書 |
| Wilson | **320** | Wilson (1927) |
| Exact (Clopper-Pearson) | **341** | Clopper & Pearson (1934) |

**検証 1: R 直接呼び出し（正規近似）**
```r
z <- qnorm(0.975)
ceiling(z^2 * 0.3 * 0.7 / 0.05^2)
# [1] 323
```
→ 一致

**検証 2: `binom` パッケージ直接呼び出し（Wilson / Exact）**
```r
# Wilson
library(binom)
n <- 320; x <- round(0.3 * n)
ci <- binom.confint(x, n, conf.level = 0.95, methods = "wilson")
(ci$upper - ci$lower) / 2   # <= 0.05
# n = 319 では半幅 > 0.05 になる（最小性）
n <- 319; x <- round(0.3 * n)
ci <- binom.confint(x, n, conf.level = 0.95, methods = "wilson")
(ci$upper - ci$lower) / 2   # > 0.05

# Exact
n <- 341; x <- round(0.3 * n)
ci <- binom.confint(x, n, conf.level = 0.95, methods = "exact")
(ci$upper - ci$lower) / 2   # <= 0.05
```
→ 一致（`tests/testthat/test-calc_one_prop_precision.R` で自動検証）

**既知の性質**
Exact（Clopper-Pearson）は保守的（区間が広め）、Wilson は Exact より狭く
カバー率も正確、正規近似は極端な p では信頼できない。本アプリでは
**Wilson を既定**とし、3 法を切替できる。

---

## 9. 非劣性試験

### 9a. 非劣性・2 標本 t 検定

engine: `R/calc_ttest_ni.R` の `calc_n_ttest_ni()`
（`pwr::pwr.t.test(alternative = "greater")`）

**入力例**

| 項目 | 値 |
|---|---|
| A 群平均 − B 群平均 (Δ) | 0（A と B は同等と想定） |
| A / B 群 SD | 4 / 4 |
| 非劣性マージン M | 2 |
| α（片側） | 0.025 |
| 目標検出力 | 0.80 |

シフト効果量 d_ni = (Δ + M) / sd_pooled = (0 + 2) / 4 = **0.5**

**このアプリの出力**
- 各群必要症例数: **64 例**

**検証**
片側 α = 0.025 の greater 検定は、両側 α = 0.05 の 2 標本 t 検定と同じ
必要 n を与える（d = 0.5 のとき）。つまりデザイン 1 と一致。
```r
pwr::pwr.t.test(d = 0.5, sig.level = 0.025, power = 0.80,
                type = "two.sample", alternative = "greater")
# n = 63.76 → 64
```
→ 一致

**参照文献**: Chow et al. (2018) Sec 3.2、Julious (2010) Ch 4。

### 9b. 非劣性・対応のある t 検定

engine: `R/calc_paired_ni.R` の `calc_n_paired_ni()`

**入力例**: 差の平均 = 0、差の SD = 4、M = 2、片側 α = 0.025、power = 0.80
→ d_ni = 0.5

**このアプリの出力**: **34 ペア**

**検証**
```r
pwr::pwr.t.test(d = 0.5, sig.level = 0.025, power = 0.80,
                type = "paired", alternative = "greater")
# n = 33.367 → 34
```
→ 一致

### 9c. 非劣性・二値（リスク差、正規近似）

engine: `R/calc_binary_ni.R` の `calc_n_binary_ni()`
（Chow 2018 Sec 4.2 の正規近似）

公式:
n_per_arm ≈ (z_{1−α} + z_{1−β})² · [p_A(1−p_A) + p_B(1−p_B)] / (p_A − p_B + M)²

**入力例 1**

| 項目 | 値 |
|---|---|
| p_A | 0.70 |
| p_B | 0.70 |
| マージン M | 0.10 |
| α（片側） | 0.025 |
| 目標検出力 | 0.80 |

**このアプリの出力**: 各群 **330 例**

**検証**
```r
z_a <- qnorm(1 - 0.025); z_b <- qnorm(0.80)
ceiling((z_a + z_b)^2 * (0.7 * 0.3 + 0.7 * 0.3) / (0 + 0.10)^2)
# [1] 330
```
→ 一致

**入力例 2（Chow et al. 2018, Example 4.2 相当）**
p_A = 0.85、p_B = 0.85、M = 0.10、片側 α = 0.025、power = 0.80
→ 各群 **201 例**（Chow 例題と一致）

**既知の制約（重要）**
リスク差ベースの**正規近似**を採用しているため、p が 0 または 1 付近で
精度が落ちる。本アプリは計算結果の下に以下の警告を表示する:

> リスク差ベースの正規近似を使用しています。
> より正確な計算には Farrington-Manning 法を推奨します。

Farrington-Manning (1990) の制約付き尤度法を使いたい場合は、
`gsDesign::nBinomial(scale = "Difference")` や `PropCIs` 等の
外部パッケージで独立に検証することを推奨する。

---

## 自動検証テストの実行方法

すべてのデザインについて、`testthat` による自動検証テストが
`tests/testthat/` 配下に置かれている。プロジェクトのルートで R を起動し:

```r
source("tests/testthat.R")
```

または:

```r
testthat::test_dir("tests/testthat")
```

を実行すれば、次の観点で計算が自動検証される:

1. **教科書例題との一致** — Cohen (1988) の d = 0.2 / 0.5 / 0.8 ケースなど
2. **`pwr` / `binom` 直接呼び出しとの一致** — 各 engine が包んでいる関数の
   出力と完全に一致することを `expect_equal` で検証
3. **単調性** — α を下げる／検出力を上げる／マージンを小さくする／半幅を
   小さくすると必要 n が単調に増える
4. **脱落率の反映** — `n_randomized = ceiling(n_evaluable / (1 - L))` が
   正しく計算される
5. **エラー検出** — 不正入力（SD ≤ 0、p ∉ [0, 1]、r ∉ [−1, 1] など）で
   `stopifnot` が落ちる

現在（2026-04-17 時点）、**計 111 件のテストがすべて合格**する。

---

## 参考文献

- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences*
  (2nd ed.). Lawrence Erlbaum Associates.
- Chow, S.-C., Shao, J., Wang, H., Lokhnygina, Y. (2018).
  *Sample Size Calculations in Clinical Research* (3rd ed.). CRC Press.
- Julious, S. A. (2010). *Sample Sizes for Clinical Trials*. CRC Press.
- Fleiss, J. L. (1981). *Statistical Methods for Rates and Proportions*
  (2nd ed.). John Wiley & Sons.
- Wilson, E. B. (1927). Probable inference, the law of succession, and
  statistical inference. *Journal of the American Statistical Association*
  22: 209–212.
- Clopper, C. J., Pearson, E. S. (1934). The use of confidence or fiducial
  limits illustrated in the case of the binomial. *Biometrika* 26: 404–413.
- Farrington, C. P., Manning, G. (1990). Test statistics and sample size
  formulae for comparative binomial trials with null hypothesis of non-zero
  risk difference or non-unity relative risk. *Statistics in Medicine* 9:
  1447–1454.
- Champely, S. (2020). *pwr: Basic Functions for Power Analysis.* R package.
  <https://CRAN.R-project.org/package=pwr>
- Dorai-Raj, S. (2022). *binom: Binomial Confidence Intervals for Several
  Parameterizations.* R package.
  <https://CRAN.R-project.org/package=binom>
- Faul, F., Erdfelder, E., Lang, A.-G., Buchner, A. (2007). G*Power 3:
  A flexible statistical power analysis program for the social, behavioral,
  and biomedical sciences. *Behavior Research Methods* 39: 175–191.
