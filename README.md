# SampleSizeApp

臨床研究者がブラウザ上で操作するだけで、試験に必要な症例数を計算できる Shiny アプリ。
計算はすべて既存の R パッケージ（`pwr` / `binom` / `stats`）に委譲しており、
論文記載用のテキスト（日本語版・英語版）や再現用 R コードも自動生成する。

## 対応するデザイン

| カテゴリ | デザイン | 使用パッケージ／関数 | 出典 |
|---|---|---|---|
| 2 群比較・連続量 | 2 標本 t 検定（群別入力） | `pwr::pwr.t.test(two.sample, two.sided)` | Cohen 1988 |
| 2 群比較・連続量 | 2 標本 t 検定（差と群別 SD で入力） | `pwr::pwr.t.test(two.sample, two.sided)` | Cohen 1988 |
| 2 群比較・連続量 | 対応のある t 検定 | `pwr::pwr.t.test(paired, two.sided)` | Cohen 1988 |
| 2 群比較・二値 | χ² 検定 | `pwr::pwr.2p.test(two.sided)` | Cohen 1988 |
| 2 群比較・二値 | Fisher の正確検定（χ² 近似） | `pwr::pwr.2p.test(two.sided)` | Cohen 1988 ※近似 |
| 1 標本（精度ベース） | 平均値の CI 半幅 | `stats::qnorm`（`n = (z·SD/E)²`） | 標準公式 |
| 1 標本（精度ベース） | 割合の CI 半幅（Wilson / Exact / 正規近似） | `binom::binom.confint`（探索）／`stats::qnorm` | Wilson 1927 / Clopper-Pearson 1934 |
| 非劣性 | 2 標本 t 検定 | `pwr::pwr.t.test(two.sample, greater)` | Chow et al. 2018 |
| 非劣性 | 対応のある t 検定 | `pwr::pwr.t.test(paired, greater)` | Chow et al. 2018 |
| 非劣性 | 二値（リスク差、正規近似） | `stats::qnorm`（Chow Sec 4.2 公式） | Chow et al. 2018 |

Fisher 版は厳密な検出力計算が `pwr` に未搭載のため χ² 近似を使う。
画面に警告を表示する。希少事象（p < 0.05 または p > 0.95）ではシミュレーション
ベースの検出力計算の併用を推奨する。

非劣性・二値は Chow et al. (2018) の正規近似。
より正確な計算には Farrington-Manning 法の使用を推奨。

## 必要な R パッケージ

R コンソールで一度だけ実行する:

```r
install.packages(c("shiny", "bslib", "pwr", "binom", "ggplot2", "testthat"))
```

| パッケージ | 用途 |
|---|---|
| `shiny`    | アプリのフレームワーク |
| `bslib`    | モダンな Bootstrap 5 テーマ／レイアウト |
| `pwr`      | 検出力・必要症例数の計算（t 検定、2 標本割合） |
| `binom`    | 二項分布に基づく CI（Wilson / Exact） |
| `ggplot2`  | 感度分析グラフの描画 |
| `testthat` | 計算関数のテスト |

## アプリの起動方法

プロジェクトのルート（このファイルがあるフォルダ）で R を起動し:

```r
shiny::runApp(".")
```

または RStudio で `app.R` を開き、右上の「Run App」ボタンを押す。

## テストの実行方法

```r
source("tests/testthat.R")
```

各 engine ごとに教科書例題または `pwr` / `binom` 直接呼び出しとの一致を検証する。

## ファイル構成

```
SampleSizeApp/
├── app.R                              # エントリポイント（page_navbar を組み立てる）
├── R/
│   ├── common.R                       # make_result()（共通 result schema）
│   ├── calc_ttest.R                   # 2 標本 t 検定（モード1 / モード2）
│   ├── calc_paired.R                  # 対応のある t 検定
│   ├── calc_binary_chisq.R            # 二値・χ²
│   ├── calc_binary_fisher.R           # 二値・Fisher（χ² 近似）
│   ├── calc_one_mean_precision.R      # 1 標本・平均（精度）
│   ├── calc_one_prop_precision.R      # 1 標本・割合（精度）
│   ├── calc_ttest_ni.R                # 非劣性・2 標本 t
│   ├── calc_paired_ni.R               # 非劣性・対応のある t
│   ├── calc_binary_ni.R               # 非劣性・二値（Chow 2018）
│   ├── paper_text.R                   # 論文記載用テキスト（日英）
│   ├── r_code_gen.R                   # 再現用 R コード生成
│   ├── glossary.R                     # ツールチップ・用語集・ガイド文
│   ├── ui_helpers.R                   # dispatch/プロット/value_box
│   └── modules.R                      # Shiny モジュール（デザインごと）
├── tests/
│   ├── testthat.R
│   └── testthat/
│       ├── test-calc_ttest.R
│       ├── test-calc_paired.R
│       ├── test-calc_binary_chisq.R
│       ├── test-calc_binary_fisher.R
│       ├── test-calc_one_mean_precision.R
│       ├── test-calc_one_prop_precision.R
│       ├── test-calc_ttest_ni.R
│       ├── test-calc_paired_ni.R
│       └── test-calc_binary_ni.R
├── CLAUDE.md
└── README.md
```

## 共通の結果スキーマ

すべての `calc_n_*` 関数は `make_result()` で生成される list を返す:

```
n_per_arm_evaluable   : 1 群あたり解析対象必要数
n_per_arm_randomized  : 1 群あたり登録必要数 = ceiling(evaluable / (1 - L))
n_total_evaluable     : 合計解析対象必要数
n_total_randomized    : 合計登録必要数
achieved_power        : 達成検出力（精度ベースなど該当しない場合は NA）
backend_pkg           : 計算に使用した R パッケージ名
backend_fun           : 計算に使用した R 関数（シグネチャ込み）
formula_ref           : 公式の出典（例: "Cohen 1988"）
dropout               : 脱落率 L
n_arms                : 群数（2 群比較 = 2、1 標本・対応あり = 1）
```

対応のある t 検定や 1 標本では `n_arms = 1` のため `n_total_* == n_per_arm_*`。

## アプリの公開（shinyapps.io）

RStudio の Console で:

```r
library(rsconnect)
deployApp(appName = "sample-size-designer")
```

初回は shinyapps.io の認証が必要。
詳細は <https://shiny.posit.co/r/articles/share/shinyapps/> を参照。

## 参照文献

- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences* (2nd ed.). Lawrence Erlbaum Associates.
- Chow, S.-C., Shao, J., Wang, H., Lokhnygina, Y. (2018). *Sample Size Calculations in Clinical Research* (3rd ed.). CRC Press.
- Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. *JASA* 22: 209-212.
- Clopper, C. J., Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika* 26: 404-413.
- Champely, S. (2020). *pwr: Basic Functions for Power Analysis.* R package. <https://CRAN.R-project.org/package=pwr>
- Dorai-Raj, S. (2022). *binom: Binomial Confidence Intervals for Several Parameterizations.* R package. <https://CRAN.R-project.org/package=binom>
