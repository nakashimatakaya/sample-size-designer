# SampleSizeApp

## プロジェクトの目的

臨床研究者がブラウザ上でポチポチ操作するだけで、試験に必要な症例数を計算できる Shiny アプリを作る。

- 使う人は統計の専門家ではない臨床医。
- 計算の結果だけでなく、使った公式と参考文献も画面に表示する。
- 論文記載用テキスト（日本語版・英語版）と、再現用の R コードも自動生成する。

## 技術

- 言語: R
- フレームワーク: Shiny + bslib（Bootstrap 5、bootswatch "flatly"）
- サンプルサイズ計算は既存の R パッケージ（`pwr`, `binom`, `stats`）を使う
- 自前で公式を実装するのではなく、必ず既存パッケージを呼ぶ
- UI は Shiny モジュールで 1 デザイン = 1 モジュールに分離

## 対応しているデザイン

| カテゴリ | デザイン | ファイル | 使用関数 |
|---|---|---|---|
| 2 群・連続量 | 2 標本 t 検定（群別） | `R/calc_ttest.R`（mode1） | `pwr.t.test(two.sample, two.sided)` |
| 2 群・連続量 | 2 標本 t 検定（差と群別 SD） | `R/calc_ttest.R`（mode2） | `pwr.t.test(two.sample, two.sided)` |
| 2 群・連続量 | 対応のある t 検定 | `R/calc_paired.R` | `pwr.t.test(paired, two.sided)` |
| 2 群・二値 | χ² 検定 | `R/calc_binary_chisq.R` | `pwr.2p.test(two.sided)` |
| 2 群・二値 | Fisher の正確検定 | `R/calc_binary_fisher.R` | `pwr.2p.test`（χ² 近似） |
| 1 標本 | 平均（精度ベース） | `R/calc_one_mean_precision.R` | `stats::qnorm` |
| 1 標本 | 割合（精度ベース） | `R/calc_one_prop_precision.R` | `binom::binom.confint` / `stats::qnorm` |
| 非劣性 | 2 標本 t 検定（片側） | `R/calc_ttest_ni.R` | `pwr.t.test(two.sample, greater)` |
| 非劣性 | 対応のある t 検定（片側） | `R/calc_paired_ni.R` | `pwr.t.test(paired, greater)` |
| 非劣性 | 二値（リスク差、正規近似） | `R/calc_binary_ni.R` | `stats::qnorm`（Chow 2018 Sec 4.2） |

## ファイル配置ルール

- 計算層（engine）は `R/calc_*.R` にデザインごとに 1 ファイル。
- engine は Shiny や UI のことを一切知らない。
- UI 関連は `R/ui_helpers.R`（プロット・dispatch・value_box）と `R/modules.R`
  （Shiny モジュール）に分離する。
- テキスト生成は `R/paper_text.R`（論文）と `R/r_code_gen.R`（再現コード）と
  `R/glossary.R`（用語集）に分離する。
- `app.R` は薄くする。`page_navbar` の組み立てとモジュールの呼び出しのみ。

## 共通 result schema

すべての `calc_n_*` 関数は `R/common.R` の `make_result()` を使って同じ形の
list を返す:

```
n_per_arm_evaluable   : 1 群あたり解析対象必要数
n_per_arm_randomized  : 1 群あたり登録必要数 = ceiling(evaluable / (1 - L))
n_total_evaluable     : 合計解析対象必要数
n_total_randomized    : 合計登録必要数
achieved_power        : 達成検出力（精度ベースは NA）
backend_pkg           : 使用 R パッケージ名
backend_fun           : 使用 R 関数シグネチャ
formula_ref           : 公式の出典文字列（例: "Cohen 1988"）
dropout               : 脱落率 L
n_arms                : 群数（2 群比較 = 2、対応あり／1 標本 = 1）
```

engine 固有の情報（h, d, margin, method, warning など）は `extras` 経由で
追加する。

## 守ってほしいルール

- 計算を書くときは必ず既存パッケージを使う。関数の出典と参照文献をコメントに書く。
- 計算部分と画面部分は別ファイルに分ける。
- テスト（`testthat`）を書く。教科書例題または `pwr` / `binom` 直接呼びの
  結果との一致で答え合わせする。
- コメントは日本語で書いてよい。
- ファイルを作ったり変更したりする前に、何をするか日本語で説明してから実行する。
- 新しいデザインを追加するときは:
  1. `R/calc_<name>.R` を作り `make_result()` で返す
  2. `tests/testthat/test-calc_<name>.R` を追加
  3. `R/ui_helpers.R` の `design_plot_vars()` / `compute_result_dispatch()` /
     `compute_y_dispatch()` に分岐を追加
  4. `R/paper_text.R` に JP/EN テンプレートを追加
  5. `R/r_code_gen.R` に R コードテンプレートを追加
  6. `R/glossary.R` の `result_hint_text()` にヒントを追加
  7. `R/modules.R` に UI/server モジュールを追加
  8. `app.R` の `page_navbar` にタブを追加

## 開発者について

日本の生物統計家。R は使えるが Shiny は初心者。プログラミングの複雑な用語は避けて、何をしているか日本語で説明しながら進めてほしい。
