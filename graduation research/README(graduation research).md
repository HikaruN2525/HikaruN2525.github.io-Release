# About graduation research：卒業研究について

## Title：タイトル
Reorganization of industry categories and risk control using similarity measures between time series data

時系列データ間の類似尺度を用いた業種区分の再編とリスクコントロール

## Summary：概要
Conventional industry classifications do not reflect the business model changes and business diversification observed in many large firms in recent years.

Therefore, in this study, we reorganize the traditional industry classifications using shape-based non-hierarchical clustering based on similarity measures among time-series data, and call the reorganized ones industry classifications in terms of convenience. We then reorganized the traditional industry classifications by using similarity measures among time-series data, and defined new industry classifications, which are not industry classifications but are referred to as industry classifications for convenience, to examine whether they can provide better risk control than traditional industry classifications when constructing portfolios.

The results of the study showed that, depending on the method used, the risk could be improved by 1% or less, the investment efficiency based on return and trainer ratio could be improved by nearly 3%, and the investment efficiency based on Sharpe ratio could be improved by 13% or more compared to the TOPIX17 industry category.

Therefore, it can be said that, apart from the conventional industry classification, a new industry classification should be defined that focuses on firm-by-firm characteristics using shape-based non-hierarchical clustering based on similarity measures among time-series data.

従来の業種区分は、近年、多くの大企業で見られるビジネスモデルの変更・事業の多角化を反映していない。

そこで本研究では、時系列データ間の類似尺度を用いた形状ベースの非階層クラスタリ ングにより従来の業種区分を再編し、その再編したものを業種ではないが便宜的に業種区分と呼ぶこととして新たな業種区分を定義し、ポートフォリオを構築する際に、従来の業種区分よりも優れたリスクコントロールを図ることができるかを検証した。

研究の結果、手法によっては TOPIX17業種区分に比べ、リスクは 1%以下の向上、リターンとトレイナーレシオによる運用効率は 3%近くの向上、シャープレシオによる運用効率は 13%以上の向上が見込めることがわかった。

したがって、従来の業種区分とは別に、時系列データ間の類似尺度を用いた形状ベースの非階層クラスタリ ングを用いて、企業単位の特徴に着目した新たな業種区分を定義すべきであるといえる。

## About "graduation research code.R"：「graduation research code.R」について

This assumes a portfolio operation of four stocks.

First, the code simulates each industry category.

Next, the code visualizes the results using a box-and-whisker diagram.

Furthermore, the code performs statistical tests. There are two types of statistical tests: the Jarque–Bera test and the multiple comparison method (Steel-Dwass method).

これは、4銘柄のポートフォリオ運用を想定している。

まず、各業種区分のシミュレーションを行うコードである。

続いて、箱ひげ図による結果の可視化を行うコードである。

さらに、統計的検定を行うコードである。これは、ジャック-ベラ検定と多重比較法(スティール-ドゥワス法)の二つがある。
