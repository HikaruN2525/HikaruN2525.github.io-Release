#github公開用コード
#シミュレーション(モンテカルロ,4銘柄)

#===========================================================================================================
##①必要なパッケージの読み込み
#===========================================================================================================

library(openxlsx) #xlsxファイル読み込みのためのパッケージ
library(ggplot2) #グラフ描画のためのパッケージ
library(dplyr) #層化抽出法のためのパッケージ
library(dtwclust) #再現性のためのパッケージ(これを実行しないと少し異なる結果が出る)

#===========================================================================================================
##②TOPIX17業種分類によるシミュレーション
#===========================================================================================================

#年率換算のための変数
N = 245

#データの読み込み
#配当込み収益率データの読み込み
DF_TOPIX17_00 <- read.xlsx("./original data.xlsx",sheet=5,startRow=1,colNames=TRUE,rowNames=TRUE)
#配当込み収益率データの使用部分のみ抽出
DF_TOPIX17_0 <- DF_TOPIX17_00[,1:245] #アウトオブサンプル期間2021年の1年間分
#DF_TOPIX17_0 <- DF_TOPIX17_00[,1:121] #アウトオブサンプル期間2021年の半年分(1〜6月)
#クラスタリングデータの読み込み
DF_TOPIX17_01 <- read.xlsx("./TOPIX17 rename.xlsx",sheet=2,colNames=TRUE,rowNames=TRUE)
#β値データの読み込み
DF_TOPIX17_02 <- read.xlsx("./Beta value.xlsx",sheet=1,colNames=TRUE,rowNames=TRUE)
#上記3つのデータを結合
DF_TOPIX17_1 <- cbind(DF_TOPIX17_01,DF_TOPIX17_02,DF_TOPIX17_0)
#データ要約の表示
head(DF_TOPIX17_1)

#最終的な結果を確認するために空の格納庫となるベクトルを準備
r_data_TOPIX17_4 = NULL #リターンの結果を格納
sd_data_TOPIX17_4 = NULL #リスクの結果を格納
Beta_data_TOPIX17_4 = NULL #β値の結果を格納
sharpe_data_TOPIX17_4 = NULL #シャープレシオの結果を格納
sharpe_data_TOPIX17_4_1 = NULL #シャープレシオの結果を格納
sharpe_data_TOPIX17_4_2 = NULL #シャープレシオの結果を格納
sharpe_data_TOPIX17_4_3 = NULL #シャープレシオの結果を格納
treynor_data_TOPIX17_4 = NULL #トレイナーレシオの結果を格納
sortino_data_TOPIX17_4 = NULL #ソルティノレシオの結果を格納
brands4_name_TOPIX17_m = data.frame() #銘柄名を格納

#再現性保持のためにseed値を準備
set.seed(0)

#層化抽出とシミュレーションの実行
for (i in 1:100000000){
  #層化抽出法(replace=FALSEにより重複を防ぐ,sizeで抽出数を指定)
  Strasamp_TOPIX17_4 <- sample_n(tbl = DF_TOPIX17_1 ,replace = FALSE ,size = 4)
  Strasamp_TOPIX17_4 <- Strasamp_TOPIX17_4 #層化抽出の繰り返し文
  #条件分岐で4つの銘柄の業種が異なる時にシミュレーションを適用
  #(!=は等しくないという意味をもつ)
  if(Strasamp_TOPIX17_4$TOPIX17[1]!=Strasamp_TOPIX17_4$TOPIX17[2]&&Strasamp_TOPIX17_4$TOPIX17[1]!=Strasamp_TOPIX17_4$TOPIX17[3]&&Strasamp_TOPIX17_4$TOPIX17[1]!=Strasamp_TOPIX17_4$TOPIX17[4]&&Strasamp_TOPIX17_4$TOPIX17[2]!=Strasamp_TOPIX17_4$TOPIX17[3]&&Strasamp_TOPIX17_4$TOPIX17[2]!=Strasamp_TOPIX17_4$TOPIX17[4]&&Strasamp_TOPIX17_4$TOPIX17[3]!=Strasamp_TOPIX17_4$TOPIX17[4]){
    print(Strasamp_TOPIX17_4[1]) #抽出された銘柄の書き出し
    DF_TOPIX17_2_4 <- t(Strasamp_TOPIX17_4) #抽出されたデータを転置
    DF_TOPIX17_3_4 <- DF_TOPIX17_2_4[-2:-1,] #クラスタ番号の行のみを削除
    stock_Return_TOPIX17_4 = as.matrix( DF_TOPIX17_3_4[ , 1:4] ) #行列として扱う
    
    #銘柄名を格納する
    brands4_name_TOPIX17_1_m <- t(as.matrix(row.names(Strasamp_TOPIX17_4[1])))
    brands4_name_TOPIX17_m <- rbind( brands4_name_TOPIX17_m, brands4_name_TOPIX17_1_m )
    
    #変数の設定
    mc_rep_TOPIX17_4 = 10000 #Number of Monte Carlo Simulations(モンテカルロシミュレーションの回数)
    training_days_TOPIX17_4 = 30 #将来何日を予測するかの日
    
    ##抽出した銘柄の処理
    
    #各銘柄に対する投資の重みつけ(ここでは等分つまり1/4ずつとしている)
    portfolio_Weights_TOPIX17_4 = t(as.matrix(rep(1/ncol(stock_Return_TOPIX17_4), ncol(stock_Return_TOPIX17_4))))
    print(portfolio_Weights_TOPIX17_4)
    
    #共分散行列と株式リターンの平均値を計算
    #株式リターンの分散共分散行列を取得
    coVarMat_TOPIX17_4 = cov(stock_Return_TOPIX17_4)
    miu_TOPIX17_4 = colMeans(stock_Return_TOPIX17_4)
    #Extend the vector to a matrix(ベクトルを行列に拡張)
    Miu_TOPIX17_4 = matrix(rep(miu_TOPIX17_4, training_days_TOPIX17_4), nrow = 4)
    
    #シミュレートされた30日間のポートフォリオリターンの初期化
    portfolio_Returns_30_TOPIX17_m_4 = matrix(0, training_days_TOPIX17_4, mc_rep_TOPIX17_4)
    
    #各銘柄のβ値
    brand_A.Beta_TOPIX17_4 <- DF_TOPIX17_2_4[2,1] #1つ目の銘柄のβを抽出
    brand_B.Beta_TOPIX17_4 <- DF_TOPIX17_2_4[2,2] #2つ目の銘柄のβを抽出
    brand_C.Beta_TOPIX17_4 <- DF_TOPIX17_2_4[2,3] #3つ目の銘柄のβを抽出
    brand_D.Beta_TOPIX17_4 <- DF_TOPIX17_2_4[2,4] #4つ目の銘柄のβを抽出
    each.Beta_TOPIX17_4 <- c(brand_A.Beta_TOPIX17_4,brand_B.Beta_TOPIX17_4,brand_C.Beta_TOPIX17_4,brand_D.Beta_TOPIX17_4) #4つの銘柄のβを配列化
    
    ##モンテカルロシミュレーション
    
    #1つのポートフォリオに対して10000回のモンテカルロシミュレーションを実行
    for (k in 1:mc_rep_TOPIX17_4) {
      Z_TOPIX17_4 = matrix ( rnorm( dim(stock_Return_TOPIX17_4)[2] * training_days_TOPIX17_4 ), ncol = training_days_TOPIX17_4 )
      #Lower Triangular Matrix from our Choleski Factorization(コレスキー分解による下三角行列)
      #正定値行列近似Q = nearPD(coVarMat)
      L_TOPIX17_4 = t( chol( coVarMat_TOPIX17_4 ) )
      #Calculate stock returns for each day(毎日の株式リターンを計算)
      daily_Returns_TOPIX17_4 = Miu_TOPIX17_4 + L_TOPIX17_4%*%Z_TOPIX17_4  
      #Calculate portfolio returns for 30 days(30日間のポートフォリオリターンを計算)
      portfolio_Returns_30_TOPIX17_4 = portfolio_Weights_TOPIX17_4 %*% daily_Returns_TOPIX17_4
      #Add it to the monte-carlo matrix(それをモンテカルロ行列に追加)
      portfolio_Returns_30_TOPIX17_m_4[,k] = portfolio_Returns_30_TOPIX17_4;
    }
    #モンテカルロシミュレーションの終了
    
    #ポートフォリオのβ値計算
    portfolio_Beta_TOPIX17_4 <- portfolio_Weights_TOPIX17_4%*%each.Beta_TOPIX17_4
    print(portfolio_Beta_TOPIX17_4)
    
    #1シナリオごと(つまり列ごと)の結果を集計
    portfolio_return_TOPIX17_4_0 = mean(portfolio_Returns_30_TOPIX17_m_4[,]) #10000シナリオのリターンの平均(期待収益)
    portfolio_return_TOPIX17_4 = ((portfolio_return_TOPIX17_4_0+1)^(N))-1 #年率換算
    return_TOPIX17 = apply(portfolio_Returns_30_TOPIX17_m_4, 2, mean) #期待収益
    Y_return_TOPIX17 = ((return_TOPIX17+1)^(N))-1 #年率換算
    risk_TOPIX17 = apply(portfolio_Returns_30_TOPIX17_m_4, 2, sd) #標準偏差(リスク)
    Y_risk_TOPIX17 = risk_TOPIX17*sqrt(N) #年率換算
    portfolio_risk_TOPIX17_4 = mean(Y_risk_TOPIX17) #10000シナリオの標準偏差(リスク)の平均
    sharpe_TOPIX17 = Y_return_TOPIX17/Y_risk_TOPIX17 #シャープレシオの計算(年率換算後の結果を用いる)
    sharpe_TOPIX17_0 = c(sharpe_TOPIX17[(sharpe_TOPIX17<0)])  #シャープレシオが負の値であるもの
    sharpe_TOPIX17_1 = c(sharpe_TOPIX17[(sharpe_TOPIX17>0)])  #シャープレシオが正の値であるもの
    sharpe_TOPIX17_2 = rep(0,10000-length(sharpe_TOPIX17_1)) #シャープレシオが負の値であるものの個数分だけ0を生成
    sharpe_TOPIX17_3 = c(sharpe_TOPIX17_1,sharpe_TOPIX17_2) #個数を10000個に戻す(つまり正の値と0のベクトルを結合)
    portfolio_sharpe_TOPIX17_4 = mean(sharpe_TOPIX17_3) #負のものを0とした時の平均(全て分母10000)
    portfolio_sharpe_TOPIX17_4_1 = mean(sharpe_TOPIX17) #全てのシャープレシオの平均
    portfolio_sharpe_TOPIX17_4_2 = mean(sharpe_TOPIX17_1) #正のものだけの平均(分母がシナリオによって異なる)
    portfolio_sharpe_TOPIX17_4_3 = mean(sharpe_TOPIX17_0) #負のものだけの平均(分母がシナリオによって異なる)
    
    treynor_TOPIX17 <- Y_return_TOPIX17/portfolio_Beta_TOPIX17_4 #トレイナーレシオの計算(年率換算後の結果を用いる)
    portfolio_treynor_TOPIX17_4 = mean(treynor_TOPIX17)
    
    p_TOPIX17 <- portfolio_Returns_30_TOPIX17_m_4
    p_TOPIX17_2 <- p_TOPIX17
    p_TOPIX17_2[(p_TOPIX17_2>0)] <- NaN
    risk2_TOPIX17 <- apply(p_TOPIX17_2,2,sd, na.rm=T) #下方リスクの算出
    Y_risk2_TOPIX17 = risk2_TOPIX17*sqrt(N) #年率換算
    sortino_TOPIX17 = Y_return_TOPIX17/Y_risk2_TOPIX17 #ソルティノレシオの計算(年率換算後の結果を用いる)
    portfolio_sortino_TOPIX17_4 <- sum(sortino_TOPIX17[!is.na(sortino_TOPIX17)])/10000 
    #meanだと分母が変わってしまう(このために以下の式はダメ)ので、10000で割ることとする
    #portfolio_sortino_TOPIX17_4 = mean(sortino_TOPIX17[!is.na(sortino_TOPIX17)])
    #全てNaN(ずっと正の値)となる列(シナリオ)が1つでもある場合に結果にNAを示すこととなるため以下でもダメ
    #portfolio_sortino_TOPIX17_4 = mean(sortino_TOPIX17)
    #全てNaNの列があるかないかは以下で確認できる
    #p_TOPIX17_3 <- p_TOPIX17_2[!apply(is.na(p_TOPIX17_2), 2, all)]
    #str(p_TOPIX17_3) これがnum[1:300000]でなければ全てNaNの列があることとなる
    
    ##10000個の結果をベクトルに格納
    
    #ポートフォリオの統計を表示する
    print(c(portfolio_return_TOPIX17_4,portfolio_risk_TOPIX17_4,
            portfolio_sharpe_TOPIX17_4,portfolio_sharpe_TOPIX17_4_1,
            portfolio_sharpe_TOPIX17_4_2,portfolio_sharpe_TOPIX17_4_3,portfolio_treynor_TOPIX17_4,portfolio_sortino_TOPIX17_4))
    
    #最終的な結果を確認するために格納庫(ベクトル)にデータを入れ込む
    #ポートフォリオの期待収益率を格納
    r_data_TOPIX17_4 = c(r_data_TOPIX17_4,portfolio_return_TOPIX17_4)
    #ポートフォリオのリスクを格納
    sd_data_TOPIX17_4 = c(sd_data_TOPIX17_4,portfolio_risk_TOPIX17_4)
    #ポートフォリオのβ値を格納
    Beta_data_TOPIX17_4 = c(Beta_data_TOPIX17_4,portfolio_Beta_TOPIX17_4)
    #ポートフォリオのシャープレシオを格納
    sharpe_data_TOPIX17_4 = c(sharpe_data_TOPIX17_4,portfolio_sharpe_TOPIX17_4)
    sharpe_data_TOPIX17_4_1 = c(sharpe_data_TOPIX17_4_1,portfolio_sharpe_TOPIX17_4_1)
    sharpe_data_TOPIX17_4_2 = c(sharpe_data_TOPIX17_4_2,portfolio_sharpe_TOPIX17_4_2)
    sharpe_data_TOPIX17_4_3 = c(sharpe_data_TOPIX17_4_3,portfolio_sharpe_TOPIX17_4_3)
    #ポートフォリオのトレイナーレシオを格納
    treynor_data_TOPIX17_4 = c(treynor_data_TOPIX17_4,portfolio_treynor_TOPIX17_4)
    #ポートフォリオのソルティノレシオを格納
    sortino_data_TOPIX17_4 = c(sortino_data_TOPIX17_4,portfolio_sortino_TOPIX17_4)
    
    #試行回数の確認
    print(length(r_data_TOPIX17_4))
    
    #for文終了条件
    if(length(r_data_TOPIX17_4) == 10000 && length(sd_data_TOPIX17_4) == 10000 && length(Beta_data_TOPIX17_4) == 10000){
      break #10000個のデータを取得したらシミュレーションを終える
    }
  }
}

#for文内で格納した10000個のポートフォリオのデータを表示する
r_data_TOPIX17_4 #10000個のポートフォリオのリターン
sd_data_TOPIX17_4 #10000個のポートフォリオのリスク
sharpe_data_TOPIX17_4 #10000個のポートフォリオの置き換えシャープレシオ
sharpe_data_TOPIX17_4_1 #10000個のポートフォリオの全部シャープレシオ
sharpe_data_TOPIX17_4_2 #10000個のポートフォリオの正シャープレシオ
sharpe_data_TOPIX17_4_3 #10000個のポートフォリオの負シャープレシオ
treynor_data_TOPIX17_4 #10000個のポートフォリオのトレイナーレシオ
sortino_data_TOPIX17_4 #10000個のポートフォリオのソルティノレシオ
Beta_data_TOPIX17_4 #10000個のポートフォリオのβ値
brands4_name_TOPIX17_m #選ばれた銘柄の一覧

#10000個のポートフォリオのリターンとリスク、β値の平均を表示
TOPIX17_r_4 <- mean(r_data_TOPIX17_4) #10000個のポートフォリオのリターンの平均
TOPIX17_sd_4 <- mean(sd_data_TOPIX17_4) #10000個のポートフォリオのリスクの平均
TOPIX17_Beta_4 <- mean(Beta_data_TOPIX17_4) #10000個のポートフォリオのβ値の平均
TOPIX17_sharpe_4 <-mean(sharpe_data_TOPIX17_4) #10000個のポートフォリオの置き換えシャープレシオ
TOPIX17_sharpe_4_1 <-mean(sharpe_data_TOPIX17_4_1) #10000個のポートフォリオの全部シャープレシオの平均
TOPIX17_sharpe_4_2 <-mean(sharpe_data_TOPIX17_4_2) #10000個のポートフォリオの正シャープレシオ
TOPIX17_sharpe_4_3 <-mean(sharpe_data_TOPIX17_4_3) #10000個のポートフォリオの負シャープレシオ
TOPIX17_treynor_4 <- mean(treynor_data_TOPIX17_4) #10000個のポートフォリオのトレイナーレシオ
TOPIX17_sortino_4 <- mean(sortino_data_TOPIX17_4) #10000個のポートフォリオのソルティノレシオ
print(c("Mean Return","Mean Risk","Mean Beta","Mean Sharpe(+0)",
        "Mean Sharpe(+-)","Mean Sharpe(+)","Mean Sharpe(-)","Mean Treynor","Mean Sortino"))#結果のラベル
print(c(TOPIX17_r_4,TOPIX17_sd_4,TOPIX17_Beta_4,TOPIX17_sharpe_4,
        TOPIX17_sharpe_4_1,TOPIX17_sharpe_4_2,TOPIX17_sharpe_4_3,TOPIX17_treynor_4,TOPIX17_sortino_4)) #結果の表示

#===========================================================================================================
##③6つのSectorによるシミュレーション
#===========================================================================================================

#年率換算のための変数
N = 245

#データの読み込み
#配当込み収益率データの読み込み
DF_Sector_00 <- read.xlsx("./original data.xlsx",sheet=5,startRow=1,colNames=TRUE,rowNames=TRUE)
#配当込み収益率データの使用部分のみ抽出
DF_Sector_0 <- DF_Sector_00[,1:245] #アウトオブサンプル期間2021年の1年間分
#DF_Sector_0 <- DF_Sector_00[,1:121] #アウトオブサンプル期間2021年の半年分(1〜6月)
#クラスタリングデータの読み込み
DF_Sector_01 <- read.xlsx("./TOPIX17 rename.xlsx",sheet=4,colNames=TRUE,rowNames=TRUE)
#β値データの読み込み
DF_Sector_02 <- read.xlsx("./Beta value.xlsx",sheet=1,colNames=TRUE,rowNames=TRUE)
#上記3つのデータを結合
DF_Sector_1 <- cbind(DF_Sector_01,DF_Sector_02,DF_Sector_0)
#データ要約の表示
head(DF_Sector_1)

#最終的な結果を確認するために空の格納庫となるベクトルを準備
r_data_Sector_4 = NULL #リターンの結果を格納
sd_data_Sector_4 = NULL #リスクの結果を格納
Beta_data_Sector_4 = NULL #β値の結果を格納
sharpe_data_Sector_4 = NULL #シャープレシオの結果を格納
sharpe_data_Sector_4_1 = NULL #シャープレシオの結果を格納
sharpe_data_Sector_4_2 = NULL #シャープレシオの結果を格納
sharpe_data_Sector_4_3 = NULL #シャープレシオの結果を格納
treynor_data_Sector_4 = NULL #トレイナーレシオの結果を格納
sortino_data_Sector_4 = NULL #ソルティノレシオの結果を格納
brands4_name_Sector_m = data.frame() #銘柄名を格納

#再現性保持のためにseed値を準備
set.seed(0)

#層化抽出とシミュレーションの実行
for (i in 1:100000000){
  #層化抽出法(replace=FALSEにより重複を防ぐ,sizeで抽出数を指定)
  Strasamp_Sector_4 <- sample_n(tbl = DF_Sector_1 ,replace = FALSE ,size = 4)
  Strasamp_Sector_4 <- Strasamp_Sector_4 #層化抽出の繰り返し文
  #条件分岐で4つの銘柄の業種が異なる時にシミュレーションを適用
  #(!=は等しくないという意味をもつ)
  if(Strasamp_Sector_4$Sector[1]!=Strasamp_Sector_4$Sector[2]&&Strasamp_Sector_4$Sector[1]!=Strasamp_Sector_4$Sector[3]&&Strasamp_Sector_4$Sector[1]!=Strasamp_Sector_4$Sector[4]&&Strasamp_Sector_4$Sector[2]!=Strasamp_Sector_4$Sector[3]&&Strasamp_Sector_4$Sector[2]!=Strasamp_Sector_4$Sector[4]&&Strasamp_Sector_4$Sector[3]!=Strasamp_Sector_4$Sector[4]){
    print(Strasamp_Sector_4[1]) #抽出された銘柄の書き出し
    DF_Sector_2_4 <- t(Strasamp_Sector_4) #抽出されたデータを転置
    DF_Sector_3_4 <- DF_Sector_2_4[-2:-1,] #クラスタ番号の行のみを削除
    stock_Return_Sector_4 = as.matrix( DF_Sector_3_4[ , 1:4] ) #行列として扱う
    
    #銘柄名を格納する
    brands4_name_Sector_1_m <- t(as.matrix(row.names(Strasamp_Sector_4[1])))
    brands4_name_Sector_m <- rbind( brands4_name_Sector_m, brands4_name_Sector_1_m )
    
    #変数の設定
    mc_rep_Sector_4 = 10000 #Number of Monte Carlo Simulations(モンテカルロシミュレーションの回数)
    training_days_Sector_4 = 30 #将来何日を予測するかの日
    
    ##抽出した銘柄の処理
    
    #各銘柄に対する投資の重みつけ(ここでは等分つまり1/4ずつとしている)
    portfolio_Weights_Sector_4 = t(as.matrix(rep(1/ncol(stock_Return_Sector_4), ncol(stock_Return_Sector_4))))
    print(portfolio_Weights_Sector_4)
    
    #共分散行列と株式リターンの平均値を計算
    #株式リターンの分散共分散行列を取得
    coVarMat_Sector_4 = cov(stock_Return_Sector_4)
    miu_Sector_4 = colMeans(stock_Return_Sector_4)
    #Extend the vector to a matrix(ベクトルを行列に拡張)
    Miu_Sector_4 = matrix(rep(miu_Sector_4, training_days_Sector_4), nrow = 4)
    
    #シミュレートされた30日間のポートフォリオリターンの初期化
    portfolio_Returns_30_Sector_m_4 = matrix(0, training_days_Sector_4, mc_rep_Sector_4)
    
    #各銘柄のβ値
    brand_A.Beta_Sector_4 <- DF_Sector_2_4[2,1] #1つ目の銘柄のβを抽出
    brand_B.Beta_Sector_4 <- DF_Sector_2_4[2,2] #2つ目の銘柄のβを抽出
    brand_C.Beta_Sector_4 <- DF_Sector_2_4[2,3] #3つ目の銘柄のβを抽出
    brand_D.Beta_Sector_4 <- DF_Sector_2_4[2,4] #4つ目の銘柄のβを抽出
    each.Beta_Sector_4 <- c(brand_A.Beta_Sector_4,brand_B.Beta_Sector_4,brand_C.Beta_Sector_4,brand_D.Beta_Sector_4) #4つの銘柄のβを配列化
    
    ##モンテカルロシミュレーション
    
    #1つのポートフォリオに対して10000回のモンテカルロシミュレーションを実行
    for (k in 1:mc_rep_Sector_4) {
      Z_Sector_4 = matrix ( rnorm( dim(stock_Return_Sector_4)[2] * training_days_Sector_4 ), ncol = training_days_Sector_4 )
      #Lower Triangular Matrix from our Choleski Factorization(コレスキー分解による下三角行列)
      #正定値行列近似Q = nearPD(coVarMat)
      L_Sector_4 = t( chol( coVarMat_Sector_4 ) )
      #Calculate stock returns for each day(毎日の株式リターンを計算)
      daily_Returns_Sector_4 = Miu_Sector_4 + L_Sector_4%*%Z_Sector_4  
      #Calculate portfolio returns for 30 days(30日間のポートフォリオリターンを計算)
      portfolio_Returns_30_Sector_4 = portfolio_Weights_Sector_4 %*% daily_Returns_Sector_4
      #Add it to the monte-carlo matrix(それをモンテカルロ行列に追加)
      portfolio_Returns_30_Sector_m_4[,k] = portfolio_Returns_30_Sector_4;
    }
    #モンテカルロシミュレーションの終了
    
    #ポートフォリオのβ値計算
    portfolio_Beta_Sector_4 <- portfolio_Weights_Sector_4%*%each.Beta_Sector_4
    print(portfolio_Beta_Sector_4)
    
    #1シナリオごと(つまり列ごと)の結果を集計
    portfolio_return_Sector_4_0 = mean(portfolio_Returns_30_Sector_m_4[,]) #10000シナリオのリターンの平均(期待収益)
    portfolio_return_Sector_4 = ((portfolio_return_Sector_4_0+1)^(N))-1 #年率換算
    return_Sector = apply(portfolio_Returns_30_Sector_m_4, 2, mean) #期待収益
    Y_return_Sector = ((return_Sector+1)^(N))-1 #年率換算
    risk_Sector = apply(portfolio_Returns_30_Sector_m_4, 2, sd) #標準偏差(リスク)
    Y_risk_Sector = risk_Sector*sqrt(N) #年率換算
    portfolio_risk_Sector_4 = mean(Y_risk_Sector) #10000シナリオの標準偏差(リスク)の平均
    sharpe_Sector = Y_return_Sector/Y_risk_Sector #シャープレシオの計算(年率換算後の結果を用いる)
    sharpe_Sector_0 = c(sharpe_Sector[(sharpe_Sector<0)])  #シャープレシオが負の値であるもの
    sharpe_Sector_1 = c(sharpe_Sector[(sharpe_Sector>0)])  #シャープレシオが正の値であるもの
    sharpe_Sector_2 = rep(0,10000-length(sharpe_Sector_1)) #シャープレシオが負の値であるものの個数分だけ0を生成
    sharpe_Sector_3 = c(sharpe_Sector_1,sharpe_Sector_2) #個数を10000個に戻す(つまり正の値と0のベクトルを結合)
    portfolio_sharpe_Sector_4 = mean(sharpe_Sector_3) #負のものを0とした時の平均(全て分母10000)
    portfolio_sharpe_Sector_4_1 = mean(sharpe_Sector) #全てのシャープレシオの平均
    portfolio_sharpe_Sector_4_2 = mean(sharpe_Sector_1) #正のものだけの平均(分母がシナリオによって異なる)
    portfolio_sharpe_Sector_4_3 = mean(sharpe_Sector_0) #負のものだけの平均(分母がシナリオによって異なる)
    
    treynor_Sector <- Y_return_Sector/portfolio_Beta_Sector_4 #トレイナーレシオの計算(年率換算後の結果を用いる)
    portfolio_treynor_Sector_4 = mean(treynor_Sector)
    
    p_Sector <- portfolio_Returns_30_Sector_m_4
    p_Sector_2 <- p_Sector
    p_Sector_2[(p_Sector_2>0)] <- NaN
    risk2_Sector <- apply(p_Sector_2,2,sd, na.rm=T) #下方リスクの算出
    Y_risk2_Sector = risk2_Sector*sqrt(N) #年率換算
    sortino_Sector = Y_return_Sector/Y_risk2_Sector #ソルティノレシオの計算(年率換算後の結果を用いる)
    portfolio_sortino_Sector_4 <- sum(sortino_Sector[!is.na(sortino_Sector)])/10000 
    #meanだと分母が変わってしまう(このために以下の式はダメ)ので、10000で割ることとする
    #portfolio_sortino_Sector_4 = mean(sortino_Sector[!is.na(sortino_Sector)])
    #全てNaN(ずっと正の値)となる列(シナリオ)が1つでもある場合に結果にNAを示すこととなるため以下でもダメ
    #portfolio_sortino_Sector_4 = mean(sortino_Sector)
    #全てNaNの列があるかないかは以下で確認できる
    #p_Sector_3 <- p_Sector_2[!apply(is.na(p_Sector_2), 2, all)]
    #str(p_Sector_3) これがnum[1:300000]でなければ全てNaNの列があることとなる
    
    ##10000個の結果をベクトルに格納
    
    #ポートフォリオの統計を表示する
    print(c(portfolio_return_Sector_4,portfolio_risk_Sector_4,
            portfolio_sharpe_Sector_4,portfolio_sharpe_Sector_4_1,
            portfolio_sharpe_Sector_4_2,portfolio_sharpe_Sector_4_3,portfolio_treynor_Sector_4,portfolio_sortino_Sector_4))
    
    #最終的な結果を確認するために格納庫(ベクトル)にデータを入れ込む
    #ポートフォリオの期待収益率を格納
    r_data_Sector_4 = c(r_data_Sector_4,portfolio_return_Sector_4)
    #ポートフォリオのリスクを格納
    sd_data_Sector_4 = c(sd_data_Sector_4,portfolio_risk_Sector_4)
    #ポートフォリオのβ値を格納
    Beta_data_Sector_4 = c(Beta_data_Sector_4,portfolio_Beta_Sector_4)
    #ポートフォリオのシャープレシオを格納
    sharpe_data_Sector_4 = c(sharpe_data_Sector_4,portfolio_sharpe_Sector_4)
    sharpe_data_Sector_4_1 = c(sharpe_data_Sector_4_1,portfolio_sharpe_Sector_4_1)
    sharpe_data_Sector_4_2 = c(sharpe_data_Sector_4_2,portfolio_sharpe_Sector_4_2)
    sharpe_data_Sector_4_3 = c(sharpe_data_Sector_4_3,portfolio_sharpe_Sector_4_3)
    #ポートフォリオのトレイナーレシオを格納
    treynor_data_Sector_4 = c(treynor_data_Sector_4,portfolio_treynor_Sector_4)
    #ポートフォリオのソルティノレシオを格納
    sortino_data_Sector_4 = c(sortino_data_Sector_4,portfolio_sortino_Sector_4)
    
    #試行回数の確認
    print(length(r_data_Sector_4))
    
    #for文終了条件
    if(length(r_data_Sector_4) == 10000 && length(sd_data_Sector_4) == 10000 && length(Beta_data_Sector_4) == 10000){
      break #10000個のデータを取得したらシミュレーションを終える
    }
  }
}

#for文内で格納した10000個のポートフォリオのデータを表示する
r_data_Sector_4 #10000個のポートフォリオのリターン
sd_data_Sector_4 #10000個のポートフォリオのリスク
sharpe_data_Sector_4 #10000個のポートフォリオの置き換えシャープレシオ
sharpe_data_Sector_4_1 #10000個のポートフォリオの全部シャープレシオ
sharpe_data_Sector_4_2 #10000個のポートフォリオの正シャープレシオ
sharpe_data_Sector_4_3 #10000個のポートフォリオの負シャープレシオ
treynor_data_Sector_4 #10000個のポートフォリオのトレイナーレシオ
sortino_data_Sector_4 #10000個のポートフォリオのソルティノレシオ
Beta_data_Sector_4 #10000個のポートフォリオのβ値
brands4_name_Sector_m #選ばれた銘柄の一覧

#10000個のポートフォリオのリターンとリスク、β値の平均を表示
Sector_r_4 <- mean(r_data_Sector_4) #10000個のポートフォリオのリターンの平均
Sector_sd_4 <- mean(sd_data_Sector_4) #10000個のポートフォリオのリスクの平均
Sector_Beta_4 <- mean(Beta_data_Sector_4) #10000個のポートフォリオのβ値の平均
Sector_sharpe_4 <-mean(sharpe_data_Sector_4) #10000個のポートフォリオの置き換えシャープレシオ
Sector_sharpe_4_1 <-mean(sharpe_data_Sector_4_1) #10000個のポートフォリオの全部シャープレシオの平均
Sector_sharpe_4_2 <-mean(sharpe_data_Sector_4_2) #10000個のポートフォリオの正シャープレシオ
Sector_sharpe_4_3 <-mean(sharpe_data_Sector_4_3) #10000個のポートフォリオの負シャープレシオ
Sector_treynor_4 <- mean(treynor_data_Sector_4) #10000個のポートフォリオのトレイナーレシオ
Sector_sortino_4 <- mean(sortino_data_Sector_4) #10000個のポートフォリオのソルティノレシオ
print(c("Mean Return","Mean Risk","Mean Beta","Mean Sharpe(+0)",
        "Mean Sharpe(+-)","Mean Sharpe(+)","Mean Sharpe(-)","Mean Treynor","Mean Sortino"))#結果のラベル
print(c(Sector_r_4,Sector_sd_4,Sector_Beta_4,Sector_sharpe_4,
        Sector_sharpe_4_1,Sector_sharpe_4_2,Sector_sharpe_4_3,Sector_treynor_4,Sector_sortino_4)) #結果の表示

#===========================================================================================================
##④DTW+k-medoids(Normal)分類によるシミュレーション
#===========================================================================================================

#年率換算のための変数
N = 245

#データの読み込み
#配当込み収益率データの読み込み
DF_DTWMedN_00 <- read.xlsx("./original data.xlsx",sheet=5,startRow=1,colNames=TRUE,rowNames=TRUE)
#配当込み収益率データの使用部分のみ抽出
DF_DTWMedN_0 <- DF_DTWMedN_00[,1:245] #アウトオブサンプル期間2021年の1年間分
#DF_DTWMedN_0 <- DF_DTWMedN_00[,1:121] #アウトオブサンプル期間2021年の半年分(1〜6月)
#クラスタリングデータの読み込み
DF_DTWMedN_01 <- read.xlsx("./clustering result (DTW+k-medoids(N)).xlsx",sheet=1,colNames=TRUE,rowNames=TRUE)
#β値データの読み込み
DF_DTWMedN_02 <- read.xlsx("./Beta value.xlsx",sheet=1,colNames=TRUE,rowNames=TRUE)
#上記3つのデータを結合
DF_DTWMedN_03 <- cbind(DF_DTWMedN_01,DF_DTWMedN_02,DF_DTWMedN_0)
#不要な列を削除
DF_DTWMedN_1 <- DF_DTWMedN_03[,-2]
#データ要約の表示
head(DF_DTWMedN_1)

#最終的な結果を確認するために空の格納庫となるベクトルを準備
r_data_DTWMedN_4 = NULL #リターンの結果を格納
sd_data_DTWMedN_4 = NULL #リスクの結果を格納
Beta_data_DTWMedN_4 = NULL #β値の結果を格納
sharpe_data_DTWMedN_4 = NULL #シャープレシオの結果を格納
sharpe_data_DTWMedN_4_1 = NULL #シャープレシオの結果を格納
sharpe_data_DTWMedN_4_2 = NULL #シャープレシオの結果を格納
sharpe_data_DTWMedN_4_3 = NULL #シャープレシオの結果を格納
treynor_data_DTWMedN_4 = NULL #トレイナーレシオの結果を格納
sortino_data_DTWMedN_4 = NULL #ソルティノレシオの結果を格納
brands4_name_DTWMedN_m = data.frame() #銘柄名を格納

#再現性保持のためにseed値を準備
set.seed(0)

#層化抽出とシミュレーションの実行
for (i in 1:100000000){
  #層化抽出法(replace=FALSEにより重複を防ぐ,sizeで抽出数を指定)
  Strasamp_DTWMedN_4 <- sample_n(tbl = DF_DTWMedN_1 ,replace = FALSE ,size = 4)
  Strasamp_DTWMedN_4 <- Strasamp_DTWMedN_4 #層化抽出の繰り返し文
  #条件分岐で4つの銘柄の業種が異なる時にシミュレーションを適用
  #(!=は等しくないという意味をもつ)
  if(Strasamp_DTWMedN_4$cluster[1]!=Strasamp_DTWMedN_4$cluster[2]&&Strasamp_DTWMedN_4$cluster[1]!=Strasamp_DTWMedN_4$cluster[3]&&Strasamp_DTWMedN_4$cluster[1]!=Strasamp_DTWMedN_4$cluster[4]&&Strasamp_DTWMedN_4$cluster[2]!=Strasamp_DTWMedN_4$cluster[3]&&Strasamp_DTWMedN_4$cluster[2]!=Strasamp_DTWMedN_4$cluster[4]&&Strasamp_DTWMedN_4$cluster[3]!=Strasamp_DTWMedN_4$cluster[4]){
    print(Strasamp_DTWMedN_4[1]) #抽出された銘柄の書き出し
    DF_DTWMedN_2_4<-t(Strasamp_DTWMedN_4) #抽出されたデータを転置(モンテカルロのため)
    DF_DTWMedN_3_4<-DF_DTWMedN_2_4[-2:-1,] #クラスタ番号の行のみを削除
    stock_Return_DTWMedN_4 = as.matrix( DF_DTWMedN_3_4[ , 1:4] ) #行列として扱う 
    
    #銘柄名を格納する
    brands4_name_DTWMedN_1_m <- t(as.matrix(row.names(Strasamp_DTWMedN_4[1])))
    brands4_name_DTWMedN_m <- rbind( brands4_name_DTWMedN_m, brands4_name_DTWMedN_1_m )
    
    #変数の設定
    mc_rep_DTWMedN_4 = 10000 #Number of Monte Carlo Simulations(モンテカルロシミュレーションの回数)
    training_days_DTWMedN_4 = 30 #将来何日を予測するかの日
    
    ##抽出した銘柄の処理
    
    #各銘柄に対する投資の重みつけ(ここでは等分つまり1/4ずつとしている)
    portfolio_Weights_DTWMedN_4 = t(as.matrix(rep(1/ncol(stock_Return_DTWMedN_4), ncol(stock_Return_DTWMedN_4))))
    print(portfolio_Weights_DTWMedN_4)
    
    #共分散行列と株式リターンの平均値を計算
    #株式リターンの分散共分散行列を取得
    coVarMat_DTWMedN_4 = cov(stock_Return_DTWMedN_4)
    miu_DTWMedN_4 = colMeans(stock_Return_DTWMedN_4)
    #Extend the vector to a matrix(ベクトルを行列に拡張)
    Miu_DTWMedN_4 = matrix(rep(miu_DTWMedN_4, training_days_DTWMedN_4), nrow = 4)
    
    #シミュレートされた30日間のポートフォリオリターンの初期化
    portfolio_Returns_30_DTWMedN_m_4 = matrix(0, training_days_DTWMedN_4, mc_rep_DTWMedN_4)
    
    #各銘柄のβ値
    brand_A.Beta_DTWMedN_4 <- DF_DTWMedN_2_4[2,1] #1つ目の銘柄のβを抽出
    brand_B.Beta_DTWMedN_4 <- DF_DTWMedN_2_4[2,2] #2つ目の銘柄のβを抽出
    brand_C.Beta_DTWMedN_4 <- DF_DTWMedN_2_4[2,3] #3つ目の銘柄のβを抽出
    brand_D.Beta_DTWMedN_4 <- DF_DTWMedN_2_4[2,4] #4つ目の銘柄のβを抽出
    each.Beta_DTWMedN_4 <- c(brand_A.Beta_DTWMedN_4,brand_B.Beta_DTWMedN_4,brand_C.Beta_DTWMedN_4,brand_D.Beta_DTWMedN_4) #4つの銘柄のβを配列化
    
    ##モンテカルロシミュレーション
    
    #1つのポートフォリオに対して10000回のモンテカルロシミュレーションを実行
    for (k in 1:mc_rep_DTWMedN_4) {
      Z_DTWMedN_4 = matrix ( rnorm( dim(stock_Return_DTWMedN_4)[2] * training_days_DTWMedN_4 ), ncol = training_days_DTWMedN_4 )
      #Lower Triangular Matrix from our Choleski Factorization(コレスキー分解による下三角行列)
      #正定値行列近似Q = nearPD(coVarMat)
      L_DTWMedN_4 = t( chol( coVarMat_DTWMedN_4 ) )
      #Calculate stock returns for each day(毎日の株式リターンを計算)
      daily_Returns_DTWMedN_4 = Miu_DTWMedN_4 + L_DTWMedN_4%*%Z_DTWMedN_4 
      #Calculate portfolio returns for 30 days(30日間のポートフォリオリターンを計算)
      portfolio_Returns_30_DTWMedN_4 = portfolio_Weights_DTWMedN_4 %*% daily_Returns_DTWMedN_4
      #Add it to the monte-carlo matrix(それをモンテカルロ行列に追加)
      portfolio_Returns_30_DTWMedN_m_4[,k] = portfolio_Returns_30_DTWMedN_4;
    }
    #モンテカルロシミュレーションの終了
    
    #ポートフォリオのβ値計算
    portfolio_Beta_DTWMedN_4 <- portfolio_Weights_DTWMedN_4%*%each.Beta_DTWMedN_4
    print(portfolio_Beta_DTWMedN_4)
    
    #1シナリオごと(つまり列ごと)の結果を集計
    portfolio_return_DTWMedN_4_0 = mean(portfolio_Returns_30_DTWMedN_m_4[,]) #10000シナリオのリターンの平均(期待収益)
    portfolio_return_DTWMedN_4 = ((portfolio_return_DTWMedN_4_0+1)^(N))-1 #年率換算
    return_DTWMedN = apply(portfolio_Returns_30_DTWMedN_m_4, 2, mean) #期待収益
    Y_return_DTWMedN = ((return_DTWMedN+1)^(N))-1 #年率換算
    risk_DTWMedN = apply(portfolio_Returns_30_DTWMedN_m_4, 2, sd) #標準偏差(リスク)
    Y_risk_DTWMedN = risk_DTWMedN*sqrt(N) #年率換算
    portfolio_risk_DTWMedN_4 = mean(Y_risk_DTWMedN) #10000シナリオの標準偏差(リスク)の平均
    sharpe_DTWMedN = Y_return_DTWMedN/Y_risk_DTWMedN #シャープレシオの計算(年率換算後の結果を用いる)
    sharpe_DTWMedN_0 = c(sharpe_DTWMedN[(sharpe_DTWMedN<0)])  #シャープレシオが負の値であるもの
    sharpe_DTWMedN_1 = c(sharpe_DTWMedN[(sharpe_DTWMedN>0)])  #シャープレシオが正の値であるもの
    sharpe_DTWMedN_2 = rep(0,10000-length(sharpe_DTWMedN_1)) #シャープレシオが負の値であるものの個数分だけ0を生成
    sharpe_DTWMedN_3 = c(sharpe_DTWMedN_1,sharpe_DTWMedN_2) #個数を10000個に戻す(つまり正の値と0のベクトルを結合)
    portfolio_sharpe_DTWMedN_4 = mean(sharpe_DTWMedN_3) #負のものを0とした時の平均(全て分母10000)
    portfolio_sharpe_DTWMedN_4_1 = mean(sharpe_DTWMedN) #全てのシャープレシオの平均
    portfolio_sharpe_DTWMedN_4_2 = mean(sharpe_DTWMedN_1) #正のものだけの平均(分母がシナリオによって異なる)
    portfolio_sharpe_DTWMedN_4_3 = mean(sharpe_DTWMedN_0) #負のものだけの平均(分母がシナリオによって異なる)
    
    treynor_DTWMedN <- Y_return_DTWMedN/portfolio_Beta_DTWMedN_4 #トレイナーレシオの計算(年率換算後の結果を用いる)
    portfolio_treynor_DTWMedN_4 = mean(treynor_DTWMedN)
    
    p_DTWMedN <- portfolio_Returns_30_DTWMedN_m_4
    p_DTWMedN_2 <- p_DTWMedN
    p_DTWMedN_2[(p_DTWMedN_2>0)] <- NaN
    risk2_DTWMedN <- apply(p_DTWMedN_2,2,sd, na.rm=T) #下方リスクの算出
    Y_risk2_DTWMedN = risk2_DTWMedN*sqrt(N) #年率換算
    sortino_DTWMedN = Y_return_DTWMedN/Y_risk2_DTWMedN #ソルティノレシオの計算(年率換算後の結果を用いる)
    portfolio_sortino_DTWMedN_4 <- sum(sortino_DTWMedN[!is.na(sortino_DTWMedN)])/10000 
    #meanだと分母が変わってしまう(このために以下の式はダメ)ので、10000で割ることとする
    #portfolio_sortino_DTWMedN_4 = mean(sortino_DTWMedN[!is.na(sortino_DTWMedN)])
    #全てNaN(ずっと正の値)となる列(シナリオ)が1つでもある場合に結果にNAを示すこととなるため以下でもダメ
    #portfolio_sortino_DTWMedN_4 = mean(sortino_DTWMedN)
    #全てNaNの列があるかないかは以下で確認できる
    #p_DTWMedN_3 <- p_DTWMedN_2[!apply(is.na(p_DTWMedN_2), 2, all)]
    #str(p_DTWMedN_3) これがnum[1:300000]でなければ全てNaNの列があることとなる
    
    ##10000個の結果をベクトルに格納
    
    #ポートフォリオの統計を表示する
    print(c(portfolio_return_DTWMedN_4,portfolio_risk_DTWMedN_4,
            portfolio_sharpe_DTWMedN_4,portfolio_sharpe_DTWMedN_4_1,
            portfolio_sharpe_DTWMedN_4_2,portfolio_sharpe_DTWMedN_4_3,portfolio_treynor_DTWMedN_4,portfolio_sortino_DTWMedN_4))
    
    #最終的な結果を確認するために格納庫(ベクトル)にデータを入れ込む
    #ポートフォリオの期待収益率を格納
    r_data_DTWMedN_4 = c(r_data_DTWMedN_4,portfolio_return_DTWMedN_4)
    #ポートフォリオのリスクを格納
    sd_data_DTWMedN_4 = c(sd_data_DTWMedN_4,portfolio_risk_DTWMedN_4)
    #ポートフォリオのβ値を格納
    Beta_data_DTWMedN_4 = c(Beta_data_DTWMedN_4,portfolio_Beta_DTWMedN_4)
    #ポートフォリオのシャープレシオを格納
    sharpe_data_DTWMedN_4 = c(sharpe_data_DTWMedN_4,portfolio_sharpe_DTWMedN_4)
    sharpe_data_DTWMedN_4_1 = c(sharpe_data_DTWMedN_4_1,portfolio_sharpe_DTWMedN_4_1)
    sharpe_data_DTWMedN_4_2 = c(sharpe_data_DTWMedN_4_2,portfolio_sharpe_DTWMedN_4_2)
    sharpe_data_DTWMedN_4_3 = c(sharpe_data_DTWMedN_4_3,portfolio_sharpe_DTWMedN_4_3)
    #ポートフォリオのトレイナーレシオを格納
    treynor_data_DTWMedN_4 = c(treynor_data_DTWMedN_4,portfolio_treynor_DTWMedN_4)
    #ポートフォリオのソルティノレシオを格納
    sortino_data_DTWMedN_4 = c(sortino_data_DTWMedN_4,portfolio_sortino_DTWMedN_4)
    
    #試行回数の確認
    print(length(r_data_DTWMedN_4))
    
    #for文終了条件
    if(length(r_data_DTWMedN_4) == 10000 && length(sd_data_DTWMedN_4) == 10000 && length(Beta_data_DTWMedN_4) == 10000){
      break #10000個のデータを取得したらシミュレーションを終える
    }
  }
}

#for文内で格納した10000個のポートフォリオのデータを表示する
r_data_DTWMedN_4 #10000個のポートフォリオのリターン
sd_data_DTWMedN_4 #10000個のポートフォリオのリスク
sharpe_data_DTWMedN_4 #10000個のポートフォリオの置き換えシャープレシオ
sharpe_data_DTWMedN_4_1 #10000個のポートフォリオの全部シャープレシオ
sharpe_data_DTWMedN_4_2 #10000個のポートフォリオの正シャープレシオ
sharpe_data_DTWMedN_4_3 #10000個のポートフォリオの負シャープレシオ
treynor_data_DTWMedN_4 #10000個のポートフォリオのトレイナーレシオ
sortino_data_DTWMedN_4 #10000個のポートフォリオのソルティノレシオ
Beta_data_DTWMedN_4 #10000個のポートフォリオのβ値
brands4_name_DTWMedN_m #選ばれた銘柄の一覧

#10000個のポートフォリオのリターンとリスク、β値の平均を表示
DTWMedN_r_4 <- mean(r_data_DTWMedN_4) #10000個のポートフォリオのリターンの平均
DTWMedN_sd_4 <- mean(sd_data_DTWMedN_4) #10000個のポートフォリオのリスクの平均
DTWMedN_Beta_4 <- mean(Beta_data_DTWMedN_4) #10000個のポートフォリオのβ値の平均
DTWMedN_sharpe_4 <-mean(sharpe_data_DTWMedN_4) #10000個のポートフォリオの置き換えシャープレシオ
DTWMedN_sharpe_4_1 <-mean(sharpe_data_DTWMedN_4_1) #10000個のポートフォリオの全部シャープレシオの平均
DTWMedN_sharpe_4_2 <-mean(sharpe_data_DTWMedN_4_2) #10000個のポートフォリオの正シャープレシオ
DTWMedN_sharpe_4_3 <-mean(sharpe_data_DTWMedN_4_3) #10000個のポートフォリオの負シャープレシオ
DTWMedN_treynor_4 <- mean(treynor_data_DTWMedN_4) #10000個のポートフォリオのトレイナーレシオ
DTWMedN_sortino_4 <- mean(sortino_data_DTWMedN_4) #10000個のポートフォリオのソルティノレシオ
print(c("Mean Return","Mean Risk","Mean Beta","Mean Sharpe(+0)",
        "Mean Sharpe(+-)","Mean Sharpe(+)","Mean Sharpe(-)","Mean Treynor","Mean Sortino"))#結果のラベル
print(c(DTWMedN_r_4,DTWMedN_sd_4,DTWMedN_Beta_4,DTWMedN_sharpe_4,
        DTWMedN_sharpe_4_1,DTWMedN_sharpe_4_2,DTWMedN_sharpe_4_3,DTWMedN_treynor_4,DTWMedN_sortino_4)) #結果の表示

#===========================================================================================================
##⑤DTW+k-medoids(standard)分類によるシミュレーション
#===========================================================================================================

#年率換算のための変数
N = 245

#データの読み込み
#配当込み収益率データの読み込み
DF_DTWMedS_00 <- read.xlsx("./original data.xlsx",sheet=5,startRow=1,colNames=TRUE,rowNames=TRUE)
#配当込み収益率データの使用部分のみ抽出
DF_DTWMedS_0 <- DF_DTWMedS_00[,1:245] #アウトオブサンプル期間2021年の1年間分
#DF_DTWMedS_0 <- DF_DTWMedS_00[,1:121] #アウトオブサンプル期間2021年の半年分(1〜6月)
#クラスタリングデータの読み込み
DF_DTWMedS_01 <- read.xlsx("./clustering result (DTW+k-medoids(S)).xlsx",sheet=1,colNames=TRUE,rowNames=TRUE)
#β値データの読み込み
DF_DTWMedS_02 <- read.xlsx("./Beta value.xlsx",sheet=1,colNames=TRUE,rowNames=TRUE)
#上記3つのデータを結合
DF_DTWMedS_03 <- cbind(DF_DTWMedS_01,DF_DTWMedS_02,DF_DTWMedS_0)
#不要な列を削除
DF_DTWMedS_1 <- DF_DTWMedS_03[,-2]
#データ要約の表示
head(DF_DTWMedS_1)

#最終的な結果を確認するために空の格納庫となるベクトルを準備
r_data_DTWMedS_4 = NULL #リターンの結果を格納
sd_data_DTWMedS_4 = NULL #リスクの結果を格納
Beta_data_DTWMedS_4 = NULL #β値の結果を格納
sharpe_data_DTWMedS_4 = NULL #シャープレシオの結果を格納
sharpe_data_DTWMedS_4_1 = NULL #シャープレシオの結果を格納
sharpe_data_DTWMedS_4_2 = NULL #シャープレシオの結果を格納
sharpe_data_DTWMedS_4_3 = NULL #シャープレシオの結果を格納
treynor_data_DTWMedS_4 = NULL #トレイナーレシオの結果を格納
sortino_data_DTWMedS_4 = NULL #ソルティノレシオの結果を格納
brands4_name_DTWMedS_m = data.frame() #銘柄名を格納

#再現性保持のためにseed値を準備
set.seed(0)

#層化抽出とシミュレーションの実行
for (i in 1:100000000){
  #層化抽出法(replace=FALSEにより重複を防ぐ,sizeで抽出数を指定)
  Strasamp_DTWMedS_4 <- sample_n(tbl = DF_DTWMedS_1 ,replace = FALSE ,size = 4)
  Strasamp_DTWMedS_4 <- Strasamp_DTWMedS_4 #層化抽出の繰り返し文
  #条件分岐で4つの銘柄の業種が異なる時にシミュレーションを適用
  #(!=は等しくないという意味をもつ)
  if(Strasamp_DTWMedS_4$cluster[1]!=Strasamp_DTWMedS_4$cluster[2]&&Strasamp_DTWMedS_4$cluster[1]!=Strasamp_DTWMedS_4$cluster[3]&&Strasamp_DTWMedS_4$cluster[1]!=Strasamp_DTWMedS_4$cluster[4]&&Strasamp_DTWMedS_4$cluster[2]!=Strasamp_DTWMedS_4$cluster[3]&&Strasamp_DTWMedS_4$cluster[2]!=Strasamp_DTWMedS_4$cluster[4]&&Strasamp_DTWMedS_4$cluster[3]!=Strasamp_DTWMedS_4$cluster[4]){
    print(Strasamp_DTWMedS_4[1]) #抽出された銘柄の書き出し
    DF_DTWMedS_2_4<-t(Strasamp_DTWMedS_4) #抽出されたデータを転置(モンテカルロのため)
    DF_DTWMedS_3_4<-DF_DTWMedS_2_4[-2:-1,] #クラスタ番号の行のみを削除
    stock_Return_DTWMedS_4 = as.matrix( DF_DTWMedS_3_4[ , 1:4] ) #行列として扱う 
    
    #銘柄名を格納する
    brands4_name_DTWMedS_1_m <- t(as.matrix(row.names(Strasamp_DTWMedS_4[1])))
    brands4_name_DTWMedS_m <- rbind( brands4_name_DTWMedS_m, brands4_name_DTWMedS_1_m )
    
    #変数の設定
    mc_rep_DTWMedS_4 = 10000 #Number of Monte Carlo Simulations(モンテカルロシミュレーションの回数)
    training_days_DTWMedS_4 = 30 #将来何日を予測するかの日
    
    ##抽出した銘柄の処理
    
    #各銘柄に対する投資の重みつけ(ここでは等分つまり1/4ずつとしている)
    portfolio_Weights_DTWMedS_4 = t(as.matrix(rep(1/ncol(stock_Return_DTWMedS_4), ncol(stock_Return_DTWMedS_4))))
    print(portfolio_Weights_DTWMedS_4)
    
    #共分散行列と株式リターンの平均値を計算
    #株式リターンの分散共分散行列を取得
    coVarMat_DTWMedS_4 = cov(stock_Return_DTWMedS_4)
    miu_DTWMedS_4 = colMeans(stock_Return_DTWMedS_4)
    #Extend the vector to a matrix(ベクトルを行列に拡張)
    Miu_DTWMedS_4 = matrix(rep(miu_DTWMedS_4, training_days_DTWMedS_4), nrow = 4)
    
    #シミュレートされた30日間のポートフォリオリターンの初期化
    portfolio_Returns_30_DTWMedS_m_4 = matrix(0, training_days_DTWMedS_4, mc_rep_DTWMedS_4)
    
    #各銘柄のβ値
    brand_A.Beta_DTWMedS_4 <- DF_DTWMedS_2_4[2,1] #1つ目の銘柄のβを抽出
    brand_B.Beta_DTWMedS_4 <- DF_DTWMedS_2_4[2,2] #2つ目の銘柄のβを抽出
    brand_C.Beta_DTWMedS_4 <- DF_DTWMedS_2_4[2,3] #3つ目の銘柄のβを抽出
    brand_D.Beta_DTWMedS_4 <- DF_DTWMedS_2_4[2,4] #4つ目の銘柄のβを抽出
    each.Beta_DTWMedS_4 <- c(brand_A.Beta_DTWMedS_4,brand_B.Beta_DTWMedS_4,brand_C.Beta_DTWMedS_4,brand_D.Beta_DTWMedS_4) #4つの銘柄のβを配列化
    
    ##モンテカルロシミュレーション
    
    #1つのポートフォリオに対して10000回のモンテカルロシミュレーションを実行
    for (k in 1:mc_rep_DTWMedS_4) {
      Z_DTWMedS_4 = matrix ( rnorm( dim(stock_Return_DTWMedS_4)[2] * training_days_DTWMedS_4 ), ncol = training_days_DTWMedS_4 )
      #Lower Triangular Matrix from our Choleski Factorization(コレスキー分解による下三角行列)
      #正定値行列近似Q = nearPD(coVarMat)
      L_DTWMedS_4 = t( chol( coVarMat_DTWMedS_4 ) )
      #Calculate stock returns for each day(毎日の株式リターンを計算)
      daily_Returns_DTWMedS_4 = Miu_DTWMedS_4 + L_DTWMedS_4%*%Z_DTWMedS_4 
      #Calculate portfolio returns for 30 days(30日間のポートフォリオリターンを計算)
      portfolio_Returns_30_DTWMedS_4 = portfolio_Weights_DTWMedS_4 %*% daily_Returns_DTWMedS_4
      #Add it to the monte-carlo matrix(それをモンテカルロ行列に追加)
      portfolio_Returns_30_DTWMedS_m_4[,k] = portfolio_Returns_30_DTWMedS_4;
    }
    #モンテカルロシミュレーションの終了
    
    #ポートフォリオのβ値計算
    portfolio_Beta_DTWMedS_4 <- portfolio_Weights_DTWMedS_4%*%each.Beta_DTWMedS_4
    print(portfolio_Beta_DTWMedS_4)
    
    #1シナリオごと(つまり列ごと)の結果を集計
    portfolio_return_DTWMedS_4_0 = mean(portfolio_Returns_30_DTWMedS_m_4[,]) #10000シナリオのリターンの平均(期待収益)
    portfolio_return_DTWMedS_4 = ((portfolio_return_DTWMedS_4_0+1)^(N))-1 #年率換算
    return_DTWMedS = apply(portfolio_Returns_30_DTWMedS_m_4, 2, mean) #期待収益
    Y_return_DTWMedS = ((return_DTWMedS+1)^(N))-1 #年率換算
    risk_DTWMedS = apply(portfolio_Returns_30_DTWMedS_m_4, 2, sd) #標準偏差(リスク)
    Y_risk_DTWMedS = risk_DTWMedS*sqrt(N) #年率換算
    portfolio_risk_DTWMedS_4 = mean(Y_risk_DTWMedS) #10000シナリオの標準偏差(リスク)の平均
    sharpe_DTWMedS = Y_return_DTWMedS/Y_risk_DTWMedS #シャープレシオの計算(年率換算後の結果を用いる)
    sharpe_DTWMedS_0 = c(sharpe_DTWMedS[(sharpe_DTWMedS<0)])  #シャープレシオが負の値であるもの
    sharpe_DTWMedS_1 = c(sharpe_DTWMedS[(sharpe_DTWMedS>0)])  #シャープレシオが正の値であるもの
    sharpe_DTWMedS_2 = rep(0,10000-length(sharpe_DTWMedS_1)) #シャープレシオが負の値であるものの個数分だけ0を生成
    sharpe_DTWMedS_3 = c(sharpe_DTWMedS_1,sharpe_DTWMedS_2) #個数を10000個に戻す(つまり正の値と0のベクトルを結合)
    portfolio_sharpe_DTWMedS_4 = mean(sharpe_DTWMedS_3) #負のものを0とした時の平均(全て分母10000)
    portfolio_sharpe_DTWMedS_4_1 = mean(sharpe_DTWMedS) #全てのシャープレシオの平均
    portfolio_sharpe_DTWMedS_4_2 = mean(sharpe_DTWMedS_1) #正のものだけの平均(分母がシナリオによって異なる)
    portfolio_sharpe_DTWMedS_4_3 = mean(sharpe_DTWMedS_0) #負のものだけの平均(分母がシナリオによって異なる)
    
    treynor_DTWMedS <- Y_return_DTWMedS/portfolio_Beta_DTWMedS_4 #トレイナーレシオの計算(年率換算後の結果を用いる)
    portfolio_treynor_DTWMedS_4 = mean(treynor_DTWMedS)
    
    p_DTWMedS <- portfolio_Returns_30_DTWMedS_m_4
    p_DTWMedS_2 <- p_DTWMedS
    p_DTWMedS_2[(p_DTWMedS_2>0)] <- NaN
    risk2_DTWMedS <- apply(p_DTWMedS_2,2,sd, na.rm=T) #下方リスクの算出
    Y_risk2_DTWMedS = risk2_DTWMedS*sqrt(N) #年率換算
    sortino_DTWMedS = Y_return_DTWMedS/Y_risk2_DTWMedS#ソルティノレシオの計算(年率換算後の結果を用いる)
    portfolio_sortino_DTWMedS_4 <- sum(sortino_DTWMedS[!is.na(sortino_DTWMedS)])/10000 
    #meanだと分母が変わってしまう(このために以下の式はダメ)ので、10000で割ることとする
    #portfolio_sortino_DTWMedS_4 = mean(sortino_DTWMedS[!is.na(sortino_DTWMedS)])
    #全てNaN(ずっと正の値)となる列(シナリオ)が1つでもある場合に結果にNAを示すこととなるため以下でもダメ
    #portfolio_sortino_DTWMedS_4 = mean(sortino_DTWMedS)
    #全てNaNの列があるかないかは以下で確認できる
    #p_DTWMedS_3 <- p_DTWMedS_2[!apply(is.na(p_DTWMedS_2), 2, all)]
    #str(p_DTWMedS_3) これがnum[1:300000]でなければ全てNaNの列があることとなる
    
    ##10000個の結果をベクトルに格納
    
    #ポートフォリオの統計を表示する
    print(c(portfolio_return_DTWMedS_4,portfolio_risk_DTWMedS_4,
            portfolio_sharpe_DTWMedS_4,portfolio_sharpe_DTWMedS_4_1,
            portfolio_sharpe_DTWMedS_4_2,portfolio_sharpe_DTWMedS_4_3,portfolio_treynor_DTWMedS_4,portfolio_sortino_DTWMedS_4))
    
    #最終的な結果を確認するために格納庫(ベクトル)にデータを入れ込む
    #ポートフォリオの期待収益率を格納
    r_data_DTWMedS_4 = c(r_data_DTWMedS_4,portfolio_return_DTWMedS_4)
    #ポートフォリオのリスクを格納
    sd_data_DTWMedS_4 = c(sd_data_DTWMedS_4,portfolio_risk_DTWMedS_4)
    #ポートフォリオのβ値を格納
    Beta_data_DTWMedS_4 = c(Beta_data_DTWMedS_4,portfolio_Beta_DTWMedS_4)
    #ポートフォリオのシャープレシオを格納
    sharpe_data_DTWMedS_4 = c(sharpe_data_DTWMedS_4,portfolio_sharpe_DTWMedS_4)
    sharpe_data_DTWMedS_4_1 = c(sharpe_data_DTWMedS_4_1,portfolio_sharpe_DTWMedS_4_1)
    sharpe_data_DTWMedS_4_2 = c(sharpe_data_DTWMedS_4_2,portfolio_sharpe_DTWMedS_4_2)
    sharpe_data_DTWMedS_4_3 = c(sharpe_data_DTWMedS_4_3,portfolio_sharpe_DTWMedS_4_3)
    #ポートフォリオのトレイナーレシオを格納
    treynor_data_DTWMedS_4 = c(treynor_data_DTWMedS_4,portfolio_treynor_DTWMedS_4)
    #ポートフォリオのソルティノレシオを格納
    sortino_data_DTWMedS_4 = c(sortino_data_DTWMedS_4,portfolio_sortino_DTWMedS_4)
    
    #試行回数の確認
    print(length(r_data_DTWMedS_4))
    
    #for文終了条件
    if(length(r_data_DTWMedS_4) == 10000 && length(sd_data_DTWMedS_4) == 10000 && length(Beta_data_DTWMedS_4) == 10000){
      break #10000個のデータを取得したらシミュレーションを終える
    }
  }
}

#for文内で格納した10000個のポートフォリオのデータを表示する
r_data_DTWMedS_4 #10000個のポートフォリオのリターン
sd_data_DTWMedS_4 #10000個のポートフォリオのリスク
sharpe_data_DTWMedS_4 #10000個のポートフォリオの置き換えシャープレシオ
sharpe_data_DTWMedS_4_1 #10000個のポートフォリオの全部シャープレシオ
sharpe_data_DTWMedS_4_2 #10000個のポートフォリオの正シャープレシオ
sharpe_data_DTWMedS_4_3 #10000個のポートフォリオの負シャープレシオ
treynor_data_DTWMedS_4 #10000個のポートフォリオのトレイナーレシオ
sortino_data_DTWMedS_4 #10000個のポートフォリオのソルティノレシオ
Beta_data_DTWMedS_4 #10000個のポートフォリオのβ値
brands4_name_DTWMedS_m #選ばれた銘柄の一覧

#10000個のポートフォリオのリターンとリスク、β値の平均を表示
DTWMedS_r_4 <- mean(r_data_DTWMedS_4) #10000個のポートフォリオのリターンの平均
DTWMedS_sd_4 <- mean(sd_data_DTWMedS_4) #10000個のポートフォリオのリスクの平均
DTWMedS_Beta_4 <- mean(Beta_data_DTWMedS_4) #10000個のポートフォリオのβ値の平均
DTWMedS_sharpe_4 <-mean(sharpe_data_DTWMedS_4) #10000個のポートフォリオの置き換えシャープレシオ
DTWMedS_sharpe_4_1 <-mean(sharpe_data_DTWMedS_4_1) #10000個のポートフォリオの全部シャープレシオの平均
DTWMedS_sharpe_4_2 <-mean(sharpe_data_DTWMedS_4_2) #10000個のポートフォリオの正シャープレシオ
DTWMedS_sharpe_4_3 <-mean(sharpe_data_DTWMedS_4_3) #10000個のポートフォリオの負シャープレシオ
DTWMedS_treynor_4 <- mean(treynor_data_DTWMedS_4) #10000個のポートフォリオのトレイナーレシオ
DTWMedS_sortino_4 <- mean(sortino_data_DTWMedS_4) #10000個のポートフォリオのソルティノレシオ
print(c("Mean Return","Mean Risk","Mean Beta","Mean Sharpe(+0)",
        "Mean Sharpe(+-)","Mean Sharpe(+)","Mean Sharpe(-)","Mean Treynor","Mean Sortino"))#結果のラベル
print(c(DTWMedS_r_4,DTWMedS_sd_4,DTWMedS_Beta_4,DTWMedS_sharpe_4,
        DTWMedS_sharpe_4_1,DTWMedS_sharpe_4_2,DTWMedS_sharpe_4_3,DTWMedS_treynor_4,DTWMedS_sortino_4)) #結果の表示

#===========================================================================================================
##⑥DTW+k-medoids(β)分類によるシミュレーション
#===========================================================================================================

#年率換算のための変数
N = 245

#データの読み込み
#配当込み収益率データの読み込み
DF_DTWMedβ_00 <- read.xlsx("./original data.xlsx",sheet=5,startRow=1,colNames=TRUE,rowNames=TRUE)
#配当込み収益率データの使用部分のみ抽出
DF_DTWMedβ_0 <- DF_DTWMedβ_00[,1:245] #アウトオブサンプル期間2021年の1年間分
#DF_DTWMedβ_0 <- DF_DTWMedβ_00[,1:121] #アウトオブサンプル期間2021年の半年分(1〜6月)
#クラスタリングデータの読み込み
DF_DTWMedβ_01 <- read.xlsx("./clustering result (DTW+k-medoids(β)).xlsx",sheet=1,colNames=TRUE,rowNames=TRUE)
#β値データの読み込み
DF_DTWMedβ_02 <- read.xlsx("./Beta value.xlsx",sheet=1,colNames=TRUE,rowNames=TRUE)
#上記3つのデータを結合
DF_DTWMedβ_03 <- cbind(DF_DTWMedβ_01,DF_DTWMedβ_02,DF_DTWMedβ_0)
#不要な列を削除
DF_DTWMedβ_1 <- DF_DTWMedβ_03[,-2]
#データ要約の表示
head(DF_DTWMedβ_1)

#最終的な結果を確認するために空の格納庫となるベクトルを準備
r_data_DTWMedβ_4 = NULL #リターンの結果を格納
sd_data_DTWMedβ_4 = NULL #リスクの結果を格納
Beta_data_DTWMedβ_4 = NULL #β値の結果を格納
sharpe_data_DTWMedβ_4 = NULL #シャープレシオの結果を格納
sharpe_data_DTWMedβ_4_1 = NULL #シャープレシオの結果を格納
sharpe_data_DTWMedβ_4_2 = NULL #シャープレシオの結果を格納
sharpe_data_DTWMedβ_4_3 = NULL #シャープレシオの結果を格納
treynor_data_DTWMedβ_4 = NULL #トレイナーレシオの結果を格納
sortino_data_DTWMedβ_4 = NULL #ソルティノレシオの結果を格納
brands4_name_DTWMedβ_m = data.frame() #銘柄名を格納

#再現性保持のためにseed値を準備
set.seed(0)

#層化抽出とシミュレーションの実行
for (i in 1:100000000){
  #層化抽出法(replace=FALSEにより重複を防ぐ,sizeで抽出数を指定)
  Strasamp_DTWMedβ_4 <- sample_n(tbl = DF_DTWMedβ_1 ,replace = FALSE ,size = 4)
  Strasamp_DTWMedβ_4 <- Strasamp_DTWMedβ_4 #層化抽出の繰り返し文
  #条件分岐で4つの銘柄の業種が異なる時にシミュレーションを適用
  #(!=は等しくないという意味をもつ)
  if(Strasamp_DTWMedβ_4$cluster[1]!=Strasamp_DTWMedβ_4$cluster[2]&&Strasamp_DTWMedβ_4$cluster[1]!=Strasamp_DTWMedβ_4$cluster[3]&&Strasamp_DTWMedβ_4$cluster[1]!=Strasamp_DTWMedβ_4$cluster[4]&&Strasamp_DTWMedβ_4$cluster[2]!=Strasamp_DTWMedβ_4$cluster[3]&&Strasamp_DTWMedβ_4$cluster[2]!=Strasamp_DTWMedβ_4$cluster[4]&&Strasamp_DTWMedβ_4$cluster[3]!=Strasamp_DTWMedβ_4$cluster[4]){
    print(Strasamp_DTWMedβ_4[1]) #抽出された銘柄の書き出し
    DF_DTWMedβ_2_4<-t(Strasamp_DTWMedβ_4) #抽出されたデータを転置(モンテカルロのため)
    DF_DTWMedβ_3_4<-DF_DTWMedβ_2_4[-2:-1,] #クラスタ番号の行のみを削除
    stock_Return_DTWMedβ_4 = as.matrix( DF_DTWMedβ_3_4[ , 1:4] ) #行列として扱う 
    
    #銘柄名を格納する
    brands4_name_DTWMedβ_1_m <- t(as.matrix(row.names(Strasamp_DTWMedβ_4[1])))
    brands4_name_DTWMedβ_m <- rbind( brands4_name_DTWMedβ_m, brands4_name_DTWMedβ_1_m )
    
    #変数の設定
    mc_rep_DTWMedβ_4 = 10000 #Number of Monte Carlo Simulations(モンテカルロシミュレーションの回数)
    training_days_DTWMedβ_4 = 30 #将来何日を予測するかの日
    
    ##抽出した銘柄の処理
    
    #各銘柄に対する投資の重みつけ(ここでは等分つまり1/4ずつとしている)
    portfolio_Weights_DTWMedβ_4 = t(as.matrix(rep(1/ncol(stock_Return_DTWMedβ_4), ncol(stock_Return_DTWMedβ_4))))
    print(portfolio_Weights_DTWMedβ_4)
    
    #共分散行列と株式リターンの平均値を計算
    #株式リターンの分散共分散行列を取得
    coVarMat_DTWMedβ_4 = cov(stock_Return_DTWMedβ_4)
    miu_DTWMedβ_4 = colMeans(stock_Return_DTWMedβ_4)
    #Extend the vector to a matrix(ベクトルを行列に拡張)
    Miu_DTWMedβ_4 = matrix(rep(miu_DTWMedβ_4, training_days_DTWMedβ_4), nrow = 4)
    
    #シミュレートされた30日間のポートフォリオリターンの初期化
    portfolio_Returns_30_DTWMedβ_m_4 = matrix(0, training_days_DTWMedβ_4, mc_rep_DTWMedβ_4)
    
    #各銘柄のβ値
    brand_A.Beta_DTWMedβ_4 <- DF_DTWMedβ_2_4[2,1] #1つ目の銘柄のβを抽出
    brand_B.Beta_DTWMedβ_4 <- DF_DTWMedβ_2_4[2,2] #2つ目の銘柄のβを抽出
    brand_C.Beta_DTWMedβ_4 <- DF_DTWMedβ_2_4[2,3] #3つ目の銘柄のβを抽出
    brand_D.Beta_DTWMedβ_4 <- DF_DTWMedβ_2_4[2,4] #4つ目の銘柄のβを抽出
    each.Beta_DTWMedβ_4 <- c(brand_A.Beta_DTWMedβ_4,brand_B.Beta_DTWMedβ_4,brand_C.Beta_DTWMedβ_4,brand_D.Beta_DTWMedβ_4) #4つの銘柄のβを配列化
    
    ##モンテカルロシミュレーション
    
    #1つのポートフォリオに対して10000回のモンテカルロシミュレーションを実行
    for (k in 1:mc_rep_DTWMedβ_4) {
      Z_DTWMedβ_4 = matrix ( rnorm( dim(stock_Return_DTWMedβ_4)[2] * training_days_DTWMedβ_4 ), ncol = training_days_DTWMedβ_4 )
      #Lower Triangular Matrix from our Choleski Factorization(コレスキー分解による下三角行列)
      #正定値行列近似Q = nearPD(coVarMat)
      L_DTWMedβ_4 = t( chol( coVarMat_DTWMedβ_4 ) )
      #Calculate stock returns for each day(毎日の株式リターンを計算)
      daily_Returns_DTWMedβ_4 = Miu_DTWMedβ_4 + L_DTWMedβ_4%*%Z_DTWMedβ_4 
      #Calculate portfolio returns for 30 days(30日間のポートフォリオリターンを計算)
      portfolio_Returns_30_DTWMedβ_4 = portfolio_Weights_DTWMedβ_4 %*% daily_Returns_DTWMedβ_4
      #Add it to the monte-carlo matrix(それをモンテカルロ行列に追加)
      portfolio_Returns_30_DTWMedβ_m_4[,k] = portfolio_Returns_30_DTWMedβ_4;
    }
    #モンテカルロシミュレーションの終了
    
    #ポートフォリオのβ値計算
    portfolio_Beta_DTWMedβ_4 <- portfolio_Weights_DTWMedβ_4%*%each.Beta_DTWMedβ_4
    print(portfolio_Beta_DTWMedβ_4)
    
    #1シナリオごと(つまり列ごと)の結果を集計
    portfolio_return_DTWMedβ_4_0 = mean(portfolio_Returns_30_DTWMedβ_m_4[,]) #10000シナリオのリターンの平均(期待収益)
    portfolio_return_DTWMedβ_4 = ((portfolio_return_DTWMedβ_4_0+1)^(N))-1 #年率換算
    return_DTWMedβ = apply(portfolio_Returns_30_DTWMedβ_m_4, 2, mean) #期待収益
    Y_return_DTWMedβ = ((return_DTWMedβ+1)^(N))-1 #年率換算
    risk_DTWMedβ = apply(portfolio_Returns_30_DTWMedβ_m_4, 2, sd) #標準偏差(リスク)
    Y_risk_DTWMedβ = risk_DTWMedβ*sqrt(N) #年率換算
    portfolio_risk_DTWMedβ_4 = mean(Y_risk_DTWMedβ) #10000シナリオの標準偏差(リスク)の平均
    sharpe_DTWMedβ = Y_return_DTWMedβ/Y_risk_DTWMedβ #シャープレシオの計算(年率換算後の結果を用いる)
    sharpe_DTWMedβ_0 = c(sharpe_DTWMedβ[(sharpe_DTWMedβ<0)])  #シャープレシオが負の値であるもの
    sharpe_DTWMedβ_1 = c(sharpe_DTWMedβ[(sharpe_DTWMedβ>0)])  #シャープレシオが正の値であるもの
    sharpe_DTWMedβ_2 = rep(0,10000-length(sharpe_DTWMedβ_1)) #シャープレシオが負の値であるものの個数分だけ0を生成
    sharpe_DTWMedβ_3 = c(sharpe_DTWMedβ_1,sharpe_DTWMedβ_2) #個数を10000個に戻す(つまり正の値と0のベクトルを結合)
    portfolio_sharpe_DTWMedβ_4 = mean(sharpe_DTWMedβ_3) #負のものを0とした時の平均(全て分母10000)
    portfolio_sharpe_DTWMedβ_4_1 = mean(sharpe_DTWMedβ) #全てのシャープレシオの平均
    portfolio_sharpe_DTWMedβ_4_2 = mean(sharpe_DTWMedβ_1) #正のものだけの平均(分母がシナリオによって異なる)
    portfolio_sharpe_DTWMedβ_4_3 = mean(sharpe_DTWMedβ_0) #負のものだけの平均(分母がシナリオによって異なる)
    
    treynor_DTWMedβ <- Y_return_DTWMedβ/portfolio_Beta_DTWMedβ_4 #トレイナーレシオの計算(年率換算後の結果を用いる)
    portfolio_treynor_DTWMedβ_4 = mean(treynor_DTWMedβ)
    
    p_DTWMedβ <- portfolio_Returns_30_DTWMedβ_m_4
    p_DTWMedβ_2 <- p_DTWMedβ
    p_DTWMedβ_2[(p_DTWMedβ_2>0)] <- NaN
    risk2_DTWMedβ <- apply(p_DTWMedβ_2,2,sd, na.rm=T) #下方リスクの算出
    Y_risk2_DTWMedβ = risk2_DTWMedβ*sqrt(N) #年率換算
    sortino_DTWMedβ = Y_return_DTWMedβ/Y_risk2_DTWMedβ#ソルティノレシオの計算(年率換算後の結果を用いる)
    portfolio_sortino_DTWMedβ_4 <- sum(sortino_DTWMedβ[!is.na(sortino_DTWMedβ)])/10000 
    #meanだと分母が変わってしまう(このために以下の式はダメ)ので、10000で割ることとする
    #portfolio_sortino_DTWMedβ_4 = mean(sortino_DTWMedβ[!is.na(sortino_DTWMedβ)])
    #全てNaN(ずっと正の値)となる列(シナリオ)が1つでもある場合に結果にNAを示すこととなるため以下でもダメ
    #portfolio_sortino_DTWMedβ_4 = mean(sortino_DTWMedβ)
    #全てNaNの列があるかないかは以下で確認できる
    #p_DTWMedβ_3 <- p_DTWMedβ_2[!apply(is.na(p_DTWMedβ_2), 2, all)]
    #str(p_DTWMedβ_3) これがnum[1:300000]でなければ全てNaNの列があることとなる
    
    ##10000個の結果をベクトルに格納
    
    #ポートフォリオの統計を表示する
    print(c(portfolio_return_DTWMedβ_4,portfolio_risk_DTWMedβ_4,
            portfolio_sharpe_DTWMedβ_4,portfolio_sharpe_DTWMedβ_4_1,
            portfolio_sharpe_DTWMedβ_4_2,portfolio_sharpe_DTWMedβ_4_3,portfolio_treynor_DTWMedβ_4,portfolio_sortino_DTWMedβ_4))
    
    #最終的な結果を確認するために格納庫(ベクトル)にデータを入れ込む
    #ポートフォリオの期待収益率を格納
    r_data_DTWMedβ_4 = c(r_data_DTWMedβ_4,portfolio_return_DTWMedβ_4)
    #ポートフォリオのリスクを格納
    sd_data_DTWMedβ_4 = c(sd_data_DTWMedβ_4,portfolio_risk_DTWMedβ_4)
    #ポートフォリオのβ値を格納
    Beta_data_DTWMedβ_4 = c(Beta_data_DTWMedβ_4,portfolio_Beta_DTWMedβ_4)
    #ポートフォリオのシャープレシオを格納
    sharpe_data_DTWMedβ_4 = c(sharpe_data_DTWMedβ_4,portfolio_sharpe_DTWMedβ_4)
    sharpe_data_DTWMedβ_4_1 = c(sharpe_data_DTWMedβ_4_1,portfolio_sharpe_DTWMedβ_4_1)
    sharpe_data_DTWMedβ_4_2 = c(sharpe_data_DTWMedβ_4_2,portfolio_sharpe_DTWMedβ_4_2)
    sharpe_data_DTWMedβ_4_3 = c(sharpe_data_DTWMedβ_4_3,portfolio_sharpe_DTWMedβ_4_3)
    #ポートフォリオのトレイナーレシオを格納
    treynor_data_DTWMedβ_4 = c(treynor_data_DTWMedβ_4,portfolio_treynor_DTWMedβ_4)
    #ポートフォリオのソルティノレシオを格納
    sortino_data_DTWMedβ_4 = c(sortino_data_DTWMedβ_4,portfolio_sortino_DTWMedβ_4)
    
    #試行回数の確認
    print(length(r_data_DTWMedβ_4))
    
    #for文終了条件
    if(length(r_data_DTWMedβ_4) == 10000 && length(sd_data_DTWMedβ_4) == 10000 && length(Beta_data_DTWMedβ_4) == 10000){
      break #10000個のデータを取得したらシミュレーションを終える
    }
  }
}

#for文内で格納した10000個のポートフォリオのデータを表示する
r_data_DTWMedβ_4 #10000個のポートフォリオのリターン
sd_data_DTWMedβ_4 #10000個のポートフォリオのリスク
sharpe_data_DTWMedβ_4 #10000個のポートフォリオの置き換えシャープレシオ
sharpe_data_DTWMedβ_4_1 #10000個のポートフォリオの全部シャープレシオ
sharpe_data_DTWMedβ_4_2 #10000個のポートフォリオの正シャープレシオ
sharpe_data_DTWMedβ_4_3 #10000個のポートフォリオの負シャープレシオ
treynor_data_DTWMedβ_4 #10000個のポートフォリオのトレイナーレシオ
sortino_data_DTWMedβ_4 #10000個のポートフォリオのソルティノレシオ
Beta_data_DTWMedβ_4 #10000個のポートフォリオのβ値
brands4_name_DTWMedβ_m #選ばれた銘柄の一覧

#10000個のポートフォリオのリターンとリスク、β値の平均を表示
DTWMedβ_r_4 <- mean(r_data_DTWMedβ_4) #10000個のポートフォリオのリターンの平均
DTWMedβ_sd_4 <- mean(sd_data_DTWMedβ_4) #10000個のポートフォリオのリスクの平均
DTWMedβ_Beta_4 <- mean(Beta_data_DTWMedβ_4) #10000個のポートフォリオのβ値の平均
DTWMedβ_sharpe_4 <-mean(sharpe_data_DTWMedβ_4) #10000個のポートフォリオの置き換えシャープレシオ
DTWMedβ_sharpe_4_1 <-mean(sharpe_data_DTWMedβ_4_1) #10000個のポートフォリオの全部シャープレシオの平均
DTWMedβ_sharpe_4_2 <-mean(sharpe_data_DTWMedβ_4_2) #10000個のポートフォリオの正シャープレシオ
DTWMedβ_sharpe_4_3 <-mean(sharpe_data_DTWMedβ_4_3) #10000個のポートフォリオの負シャープレシオ
DTWMedβ_treynor_4 <- mean(treynor_data_DTWMedβ_4) #10000個のポートフォリオのトレイナーレシオ
DTWMedβ_sortino_4 <- mean(sortino_data_DTWMedβ_4) #10000個のポートフォリオのソルティノレシオ
print(c("Mean Return","Mean Risk","Mean Beta","Mean Sharpe(+0)",
        "Mean Sharpe(+-)","Mean Sharpe(+)","Mean Sharpe(-)","Mean Treynor","Mean Sortino"))#結果のラベル
print(c(DTWMedβ_r_4,DTWMedβ_sd_4,DTWMedβ_Beta_4,DTWMedβ_sharpe_4,
        DTWMedβ_sharpe_4_1,DTWMedβ_sharpe_4_2,DTWMedβ_sharpe_4_3,DTWMedβ_treynor_4,DTWMedβ_sortino_4)) #結果の表示

#===========================================================================================================
##⑦分類しないシミュレーション
#===========================================================================================================

#年率換算のための変数
N = 245

#データの読み込み
#配当込み収益率データの読み込み
DF_ALL_00 <- read.xlsx("./original data.xlsx",sheet=5,startRow=1,colNames=TRUE,rowNames=TRUE)
#配当込み収益率データの使用部分のみ抽出
DF_ALL_0 <- DF_ALL_00[,1:245] #アウトオブサンプル期間2021年の1年間分
#DF_ALL_0 <- DF_ALL_00[,1:121] #アウトオブサンプル期間2021年の半年分(1〜6月)
#番号の割り当て
DF_ALL_01 <- c(1:223)
#β値データの読み込み
DF_ALL_02 <- read.xlsx("./Beta value.xlsx",sheet=1,colNames=TRUE,rowNames=TRUE)
#上記3つのデータを結合
DF_ALL_1  <- cbind(DF_ALL_01,DF_ALL_02,DF_ALL_0)
#データ要約の表示
head(DF_ALL_1)

#最終的な結果を確認するために空の格納庫となるベクトルを準備
r_data_ALL_4 = NULL #リターンの結果を格納
sd_data_ALL_4 = NULL #リスクの結果を格納
Beta_data_ALL_4 = NULL #β値の結果を格納
sharpe_data_ALL_4 = NULL #シャープレシオの結果を格納
sharpe_data_ALL_4_1 = NULL #シャープレシオの結果を格納
sharpe_data_ALL_4_2 = NULL #シャープレシオの結果を格納
sharpe_data_ALL_4_3 = NULL #シャープレシオの結果を格納
treynor_data_ALL_4 = NULL #トレイナーレシオの結果を格納
sortino_data_ALL_4 = NULL #ソルティノレシオの結果を格納
brands4_name_ALL_m = data.frame() #銘柄名を格納

#再現性保持のためにseed値を準備
set.seed(0)

#層化抽出とシミュレーションの実行
for (i in 1:100000000){
  #層化抽出法(replace=FALSEにより重複を防ぐ,sizeで抽出数を指定)
  Strasamp_ALL_4 <- sample_n(tbl = DF_ALL_1 ,replace = FALSE ,size = 4)
  Strasamp_ALL_4 <- Strasamp_ALL_4 #層化抽出の繰り返し文
  #条件分岐で3つの銘柄の業種が異なる時にシミュレーションを適用
  #(!=は等しくないという意味をもつ)
  if(Strasamp_ALL_4$DF_ALL_01[1]!=Strasamp_ALL_4$DF_ALL_01[2]&&Strasamp_ALL_4$DF_ALL_01[1]!=Strasamp_ALL_4$DF_ALL_01[3]&&Strasamp_ALL_4$DF_ALL_01[1]!=Strasamp_ALL_4$DF_ALL_01[4]&&Strasamp_ALL_4$DF_ALL_01[2]!=Strasamp_ALL_4$DF_ALL_01[3]&&Strasamp_ALL_4$DF_ALL_01[2]!=Strasamp_ALL_4$DF_ALL_01[4]&&Strasamp_ALL_4$DF_ALL_01[3]!=Strasamp_ALL_4$DF_ALL_01[4]){
    print(Strasamp_ALL_4[1]) #抽出された銘柄の書き出し
    DF_ALL_2_4<-t(Strasamp_ALL_4) #抽出されたデータを転置(モンテカルロのため)
    DF_ALL_3_4<-DF_ALL_2_4[-2:-1,] #クラスタ番号の行のみを削除
    stock_Return_ALL_4 = as.matrix( DF_ALL_3_4[ , 1:4] ) #行列として扱う 
    
    #銘柄名を格納する
    brands4_name_ALL_1_m <- t(as.matrix(row.names(Strasamp_ALL_4[1])))
    brands4_name_ALL_m <- rbind( brands4_name_ALL_m, brands4_name_ALL_1_m )
    
    #変数の設定
    mc_rep_ALL_4 = 10000 #Number of Monte Carlo Simulations(モンテカルロシミュレーションの回数)
    training_days_ALL_4 = 30 #将来何日を予測するかの日
    
    ##抽出した銘柄の処理
    
    #各銘柄に対する投資の重みつけ(ここでは等分つまり1/4ずつとしている)
    portfolio_Weights_ALL_4 = t(as.matrix(rep(1/ncol(stock_Return_ALL_4), ncol(stock_Return_ALL_4))))
    print(portfolio_Weights_ALL_4)
    
    #共分散行列と株式リターンの平均値を計算
    #株式リターンの分散共分散行列を取得
    coVarMat_ALL_4 = cov(stock_Return_ALL_4)
    miu_ALL_4 = colMeans(stock_Return_ALL_4)
    #Extend the vector to a matrix(ベクトルを行列に拡張)
    Miu_ALL_4 = matrix(rep(miu_ALL_4, training_days_ALL_4), nrow = 4)
    
    #シミュレートされた30日間のポートフォリオリターンの初期化
    portfolio_Returns_30_ALL_m_4 = matrix(0, training_days_ALL_4, mc_rep_ALL_4)
    
    #各銘柄のβ値
    brand_A.Beta_ALL_4 <- DF_ALL_2_4[2,1] #1つ目の銘柄のβを抽出
    brand_B.Beta_ALL_4 <- DF_ALL_2_4[2,2] #2つ目の銘柄のβを抽出
    brand_C.Beta_ALL_4 <- DF_ALL_2_4[2,3] #3つ目の銘柄のβを抽出
    brand_D.Beta_ALL_4 <- DF_ALL_2_4[2,4] #4つ目の銘柄のβを抽出
    each.Beta_ALL_4 <- c(brand_A.Beta_ALL_4,brand_B.Beta_ALL_4,brand_C.Beta_ALL_4,brand_D.Beta_ALL_4) #4つの銘柄のβを配列化
    
    ##モンテカルロシミュレーション
    
    #1つのポートフォリオに対して10000回のモンテカルロシミュレーションを実行
    for (k in 1:mc_rep_ALL_4) {
      Z_ALL_4 = matrix ( rnorm( dim(stock_Return_ALL_4)[2] * training_days_ALL_4 ), ncol = training_days_ALL_4 )
      #Lower Triangular Matrix from our Choleski Factorization(コレスキー分解による下三角行列)
      #正定値行列近似Q = nearPD(coVarMat)
      L_ALL_4 = t( chol( coVarMat_ALL_4 ) )
      #Calculate stock returns for each day(毎日の株式リターンを計算)
      daily_Returns_ALL_4 = Miu_ALL_4 + L_ALL_4%*%Z_ALL_4 
      #Calculate portfolio returns for 30 days(30日間のポートフォリオリターンを計算)
      portfolio_Returns_30_ALL_4 = portfolio_Weights_ALL_4 %*% daily_Returns_ALL_4
      #Add it to the monte-carlo matrix(それをモンテカルロ行列に追加)
      portfolio_Returns_30_ALL_m_4[,k] = portfolio_Returns_30_ALL_4;
    }
    #モンテカルロシミュレーションの終了
    
    #ポートフォリオのβ値計算
    portfolio_Beta_ALL_4 <- portfolio_Weights_ALL_4%*%each.Beta_ALL_4
    print(portfolio_Beta_ALL_4)
    
    #1シナリオごと(つまり列ごと)の結果を集計
    portfolio_return_ALL_4_0 = mean(portfolio_Returns_30_ALL_m_4[,]) #10000シナリオのリターンの平均(期待収益)
    portfolio_return_ALL_4 = ((portfolio_return_ALL_4_0+1)^(N))-1 #年率換算
    return_ALL = apply(portfolio_Returns_30_ALL_m_4, 2, mean) #期待収益
    Y_return_ALL = ((return_ALL+1)^(N))-1 #年率換算
    risk_ALL = apply(portfolio_Returns_30_ALL_m_4, 2, sd) #標準偏差(リスク)
    Y_risk_ALL = risk_ALL*sqrt(N) #年率換算
    portfolio_risk_ALL_4 = mean(Y_risk_ALL) #10000シナリオの標準偏差(リスク)の平均
    sharpe_ALL = Y_return_ALL/Y_risk_ALL #シャープレシオの計算(年率換算後の結果を用いる)
    sharpe_ALL_0 = c(sharpe_ALL[(sharpe_ALL<0)])  #シャープレシオが負の値であるもの
    sharpe_ALL_1 = c(sharpe_ALL[(sharpe_ALL>0)])  #シャープレシオが正の値であるもの
    sharpe_ALL_2 = rep(0,10000-length(sharpe_ALL_1)) #シャープレシオが負の値であるものの個数分だけ0を生成
    sharpe_ALL_3 = c(sharpe_ALL_1,sharpe_ALL_2) #個数を10000個に戻す(つまり正の値と0のベクトルを結合)
    portfolio_sharpe_ALL_4 = mean(sharpe_ALL_3) #負のものを0とした時の平均(全て分母10000)
    portfolio_sharpe_ALL_4_1 = mean(sharpe_ALL) #全てのシャープレシオの平均
    portfolio_sharpe_ALL_4_2 = mean(sharpe_ALL_1) #正のものだけの平均(分母がシナリオによって異なる)
    portfolio_sharpe_ALL_4_3 = mean(sharpe_ALL_0) #負のものだけの平均(分母がシナリオによって異なる)
    
    treynor_ALL <- Y_return_ALL/portfolio_Beta_ALL_4 #トレイナーレシオの計算(年率換算後の結果を用いる)
    portfolio_treynor_ALL_4 = mean(treynor_ALL)
    
    p_ALL <- portfolio_Returns_30_ALL_m_4
    p_ALL_2 <- p_ALL
    p_ALL_2[(p_ALL_2>0)] <- NaN
    risk2_ALL <- apply(p_ALL_2,2,sd, na.rm=T) #下方リスクの算出
    Y_risk2_ALL = risk2_ALL*sqrt(N) #年率換算
    sortino_ALL = Y_return_ALL/Y_risk2_ALL #ソルティノレシオの計算(年率換算後の結果を用いる)
    portfolio_sortino_ALL_4 <- sum(sortino_ALL[!is.na(sortino_ALL)])/10000 
    #meanだと分母が変わってしまう(このために以下の式はダメ)ので、10000で割ることとする
    #portfolio_sortino_ALL_4 = mean(sortino_ALL[!is.na(sortino_ALL)])
    #全てNaN(ずっと正の値)となる列(シナリオ)が1つでもある場合に結果にNAを示すこととなるため以下でもダメ
    #portfolio_sortino_ALL_4 = mean(sortino_ALL)
    #全てNaNの列があるかないかは以下で確認できる
    #p_ALL_3 <- p_ALL_2[!apply(is.na(p_ALL_2), 2, all)]
    #str(p_ALL_3) これがnum[1:300000]でなければ全てNaNの列があることとなる
    
    ##10000個の結果をベクトルに格納
    
    #ポートフォリオの統計を表示する
    print(c(portfolio_return_ALL_4,portfolio_risk_ALL_4,
            portfolio_sharpe_ALL_4,portfolio_sharpe_ALL_4_1,
            portfolio_sharpe_ALL_4_2,portfolio_sharpe_ALL_4_3,portfolio_treynor_ALL_4,portfolio_sortino_ALL_4))
    
    #最終的な結果を確認するために格納庫(ベクトル)にデータを入れ込む
    #ポートフォリオの期待収益率を格納
    r_data_ALL_4 = c(r_data_ALL_4,portfolio_return_ALL_4)
    #ポートフォリオのリスクを格納
    sd_data_ALL_4 = c(sd_data_ALL_4,portfolio_risk_ALL_4)
    #ポートフォリオのβ値を格納
    Beta_data_ALL_4 = c(Beta_data_ALL_4,portfolio_Beta_ALL_4)
    #ポートフォリオのシャープレシオを格納
    sharpe_data_ALL_4 = c(sharpe_data_ALL_4,portfolio_sharpe_ALL_4)
    sharpe_data_ALL_4_1 = c(sharpe_data_ALL_4_1,portfolio_sharpe_ALL_4_1)
    sharpe_data_ALL_4_2 = c(sharpe_data_ALL_4_2,portfolio_sharpe_ALL_4_2)
    sharpe_data_ALL_4_3 = c(sharpe_data_ALL_4_3,portfolio_sharpe_ALL_4_3)
    #ポートフォリオのトレイナーレシオを格納
    treynor_data_ALL_4 = c(treynor_data_ALL_4,portfolio_treynor_ALL_4)
    #ポートフォリオのソルティノレシオを格納
    sortino_data_ALL_4 = c(sortino_data_ALL_4,portfolio_sortino_ALL_4)
    
    #試行回数の確認
    print(length(r_data_ALL_4))
    
    #for文終了条件
    if(length(r_data_ALL_4) == 10000 && length(sd_data_ALL_4) == 10000 && length(Beta_data_ALL_4) == 10000){
      break #10000個のデータを取得したらシミュレーションを終える
    }
  }
}

#for文内で格納した10000個のポートフォリオのデータを表示する
r_data_ALL_4 #10000個のポートフォリオのリターン
sd_data_ALL_4 #10000個のポートフォリオのリスク
sharpe_data_ALL_4 #10000個のポートフォリオの置き換えシャープレシオ
sharpe_data_ALL_4_1 #10000個のポートフォリオの全部シャープレシオ
sharpe_data_ALL_4_2 #10000個のポートフォリオの正シャープレシオ
sharpe_data_ALL_4_3 #10000個のポートフォリオの負シャープレシオ
treynor_data_ALL_4 #10000個のポートフォリオのトレイナーレシオ
sortino_data_ALL_4 #10000個のポートフォリオのソルティノレシオ
Beta_data_ALL_4 #10000個のポートフォリオのβ値
brands4_name_ALL_m #選ばれた銘柄の一覧

#10000個のポートフォリオのリターンとリスク、β値の平均を表示
ALL_r_4 <- mean(r_data_ALL_4) #10000個のポートフォリオのリターンの平均
ALL_sd_4 <- mean(sd_data_ALL_4) #10000個のポートフォリオのリスクの平均
ALL_Beta_4 <- mean(Beta_data_ALL_4) #10000個のポートフォリオのβ値の平均
ALL_sharpe_4 <-mean(sharpe_data_ALL_4) #10000個のポートフォリオの置き換えシャープレシオ
ALL_sharpe_4_1 <-mean(sharpe_data_ALL_4_1) #10000個のポートフォリオの全部シャープレシオの平均
ALL_sharpe_4_2 <-mean(sharpe_data_ALL_4_2) #10000個のポートフォリオの正シャープレシオ
ALL_sharpe_4_3 <-mean(sharpe_data_ALL_4_3) #10000個のポートフォリオの負シャープレシオ
ALL_treynor_4 <- mean(treynor_data_ALL_4) #10000個のポートフォリオのトレイナーレシオ
ALL_sortino_4 <- mean(sortino_data_ALL_4) #10000個のポートフォリオのソルティノレシオ
print(c("Mean Return","Mean Risk","Mean Beta","Mean Sharpe(+0)",
        "Mean Sharpe(+-)","Mean Sharpe(+)","Mean Sharpe(-)","Mean Treynor","Mean Sortino"))#結果のラベル
print(c(ALL_r_4,ALL_sd_4,ALL_Beta_4,ALL_sharpe_4,
        ALL_sharpe_4_1,ALL_sharpe_4_2,ALL_sharpe_4_3,ALL_treynor_4,ALL_sortino_4)) #結果の表示

#===========================================================================================================
##⑧結果のまとめその1(箱ひげ図)
#===========================================================================================================

##上記の実行結果を保存するためのデータフレームを作成
#②〜⑦の6つの業種分類の10000個のポートフォリオのリターンを書き出すためのデータフレームを作成
box_return_monte_carlo_4bra_2 <- data.frame("TOPIX17"=r_data_TOPIX17_4,"Sector"=r_data_Sector_4,"DTW+k-medoids(N)"=r_data_DTWMedN_4,"DTW+k-medoids(S)"=r_data_DTWMedS_4,"DTW+k-medoids(β)"=r_data_DTWMedβ_4,"ALL"=r_data_ALL_4)
#②〜⑦の6つの業種分類の10000個のポートフォリオのリスクを書き出すためのデータフレームを作成
box_risk_monte_carlo_4bra_2 <- data.frame("TOPIX17"=sd_data_TOPIX17_4,"Sector"=sd_data_Sector_4,"DTW+k-medoids(N)"=sd_data_DTWMedN_4,"DTW+k-medoids(S)"=sd_data_DTWMedS_4,"DTW+k-medoids(β)"=sd_data_DTWMedβ_4,"ALL"=sd_data_ALL_4)
#②〜⑦の6つの業種分類の10000個のポートフォリオの置き換えシャープレシオを書き出すためのデータフレームを作成
box_sharp_monte_carlo_4bra_2 <- data.frame("TOPIX17"=sharpe_data_TOPIX17_4,"Sector"=sharpe_data_Sector_4,"DTW+k-medoids(N)"=sharpe_data_DTWMedN_4,"DTW+k-medoids(S)"=sharpe_data_DTWMedS_4,"DTW+k-medoids(β)"=sharpe_data_DTWMedβ_4,"ALL"=sharpe_data_ALL_4)
#②〜⑦の6つの業種分類の10000個のポートフォリオの全部シャープレシオを書き出すためのデータフレームを作成
box_sharp_1_monte_carlo_4bra_2 <- data.frame("TOPIX17"=sharpe_data_TOPIX17_4_1,"Sector"=sharpe_data_Sector_4_1,"DTW+k-medoids(N)"=sharpe_data_DTWMedN_4_1,"DTW+k-medoids(S)"=sharpe_data_DTWMedS_4_1,"DTW+k-medoids(β)"=sharpe_data_DTWMedβ_4_1,"ALL"=sharpe_data_ALL_4_1)
#②〜⑦の6つの業種分類の10000個のポートフォリオの正シャープレシオを書き出すためのデータフレームを作成
box_sharp_2_monte_carlo_4bra_2 <- data.frame("TOPIX17"=sharpe_data_TOPIX17_4_2,"Sector"=sharpe_data_Sector_4_2,"DTW+k-medoids(N)"=sharpe_data_DTWMedN_4_2,"DTW+k-medoids(S)"=sharpe_data_DTWMedS_4_2,"DTW+k-medoids(β)"=sharpe_data_DTWMedβ_4_2,"ALL"=sharpe_data_ALL_4_2)
#②〜⑦の6つの業種分類の10000個のポートフォリオの負シャープレシオを書き出すためのデータフレームを作成
box_sharp_3_monte_carlo_4bra_2 <- data.frame("TOPIX17"=sharpe_data_TOPIX17_4_3,"Sector"=sharpe_data_Sector_4_3,"DTW+k-medoids(N)"=sharpe_data_DTWMedN_4_3,"DTW+k-medoids(S)"=sharpe_data_DTWMedS_4_3,"DTW+k-medoids(β)"=sharpe_data_DTWMedβ_4_3,"ALL"=sharpe_data_ALL_4_3)
#②〜⑦の6つの業種分類の10000個のポートフォリオのトレイナーレシオを書き出すためのデータフレームを作成
box_treynor_monte_carlo_4bra_2 <- data.frame("TOPIX17"=treynor_data_TOPIX17_4,"Sector"=treynor_data_Sector_4,"DTW+k-medoids(N)"=treynor_data_DTWMedN_4,"DTW+k-medoids(S)"=treynor_data_DTWMedS_4,"DTW+k-medoids(β)"=treynor_data_DTWMedβ_4,"ALL"=treynor_data_ALL_4)
#②〜⑦の6つの業種分類の10000個のポートフォリオのソルティノレシオを書き出すためのデータフレームを作成
box_sortino_monte_carlo_4bra_2 <- data.frame("TOPIX17"=sortino_data_TOPIX17_4,"Sector"=sortino_data_Sector_4,"DTW+k-medoids(N)"=sortino_data_DTWMedN_4,"DTW+k-medoids(S)"=sortino_data_DTWMedS_4,"DTW+k-medoids(β)"=sortino_data_DTWMedβ_4,"ALL"=sortino_data_ALL_4)

##読み込んだデータからboxplotのためのベクトルを作成
#②〜⑦の6つの業種分類の10000個のポートフォリオのリターンのboxplotのためのベクトルを作成
box_return_monte_carlo_4bra_2_2 <- c(box_return_monte_carlo_4bra_2[,1],box_return_monte_carlo_4bra_2[,2],box_return_monte_carlo_4bra_2[,3],box_return_monte_carlo_4bra_2[,4],box_return_monte_carlo_4bra_2[,5],box_return_monte_carlo_4bra_2[,6])
#②〜⑦の6つの業種分類の10000個のポートフォリオのリスクのboxplotのためのベクトルを作成
box_risk_monte_carlo_4bra_2_2 <- c(box_risk_monte_carlo_4bra_2[,1],box_risk_monte_carlo_4bra_2[,2],box_risk_monte_carlo_4bra_2[,3],box_risk_monte_carlo_4bra_2[,4],box_risk_monte_carlo_4bra_2[,5],box_risk_monte_carlo_4bra_2[,6])
#②〜⑦の6つの業種分類の10000個のポートフォリオの置き換えシャープレシオのboxplotのためのベクトルを作成
box_sharp_monte_carlo_4bra_2_2 <- c(box_sharp_monte_carlo_4bra_2[,1],box_sharp_monte_carlo_4bra_2[,2],box_sharp_monte_carlo_4bra_2[,3],box_sharp_monte_carlo_4bra_2[,4],box_sharp_monte_carlo_4bra_2[,5],box_sharp_monte_carlo_4bra_2[,6])
#②〜⑦の6つの業種分類の10000個のポートフォリオの全部シャープレシオのboxplotのためのベクトルを作成
box_sharp_1_monte_carlo_4bra_2_2 <- c(box_sharp_1_monte_carlo_4bra_2[,1],box_sharp_1_monte_carlo_4bra_2[,2],box_sharp_1_monte_carlo_4bra_2[,3],box_sharp_1_monte_carlo_4bra_2[,4],box_sharp_1_monte_carlo_4bra_2[,5],box_sharp_1_monte_carlo_4bra_2[,6])
#②〜⑦の6つの業種分類の10000個のポートフォリオの正シャープレシオのboxplotのためのベクトルを作成
box_sharp_2_monte_carlo_4bra_2_2 <- c(box_sharp_2_monte_carlo_4bra_2[,1],box_sharp_2_monte_carlo_4bra_2[,2],box_sharp_2_monte_carlo_4bra_2[,3],box_sharp_2_monte_carlo_4bra_2[,4],box_sharp_2_monte_carlo_4bra_2[,5],box_sharp_2_monte_carlo_4bra_2[,6])
#②〜⑦の6つの業種分類の10000個のポートフォリオの負シャープレシオのboxplotのためのベクトルを作成
box_sharp_3_monte_carlo_4bra_2_2 <- c(box_sharp_3_monte_carlo_4bra_2[,1],box_sharp_3_monte_carlo_4bra_2[,2],box_sharp_3_monte_carlo_4bra_2[,3],box_sharp_3_monte_carlo_4bra_2[,4],box_sharp_3_monte_carlo_4bra_2[,5],box_sharp_3_monte_carlo_4bra_2[,6])
#②〜⑦の6つの業種分類の10000個のポートフォリオのトレイナーレシオのboxplotのためのベクトルを作成
box_treynor_monte_carlo_4bra_2_2 <- c(box_treynor_monte_carlo_4bra_2[,1],box_treynor_monte_carlo_4bra_2[,2],box_treynor_monte_carlo_4bra_2[,3],box_treynor_monte_carlo_4bra_2[,4],box_treynor_monte_carlo_4bra_2[,5],box_treynor_monte_carlo_4bra_2[,6])
#②〜⑦の6つの業種分類の10000個のポートフォリオのソルティノレシオのboxplotのためのベクトルを作成
box_sortino_monte_carlo_4bra_2_2 <- c(box_sortino_monte_carlo_4bra_2[,1],box_sortino_monte_carlo_4bra_2[,2],box_sortino_monte_carlo_4bra_2[,3],box_sortino_monte_carlo_4bra_2[,4],box_sortino_monte_carlo_4bra_2[,5],box_sortino_monte_carlo_4bra_2[,6])

#boxplotのための業種分類名のベクトルを作成
name <- c(rep("TOPIX17", 10000),rep("Sector", 10000),rep("DTW+k-medoids(N)", 10000),rep("DTW+k-medoids(S)", 10000),rep("DTW+k-medoids(β)", 10000),rep("No classification", 10000))

##boxplotのためのデータフレームを作成
#②〜⑦の6つの業種分類の10000個のポートフォリオのリターンのboxplotのためのデータフレームを作成
box_return_monte_carlo_4bra_3_2 <- data.frame("Value"=box_return_monte_carlo_4bra_2_2,"Industry.classification"=name)
#②〜⑦の6つの業種分類の10000個のポートフォリオのリスクのboxplotのためのデータフレームを作成
box_risk_monte_carlo_4bra_3_2 <- data.frame("Value"=box_risk_monte_carlo_4bra_2_2,"Industry.classification"=name)
#②〜⑦の6つの業種分類の10000個のポートフォリオの置き換えシャープレシオのboxplotのためのデータフレームを作成
box_sharp_monte_carlo_4bra_3_2 <- data.frame("Value"=box_sharp_monte_carlo_4bra_2_2,"Industry.classification"=name)
#②〜⑦の6つの業種分類の10000個のポートフォリオの全部シャープレシオのboxplotのためのデータフレームを作成
box_sharp_1_monte_carlo_4bra_3_2 <- data.frame("Value"=box_sharp_1_monte_carlo_4bra_2_2,"Industry.classification"=name)
#②〜⑦の6つの業種分類の10000個のポートフォリオの正シャープレシオのboxplotのためのデータフレームを作成
box_sharp_2_monte_carlo_4bra_3_2 <- data.frame("Value"=box_sharp_2_monte_carlo_4bra_2_2,"Industry.classification"=name)
#②〜⑦の6つの業種分類の10000個のポートフォリオの負シャープレシオのboxplotのためのデータフレームを作成
box_sharp_3_monte_carlo_4bra_3_2 <- data.frame("Value"=box_sharp_3_monte_carlo_4bra_2_2,"Industry.classification"=name)
#②〜⑦の6つの業種分類の10000個のポートフォリオのトレイナーレシオのboxplotのためのデータフレームを作成
box_treynor_monte_carlo_4bra_3_2 <- data.frame("Value"=box_treynor_monte_carlo_4bra_2_2,"Industry.classification"=name)
#②〜⑦の6つの業種分類の10000個のポートフォリオのソルティノレシオのboxplotのためのデータフレームを作成
box_sortino_monte_carlo_4bra_3_2 <- data.frame("Value"=box_sortino_monte_carlo_4bra_2_2,"Industry.classification"=name)

##箱ひげ図の可視化
#②〜⑦の6つの業種分類の10000個のポートフォリオのリターンのboxplot
ggplot(box_return_monte_carlo_4bra_3_2,aes(x=Industry.classification,y= Value))+geom_boxplot()+
  ggtitle("Return Data (monte carlo,4brands)") + theme(plot.title = element_text(hjust = 0.5)) + stat_summary(fun = mean, geom = "point" ,size=3 ,fill = "grey",shape = 4)
#②〜⑦の6つの業種分類の10000個のポートフォリオのリスクのboxplot
ggplot(box_risk_monte_carlo_4bra_3_2,aes(x=Industry.classification,y= Value))+geom_boxplot()+
  ggtitle("Risk Data (monte carlo,4brands)") + theme(plot.title = element_text(hjust = 0.5)) + stat_summary(fun = mean, geom = "point" ,size=3 ,fill = "grey",shape = 4)
#②〜⑦の6つの業種分類の10000個のポートフォリオの置き換えシャープレシオのboxplot
ggplot(box_sharp_monte_carlo_4bra_3_2,aes(x=Industry.classification,y= Value))+geom_boxplot()+
  ggtitle("Sharperatio Data (monte carlo,4brands)") + theme(plot.title = element_text(hjust = 0.5)) + stat_summary(fun = mean, geom = "point" ,size=3 ,fill = "grey",shape = 4)
#②〜⑦の6つの業種分類の10000個のポートフォリオの全部シャープレシオのboxplot
ggplot(box_sharp_1_monte_carlo_4bra_3_2,aes(x=Industry.classification,y= Value))+geom_boxplot()+
  ggtitle("Sharperatio Data (monte carlo,4brands)") + theme(plot.title = element_text(hjust = 0.5)) + stat_summary(fun = mean, geom = "point" ,size=3 ,fill = "grey",shape = 4)
#②〜⑦の6つの業種分類の10000個のポートフォリオの正シャープレシオのboxplot
ggplot(box_sharp_2_monte_carlo_4bra_3_2,aes(x=Industry.classification,y= Value))+geom_boxplot()+
  ggtitle("Sharperatio Data (monte carlo,4brands)") + theme(plot.title = element_text(hjust = 0.5)) + stat_summary(fun = mean, geom = "point" ,size=3 ,fill = "grey",shape = 4)
#②〜⑦の6つの業種分類の10000個のポートフォリオの負シャープレシオのboxplot
ggplot(box_sharp_3_monte_carlo_4bra_3_2,aes(x=Industry.classification,y= Value))+geom_boxplot()+
  ggtitle("Sharperatio Data (monte carlo,4brands)") + theme(plot.title = element_text(hjust = 0.5)) + stat_summary(fun = mean, geom = "point" ,size=3 ,fill = "grey",shape = 4)
#②〜⑦の6つの業種分類の10000個のポートフォリオのトレイナーレシオのboxplot
ggplot(box_treynor_monte_carlo_4bra_3_2,aes(x=Industry.classification,y= Value))+geom_boxplot()+
  ggtitle("Treynorratio Data (monte carlo,4brands)") + theme(plot.title = element_text(hjust = 0.5)) + stat_summary(fun = mean, geom = "point" ,size=3 ,fill = "grey",shape = 4)
#②〜⑦の6つの業種分類の10000個のポートフォリオのソルティノレシオのboxplot
ggplot(box_sortino_monte_carlo_4bra_3_2,aes(x=Industry.classification,y= Value))+geom_boxplot()+
  ggtitle("Sortinoratio Data (monte carlo,4brands)") + theme(plot.title = element_text(hjust = 0.5)) + stat_summary(fun = mean, geom = "point" ,size=3 ,fill = "grey",shape = 4)

#===========================================================================================================
##⑨結果のまとめその2(統計的検定)
#===========================================================================================================

###(9.1)Jarque-Bera(ジャック-ベラ検定)検定
#ダゴスティーノのK二乗検定と同様に歪度及び尖度に基づき正規性を検定
library(tseries)
##60000個のリターンデータの検定
normaltest_return_monte_carlo_4bra <- c(box_return_monte_carlo_4bra_2[,1],box_return_monte_carlo_4bra_2[,2],box_return_monte_carlo_4bra_2[,3],box_return_monte_carlo_4bra_2[,4],box_return_monte_carlo_4bra_2[,5],box_return_monte_carlo_4bra_2[,6])
normaltest_return_monte_carlo_4bra_2 <- as.numeric(normaltest_return_monte_carlo_4bra)
jarque.bera.test(normaltest_return_monte_carlo_4bra_2)

##60000個のリスクデータの検定
normaltest_risk_monte_carlo_4bra <- c(box_risk_monte_carlo_4bra_2[,1],box_risk_monte_carlo_4bra_2[,2],box_risk_monte_carlo_4bra_2[,3],box_risk_monte_carlo_4bra_2[,4],box_risk_monte_carlo_4bra_2[,5],box_risk_monte_carlo_4bra_2[,6])
normaltest_risk_monte_carlo_4bra_2 <- as.numeric(normaltest_risk_monte_carlo_4bra)
jarque.bera.test(normaltest_risk_monte_carlo_4bra_2)

##60000個の置き換えシャープレシオデータの検定
normaltest_sharp_monte_carlo_4bra <- c(box_sharp_monte_carlo_4bra_2[,1],box_sharp_monte_carlo_4bra_2[,2],box_sharp_monte_carlo_4bra_2[,3],box_sharp_monte_carlo_4bra_2[,4],box_sharp_monte_carlo_4bra_2[,5],box_sharp_monte_carlo_4bra_2[,6])
normaltest_sharp_monte_carlo_4bra_2 <- as.numeric(normaltest_sharp_monte_carlo_4bra)
jarque.bera.test(normaltest_sharp_monte_carlo_4bra_2)

##60000個の全部シャープレシオデータの検定
normaltest_sharp_1_monte_carlo_4bra <- c(box_sharp_1_monte_carlo_4bra_2[,1],box_sharp_1_monte_carlo_4bra_2[,2],box_sharp_1_monte_carlo_4bra_2[,3],box_sharp_1_monte_carlo_4bra_2[,4],box_sharp_1_monte_carlo_4bra_2[,5],box_sharp_1_monte_carlo_4bra_2[,6])
normaltest_sharp_1_monte_carlo_4bra_2 <- as.numeric(normaltest_sharp_1_monte_carlo_4bra)
jarque.bera.test(normaltest_sharp_1_monte_carlo_4bra_2)

##60000個の正シャープレシオデータの検定
normaltest_sharp_2_monte_carlo_4bra <- c(box_sharp_2_monte_carlo_4bra_2[,1],box_sharp_2_monte_carlo_4bra_2[,2],box_sharp_2_monte_carlo_4bra_2[,3],box_sharp_2_monte_carlo_4bra_2[,4],box_sharp_2_monte_carlo_4bra_2[,5],box_sharp_2_monte_carlo_4bra_2[,6])
normaltest_sharp_2_monte_carlo_4bra_2 <- as.numeric(normaltest_sharp_2_monte_carlo_4bra)
jarque.bera.test(normaltest_sharp_2_monte_carlo_4bra_2)

##60000個の負シャープレシオデータの検定
normaltest_sharp_3_monte_carlo_4bra <- c(box_sharp_3_monte_carlo_4bra_2[,1],box_sharp_3_monte_carlo_4bra_2[,2],box_sharp_3_monte_carlo_4bra_2[,3],box_sharp_3_monte_carlo_4bra_2[,4],box_sharp_3_monte_carlo_4bra_2[,5],box_sharp_3_monte_carlo_4bra_2[,6])
normaltest_sharp_3_monte_carlo_4bra_2 <- as.numeric(normaltest_sharp_3_monte_carlo_4bra)
jarque.bera.test(normaltest_sharp_3_monte_carlo_4bra_2)

##60000個のトレイナーレシオデータの検定
normaltest_treynor_monte_carlo_4bra <- c(box_treynor_monte_carlo_4bra_2[,1],box_treynor_monte_carlo_4bra_2[,2],box_treynor_monte_carlo_4bra_2[,3],box_treynor_monte_carlo_4bra_2[,4],box_treynor_monte_carlo_4bra_2[,5],box_treynor_monte_carlo_4bra_2[,6])
normaltest_treynor_monte_carlo_4bra_2 <- as.numeric(normaltest_treynor_monte_carlo_4bra)
jarque.bera.test(normaltest_treynor_monte_carlo_4bra_2)

###(9.2)多重比較法
#Steel-Dwass(スティール・ドゥワス)の方法による多重比較
Steel.Dwass <- function(data,                                                # データベクトル
                        group)                                          # 群変数ベクトル
{
  OK <- complete.cases(data, group)                            # 欠損値を持つケースを除く
  data <- data[OK]
  group <- group[OK]
  n.i <- table(group)                                          # 各群のデータ数
  ng <- length(n.i)                                            # 群の数
  t <- combn(ng, 2, function(ij) {
    i <- ij[1]
    j <- ij[2]
    r <- rank(c(data[group == i], data[group == j]))     # 群 i, j をまとめてランク付け
    R <- sum(r[1:n.i[i]])                                        # 検定統計量
    N <- n.i[i]+n.i[j]                                   # 二群のデータ数の合計
    E <- n.i[i]*(N+1)/2                                  # 検定統計量の期待値
    V <- n.i[i]*n.i[j]/(N*(N-1))*(sum(r^2)-N*(N+1)^2/4)  # 検定統計量の分散
    return(abs(R-E)/sqrt(V))                                # t 値を返す
  })
  p <- ptukey(t*sqrt(2), ng, Inf, lower.tail=FALSE)            # P 値を計算
  result <- cbind(t, p)                                                # 結果をまとめる
  rownames(result) <- combn(ng, 2, paste, collapse=":")
  return(result)
}

#以下で多用するベクトルの作成
G_2_2 <- rep(1:6, c(10000, 10000, 10000, 10000, 10000, 10000))

#リターンデータの検定
D_return_monte_carlo_4bra_2 <- c(box_return_monte_carlo_4bra_2[,1],box_return_monte_carlo_4bra_2[,2],box_return_monte_carlo_4bra_2[,3],box_return_monte_carlo_4bra_2[,4],box_return_monte_carlo_4bra_2[,5],box_return_monte_carlo_4bra_2[,6])
Steel.Dwass(D_return_monte_carlo_4bra_2, G_2_2)
#リスクデータの検定
D_risk_monte_carlo_4bra_2 <- c(box_risk_monte_carlo_4bra_2[,1],box_risk_monte_carlo_4bra_2[,2],box_risk_monte_carlo_4bra_2[,3],box_risk_monte_carlo_4bra_2[,4],box_risk_monte_carlo_4bra_2[,5],box_risk_monte_carlo_4bra_2[,6])
Steel.Dwass(D_risk_monte_carlo_4bra_2, G_2_2)
#置き換えシャープレシオデータの検定
D_sharp_monte_carlo_4bra_2 <- c(box_sharp_monte_carlo_4bra_2[,1],box_sharp_monte_carlo_4bra_2[,2],box_sharp_monte_carlo_4bra_2[,3],box_sharp_monte_carlo_4bra_2[,4],box_sharp_monte_carlo_4bra_2[,5],box_sharp_monte_carlo_4bra_2[,6])
Steel.Dwass(D_sharp_monte_carlo_4bra_2, G_2_2)
#全部シャープレシオデータの検定
D_sharp_1_monte_carlo_4bra_2 <- c(box_sharp_1_monte_carlo_4bra_2[,1],box_sharp_1_monte_carlo_4bra_2[,2],box_sharp_1_monte_carlo_4bra_2[,3],box_sharp_1_monte_carlo_4bra_2[,4],box_sharp_1_monte_carlo_4bra_2[,5],box_sharp_1_monte_carlo_4bra_2[,6])
Steel.Dwass(D_sharp_1_monte_carlo_4bra_2, G_2_2)
#正シャープレシオデータの検定
D_sharp_2_monte_carlo_4bra_2 <- c(box_sharp_2_monte_carlo_4bra_2[,1],box_sharp_2_monte_carlo_4bra_2[,2],box_sharp_2_monte_carlo_4bra_2[,3],box_sharp_2_monte_carlo_4bra_2[,4],box_sharp_2_monte_carlo_4bra_2[,5],box_sharp_2_monte_carlo_4bra_2[,6])
Steel.Dwass(D_sharp_2_monte_carlo_4bra_2, G_2_2)
#負シャープレシオデータの検定
D_sharp_3_monte_carlo_4bra_2 <- c(box_sharp_3_monte_carlo_4bra_2[,1],box_sharp_3_monte_carlo_4bra_2[,2],box_sharp_3_monte_carlo_4bra_2[,3],box_sharp_3_monte_carlo_4bra_2[,4],box_sharp_3_monte_carlo_4bra_2[,5],box_sharp_3_monte_carlo_4bra_2[,6])
Steel.Dwass(D_sharp_3_monte_carlo_4bra_2, G_2_2)
#トレイナーレシオデータの検定
D_treynor_monte_carlo_4bra_2 <- c(box_treynor_monte_carlo_4bra_2[,1],box_treynor_monte_carlo_4bra_2[,2],box_treynor_monte_carlo_4bra_2[,3],box_treynor_monte_carlo_4bra_2[,4],box_treynor_monte_carlo_4bra_2[,5],box_treynor_monte_carlo_4bra_2[,6])
Steel.Dwass(D_treynor_monte_carlo_4bra_2, G_2_2)
#ソルティノレシオデータの検定
D_sortino_monte_carlo_4bra_2 <- c(box_sortino_monte_carlo_4bra_2[,1],box_sortino_monte_carlo_4bra_2[,2],box_sortino_monte_carlo_4bra_2[,3],box_sortino_monte_carlo_4bra_2[,4],box_sortino_monte_carlo_4bra_2[,5],box_sortino_monte_carlo_4bra_2[,6])
Steel.Dwass(D_sortino_monte_carlo_4bra_2, G_2_2)
