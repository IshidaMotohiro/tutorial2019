## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, cache = TRUE, comment=NA)
library(tidyverse)
library(stringi)
library(gapminder)
library(broom)
library(glmnet)


## ------------------------------------------------------------------------
gap <- gapminder %>% 
  filter(country %in% 
  c("Japan", "China", "Korea, Rep."))


## ------------------------------------------------------------------------
p <- gap %>% 
  ggplot() + 
  aes(gdpPercap,
      lifeExp, 
      col = country) +
  geom_line()


## --------------------------------------------------------------
p


## --------------------------------------------------------------
gap %>% filter(country=="China") %>%
  select(gdpPercap, lifeExp) %>% cor()

gap %>% filter(country=="Korea, Rep.")%>%
  select(gdpPercap, lifeExp) %>% cor()

gap %>% filter(country=="Japan") %>%
  select(gdpPercap, lifeExp) %>% cor()


## ------------------------------------------------------------------------
gap %>% filter(country == "China") %>% 
  cor.test(data=., ~lifeExp +gdpPercap)


## --------------------------------------------------------------
library(broom)


## ------------------------------------------------------------------------
gap %>%  nest(-country) %>% 
  mutate(test = map(data, 
          ~cor.test(data=., 
              ~ lifeExp + gdpPercap)),
          res = map(test, tidy)) %>% 
              unnest(res, .drop = TRUE)


## ------------------------------------------------------------------------
gap %>% nest(-country) 


## ------------------------------------------------------------------------
gap %>% nest(-country) %>% 
              select(data) %>% unnest() 


## ------------------------------------------------------------------------
gap %>%  nest(-country) %>% 
 mutate(AVG=map(data,~mean(.$lifeExp))) 


## ------------------------------------------------------------------------
list(x = 1:3, y=1:6, z= 1:10) %>% 
  map(mean)


## --------------------------------------------------------------
list(x = 1:3, y=1:6, z= 1:10) %>%
   map(mean)
list(x = 1:3, y=1:6, z= 1:10) %>%
  map(~mean(.))


## --------------------------------------------------------------
## dat <-list.files(pattern = "*.csv")%>%
##           map_df(read_csv)


## ------------------------------------------------------------------------
gap %>% nest(-country) %>% 
       mutate(AVG = map(data, 
                ~ mean(.$lifeExp))) %>% 
            unnest(AVG)


## ------------------------------------------------------------------------
gap %>% filter(country == "Japan") %>% 
  lm(data=., lifeExp ~ gdpPercap) %>% 
     summary()


## ------------------------------------------------------------------------
gap %>% filter(country == "Japan") %>% 
  lm(data=.,lifeExp ~ gdpPercap) %>%
    summary() %>% tidy()


## ------------------------------------------------------------------------
gap %>% nest(-country) %>% 
 mutate(fit = map(data, ~ 
  lm(data=., lifeExp ~ gdpPercap)),
  res = map(fit, tidy)) %>% unnest(res)


## ------------------------------------------------------------------------
gap %>% nest(-country) %>% 
  mutate(fit = map(data, 
    ~ lm(data=., lifeExp ~ gdpPercap)),
      res = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)) 


## --------------------------------------------------------------
gap_lm <- gap %>% nest(-country) %>% 
   mutate(fit = map(data, 
          ~ lm(data=.x, lifeExp ~ gdpPercap)),
          res = map(fit, tidy),
          glanced = map(fit, glance),
          augmented = map(fit, augment)) 


## ------------------------------------------------------------------------
gap_lm %>% unnest(glanced,.drop = TRUE)


## ------------------------------------------------------------------------
gap_lm %>% unnest(augmented,.drop = TRUE)


## --------------------------------------------------------------
## # fit方法はそれぞれ
## stats::glm(Y~X,
##     "binomial")
## kernlab::ksvm(Y~X)
## # predictは共通
## predict(testX)


## ------- Python -------------------------------------------------------
## # ロジスティック
## clf1 = Lasso()
## # サポートベクタ
## clf2 = svm.SVC()
## # fitで統一
## clf1.fit(X, y)
## # 予測
## clf1.predict(testX)


## ------------ Python --------------------------------------------------
## pipeline = Pipeline(
##   [('vec',CountVectorizer()),
##     ('clf', SGDClassifier()),])
## parameters = {
##     'vec__ngram_range': ((1, 1), (1, 2)),
##     'clf__alpha': (0.00001, 0.000001),}
## grid_search = GridSearchCV(pipeline,
##       parameters, n_jobs=-1, verbose=1)
## grid_search.fit(X, y)


## --------------------------------------------------------------
## library(rtweet)
## iPhone <- tw_lists %>% select(status_id) %>%
##             pull() %>% lookup_tweets()




## ----, ----------------------------------------------
## ツィートデータのstatus_idごとにstext列を形態素解析にかけ、その結果をデータフレームに変換しますが、
## 処理がやや複雑なため、ここでは解析結果ファイル iPhone_df.Rdataをロードします。
## 詳細は、近著『Rによるテキストマイニング入門 応用編』森北出版をご参照ください。




## --------------------------------------------------------------
load("iPhone.Rdata")


## ------------------------------------------------------------------------
iPhone %>% dim()


## ------------------------------------------------------------------------
# 評価内訳
table(iPhone$Y)


## --------------------------------------------------------------
## 以下処理に数分かかることがあります
## Y ~ .（説明変数全体）
iPhone_glm <- glm(
 Y ~ .,
 data= iPhone,
 family = "binomial")



## ------------------------------------------------------------------------
library(broom)
iPhone_glm %>% tidy()


## ------------------------------------------------------------------------
iPhone_glm %>% tidy() %>% 
  filter(p.value < .01) %>% 
    arrange(p.value)


## ------------------------------------------------------------------------
preds <- round(
 predict(iPhone_glm,
type="response"))


## ------------------------------------------------------------------------
table(preds, 
      iPhone$Y) 



## --------------------------------------------------------------
## index <- sample(
##    N, N * 0.7)
## train <-dat[index,]
## test <- dat[-index,]


## --------------------------------------------------------------
## index <- caret::
## createDataPartition
##  (y = dat$Y,
##    p = 0.7)
## train <-dat[ index,]
## test  <-dat[-index,]


## --------------------------------------------------------------
library(caret)
set.seed(123) # 再現性のため乱数の種を設定
index <- createDataPartition(y = iPhone$Y, p = 0.7, list = FALSE)
training <- iPhone[ index,  ]
testing  <- iPhone[-index, ] 


## ------------------------------------------------------------------------
# 訓練データから目的変数を抽出行列化
train_X <- training %>% select(-Y) %>% 
  as.matrix(dimnames = #列名（単語）を残す
              list(NULL, colnames(.))) 
# 目的変数をベクトルに
train_Y <- training$Y
# テストデータにも適用
test_X <- testing %>% select(-Y) %>% 
  as.matrix(dimnames = list(NULL, colnames(.))) #列名（単語）を残す
test_Y <- testing$Y


## --------------------------------------------------------------
## 以下、処理に数分かかることがあります
## install.packages("doParallel")
library(doParallel)
registerDoParallel(parallel::detectCores()-1)

lasso <- glmnet(
  x = train_X,
  y = train_Y,
  family="binomial",
   parallel = TRUE)# if set registerDoParallel()



## ------------------------------------------------------------------------
lasso$lambda.min; lasso$lambda.1se


## ------------------------------------------------------------------------
las_pr <-predict(lasso,newx = test_X, 
                       type = "class")
confusionMatrix(table(las_pr, test_Y))


## ----, ----------------------------------------------
## 
## ## ROC{#ROC2}
## 
## |        | 実測値     | T  |   F |
## |--------+------------+------------+------------|
## | 予測値 |          T |   0 |  0 |
## |        |          F  |  7 |  3 |
## 

## ------------------------------------------------------------------------
## install.packages("pROC")
library(pROC)
lasso_res <- 
  predict(
   lasso,
   s="lambda.min", 
  newx =test_X, 
  type ="response")
lasso_roc <- 
  roc(test_Y, 
   lasso_res)


## ------------------------------------------------------------------------
ggroc(lasso_roc, 
 legacy.axes=TRUE)


## --------------------------------------------------------------
## glmnetでは、関数が交差法をサポートしている
## 以下、処理に時間がかかります
## lasso_cv_auc <- cv.glmnet(x = X, y = y,
##   type.measure = "auc",#aucを指定して、AUCを基準として最適な予測モデルを判定
##   alpha = 1, family = "binomial")


## --------------------------------------------------------------
## install.packages("kernlab")
##　以下の処理は相当に時間がかかります
library(kernlab)
svm_kernlab  <- ksvm(Y ~ .,
      data = training, scaled = FALSE,
      kernel="rbfdot", type="C-svc",
      cross = 10)


## --------------------------------------------------------------
## # fit方法はそれぞれ
## glmnet::glm(Y ~ X,
##     "binomial")
## 
## kernlab::ksvm(
##   Y ~ X, C=1)


## ------------Python--------------------------------------------------
## clf1 = Lasso()
## clf2 = svm.SVC()
## # メソッドは統一
## clf1.fit(X, y)
## clf2.fit(X, y)


## --------------------------------------------------------------
## caret::train(
##     x = 説明変数
##   , y = 目的変数
##   , method = 分析手法
##   , tuneGrid = モデルパラメータの範囲
##   , trControl = 交差法の指定
##   , preProcess = データの加工指定
##    )


## --------------------------------------------------------------
## caret::train(
##   x = 説明変数,
##   y = 目的変数,
##   method = 分析手法 ,
##   tuneGrid = モデルパラメータの範囲 ,
##   trControl = 交差法 , #ここのカンマでエラーに
##   # preProcess = データの加工指定 ,
##    )


## --------------------------------------------------------------
library(caret)
train_cntrl <-trainControl(
     method = "cv", number = 10,
     classProbs = TRUE,
     summaryFunction = twoClassSummary)
train_grid <- expand.grid(
        alpha  = seq(0, 1.0, by = 0.01) ,
        lambda = 10^(0:10 * -1))

## preProcess = c("center", "scale")
## preProcess = c("pca")#この場合はcenterとscaleも適用される


## --------------------------------------------------------------
caret_glm <- caret::train(Y ~ ., data =training
    , method = "glmnet"
    , tuneGrid = train_grid
    , trControl = train_cntrl
    , metric = "ROC"
    #, preProcess = c("center","scale")
         )


## --------------------------------------------------------------
caret_svm <- caret::train(Y ~ .
    , data = training
    , method = "svmRadial",
    , tuneGrid = expand.grid(C=1:10,
    ,   sigma = seq(0.0, 0.9, by = 0.1)),
    , trControl = trainControl(
    ,   method = "cv", number = 10)
   # ,preProcess = c("center","scale")
       )


## --------------------------------------------------------------
## my_train <- function(method,formula,data){
##    caret::train(formula, data, method
##      , trControl = caret::trainControl(
##           , method = 'cv')) }
## models <- tibble(methods =
##         c('lm', 'glmnet', "kernlab"))
## models %>% mutate(fits = map(methods,
##             train_mds, Y~X, my_train))


## ----message=TRUE--------------------------------------------------------
library(tidymodels)


## ------------------------------------------------------------------------
splitted_data <- initial_split(
  iPhone, p = 0.5, strata = c('Y')) 
train_data <- training(splitted_data)
test_data <- testing(splitted_data)


## ------------------------------------------------------------------------
rec <- recipe(Y ~ ., data = train_data)


## --------------------------------------------------------------
## #今回は使っていない
## rec <- rec %>% step_scale(all_numeric()) %>%
##   step_corr(all_numeric(),
##               - all_outcomes(),
##             threshold = 0.5) %>%
##   step_dummy(all_nominal()) #baseRで不要


## ------------------------------------------------------------------------
rec_dat <- rec %>% prep(training = 
                          train_data)


## ------------------------------------------------------------------------
train_juiced <- rec_dat %>% juice() 
test_baked <- rec_dat %>% 
  bake(test_data)


## ------------------------------------------------------------------------
glmnet_model_tidy <-
  logistic_reg(mixture = 1, 
               penalty = 10^(0:5 * -1)) %>% 
                  set_engine("glmnet")
translate(glmnet_model_tidy)#モデルの確認


## ------------------------------------------------------------------------
lasso_tidy <- glmnet_model_tidy %>% 
      fit(Y ~ ., data = train_juiced)


## ------------------------------------------------------------------------
preds <- test_baked %>% 
 select(Y) %>% 
 bind_cols(fitted= 
     multi_predict(
    lasso_tidy, 
    test_baked))


## --------------------------------------------------------------
preds


## ------------------------------------------------------------------------
preds$.pred[[1]]


## ------------------------------------------------------------------------
preds_f <- preds %>% 
 mutate(.pred =map(
  .pred,bind_rows))%>% 
  unnest()


## --------------------------------------------------------------
preds_f


## ------------------------------------------------------------------------
preds_f %>% group_by(penalty) %>% 
                     metrics(Y, .pred)


## --------------------------------------------------------------
## rf_ <- rand_forest(mode = "classification",
##     trees = 20, min_n = 100,
##     mtry = 1000) %>%
##   set_engine("randomForest", seed = 123)
## rf_fit <- tf_ %>%
##       fit(Y ~ ., data = train_baked)


## --------------------------------------------------------------
## rf <- rand_forest(trees = 10,
##           mode = "classification") %>%
##              set_engine("ranger")
## lasso  <- logistic_reg(penalty=1) %>%
##  set_engine("glmnet",family="multinomial")
## models <- list(rf, lasso)
## fitted <- models %>% map(
##    ~ fit(.,Y ~ .,data = train))

