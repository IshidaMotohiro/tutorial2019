## ----setup, include=FALSE------------------------------------------------
library(tidyverse)
library(stringi)
library(gapminder)
library(broom)
library(glmnet)


## ----echo=FALSE----------------------------------------------------------
gapminder <- gapminder %>% 
  filter(country %in% c("Japan", "China", "Korea, Rep."))


## ------------------------------------------------------------------------
p <- gapminder %>% 
  ggplot(
  aes(lifeExp, 
      gdpPercap,
      col = country)) +
  geom_line()


## ----echo=FALSE----------------------------------------------------------
p


## ----eval=FALSE----------------------------------------------------------
## gapminder %>% filter(country == "China") %>%
##   select(lifeExp, gdpPercap) %>% cor()
## 
## gapminder %>% filter(country == "Korea, Rep.") %>%
##   select(lifeExp, gdpPercap) %>% cor()
## 
## gapminder %>% filter(country == "Japan") %>%
##   select(lifeExp, gdpPercap) %>% cor()


## ------------------------------------------------------------------------
gapminder %>% filter(country == "China") %>% 
  cor.test(data=., ~ lifeExp +gdpPercap)


## ----echo=FALSE----------------------------------------------------------
library(broom)


## ------------------------------------------------------------------------
gapminder %>%  nest (-country) %>% 
  mutate(test = map(data, 
    ~cor.test(data=., ~ lifeExp +gdpPercap)),
    res = map(test, tidy)) %>% unnest(res, .drop = TRUE)


## ------------------------------------------------------------------------
gapminder %>% nest (-country) 


## ------------------------------------------------------------------------
gapminder %>% nest (-country) %>% select(data) %>% 
  unnest() 


## ------------------------------------------------------------------------
list(x = 1:3, y=1:6, z= 1:10) %>% map(~mean(.))


## ----eval=FALSE----------------------------------------------------------
## dat <- list.files(pattern = "*.csv") %>%
##           map_df(read_csv)


## ------------------------------------------------------------------------
gapminder %>%  nest (-country) %>% 
  mutate(AVG = map(data, ~ mean(.$lifeExp))) 


## ------------------------------------------------------------------------
gapminder %>%  nest (-country) %>% 
  mutate(AVG = map(data, ~ mean(.$lifeExp))) %>% 
  unnest(AVG)


## ------------------------------------------------------------------------
gapminder %>% filter(country == "Japan") %>% 
  lm(data=., lifeExp ~ gdpPercap) %>% summary()


## ------------------------------------------------------------------------
gapminder %>% filter(country == "Japan") %>% 
  lm(data=., lifeExp ~ gdpPercap) %>% summary() %>% 
  tidy()


## ------------------------------------------------------------------------
  gapminder %>% nest (-country) %>% 
  mutate(fit = map(data, ~ 
            lm(data=., lifeExp ~ gdpPercap)),
         res = map(fit, tidy)) %>% unnest(res)


## ------------------------------------------------------------------------
gapminder %>% nest (-country) %>% 
   mutate(fit = map(data, 
            ~ lm(data=.x, lifeExp ~ gdpPercap)),
          res = map(fit, tidy),
          glanced = map(fit, glance),
          augmented = map(fit, augment)) 


## ----echo=FALSE----------------------------------------------------------
gap_lm <- gapminder %>% nest (-country) %>% 
   mutate(fit = map(data, ~ lm(data=.x, lifeExp ~ gdpPercap)),
          res = map(fit, tidy),
          glanced = map(fit, glance),
          augmented = map(fit, augment)) 


## ------------------------------------------------------------------------
gap_lm %>% unnest(glanced, .drop = TRUE)


## ------------------------------------------------------------------------
gap_lm %>% unnest(augmented, .drop = TRUE)


## ----eval=FALSE----------------------------------------------------------
## library(rtweet)
## iPhone <- tw_lists %>% select(status_id) %>%
##                          pull() %>% lookup_tweets()



## ----echo=FALSE, eval=FALSE----------------------------------------------
## ツィートデータのstatus_idごとにstext列を形態素解析にかけ、その結果をデータフレームに変換しますが、
## 処理がやや複雑なため、ここでは解析結果ファイル iPhone_df.Rdataをロードします。
## 詳細は、近著『Rによるテキストマイニング入門 応用編』森北出版をご参照ください。



## ------------------------------------------------------------------------
iPhone %>% dim()


## ------------------------------------------------------------------------
table(iPhone$Y)


## ----eval=FALSE----------------------------------------------------------
iPhone_glm <- glm(Y ~ . , data= iPhone,   family = binomial())



## ------------------------------------------------------------------------
library(broom)
iPhone_glm %>% tidy()


## ------------------------------------------------------------------------
library(broom)
iPhone_glm %>% tidy() %>% filter(p.value < .01) %>% 
  arrange(p.value)


## ------------------------------------------------------------------------
preds <- round(
  predict(iPhone_glm,
  type="response"))


## ------------------------------------------------------------------------
table(preds, iPhone$Y) 


## ----eval=FALSE, echo=FALSE----------------------------------------------
## 
## $min \left [ \frac{1}{2N} \sum_{i=1}^N (y_i - \beta_0 - x_i^T \beta) + \lambda P_\alpha (\beta) \right]$
## $P_\alpha (\beta) = \sum_{j=1}^p \left[ \frac{1}{2} (1 - \alpha) \beta_j^2 + \alpha|\beta_j| \right]$
## 


## ----eval=FALSE----------------------------------------------------------
## index <- sample(N,
##              N * 0.5)
## index %>% NROW()
## train <- dat[index, ]
## test <-  dat[-index, ]


## ----eval=FALSE----------------------------------------------------------
## index <- createDataPartition
##   (y = dat$Y, p = 0.7)
## train <- dat[ index,  ]
## test  <- dat[-index, ]


## ----echo=FALSE----------------------------------------------------------
library(caret)
set.seed(123) # 再現性のため乱数の種を設定
index <- createDataPartition(y = iPhone$Y, p = 0.7, list = FALSE)
training <- iPhone[ index,  ]
testing  <- iPhone[-index, ] 


## ------------------------------------------------------------------------
# 訓練データを整形して行列にする
train_X <- training %>% select(-Y) %>% 
  as.matrix(dimnames = list(NULL, colnames(.))) #列名（単語）を残す
# 目的変数のベクトル
train_Y <- training$Y
# テストデータを整形して行列にする
test_X <- testing %>% select(-Y) %>% 
  as.matrix(dimnames = list(NULL, colnames(.))) #列名（単語）を残す
# 目的変数のベクトル
test_Y <- testing$Y




## ----echo=FALSE----------------------------------------------------------
plot(lasso)


## ------------------------------------------------------------------------
lasso$lambda.min; lasso$lambda.1se


## ------------------------------------------------------------------------
lasso_preds <- predict(lasso, newx = test_X, 
                       type = "class")
confusionMatrix(table(lasso_preds, test_Y))


## ------------------------------------------------------------------------
library(pROC)
lasso_response <- predict(
  lasso, s = "lambda.min", 
  newx = test_X, 
  type = "response")
lasso_roc <- roc(test_Y, 
as.numeric(lasso_response))


## ------------------------------------------------------------------------
ggroc(lasso_roc, legacy.axes = TRUE)


## ----eval=FALSE----------------------------------------------------------
## lasso_cv_auc <- cv.glmnet(x = X, y = y,
##   type.measure = "auc",#aucを指定
##   alpha = 1, family = "binomial")


## ----eval=FALSE----------------------------------------------------------
## library(kernlab)
## svm_kernlab  <- ksvm(Y ~ .,
##       data = training, scaled = FALSE,
##       kernel="rbfdot", type="C-svc",
##       cross = 10)


## ----eval=FALSE----------------------------------------------------------
## caret::train(
##         x = 説明変数
##         , y = 目的変数
##         , method = 分析手法
##         , tuneGrid = モデルパラメータの範囲
##         , trControl = 交差法の指定
##         , preProcess = データの加工指定
##         )


## ----eval=FALSE----------------------------------------------------------
## library(caret)
## train_cntrl <-trainControl(method = "cv",
##         number = 10)
## train_grid <- expand.grid(
##         alpha  = seq(0, 1.0, by = 0.01) ,
##         lambda = 10^(0:10 * -1))


## ------------------------------------------------------------------------
preProcess = c("center", "scale")
preProcess = c("pca")#この場合はcenterとscaleも適用される


## ----eval=FALSE----------------------------------------------------------
## caret::train(Y ~ ., data = training
##          , method = "glmnet"
##          , tuneGrid = train_grid
##          , trControl = train_cntrl
##          # , preProcess = c("center", "scale")
##          )


## ----eval=FALSE----------------------------------------------------------
## caret::train(Y ~ ., data = training,
##        , method = "svmRadial",
##        , tuneGrid = expand.grid(C = 1:10,
##        ,   sigma = seq(0.0, 0.9, by = 0.1),
##        , trControl = trainControl(
##        ,       method = "cv", number = 10),
##        # ,preProcess = c("center", "scale")
##        )


## ----message=TRUE--------------------------------------------------------
library(tidymodels)


## ------------------------------------------------------------------------
splitted_data <- initial_split(iPhone, 
                  p = 0.5, strata = c('Y')) 
train_data <- training(splitted_data)
test_data <- testing(splitted_data)


## ------------------------------------------------------------------------
rec <- recipe(Y ~ ., data = train_data)


## ------------------------------------------------------------------------
rec_dat <- rec %>% prep(training = train_data)


## ------------------------------------------------------------------------
train_baked <- rec_dat %>% juice() 
test_baked <- rec_dat %>% bake(test_data)


## ----eval=FALSE----------------------------------------------------------
glmnet_model_tidy <- logistic_reg(mixture = 0,
                       penalty = 10^(0:5 * -1)) %>%
                         set_engine("glmnet")



## ----eval=FALSE----------------------------------------------------------
lasso_tidy <- glmnet_model_tidy %>%
  fit(Y ~ ., data = train_baked)


## ------------------------------------------------------------------------
preds  <- test_baked %>% 
  select(Y) %>% 
  bind_cols(fitted = 
  multi_predict(lasso_tidy, 
                test_baked))


## ------------------------------------------------------------------------
preds


## ------------------------------------------------------------------------
preds$.pred[[1]]


## ------------------------------------------------------------------------
preds_flat <- preds %>% 
  mutate(.pred = 
  map(.pred, bind_rows)) %>% 
  unnest()


## ------------------------------------------------------------------------
preds_flat


## ------------------------------------------------------------------------
preds_flat %>% group_by(penalty) %>% 
  metrics(Y, .pred)


## ----eval=FALSE----------------------------------------------------------
rf_tidy <- rand_forest(mode = "classification",
              trees = 20, min_n = 100,
              mtry = 1000) %>%
                set_engine("randomForest", seed = 123)
rf_tidy_fit <- tf_tidy %>% fit(Y ~ .,
                               data = train_baked)

