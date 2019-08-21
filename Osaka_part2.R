## ----setup, include=FALSE------------------------------------------------
library(tidyverse)
library(stringi)
library(gapminder)
library(broom)
library(glmnet)


## --------------------------------------------------------------
gapminder <- gapminder %>% 
  filter(country %in% c("Japan", "China", "Korea, Rep."))


## ------------------------------------------------------------------------
p <- gapminder %>% 
  ggplot(
  aes(lifeExp, 
      gdpPercap,
      col = country)) +
  geom_line()


## --------------------------------------------------------------
p



## ------------------------------------------------------------------------
gapminder %>% filter(country == "China") %>% 
  cor.test(data=., ~ lifeExp +gdpPercap)


## ----, ----------------------------------------------
## # https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
## # https://suryu.me/post/r_advent_calendar_day3/


## --------------------------------------------------------------
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
list(x = 1:3, y=1:6, z= 1:10) %>% map(mean)


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
  mutate(fit = map(data, ~ lm(data=.x, lifeExp ~ gdpPercap)),
         res = map(fit, tidy)) %>% unnest(res)


## ------------------------------------------------------------------------
gapminder %>% nest (-country) %>% 
   mutate(fit = map(data, ~ lm(data=.x, lifeExp ~ gdpPercap)),
          res = map(fit, tidy),
          glanced = map(fit, glance),
          augmented = map(fit, augment)) 


## --------------------------------------------------------------
gap_lm <- gapminder %>% nest (-country) %>% 
   mutate(fit = map(data, ~ lm(data=.x, lifeExp ~ gdpPercap)),
          res = map(fit, tidy),
          glanced = map(fit, glance),
          augmented = map(fit, augment)) 



## ------------------------------------------------------------------------
gap_lm %>% unnest(glanced, .drop = TRUE)


## ------------------------------------------------------------------------
gap_lm %>% unnest(augmented, .drop = TRUE)


## 機械学習

## --------- 加工済みデータ -----------------------------------------------------
download.file("https://github.com/IshidaMotohiro/tutorial2019/blob/master/iPhone.Rdata?raw=true", destfile= "iPhone.Rdata")
load("iPhone.Rdata")




## --------------------------------------------------------------
iPhone_glm <- glm(Y ~ ., data= iPhone,
                  family = binomial())
summary(iPhone_glm)


## ------------------------------------------------------------------------
library(broom)
iPhone_glm %>% tidy()


## ------------------------------------------------------------------------
library(broom)
iPhone_glm %>% tidy() %>% filter(p.value < .05)


## ------------------------------------------------------------------------
pred <- round(
  predict(iPhone_glm,
  type="response"))


## ------------------------------------------------------------------------
table(pred, iPhone$Y) 


## ----- lasso 回帰 ----------------------------------------------

## ----- データを説明変数（行列）と目的変数に分離-------------------------------------------------------------------
iPhone_X <- iPhone %>% 
  select(-Y) %>% 
   as.matrix(dimnames = list(NULL, 
     colnames(iPhone)))#列名（単語）を残す




## --------------------------------------------------------------
library(glmnet)
lasso_ <- glmnet(
  x = iPhone_X,
  y = iPhone$Y,
  family= "binomial")


## --------------------------------------------------------------
plot(lasso_, xvar = "lambda")


## ------------------------------------------------------------------------
library(caret)
# set.seed(123)
index <- createDataPartition(
  y = iPhone$Y,
  p = 0.7,
  list = FALSE
)
training <- iPhone[ index,  ]
testing  <- iPhone[-index, ]


## ------------------------------------------------------------------------
## 訓練データを説明行列Xと目的変数yに
X <- training %>% select(-Y) %>% 
       as.matrix(dimnames = list(NULL, 
                 colnames(training)))#列名（単語）を残す
y <- training$Y
## テストデータも　y2 と X2 に分ける
X2 <- testing %>% select(-Y) %>% 
   as.matrix(dimnames = list(NULL, 
     colnames(training)))#列名（単語）を残す
y2 <- testing$Y


## ----- 並列化 ---------------------------------------------------------
library(doParallel)# 並列化
cl <- makeCluster(4) 
registerDoParallel(cl)

## ------（時間がかかる処理）--------------------------------------------------------
lasso_cv <-
  cv.glmnet(
   x = X,
   y = y,
   alpha = 1,
   family = "binomial",
   parallel = TRUE)


## --------------------------------------------------------------
plot(lasso_cv)


## ------------------------------------------------------------------------
lasso_cv$lambda.min; lasso_cv$lambda.1se


## ------------------------------------------------------------------------
preds <- predict(lasso_cv$glmnet.fit, newx = X2,
                 s = lasso_cv$lambda.min, 
                 type = "response")
# predは確率なので、丸めて１か０にする
table(round(preds), testing$Y)


## ------------------------------------------------------------------------
library(pROC)
lasso_auc_pred <- predict(lasso_cv, s = "lambda.min", 
                          newx = X2, type = "response")
lasso_roc <- roc(y2, as.numeric(lasso_auc_pred))


## ------------------------------------------------------------------------
ggroc(lasso_roc, legacy.axes = TRUE)


## ------------------------------------------------------------------------
auc(lasso_roc)


## --------------------------------------------------------------
## lasso_cv_auc <- cv.glmnet(
##   x = X,
##   y = y,
##   type.measure = "auc",#aucを指定
##   alpha = 1, family = "binomial",
##   parallel = TRUE)


## --------------------------------------------------------------
library(caret)
train_cntrl <-trainControl(method = "cv", number = 10)
train_grid <- expand.grid(alpha  = seq(0, 1.0, by = 0.01) ,
                          lambda = 10^(0:10 * -1))


## ------------------------------------------------------------------------
preProcess = c("center", "scale")
preProcess = c("pca")#この場合はcenterとscaleも適用される


## --------------------------------------------------------------
elastic  <- train(x = X,
                  y = as.factor(y),
                  method = "glmnet",
                  tuneGrid = train_grid,
                  trControl = train_cntrl,
                  preProcess = c("center", "scale"))


## ----, ----------------------------------------------
library(tidymodels)
splitted_data <- initial_split(iPhone, p = 0.5, strata = c('Y'))
training_data <- training(splitted_data)
training_data <- training_data %>% mutate(Y = as.factor(Y))
test_data <- testing(splitted_data)
test_data <- test_data %>% mutate(Y = as.factor(Y))
rec <- recipe(Po ~ ., data = training_data)
rec_dat <- rec %>% prep(training = training_data)
train_baked <- rec_dat %>% juice()
test_baked <- rec_dat %>% bake(test_data)
logistic_reg(mixture = 0, penalty  = 10^(0:10 * -1)) %>%
                       set_engine("glmnet")

