

 files <- list.files("excel", pattern = "xlsx", full.names = TRUE)
 files
 
 library(tidyverse)
 #ファイル名から学生番号と名前を抽出
 files %>% walk(
   {
     file <- str_remove(., " ")
     学生番号 <- stringi::stri_trans_nfkc_casefold(file) %>%
       str_extract("\\d{8,10}\\p{Han}*.xlsx$")
     print(学生番号)
   }
 )
 
 ## 上の処理は以下と同じ
 for (i in seq_along(files)){
   files[i] <- str_remove(files[i], " ")
   学生番号 <- stringi::stri_trans_nfkc_casefold(files[i]) %>%
     str_extract("\\d{8,10}\\p{Han}*.xlsx$")
   print(学生番号)
 }
 
 
 #ファイル名から学生番号と名前を抽出
 files %>% walk(
   {
     file_name <- stringi::stri_trans_nfkc_casefold(.)
     file_name <- str_remove(file_name, " ")
     file_name <- str_match(file_name, "(\\d{8,10})(\\p{Han}*).xlsx$")
     print(file_name)
   }
 )
 
 
#  library(rJava)
 library(XLConnect)# needs Java
 
  seiseki <- tibble(
  学生番号 = character(),   # {学生番号} , 
  名前 = character(), # {名前}, 
   A11 = character(),  # {合計の式}
   B11  = character(), # {平均値の式},
   C11 = character())  #{標準偏差の式})
 
 seiseki
 
 for (i in seq_along(files)){
   file_name <- stringi::stri_trans_nfkc_casefold(files[i])
   file_name <- str_remove(file_name, " ")
   file_name <- str_match(file_name, "(\\d{8,10})(\\p{Han}*).xlsx$")
  学生番号_ <- file_name[2]
  名前_ <- file_name[3]
  wb <- loadWorkbook( files[i], create = FALSE )
  A11_ <- try(getCellFormula(wb, 
                       "Sheet1",
                       # 行番号と列番号
                       11, 1), silent = TRUE)
  A11_ <- ifelse(class(A11_) == "try-error", "", A11_)
  B11_ <- try(getCellFormula(wb, 
                        "Sheet1",
                        # 行番号と列番号
                        11, 2), silent = TRUE)
  B11_ <- ifelse(class(B11_) == "try-error", "", B11_)
  
  C11_ <- try(getCellFormula(wb, 
                        "Sheet1",
                        # 行番号と列番号
                        11, 3), silent = TRUE)
  C11_ <- ifelse(class(C11_) == "try-error", "", C11_)
  seiseki <- seiseki %>% add_row(学生番号 = 学生番号_ ,名前 = 名前_ , A11 = A11_, B11 = B11_, C11 = C11_)
 }
 
 seiseki
 seiseki_ <- seiseki %>% mutate(
   A11_ = if_else(A11 == "SUM(A2:A10)", 1, 0),
   B11_ = if_else(B11 == "AVERAGE(B2:B10)", 1, 0),
   C11_ = if_else(A11 == "STDEV(C2:C10)", 1, 0),
   合計 = A11_ * 30 + B11_ * 30 + C11_ * 40
 ) %>% select(学生番号,名前, 合計)
 
 seiseki_ 
 ##成績原簿に転機
 library(readxl)
 genbo <- read_xlsx(path = "seiseki.xlsx")
 genbo
 
 genbo <- genbo %>% left_join(seiseki_ %>% mutate(学生番号 = as.numeric(学生番号))) 
 
 genbo %>% select(-評価) %>% rename(評価=合計)
 
 #################################
 ##あるいは個々のExcelファイルから必要なデータを抽出する関数を用意して処理する
 library(XLConnect)
 from_excel <- function(xlsx){
   file_name <- stringi::stri_trans_nfkc_casefold(xlsx)
   file_name <- str_remove(file_name, " ")
   file_name <- str_match(file_name, "(\\d{8,10})(\\p{Han}*).xlsx$")
   print(file_name)
   学生番号_ <- file_name[2]
   名前_ <- file_name[3]
   wb <- loadWorkbook(xlsx, create = FALSE )
   A11_ <- try(getCellFormula(wb, 
                              "Sheet1",
                              # 行番号と列番号
                              11, 1), silent = TRUE)
   A11_ <- ifelse(class(A11_) == "try-error", "", A11_)
   B11_ <- try(getCellFormula(wb, 
                              "Sheet1",
                              # 行番号と列番号
                              11, 2), silent = TRUE)
   B11_ <- ifelse(class(B11_) == "try-error", "", B11_) 
   
   C11_ <- try(getCellFormula(wb, 
                              "Sheet1",
                              # 行番号と列番号
                              11, 3), silent = TRUE)
   C11_ <- ifelse(class(C11_) == "try-error", "", C11_ )
   tibble(学生番号 = 学生番号_ ,名前 = 名前_ , A11 = A11_, B11 = B11_, C11 = C11_ )
 }

 
seiseki <- files %>% map_df(from_excel)# 
seiseki
 seiseki_ <- seiseki %>%  mutate(
   A11_ = if_else(A11 == "SUM(A2:A10)", 1, 0),
   B11_ = if_else(B11 == "AVERAGE(B2:B10)", 1, 0),
   C11_ = if_else(C11 == "STDEV(C2:C10)", 1, 0),
   合計 = A11_ * 30 + B11_ * 30 + C11_ * 40
 ) %>% select(学生番号, 名前, 合計)
 
 seiseki_ 
 ##成績原簿に転機
 library(readxl)
 genbo <- read_xlsx(path = "seiseki.xlsx")
 genbo
 
 genbo <- genbo %>% left_join(seiseki_ %>% mutate(学生番号 = as.numeric(学生番号))) 
 
 genbo %>% select(-評価) %>% rename(評価=合計)
 