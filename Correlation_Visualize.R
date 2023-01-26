# 대학 이름 벡터 생성 (포맷팅 용)
univ.names <- c("중앙숭실", "아주대", "인하대", "서울대", "건국대", "경희외대", "국민고려", "연세이화")

# 월세 데이터 받아오기
for (j in univ.names){
  assign(paste0("dat.",j), read.csv(file=sprintf("./data/%s/%s_csv.csv",j, j), header=T))

  # dat 계약년월 데이터 확인
  print(eval(parse(text=paste0("dat.",j)))$계약년월)
  
  # order을 이용한 정렬
  assign(paste0("dat_order.",j), eval(parse(text=paste0("dat.",j)))[order(eval(parse(text=paste0("dat.",j)))$계약년월), ])
  print(eval(parse(text=paste0("dat_order.",j))))
  # N/A 제거
  
  # 필요한 데이터만 뽑아오기
  assign(paste0("y.",j), eval(parse(text=paste0("dat_order.",j))))
  print(head(eval(parse(text=paste0("y.",j)))$계약년월))  # 확인
  tail(eval(parse(text=paste0("y.",j)))$계약년월)  # 확인
  
  assign(paste0("y.",j), eval(parse(text=paste0("y.",j)))[c(1:6)])
  print(eval(parse(text=paste0("y.",j))))
  
  # install.packages("stringr")
  library(stringr)

  # 데이터 내 공백 삭제
  assign(paste0("A.",j), c())
  for (i in eval(parse(text=paste0("y.",j)))[5]){
    print(i)
    num_i <- str_trim(i)
    assign(paste0("A.",j), c(eval(parse(text=paste0("A.",j))), num_i))
  }
  
  # 쉼표 삭제
  assign(paste0("A.",j), gsub(",", "", eval(parse(text=paste0("A.",j)))))
  
  # 월세전환율; 전환월세 = 기존월세 + 보증금*exchange_rate/12
  exchange_rate <- 0.0375 
  
  
  eval.y <- eval(parse(text=paste0("y.",j)))
  
  # 보증금을 월세로 전환
  eval.y[,"Exchanged_Monthly"] <- eval(parse(text=paste0("y.",j)))[6] + as.numeric(eval(parse(text=paste0("A.",j))))*exchange_rate/12 # (보증금)전월세 정규화
  
  # 1제곱미터 당 월세 가격
  eval.y[,"per_area"] <- eval.y[7]/eval.y[2]
  
  # 1열 : 거래날짜, 2열 : 1제곱미터당 월세가격 인 데이터프레임
  assign(paste0("target.",j), eval.y[c(4, 8)])
  
  eval.target <- eval(parse(text=paste0("target.", j)))
  library(dplyr)
  eval.target <- eval.y[c(4, 8)] # 년월일, m^2 당 월세가격
  
  # N/A 제거
  eval.target <- na.omit(eval.target)
  
  # Infimum 제거
  eval.target <- eval.target %>% filter(!is.infinite(per_area))

  # 매달 1제곱미터 당 월세가격 평균
  assign(paste0("monthly.mean.",j), c())
  for (k in 16:21) {
    for (i in 1:12) {
      if (i < 10) {
        result = mean(as.matrix(subset(eval.target, 계약년월 == sprintf("20%d0%d", k, i))[2]))
      } else {
        result = mean(as.matrix(subset(eval.target, 계약년월 == sprintf("20%d%d", k, i))[2]))
      }
      assign(paste0("monthly.mean.",j), c(eval(parse(text=paste0("monthly.mean.",j))), result)) # 벡터에 추가
    }
  }
}
print(monthly.mean.건국대)

library(ggplot2)

# datalab 처리

# 데이터랩 받아오기

for (j in univ.names){
  datalab.1 <- read.csv(file=sprintf("./data/%s/%s_2016_데이터랩.csv", j, j), header=T)
  datalab.2 <- read.csv(file=sprintf("./data/%s/%s_2017_데이터랩.csv", j, j), header=T)
  datalab.3 <- read.csv(file=sprintf("./data/%s/%s_2018_데이터랩.csv", j, j), header=T)
  datalab.4 <- read.csv(file=sprintf("./data/%s/%s_2019_데이터랩.csv", j, j), header=T)
  datalab.5 <- read.csv(file=sprintf("./data/%s/%s_2020_데이터랩.csv", j, j), header=T)
  datalab.6 <- read.csv(file=sprintf("./data/%s/%s_2021_데이터랩.csv", j, j), header=T)
  datalab <- rbind(datalab.1, datalab.2)
  datalab <- rbind(datalab, datalab.3)
  datalab <- rbind(datalab, datalab.4)
  datalab <- rbind(datalab, datalab.5)
  datalab <- rbind(datalab, datalab.6)
  
  # datalab 매트릭스 변환
  assign(paste0("matrix.datalab.",j), as.matrix(datalab))  
  
  # 빈 벡터 생성
  B <- c()  
  
  # 데이터 내 공백 삭제
  eval.matrix <- eval(parse(text=paste0("matrix.datalab.",j)))
  for (i in eval.matrix[,2]) {
    str_i <- str_trim(i)
    B=c(B, str_i)
  }
  
  # 숫자형으로 변환
  num.B <- as.numeric(B)

  #빈 벡터 생성
  year.month.str <- c()
  # 자료 정규화 (날짜 월별로 변환)
  for (i in eval.matrix[, 1]) {
    year.month = substr(i, 1, 7)
    year.month.str <- c(year.month.str, year.month)
  }
  
  # 자료 변환
  eval.matrix[, 1] <- year.month.str
  
  # 데이터프레임으로 변환
  target.datalab <- as.data.frame(eval.matrix)
  
  # 공백 삭제
  D <- c()
  for (i in target.datalab[,2]) {
    str_i <- str_trim(i)
    D=c(D, str_i)
  }
  
  # 숫자로 변환
  target.datalab[2] <- as.numeric(D)

  # 빈 벡터 생성
  monthly.datalab <- c()
  
  # 월 별 평균 데이터 검색량 계산
  for (k in 16:21) {
    for (i in 1:12) {
      if (i < 10) {
        result = mean(as.matrix(subset(target.datalab, 날짜 == sprintf("20%d-0%d", k, i))[2]))
      } else {
        result = mean(as.matrix(subset(target.datalab, 날짜 == sprintf("20%d-%d", k, i))[2]))
      }
      monthly.datalab <- c(monthly.datalab, result) # 벡터에 추가
      assign(paste0("monthly.datalab.", j), monthly.datalab) # 벡터에 추가
    }
  }
}
print(monthly.datalab.건국대)


# 전체 데이터 회귀분석
par(mfrow = c(2, 4))
for (j in univ.names) {
  plot(eval(parse(text=paste0("monthly.datalab.", j)))[1:72], eval(parse(text=paste0("monthly.mean.", j)))[1:72], xlab = paste("monthly datalab", j), ylab = paste("monthly mean", j)) # 산점도 생성
  lm_fit <- lm(eval(parse(text=paste0("monthly.mean.", j)))[1:72] ~ eval(parse(text=paste0("monthly.datalab.", j)))[1:72]) # 선형 모델 생성
  abline(lm_fit)  # 선형 모델 생성
}

# 년 구분 회귀분석
par (mfrow = c(2, 3))
for (j in univ.names[8]) {
  plot(eval(parse(text=paste0("monthly.datalab.", j)))[1:12], eval(parse(text=paste0("monthly.mean.", j)))[1:12], xlab = paste("2016 monthly datalab", j), ylab = paste("2016 monthly mean", j)) # 산점도 생성
  lm_fit <- lm(eval(parse(text=paste0("monthly.mean.", j)))[1:12] ~ eval(parse(text=paste0("monthly.datalab.", j)))[1:12]) # 선형 모델 생성
  abline(lm_fit)  # 선형 모델 생성
  
  plot(eval(parse(text=paste0("monthly.datalab.", j)))[13:24], eval(parse(text=paste0("monthly.mean.", j)))[13:24], xlab = paste("2017 monthly datalab", j), ylab = paste("2017 monthly mean", j)) # 산점도 생성
  lm_fit <- lm(eval(parse(text=paste0("monthly.mean.", j)))[13:24] ~ eval(parse(text=paste0("monthly.datalab.", j)))[13:24]) # 선형 모델 생성
  abline(lm_fit)  # 선형 모델 생성
  
  plot(eval(parse(text=paste0("monthly.datalab.", j)))[25:36], eval(parse(text=paste0("monthly.mean.", j)))[25:36], xlab = paste("2018 monthly datalab", j), ylab = paste("2018 monthly mean", j)) # 산점도 생성
  lm_fit <- lm(eval(parse(text=paste0("monthly.mean.", j)))[25:36] ~ eval(parse(text=paste0("monthly.datalab.", j)))[25:36]) # 선형 모델 생성
  abline(lm_fit)  # 선형 모델 생성
  
  plot(eval(parse(text=paste0("monthly.datalab.", j)))[37:48], eval(parse(text=paste0("monthly.mean.", j)))[37:48], xlab = paste("2019 monthly datalab", j), ylab = paste("2019 monthly mean", j)) # 산점도 생성
  lm_fit <- lm(eval(parse(text=paste0("monthly.mean.", j)))[37:48] ~ eval(parse(text=paste0("monthly.datalab.", j)))[37:48]) # 선형 모델 생성
  abline(lm_fit)  # 선형 모델 생성
  
  plot(eval(parse(text=paste0("monthly.datalab.", j)))[49:60], eval(parse(text=paste0("monthly.mean.", j)))[49:60], xlab = paste("2020 monthly datalab", j), ylab = paste("2020 monthly mean", j)) # 산점도 생성
  lm_fit <- lm(eval(parse(text=paste0("monthly.mean.", j)))[49:60] ~ eval(parse(text=paste0("monthly.datalab.", j)))[49:60]) # 선형 모델 생성
  abline(lm_fit)  # 선형 모델 생성
  
  plot(eval(parse(text=paste0("monthly.datalab.", j)))[61:72], eval(parse(text=paste0("monthly.mean.", j)))[61:72], xlab = paste("2021 monthly datalab", j), ylab = paste("2021 monthly mean", j)) # 산점도 생성
  lm_fit <- lm(eval(parse(text=paste0("monthly.mean.", j)))[61:72] ~ eval(parse(text=paste0("monthly.datalab.", j)))[61:72]) # 선형 모델 생성
  abline(lm_fit)  # 선형 모델 생성
}

for (j in univ.names) {
  print(cor(eval(parse(text=paste0("monthly.datalab.", j)))[1:72], eval(parse(text=paste0("monthly.mean.", j)))[1:72])) # correlation 판단
}

# 데이터랩과 월세 간 상관계수 계산
for (j in univ.names){
  assign(paste0("cor.",j), c())
  for (i in 1:6) {
    assign(paste0("cor.",j), c(eval(parse(text=paste0("cor.",j))), cor(eval(parse(text=paste0("monthly.datalab.",j)))[((i-1)*12+1):(i*12)],
                                                                       eval(parse(text=paste0("monthly.mean.",j)))[((i-1)*12+1):(i*12)])))
  }
}

# correlation dataframe
cor.dataframe <- data.frame(cor.건국대, cor.경희외대, cor.국민고려, cor.서울대, cor.아주대, cor.연세이화, cor.인하대, cor.중앙숭실)
rownames(cor.dataframe) <- c(2016, 2017, 2018, 2019, 2020, 2021)
colnames(cor.dataframe) <- c("건국대", "경희외대", "국민고려", "서울대", "아주대", "연세이화", "인하대", "중앙숭실")
cor.dataframe
t(cor.dataframe)

##### 시계열 데이터 분석

for (j in univ.names){
  z <- eval(parse(text=paste0("target.",j)))
  assign(paste0("tsdata.",j), ts(eval(parse(text=paste0("monthly.mean.", j))), start = c(2016, 1), frequency = 12))
  assign(paste0("m.", j), decompose(eval(parse(text=paste0("tsdata.", j)))))
}



par(mfrow=c(1,3))
plot(stl(tsdata.건국대, "periodic"))

ts.plot(monthly.mean.건국대-m.건국대$seasonal)    # 계절성 제거 데이터
ts.plot(monthly.mean.건국대-m.건국대$trend)       # 추세 제거 데이터
ts.plot(monthly.mean.건국대-m.건국대$random)      # random = 예측 불가능한 요인, 돌발적인 요인 # 원 데이터 - 추세 - 순환 - 계절성

ts.plot(monthly.mean.경희외대-m.경희외대$seasonal)
ts.plot(monthly.mean.경희외대-m.경희외대$trend)
ts.plot(monthly.mean.경희외대-m.경희외대$random)

ts.plot(monthly.mean.서울대-m.서울대$seasonal)
ts.plot(monthly.mean.서울대-m.서울대$trend)
ts.plot(monthly.mean.서울대-m.서울대$random)

ts.plot(monthly.mean.중앙숭실-m.중앙숭실$seasonal)
ts.plot(monthly.mean.중앙숭실-m.중앙숭실$trend)
ts.plot(monthly.mean.중앙숭실-m.중앙숭실$random)

ts.plot(monthly.mean.인하대-m.인하대$seasonal)
ts.plot(monthly.mean.인하대-m.인하대$trend)
ts.plot(monthly.mean.인하대-m.인하대$random)

ts.plot(monthly.mean.아주대-m.아주대$seasonal)
ts.plot(monthly.mean.아주대-m.아주대$trend)
ts.plot(monthly.mean.아주대-m.아주대$random)

ts.plot(monthly.mean.국민고려-m.국민고려$seasonal)
ts.plot(monthly.mean.국민고려-m.국민고려$trend)
ts.plot(monthly.mean.국민고려-m.국민고려$random)

ts.plot(monthly.mean.연세이화-m.연세이화$seasonal)
ts.plot(monthly.mean.연세이화-m.연세이화$trend)
ts.plot(monthly.mean.연세이화-m.연세이화$random)