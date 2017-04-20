#####################################################################################
# 데이터 준비
# AHD 데이터는 흉부외과 환자 303명을 관찰한 데이터로, 
# AHD 칼럼에 각 환자들이 심장병이 있는지 여부가 기록되어 있습니다.

AHD <- read.csv("C:/Rstudy/21.세미나/AHD/AHD.csv")
str(AHD)

#####################################################################################
# cluser 분석
# AHD를 제외하고 Kmeans를 실행하여 해당 클러스터의 결과가 AHD와 어느정도 일치하는 지를 확인

# AHD를 입력 변수와 결과 변수로 분리
library(dplyr)

#1.AHD 값을 별도로 저장
AHD.data <- AHD %>% select (X:Thal)
AHD.result <- AHD %>% select (AHD)
table(AHD.result)

#2.NA 값 보정
sum(is.na(AHD.data))  # 전체 NA 갯수 확인
colSums(is.na(AHD.data))  # 컬러별 NA 갯수 확인
table(AHD.data$Ca, useNA = "ifany")
AHD.data$Ca <- ifelse(is.na(AHD.data$Ca),9,AHD.data$Ca)  # NA를 9로 치환

table(AHD.data$Thal, useNA = "ifany")
Thal_factor <- c(levels(AHD.data$Thal),"unknown")  # factor labels를 미리 생성
AHD.data$Thal <- ifelse(is.na(AHD.data$Thal),4,AHD.data$Thal) # labels = 4를 추가시키고
AHD.data$Thal <- factor(AHD.data$Thal, labels = Thal_factor) # factor로 변경
table(AHD.data$Thal, useNA = "ifany") # 결과확인

str(AHD.data)

#3.FACTOR를 가변수로 변환
library(reshape2)
ChestPain.cast <- dcast(AHD.data, X ~ ChestPain, length)[2:4]  # X와 마지막 가변수 제외
colnames(ChestPain.cast) <- paste0("ChestPain_",colnames(ChestPain.cast)) # 컬럼명 수정
AHD.castdata <- cbind(AHD.data, ChestPain.cast)

Thal.cast <- dcast(AHD.data, X ~ Thal, length)[2:4]  # X와 마지막 가변수 제외
colnames(Thal.cast) <- paste0("Thal_",colnames(Thal.cast)) # 컬럼명 수정
AHD.castdata <- cbind(AHD.castdata, Thal.cast)

AHD.castdata$X <- NULL  # X(ID) 제거
AHD.castdata$ChestPain <- NULL  # 가변수 생성후 제거
AHD.castdata$Thal <- NULL # 가변수 생성후 제거


# 값 표준화
AHD.castdata <- scale(AHD.castdata)

# 클러스터 갯수를 바꾸어 가며 최적의 클러스터 숫자를 확인

# 클러스터 갯수별 withinss 비교
wss <- data.frame()
for (i in 1:10) {
  wss <- rbind(wss, data.frame(seq = i, value = sum(kmeans(AHD.castdata,i)$withinss)))
}

library(ggplot2)
ggplot(wss, aes(x=seq, y=value)) + geom_point() + geom_line()

# cluster 값을 2로 한다.
AHD.cluster <- kmeans(AHD.castdata,2)

# cluster 와 원래 AHD와 비교
kmean_diff <- data.frame(res=AHD.cluster$cluster, ori=AHD.result)
table(kmean_diff[,1],kmean_diff[,2])

#####################################################################################
# 연관분석
# AHD 결과와 가장 연관있는 경우를 확인


AHD.factor <- AHD[,-1] # X(ID) 제외
str(AHD.factor)
col_freq <- data.frame(freq = unlist(lapply(AHD.factor, function(x) {length(unique(x))})),
                       coltype = unlist(lapply(AHD.factor, class)))  # 컬럼별 unique한 값 계산 및 class 확인
factor_col <- mutate(col_freq, colname = rownames(col_freq)) %>% filter(freq < 6 & coltype != 'factor') # unique한 값이 6보다 적으면서 factor가 아닌 컬럼 
# 1. 해당 컬럼 factor화
factor_col

AHD.factor$Sex <- factor(AHD.factor$Sex)
AHD.factor$Fbs <- factor(AHD.factor$Fbs)
AHD.factor$RestECG <- factor(AHD.factor$RestECG)
AHD.factor$ExAng <- factor(AHD.factor$ExAng)
AHD.factor$Slope <- factor(AHD.factor$Slope)
AHD.factor$Ca <- factor(AHD.factor$Ca)

cut_col <- mutate(col_freq, colname = rownames(col_freq)) %>% filter(freq >= 6 & coltype != 'factor') # unique한 값이 6보다 크면서 factor가 아닌 컬럼 
cut_col
AHD.factor$Age <- cut(AHD.factor$Age,4)
AHD.factor$RestBP <- cut(AHD.factor$RestBP,4)
AHD.factor$Chol <- cut(AHD.factor$Chol,4)
AHD.factor$MaxHR <- cut(AHD.factor$MaxHR,4)
AHD.factor$Oldpeak <- cut(AHD.factor$Oldpeak,4)

str(AHD.factor)
# 2. trans 데이터로 변경
library(arules)
AHD.trans <- as(AHD.factor, "transactions")

# 데이터 확인 
inspect(AHD.trans[1:5])
itemFrequencyPlot(AHD.trans, support=0.3)
image(AHD.trans[1:10])

# 3. 연관분석 실행
AHDrules <- apriori(AHD.trans, parameter = list(support = 0.1, confidence = 0.3, minlen = 5))
inspect(AHDrules[1:3])

# 4. AHD 결과와 연관되는 항목 조회
rulesNo <- subset(AHDrules, subset = rhs %in% "AHD=No" & lift > 1.5)
inspect(head(sort(rulesNo, by="confidence")))  

rulesYes <- subset(AHDrules, subset = rhs %in% "AHD=Yes" & lift > 1.5)
inspect(head(sort(rulesYes, by="confidence"))) 
        

#####################################################################################
# 분류분석
# 분류 분석을 통하여 중요한 변수를 파악

# 1.데이터 준비
AHD.tree <- AHD 
str(AHD.tree)

# rpart 실행 (변수를 확인하기 때문에 test/train 분리 안함)
library(rpart)
AHD.rpart.md <- rpart(AHD ~ ., data = AHD.tree, control = rpart.control(minsplit=10))

# 예측값 생성 후 정확도 확인
AHD.rpart.prob <- predict(AHD.rpart.md, AHD.tree)
AHD.rpart.pred <- ifelse(AHD.rpart.prob > 0.5, "No","Yes")[,1]
table(AHD.rpart.pred, AHD.tree$AHD) # 결과값 확인결과 모델에 문제 없어보임

library(rpart.plot)
rpart.plot(AHD.rpart.md)  # 중요 변수 확인 (Thal=normal, ChestPain=nonanginal,nontypical,  Ca< 0.5)


