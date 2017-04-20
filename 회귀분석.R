# ※ 다중회귀분석의 순서

# 1단계. lm함수를 사용하여 회귀분석 모델을 생성한다.
# 2단계. 모델이 통계적으로 유의한지 여부를 확인한다.
# 3단계. 필요한 경우 partial F-test를 통해 추가할 새로운 변수와 삭제할 변수는 없는지 확인한다.
# 4단계. predict함수를 사용하여 새로운 데이터 셋에 대한 예측값을 구한다.

### 다중회귀분석
 
# swiss : 1888년 스위스의 47개 프랑스어를 사용하는 지역의 토양의 비옥도(fertility)와 5개의 사회경제지표들
# 1. 필요한 패키지 및 데이터를 로드한다
require(datasets); require(ggplot2)
data(swiss)
str(swiss)
summary(swiss)
 

# 2. 데이터셋 swiss의 Infant.Mortality을 대상으로 분석할 것이므로 Infant.Mortality가 정규분포를 따르고 있는지를 확인
#    : Q-Q plot상의 직선에서 점들이 크게 벗어나 있지 않는다면 Infant.Mortality 변수는 정규 분포를 따른다고 볼 수 있다.

hist(swiss$Infant.Mortality)
qqnorm(swiss$Infant.Mortality)
qqline(swiss$Infant.Mortality)


# 3.로지스틱 회귀분석 모델 작성 (MODEL FITTING)
# swiss 데이터셋의 다른 모든 변수와 Infant.Mortality 간의 관계를 살펴보자.
#      : F-statistic 결과의 p-value를 보면 0.03665로 0.05보다 작아 이 모델이 유의하다고 판단
#      : Infant.Mortality와 유의한 관계가 있는 변수는 Fertility 하나이다
model<-lm(Infant.Mortality~. ,data=swiss)
summary(model)

# 4. Fertility 변수를 제외한 다른 변수들을 모두 삭제했을 때 모델이 통계적으로 달라지는지를 확인하기 위해 Partial F-test
#    : p-value는 0.4427로 두 모델의 Infant.Mortality에 대한 설명력에는 차이가 없다고 볼 수 있다.
model_simple<-lm(Infant.Mortality~Fertility ,data=swiss)
anova(model, model_simple)
 


# 5. predict함수를 사용하여 새로운 Fertility에 대한 Infant.Mortality를 예측	

new_Fertility<-rnorm(10, mean=mean(swiss$Fertility), sd=sd(swiss$Fertility))
new_Fertility<-as.data.frame(new_Fertility)
colnames(new_Fertility)<-c("Fertility")
predict(model_simple, new_Fertility, interval="prediction")

# 6. 다중공선성 문제 (Multicollinearity)

# 다중공선성을 판단하는 방법에는 여러가지가 있지만,
# R에서는 vif 함수를 사용해 VIF값을 간단히 구할 수 있으며, 보통 VIF 값이 4가 넘으면 다중공선성이 존재한다고 본다.
# 아래와 같이 car 패키지의 vif 함수를 사용하여 다중공선성 문제를 판단한다. 
require(car)
vif(model)

### 로지스틱 회귀분석 

# 1. 데이터셋 로드
#    : admit 변수는 지원자의 입학 여부
#      gre, gpa 변수는 수치형 변수인 시험 점수
#      rank는 지원자의 학교 등급을 나타내며 1~4의 값을 가지는 범주형 변수
data <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
str(data)
head(data)

# 2. 데이터 클린징 (DATA CLEANSING)
data$rank <-as.factor(data$rank)
str(data)

# 3.로지스틱 회귀분석 모델 작성 (MODEL FITTING)
train<-data[1:200, ]
test<-data[201:400, ]
model <- glm(admit ~ gre + gpa + rank, data =train, family = "binomial")
summary(model)

# 4. 모델 해석
#   : Null deviance와 residual deviance의 차이는 Null 모델과 비교하여 해당 변수가 모델에 포함 되었을 때 모델의 성능이 얼마나 나아지는지를 보여주는 수치입니다. 
#     rank 가 포함되었을 때 resid. Dev 의 값이 역시 가장 많이 하락하네요.

anova(model, test="Chisq")

# 로지스틱 회귀분석에도 선형 회귀 분석에서의   R2 와 유사한 개념이 존재하는데요, Mcfadden R2으로 모델 fit을 확인할 수 있습니다.
# “pscl” 패키지의 pR2 함수를 사용하여 Mcfadden R2 를 간단히 알아 볼 수 있는데요.
# R2값이 0.2인 것으로 보아 모델이 train 데이터 셋의 분산의 약 20% 정도 밖에 설명해 주지 못한다고 봐야 겠네요.
library(pscl)
pR2(model)

# 5. 모델 평가
# ROC 커브 아래의 영역 AUC가 1에 가까울 수록 모델의 예측 정확도가 높다고 할 수 있다
library(ROCR)
p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$admit)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# ROC 그래프 아래 영역의 넓이인  AUC를 구해볼까요?
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# 결과를 종합해 보면 모델은 유의하지만, 예측력은 그다지 높지는 않은 것으로 보이네요.
# 설명 변수를 추가하고, 유의하지 않은 변수를 제거하고 여러 번 모델을 다시 fitting 하는 작업이 필요할 것 같습니다.
# 작업 하고 계신 모델의 성능이 좋지 않다고 너무 낙담하지는 마세요.
# 모델의 성능을 높이기 위해서는 아래 세 가지 작업을 반복 수행해야 합니다.
#    1. 적당한 설명 변수를 입력 하는 것
#    2. 적절한 모델/알고리즘을 선택하는 것
#    3. 위 1~2번을 반복
# 대부분의 머신 러닝을 사용한 예측 모델은 위 과정을 반복하면서 더욱 정교해지니까요!

