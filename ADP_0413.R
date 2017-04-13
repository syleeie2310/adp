#필수예제 실기

#01 Data Frame abc에 class라는 변수가 있는데 값이 1,2,3일 경우 가능하다. 현재 데이터 상에는 1과 2 두가지 종류가 들어있다. 이 변수를 classification에서 타겟 변수로 활용하고자 하는 경우 어떻게 해야 하는가?
abc$class <- factor(abc$class, levels=c('1','2','3'))

#02 party와 rpart 두개의 package를 한 번에 설치하고 싶다. 하나의 명렁어로 만들어 보아라.
install.packages(c('party', 'rpart'))

#03 datasets 패키지에 있는 sample data 목록을 보고 싶다.
data[package='datasets']

#04 dataframe abc에 칼럼이 5개가 있는데 3번째와 5번째 컬럼을 삭제하고 싶다.
abc <- abc[,-c(3,5)]

#05 dataframe abc의 100번째에서 110번째 줄과 120번째 줄을 삭제하고 싶다.
abc <- abc[-c(100:110,120),]

#06 dataframe abc와 def는 같은 구조를 갖고 있다. 데이터를 병합하는 방법은?
tmp <- rbind(abc,def)

#07 abc 데이터와 def데이터와 같은 고객 수를 갖고 있고 고객명으로 정렬되어 있는 경우 두 개의 데이터를 결합하고 싶다.
tmp <- cbind(abc,def)

#08 현재 메모리에 있는 변수들 중에서 a, b, c를 지우고 싶다.
rm(a,b,c)

#09 메모리에 있는 모든 것을 지우고 싶다.
rm(list=ls())

#10 현재 메모리에 있는 자료 중 a, d, f만 이름을 'tmp'로 하는 rdata format으로 저장하고 싶다.
save(a,d,f,file='tmp.rdata')

#11 현재 메모리에 있는 모든 정보를 1개의 파일로 저장하고 싶다.
save.image('image.rdata')

#12 저장된 a.rdata 파일의 변수들을 읽어 들이고 싶다.
load("a.rdata")

#13 현재 작업위치를 알고 싶다.
getwd()

#14 abc dataframe의 length 변수에 NA가 있는 경우 150으로 대체하고 싶다.
ind <- which(is.na(abc$length))
abc[ind, "length"] <- 150

#15 doMC package를 이용해서 4개의 core를 사용해 병렬처리를 하고 싶다.
library(doMC)
registerDoMC(4)


#Exercise
#다음 예제 데이터는 "R을 이용한 통계 프로그래밍 기초"를 참고한 1999년 미국 회사의 CEO와 성과 자료다. 이 데이터를 기반으로 다음에 답하시오.

#1 표의 빈칸에 해당하는 기초 통계량을 구하시오.
#2 profits에 따른 CEO의 salary에 대한 산점도를 그리고 상관계수를 구하시오.
#3 profits에 따른 CEO의 salary에 대한 단순선형회귀식을 구하고 분석하시오.
#4 profits과 age, sales에 따른 CEO의 salary의 회귀식을 구하고 분석하시오.
#5 profits과 age, sales, tenure, assets에 따른 CEO의 salary의 회귀식을 구하고 분석하시오.
#6 후진제거법으로 최적의 회귀식을 찾으시오. 단, salary를 종속변수로 한다.
#7 전진제거법으로 최적의 회귀식을 찾으시오. 단, salary를 종속변수로 한다.
#8 단계적방법으로 최적의 회귀식을 찾으시오. 단, salary를 종속변수로 한다.
'''
salary = 1999 salary + bonuses
tenure = number of years as CEO (=0 if less than 6 months)
age = age of CEO
sales = total 1998 sales revenue of firm i
profits = 1998 profits for firm i
assets = total assets of firm i in 1998
'''
data(cars)
head(cars)
m <- lm(dist~speed, cars)
m
summary(m)

fitted (m)[1:4]
residuals(m)[1:4]
fitted (m)[1:4] + residuals (m)[1:4]
confint(m)

m <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris )
m
summary(m)
install.packages('mlbench')
library(mlbench)
data(BostonHousing)
head(BostonHousing)
?BostonHousing #median value of owner-occupied homes in USD 1000's

m <- lm ( medv ~ . , data = BostonHousing)
m3 <- lm ( medv ~ 1 , data = BostonHousing)
?step
formula(m)

m_both <- step(m, direction = "both")
m_backward <- step(m, direction = "backward")
m_forward <- step(m3, direction = "forward", scope=formula(m))

formula(m_both)
formula(m_backward)
formula(m_forward)

summary(m_forward)$r.squared
summary(m_backward)$r.squared

summary(m_forward)$adj.r.squared
summary(m_backward)$adj.r.squared


install.packages ("leaps")
library(leaps)
m <- regsubsets (medv ~ ., data = BostonHousing)
summary(m)
summary(m)$adjr2
plot(m)

#Ref : http://rstudio-pubs-static.s3.amazonaws.com/190997_40fa09db8e344b19b14a687ea5de914b.html

#Exercise
#1. 박근혜 대통령의 "2015 신년구상 기자회견'의 연설문으로 워드 클라우드를 그려보려고 한다. 데이터는 청와대 홈페이지에 접속하여 수집할 수 있다.
#   (반드시 연설문을 'UTF-8' 형태의 텍스트 파일(.txt)로 저장하여 진행한다.)

library(KoNLP) # 한국어 단어 추출
library(wordcloud) # 워드 클라우드 생성

#readLines 함수를 이용하여 연설문이 저장된 파일 경로를 입력하여 불러온다. 이 때 파일 경로는 텍스트 파일명과 그 확장자까지 모두 작성되어야 하고, 인코딩을 'UTF-8'로 지정한다.
setwd("/Users/syleeie/ADP")
park.text <- readLines('./이정미_파면결정문.txt', encoding = 'UTF-8')
head(park.text, 3)

#readLines 함수를 사용하므로, 하나의 연설문이 여러 줄의 데이터로 읽어온 것을 확인할 수 있다.
#따라서 다음과 같은 명령을 실행하여 여러 줄의 데이터를 하나의 데이터로 묶어 준다.
park <- paste(park.text, collapse = ' ')
head(park)

#저장된 연설문 데이터에서 단어를 추출하기 위한 코드를 실행한다. 추출된 단어에서 유니크한 것만 뽑아내 저장한다.
tran <- Map(extractNoun, park)
tran <- unique(tran)

#2글자에서 4글자 사이의 한글 단어만 선택한다.
tran <- sapply(tran, function(x){
  Filter(function(y){
    nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)
  }, x)
})

head(tran, 2)

#워드 클라우드를 그리기 위한 형태를 만들기 위해 다음과 같은 코드를 실행한다.
tran.table <- as.data.frame(table(tran))
head(tran.table, 3)

#결과를 보면 추출된 단어는 tran에, 그 명사의 빈도수는 Freq에 저장된 tran.table에 생성되었다.
#마지막으로 추출된 단어에서 빈도수가 3 이상인 단어들만 사용하여 워드 클라우드를 그린다.
tran.table$tran
tran.table$Freq

wordcloud(words=tran.table$tran, freq=tran.table$Freq, min.freq=3, random.order=F, colors=brewer.pal(5,'Dark2'), family="AppleGothic")

#2. 미국의 인기 드라마 그레이 아나토미(Grey's Anatomy)에 등장했던 여러 주인공들 간의 연인 관계 데이터로 사회연결망 분석(SNA)를 해보고자 한다.
#데이터는 http://www.babelgraph.org/data/ga_edgelist.csv에서 입수할 수 있다
#먼저 필요한 패키지를 로드하고 기본 그래프를 그린다.

install.packages('igraph')
library(igraph)
ga.data <- read.csv('./ga_edgelist.csv', header=T)
head(ga.data)
g <- graph.data.frame(ga.data, directed=F)
g$layout <- layout.fruchterman.reingold(g)
plot(g)

#노드의 라벨을 모두 지우고 노드의 크기를 연결정도(degree) 값에 2를 곱하여 지정한다.
V(g)$label <- NA
V(g)$size <- degree(g) * 2
plot(g)

#closeness를 계산하고 closeness에 따라 노드의 색상을 달리하도록 설정한다.
clo <- closeness(g)
clo.score <- round((clo-sin(clo)) * length(clo) / max(clo) ) + 1
clo.colors <- rev(heat.colors(max(clo.score)))
V(g)$color <- clo.colors[clo.score]
plot(g)

#betweesness를 계싼하고 betweeness에 따라 노드의 색상을 달리하도록 설정한다.
btw <- betweenness(g)
btw.score <- round(btw) + 1
btw.colors <- rev(heat.colors(max(btw.score)))
V(g)$color <- btw.colors[btw.score]
plot(g)

#3 다음은 페이스북에서 나와 친구들의 정보를 가져와 관계를 나타내는 방법이다. 페이스북 데이터를 불려오기 위해서는 반드시 R stduio가 아닌 R GUI 환경에서 진행해야 한다.
install.packages("Rfacebook")
install.packages('Rook')

library(Rfacebook)
library(Rook)
#https://developers.facebook.com/app로 들어가 1번에 해당하는 Register as a Developer를 클릭한다.
#Facebook Platform Policy와 Facebook 개인정보취급방침에 대해 동의하고 가입한후 2번 상자와 'My-Apps-Add a New App'를 클릭한다.
#다음과 같은 화면이 나오면 3번 상자의 website를 클릭한다.
#choose an existing app or type name of your new app부분에 새로 생성할 app의 이름을 작성하고 4번의 create new facebook App ID를 클릭한다.
#My Apps-Settings에 가서 App Secret을 보기 위해 5의 show 버튼을 클릭한다.
#보안 확인을 위해 비밀번호를 다시 입력한 후 제출한다.
#위의 과정을 진행하면 [그림5]의 App Secret값이 나타나게 된다. 이값을 사용하여 R 스크립트에서 다음과 같은 코드를 실행한다.
#물론 app_id와 app_secret 부분에서는 본인 App의 ID와 Secret 값을 넣으면 된다.

fb_oauth <- fbOAutu(app_id='', app_secret='')
#위의 R 코드의 실행 결과를 보면, URL주소와 함께 Facebook App Setting에서 Site URL을 붙여 넣으라는 메세지가 뜨게 된다.
#이를 사용하여 Facebook App setting에 들어가서 6번 박스의 Add Platform을 클릭한다.
#플랫폼(platform)을 선택하라는 화면이 나오면, website를 선택하고, [그림 8]에서 Site URL에 R 메세지를 복사한 URL 주소를 넣은 후 저장한다.
#Authentication complete. Please close this page and return to R.'이라는 창이 보이면 R스크립트 창으로 돌아와서 아무 키나 누른다.
#그러면 아래와 같이 인증이 완료되었다는 메세지가 추가로 나타난다.
#이제 페이스북에서 내 정보를 비롯한 다양한 정보를 가져올 수 있게 되었다.
#먼저 데이터를 저장한다.

save(fb_oauth, file='fb_oauth')
load('fb_oauth')

#getUser 함수를 통해 내 정보를 가져옫록 한다. token은 위에서 만든 fb_oauth를 이용한다. 이를 통해 만든 데이터에서 내 페이스북 정보를 확인할 수 있다.
me <- getUsers('me', token=fb_oauth)
me
my_friends <- getfriends(token=fb_oauth)
head(my_friends, n=5)
my_friends_info <- getUsers(my_friends$id, token=fb_oauth, private_info=TRUE)
colnames(my_friends_info)
table(my_friends_info$relationship_status)

#또한 getNetworks 함수로 내 친구들끼리의 관계도 알아볼 수 있다. 이 자료를 가지고 SNA 그래프를 그럴수도 있다.
g.data <- getNetwork(token=fb_oauth, format='edgelist', verbose=T)
library(igraph)
g.data <- as.data.frame(g.data)
g <_ graph.data.frame(g.data)
g$layout <- layout.fruchterman.reingold(g)
#plot(g)

#Exercise
#R에 내장된 Orange 데이터를 활용해 다음을 수행한다.
#라인그래프를 그리시오.
#얼굴 그림을 그리시오.
#별 그림을 그리시오.

#다음과 같은 데이터를 활용해 x축은 age로, y축은 circumference 변수로 라인그래프를 그리시오. 이때 라인그래프의 색은 Tree 변수로 지정한다.
Orange
ggplot(Orange, aes(x=age, y=circumference, colour=Tree)) + geom_line()
str(Orange)
Orange$Tree <- as.numeric(Orange$Tree)
str(Orange)
faces(Orange)
stars(Orange)

