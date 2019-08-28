apple_df<-read.table(file="clipboard",header=T,sep="\t", stringsAsFactors=T)
attach(apple_df)
summary(apple_df)

str(apple_df)
apple_df
# 품종별 무게 분포
boxplot(weight~model, data=apple_df, ylab="weight")

# 품종별 당도 분포
boxplot(sugar~model, data=apple_df, ylab="sugar")


# 품종별 산도 분포
boxplot(acid~model, data=apple_df, ylab="acid")

install.packages("ggplot2")
library("ggplot2")

# ggplot 데이터 영역 설정
# 색상별 사과 개수를 bar로 표현하고 bar 안의 색상은 사과 품종으로 구분(fill)
k<- ggplot(apple_df, aes(factor(color), fill=factor(model)))

# bar plot으로 표현
k+geom_bar()

# 분류분석

# 사과 데이터
summary(apple_df)

# 패키지 로드
library(caret)

# 훈련 데이터(전체 중 80% 사용)
apple_train_idx<- createDataPartition(apple_df$model, p=0.8, list = F)

#총 데이터 개수
nrow(apple_df)

#훈련 데이터 추출할 인덱스 개수 확인
nrow(apple_train_idx)

#훈련 데이터 추출
apple_train_df <- apple_df[apple_train_idx, ]

#테스트 데이터 추출
apple_test_df <- apple_df[-apple_train_idx, ]

#rpart 패키지 로드
library(rpart)

# 분류분석 - rpart 함수 실행
apple_rpart_result <- rpart(model~., data=apple_train_df, control=rpart.control(minsplit=2))

# rpart.plot 패키지 로드
library(rpart.plot)

# 의사결정 트리 그리기
rpart.plot(apple_rpart_result)

# <평가 파트>
# 실제 값과 에상 값을 한 눈에 볼 수 있게 데이터프레임 만들기
# actual : 실제 값, expect : 예상 값
actual <- apple_test_df$model
expect <- predict(apple_rpart_result, apple_test_df, type="class")

# 라이브러리 로드
library(caret)

# confusionMatrix 실행
confusionMatrix(expect, actual, mode="everything")

# 과적합 문제 보완하기 위해 가지치기 ( 정확ㄷ, Kappa 수치 높음)

# cp table 조회
apple_rpart_result$cptable

# 가지치기 - 오류율이 6%로 높아지지만, nsplit이 4인 cp값 적용
library(rpart.plot)
apple_prune_tree <- prune(apple_rpart_result, cp=0.0625)

# 가지치기 후 모델 확인
rpart.plot(apple_prune_tree)

# 가지치기 후 성능 테스트
actual <- apple_test_df$model
expect <- predict(apple_prune_tree, apple_test_df, type ="class")

# 라이브러리 로드
library(caret)
confusionMatrix(expect, actual, mode="everything")

# 정확도, Kappa 수치 동일



# iris 데이터로 연습
str(iris)

# 팩터 레벨 확인 (꽃 종류)
levels(iris$Species)

# 데이터 총 건수
nrow(iris)

# 꽃 종류별 개수
table(iris$Species)

library(caret)

# createDataPartition을 활용해 추출할 위치 정보를 벡터로 반환받음(list=FALSE)
# iris$Species 기준으로 각 종류별로 80%씩 도출
iris_row_idx <- createDataPartition(iris$Species, p=0.8, list=F)

# 결괏값 확인(데이터 프레임에서 추출할 행 번호를 얻음)
str(iris_row_idx)

# 추출할 위치 정보를 활용해 iris 데이터셋에서 훈련 데이터 추출
iris_train_data<- iris[iris_row_idx, ]

# 추출한 iris_train_data 확인
str(iris_train_data)

# iris_train_data의 꽃 종류별 데이터 수 확인
table(iris_train_data$Species)

# 테스트 데이터 추출(iris_row_idx를 제외한 행 데이터 추출)
# 벡터 내 존재하는 인덱스를 제외하라는 의미는 "-"기호를 이용함
iris_test_data <- iris[-iris_row_idx, ]

# iris_train_data 확인
str(iris_test_data)

# 테스트 데이터 확인(꽃 종류별로 균일하게 10개씩 총 30건을 추출함)
table(iris_test_data$Species)

# 훈련 데이터 확인
summary(iris_train_data)

# 테스트 데이터 확인
summary(iris_test_data)

library(rpart)

# 분류분석 - rpart 함수 실행
# iris_train_data의 모든 항목을 넣기 위해 "." 사용
# Species~. (의미: Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width)
iris_rpart_result <- rpart(Species~., data=iris_train_data, control=rpart.control(minsplit=2))

# 분류분석 결괏값 출력
iris_rpart_result


library(rpart.plot)

# 의사결정 트리 그래프 출력
rpart.plot(iris_rpart_result)

# CP값 조회(의사결정 가지를 생성할 때 소요되는 복잡도를 의미함)

iris_rpart_result$cptable

# 가지치기
iris_prune_tree<- prune(iris_rpart_result, cp=0.0125)

# Deicision Tree 그리기
rpart.plot(iris_prune_tree)

# nsplit(분류기준) 2개이며 error 0.0625로 최선은 아니나 우수한 CP값을 사용

# 예측하기
# 테스트 데이터 확인 = 훈련 데이터와 칼럼명이 같아야 함(단 종속변수 칼럼은 없어도 됨)
str(iris_test_data)

# predict 함수 실행
predict(iris_rpart_result, iris_test_data, type="class")

# 실제 값과 예상 값을 한 눈에 볼 수 있게 데이터프레임 만들기
actual <- iris_test_data$Species
expect <- predict(iris_rpart_result, iris_test_data, type="class")

# 데이터프레임 만들기
iris_predict_df <- data.frame(actual, expect)

iris_predict_df

# 평가하기 - 혼동행렬 만들기
table(iris_predict_df)

# confusionMatrix(caret 패키지) -> 혼동행렬과 함께 Accuracy, Precision, Specificity, Recall(Sensitivity), Cohen's Kappa 계수 등 제공
library(caret)

# confusionMatrix 수행
confusionMatrix(expect, actual, mode="everything")