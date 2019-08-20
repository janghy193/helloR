bef <- c(34,76,76,63,73,75,67,78,81,53,58,81,77,80,43,65,76,63,54,64,85,54,70,71,71,55,40,78,76,100,51,93,64,42,63,61,82,67,98,59,63,84,50,67,80,83,66,86,57,48)
aft<- c(74,87,89,98,65,82,70,70,70,84,56,76,72,69,73,61,83,82,89,75,48,72,80,66,82,71,49,54,70,65,74,63,65,101,82,75,62,83,90,76,87,90,78,63,59,79,74,65,77,74)

boxplot(bef,aft,names=c("수강전","수강후"))

var.test(bef,aft)


t.test(bef,aft,paired=TRUE,var.equal=TRUE,alternative="two.sided")



purifier_df<-read.table(file="clipboard", header=TRUE, sep="\t", stringsAsFactors=TRUE)

str(purifier_df)

#x축 : 총 정수기 대여 수, y축 : AS 소요시간
plot(purifier_df$purifier, purifier_df$as_time, xlab="Numb of purifier lent", ylab="AS time taken")

#x축 : 노후 정수기 대여 수, y축 : AS 소요시간
plot(purifier_df$old_purifier, purifier_df$as_time, xlab="Numb of old purifier", ylab="AS time taken")

cor(purifier_df$purifier,purifier_df$as_time)

cor(purifier_df$old_purifier,purifier_df$as_time)

summary(purifier_df)

cor(purifier_df$purifier,purifier_df$old_purifier)

#전월 정수기 총 대여수와 10년 이상 된 정수기와의 상관관계가 0.6 으로 매우 큼
#(전월 기준 정수기 총 대여 수에는 전월 기준 10년 이상 노후 정수기 대여 수 가 포함되어있음)

cor((purifier_df$purifier - purifier_df$old_purifier),purifier_df$old_purifier)

str(purifier_df)
purifier_df$new_purifier <- purifier_df$purifier - purifier_df$old_purifier

str(purifier_df)

#회귀분석 수행(lm)
#종속변수 : AS시간(as_time)
#독립변수 : 10년 미만 정수기(new_purifier), 10년 이상 정수기(old_purifier)
lm_result<-lm(as_time~new_purifier + old_purifier, data=purifier_df)

#결괏값 확인
summary(lm_result)

input_predict<-data.frame(new_purifier=300000, old_purifier=70000)

predict_as_time <- predict(lm_result, input_predict)

predict_as_time

#AS기사 1명이 한 달간 처리하는 AS시간 = 8시간 * 20일
predict_as_time / (8*20)


#구간추정
predict_as_time<-predict(lm_result,input_predict,interval="confidence",level = 0.95)

#익월 AS시간이 '43,414시간' ~ '43,824시간'이 될 가능성을 95%로 예측
predict_as_time