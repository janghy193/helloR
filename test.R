
bef<-c(842,805,745,655,731,589,602,813,680,811,
       906,837,729,595,781,699,626,920,637,753,
       808,729,680,660,868,892,613,734,772,584)
aft<-c(767,789,745,731,810,721,762,691,775,798,
       675,754,790,710,702,677,753,674,756,810,
       690,828,761,706,709,780,720,690,734,724)

nbf<-length(bef)
naf<-length(aft)

vbf<-var(bef)
vaf<-var(aft)

f<-vbf/vaf

pv=1-pf(f,nbf,naf)

pv

View(pv)
test <- "안녕??"

test
edit(test)
test_m
name<-c("김가수","박인호","어만데","이기성")
age<-c(23,28,15,22)
weight<-c(67,75,73,80)
ex_df<-data.frame(name,age,weight)
ex_df
edit(ex_df)

colnames(iris)[3]<-"Third"
head(iris)
data(iris)
head(iris,2)


iris[c(T,T,F),c(1,3,4)]
iris2<-iris[c(1,2,3,4),]
iris2
iris2[c(T,F),]
iris[c(T,F),]
str(iris)

subset(iris, Sepal.Length >7)
a<-c(1:10)
a
factor(bef)

print(test)

str(test)

str(pv)

