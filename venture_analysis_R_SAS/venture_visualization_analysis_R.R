
dat1<-read.csv('venture.csv')

library(ggplot2)
library(GGally)
library(geoR)
library("nortest")


#------------------------------------------------------------------
#######데이터 탐색#######
par(mfrow=c(3,4))

##종속변수 중 '자산총계(property)' 
property1<-log(dat1$'property')
hist(property1,col="skyblue",main="",xlab="자산 총계")


#종속변수 중 '매출액(sales)' 
sales1<-log(dat1$'sales')
hist(sales1,col="skyblue",main="",xlab="매출액")


#종속변수 중 '영업이익(business_profit)' 
business_profit1<-(dat1$'business_profit')**0.15
hist(business_profit1,col="skyblue",main="",xlab="영업 이익")

#종속변수 중 '
debt1<-(dat1$debt)**0.1
hist(debt1,col="skyblue",main="",xlab="부채 총계")

#독립변수 중 부채비율
bet<-log(dat1$debt.ratio)**0.75
hist(bet,col="skyblue",main="",xlab="부채 비율")


#총자산회전율
set<-log(dat1$asset.ratio)**0.7
hist(set,col="skyblue",main="",xlab="총 자산 회전율")


#고용인력 학력별 구성비 (고졸)
whigh <- dat1$high * dat1$member
whigh1<-log(whigh)
hist(whigh1,col="skyblue",main="",xlab="최종학력: 고졸")


#고용인력 학력별 구성비 (전문대졸)
wcollege <- dat1$college * dat1$member
wcollege1<-log(wcollege)
hist(wcollege1,col="skyblue",main="",xlab="최종학력: 전문대졸")


#고용인력 학력별 구성비 (4년대졸)
wuniversity <- dat1$university * dat1$member
wuniversity1<-log(wuniversity)
hist(wuniversity1,col="skyblue",main="",xlab="최종학력: 4년대졸")


#고용인력 학력별 구성비(석박사)
wmaster <- dat1$master * dat1$member
wmaster1<-log(wmaster)
hist(wmaster1,col="skyblue",main="",xlab="최종학력: 석박사")



#매출액 성장률 
dat1$growth1<-(log(dat1$growth))**0.2
hist(dat1$growth1,col="skyblue",main="",xlab="매출액 성장률")


#당기순이익
dat1$profit1<-log(dat1$profit)
dat1$bisratio1[is.na(dat1$bisratio1)]<-0
hist(dat1$bisratio1,col="skyblue",main="Histogram of bisratio",xlab="bisratioR")


#------------------------------------------------------------------
########tree map#############

library(treemap)

str(dat2)
treemap(dat2,
       index=c("WAKE", "CHECK"),
       vSize="asset.ratio",
       vColor="debt.ratio",
       title.legend="debt ratio value",
       type="value")

#########joy plot################

library(ggplot2)
library(Rcpp)
library(colorspace)
library(ggjoy)

dat2$WAKE.step<-as.factor(dat2$WAKE)
str(dat2)
ggplot(dat2, aes(x = debt, y = WAKE.step, 
  fill=WAKE.step),xlim=c(0,10000)) + 
  geom_joy()+scale_fill_cyclical(values = c("seagreen", "salmon",
                                            "royalblue","yellow","gray"))


dat2<-cbind(dat1,property1,sales1, business_profit1, whigh1, wcollege1, wuniversity1,wmaster1, bet, set)
xxx<-subset(dat2,select=c('whigh1', 'wcollege1', 'wuniversity1','wmaster1'))
yyy<-subset(dat2,select=c('property1','sales1','business_profit1'))

xxx[is.na(xxx)]<-0
yyy[is.na(yyy)]<-0
dat3<-cbind(xxx,yyy)

is.nan(dat3)
is.infinite(dat3$wcollege1)
is.finite(dat3)

dat3$whigh1<-replace(dat3$whigh1, is.infinite(dat3$whigh1),0)
dat3$wcollege1<-replace(dat3$wcollege1, is.infinite(dat3$wcollege1),0)
dat3$wuniversity1<-replace(dat3$wuniversity1, is.infinite(dat3$wuniversity1),0)
dat3$wmaster1<-replace(dat3$wmaster1, is.infinite(dat3$wmaster1),0)

xxx$whigh1<-replace(xxx$whigh1, is.infinite(xxx$whigh1),0)
xxx$wcollege1<-replace(xxx$wcollege1, is.infinite(xxx$wcollege1),0)
xxx$wuniversity1<-replace(xxx$wuniversity1, is.infinite(xxx$wuniversity1),0)
xxx$wmaster1<-replace(xxx$wmaster1, is.infinite(xxx$wmaster1),0)

dat3
str(dat3)

sum(is.na(dat3))

##상관관계 그래프
install.packages('corrplot')
library(corrplot)
co<-cor(dat3)
corrplot(co, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         , addCoef.col="black", addcolorlabel="no", order="AOE")


#------------------------------------------------------------------
#############정준상관분석##############

library(CCA)

b_scal<-scale(dat3)
str(b_scal)
x1<-b_scal[,1:4]
y1<-b_scal[,5:7]

cc1 <- cc(x1,y1)
cc1

# display the canonical correlations, 표본정준상관계수, (정준 변수들의 corr)
cc1$cor

#canonical coefficients, 정준계수
cc1$xcoef
cc1$ycoef


#표본정준적재, 표본정준교차행렬
cc2 <- comput(x1,y1, cc1) #corr을 계산해 주는 명령어가 comput!
cc2[]

dim(dat3) #1994

#xscores, yscores : 정준변량
cc2$xscores #1994명에 대한 계산된 정준 변수 값. = V
cc2$yscores #1994명에 대한 계산된 정준 변수 값. = W


#표본정준적재, 
cc2$corr.X.xscores #V와 X의 corr값
cc2$corr.Y.yscores #sas하고 부호 반대일수 있음.

#표본정준교차행렬
cc2$corr.X.yscores # X변수들하고 W 사이의 corr
#w1하고 가장 상관계수 높은게 임.
cc2$corr.Y.xscores # Y변수들하고 V 사이의 corr



#wilks 람다 구해서 p-value 구함.
# tests of canonical dimensions
ev <- (1 - cc1$cor^2) 
#로우 값 제곱
ev  #1-(로우i^2)

n <- dim(xxx)[1] #행의 길이
p <- dim(xxx)[2] #열의 길이
q <- dim(yyy)[2]
k <- min(p, q) # 실제 k값은 3. 두 변수가 다 3개씩 가지고 있으니까
m <- n - 3/2 - (p + q)/2

rev(ev) # 순서만 바꾼 것. 
cumprod(rev(ev)) #뒤의 숫자부터 누적해서 곱하는 것. (rev가 있기 떄문에!)
#만약 그냥 cumprod(ev)라면 앞에 숫자부터 누적해서 곱하는 것.


# initialize (=초기 내용을 설정하다.)
d1 <- d2 <- f <- vector("numeric", k)
#d1, d2 모두 0 0 0으로 나옴.


#p.257 (6-48) 공식
for (i in 1:k) {
    s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si <- 1/s
    d1[i] <- p * q #분자의 자유도
    d2[i] <- m * s - p * q/2 + 1 #분모의 자유도
    r <- (1 - w[i]^si)/w[i]^si
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1
    q <- q - 1
}
pv <- pf(f, d1, d2, lower.tail = FALSE) #lower.tail의 defult가 FALSE일때는 
# 크거나 같은 값을 준것 =P-value!!!(p.257 그림 참고)


(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))

##그래프
plt.var(cc1, d1=1, d2=2,var.label=TRUE)
plt.indiv(cc1, d1=1, d2=2)

plt.cc(cc1,type='b',var.label=TRUE, d1 = 1, d2 = 2) 



#------------------------------------------------------------------
##################분류 및 판별 분석####################

dat2
str(dat2)
sum(is.na(dat3$business_profit))
dat3<-dat2[complete.cases(dat2[,c("set")]),]
str(dat3)
sum(is.na(dat2$set))
table(dat2$GOLD)
sum(is.na(dat3$business_profit1))
dat3$business_profit1[is.na(dat3$business_profit1)]<-0
library(MASS)
lda(GOLD ~ property1 + sales1 + bet + business_profit1  , data=dat3, prior=c(1,1)/2, CV=TRUE)

fit <- lda(GOLD ~ property1 + sales1 + bet + business_profit1 , data=dat3, CV=TRUE)
fit
str(fit)
ct <- table(dat3$GOLD, fit$class)
ct


fit2 <- lda(GOLD ~  property1 + sales1 + bet + business_profit1, data=dat3)
fit2

plot(fit2)
plot(fit2, dimen=1, type="both")

fit3 <- qda(GOLD ~ property1 + sales1 + bet + business_profit1, data=dat3, prior=c(1,1)/2,CV=TRUE)
fit3


qda(dat3[,c( 'property1' ,'sales1' ,'bet','business_profit1')], dat3$GOLD) 

install.packages('klaR')
library(klaR)
dat3$GOLD2<-as.factor(as.character(dat3$GOLD))
partimat(GOLD2 ~   property1 + sales1 + bet + business_profit1  ,data=dat3,method="lda")

partimat(GOLD2 ~  property1 + sales1 + bet + business_profit1  ,data=dat3,method="qda")
par(mfrow=c(1,3))

partimat(GOLD2 ~ property1 + sales1 + bet + business_profit1  ,data=dat3,method="qda")


pairs(dat3[c('bet','set','business_profit1' ,'property1')], main="Scatter plots- 3 Cylinders ",
      pch = 21, bg = c("red", "green3", "blue")[unclass(dat3$GOLD2)])


ct2 <- table(dat3$GOLD, fit3$class)
ct2 


prop.table(ct2, 1)

1-sum(diag(prop.table(ct2)))



#####로지스틱 

step(glm(GOLD2 ~ bet +  property1 + business_profit1 +WAKE,family=binomial,
         dat3))

fit<-glm(I(GOLD==2) ~   bet + property1 + business_profit1 ,family=binomial,dat3)
summary(fit)

fitted(fit)
predict(fit)

fit.b<-glm(formula = GOLD2 ~  property1 + bet + business_profit1 +WAKE, family = binomial, data = dat3)
summary(fit.b)
anova(fit.b,test='Chisq')

fit.bb<-glm(formula = GOLD2 ~  property1 + bet + business_profit1 , family = binomial,data = dat3)
fit.bb

anova(fit.bb,fit.b,test='Chisq')


l<-glm(formula =GOLD2 ~  property1 + bet + business_profit1 , family = binomial,data = dat3)
l
risk1<-predict(fit.bb,type='response')
head(risk1)

library(ResourceSelection)
hl <- hoslem.test(fit.bb$y, fitted(fit.bb), g=5)
hl

?pamx

library(PredictABEL)


str(dat3)
plotCalibration(data=dat3,cOutcome=28, 
                predRisk=risk1,
                groups=5, rangeaxis=c(0,1),
                plottitle='Calibration for the logistic model')

