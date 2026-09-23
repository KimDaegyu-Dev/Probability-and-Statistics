###제2장. 자료의 표현
##1. 도수분포표 작성 
Data <- read.csv(file='data/2장_1. 중력가속도.csv', header = TRUE)
attach(Data)
str(Data)
min(Gravity); max(Gravity)  # 최댓/최솟값 계산
range(Gravity)              # 최댓/최솟값 계산
max(Gravity)-min(Gravity)   # 통계량 범위 계산
1 + log2(100)               # (7.643856) => 계급의 개수를 8로 결정 

# 계급구간을 임의로 지정
Data <- transform(Gravity, 
           Class = ifelse(Gravity >= -400 & Gravity <= -200, "-400~-200",
                    ifelse(Gravity > -200 & Gravity <= -150, "-200~-150",
                     ifelse(Gravity > -150 & Gravity <= -100, "-150~-100",
                      ifelse(Gravity > -100 & Gravity <= 0, "-100~0",
                       ifelse(Gravity > 0 & Gravity <= 100, "0~100",
                        ifelse(Gravity > 100 & Gravity <= 150, "100~150",
                         ifelse(Gravity > 150 & Gravity <= 200,
                                "150~200", "200~600"))))))))
table(Data$Class) # 원하는 순서가 아님

# 원하는 순서로 하기 위하여 
# 변수 Class를 인자형(factor)으로 하고, 수준(level)을 원하는 순서로 지정
Data$Class <- factor(Data$Class, 
                       levels=c("-400~-200", "-200~-150",
                                "-150~-100", "-100~0",
                                "0~100", "100~150",
                                "150~200", "200~600"))
table(Data$Class)
detach(Data)



##2. 히스토그램
Data <- read.csv(file='data/2장_1. 중력가속도.csv', header = TRUE)
attach(Data)
#기본형
hist(Gravity)

# 계급구간을 원하는대로 표현하기
Break.Val= c(-400, -200, -150, -100, 0, 100, 150, 200, 600)
Range.Val = c(-400, -200, -150, -100, 0, 100, 150, 200, 600)
hist(Gravity, freq= FALSE, breaks= Break.Val, xlim= range(Range.Val),
     xlab= "중력 측정 차이", ylab= "밀도(Density)",
     col= "gray", label= FALSE, main= NULL) 
detach(Data)



##3. 줄기-잎 그림
Data <- read.csv(file='data/2장_2. 줄기잎그림.csv', header = TRUE)
attach(Data)
# 기본형
stem(Height)
# 줄기 조절
stem(Height, scale=0.5)
detach(Data)



##4. 상자 그림
Data <- read.csv(file='data/2장_3. 상자그림.csv', header = TRUE)
attach(Data)
boxplot(Height ~ Gender)
detach(Data)



##5. 다변량자료 
Data <- read.csv("data/2장_4. 다변량자료.csv", header=T)
attach(Data)

#히스토그램
par(mfrow=c(2,2))
Break.Val=c(2.0, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 3.75, 4, 4.25, 4.5)
hist(Data$고교성적, breaks= Break.Val, xlab="", freq=T, ylab="도수(Frequency)",
     col="grey", label=FALSE, main="고교성적")
hist(Data$수능언어성적, xlab="", freq=T, ylab="도수(Frequency)",
     col="grey", label=FALSE, main="수능언어성적") 
hist(Data$수능영어성적, xlab="", freq=T, ylab="도수(Frequency)",
     col="grey", label=FALSE, main="수능영어성적") 
hist(Data$대학성적, xlab="", freq=T, ylab="도수(Frequency)",
     col="grey", label=FALSE, main="대학성적") 

# 산점도
par(mfrow=c(3,2))
plot(수능언어성적 ~ 고교성적)    	# cor 0.056
plot(수능영어성적 ~ 고교성적)    	# cor 0.032
plot(대학성적 ~ 고교성적)	      	# cor 0.703
plot(수능영어성적 ~ 수능언어성적 )      # cor -0.328
plot(대학성적 ~ 수능언어성적 )		# cor -0.107
plot(대학성적 ~ 수능영어성적 )		# cor 0.133
detach(Data)




###제4장. 확률변수 및 확률분포
## 4.8 이산분포의 예
##4.8.1. 이항분포

#[예제 4.14]
#확률질량함수
# 반올림함수 round 사용, 소수점 셋째자리
round(dbinom(0,4,1/6),3)
round(dbinom(1,4,1/6),3)
round(dbinom(2,4,1/6),3)
round(dbinom(3,4,1/6),3)
round(dbinom(4,4,1/6),3)

#누적분포
round(pbinom(0,4,1/6),3)
round(pbinom(1,4,1/6),3)
round(pbinom(2,4,1/6),3)
round(pbinom(3,4,1/6),3)
round(pbinom(4,4,1/6),3)



##4.8.2. 초기하분포
#[예제] 4.15]
#확률질량함수와 누적분포
round(dhyper(1,3,37,5),4)
round(phyper(1,3,37,5)- phyper(0,3,37,5),4)



##4.8.3. 포아송분포
#[예제 4.16]
#누적분포 이용
round(1-ppois(10,7), 3)

#확률질량함수 이용
Mean = 7; Prob = 0
for (i in 0:10){Prob <- Prob + dpois(i, Mean)}
Prob
round(1 - Prob, 3)


#[예제 4.16]
#이항분포 이용
pbinom(5,8000,0.001)

#포아송분포 이용
ppois(5,8)



## 4.9 연속형분포의 예
##4.9.1. 정규분포

#[예제 4.18]
round(pnorm(50, mean=70, sd=12),4)
round(pnorm(-1.67),4)

round(qnorm(0.85),2)
round(qnorm(0.85),2)*12 + 70

#[예제 4.19]
round(pnorm(834,800,40),4) - round(pnorm(778,800,40),4)
round(pnorm(0.85),4) - round(pnorm(-.55),4)



##4.9.2. 지수분포

#[예제 4.20]
round(pexp(1,1/2),4)
round(pbinom(3,10,pexp(1,1/2)),4)




###제5장. 표본분포
##5.2. 중심극한의 정리

#[예제 5.2]
round(pbinom(7,12,0.5) - pbinom(3,12,0.5), 4)
round(pbinom(7,12,0.5),4) - round(pbinom(3,12,0.5),4)

round(pnorm(7.5,6,1.732) - pnorm(3.5,6,1.732), 4)
round(pnorm(7.5,6,1.732),4) - round(pnorm(3.5,6,1.732),4)

round(pnorm(0.87) - pnorm(-1.44), 4)
round(pnorm(0.87),4) - round(pnorm(-1.44),4)


#[예제 5.3]

round(1 - pbinom(495,900,1/2),4)
round(1 - pnorm(495,450,15),4)
round(1 - pnorm(494.5,450,15),4)



##5.3. 표본비율의 분포
#[예제 5.4]

# X ~ B(36,0.6)
# 0.5 <= hat(p)=X/n <= 0.7
36*0.5; 36*0.7           # 18 <= X <= 25.2
round(pbinom(25,36,0.6) - pbinom(17,36,0.6),4)
round(pnorm(0.7,0.6,sqrt(0.6*0.4/36)) - pnorm(0.5,0.6,sqrt(0.6*0.4/36)),4)
round(pnorm(0.7,0.6,sqrt(0.6*0.4/36)),4) - round(pnorm(0.5,0.6,sqrt(0.6*0.4/36)),4)
round(pnorm(1.22) - pnorm(-1.22),4)
round(pnorm(1.22),4) - round(pnorm(-1.22),4)



##5.4. 표본분포들

# t-분포의 그래프
par(mfrow= c(1,1))
X <- seq(-4, 4, length= 100)
Hx <- dnorm(X)

Degf <- c(1, 3, 8, 30)
Colors <- c("red", "blue", "darkgreen", "gold", "black")
Labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(X, Hx, type= "l", lty= 2, xlab= "t(or z) value",
     ylab= "Density", main= "자유도에 따른 여러가지 t-분포들")

for (i in 1:4){lines(X, dt(X, Degf[i]), lwd= 2, col= Colors[i])}

legend("topright", inset= .05, title= "Distributions",
       Labels, lwd= 2, lty= c(1, 1, 1, 1, 2), col= Colors)


#t_alpha(d.f.)
round(qt(0.05,5,lower.tail= FALSE),4)
round(qt(0.95,5),4)
round(qt(0.95,5,lower.tail= TRUE),4)


# 분위수
round(pt(2.015,5,lower.tail= FALSE),4)
round(1-pt(2.015,5),4)
round(1-pt(2.015,5,lower.tail= TRUE),4)


# 자유도 무한대의 분위수 
round(pnorm(1.645, lower.tail= FALSE),4)
round(pt(1.645, df= Inf, lower.tail= FALSE),4)
round(1-pt(1.645, df= Inf),4)
round(1-pt(1.645, df= Inf, lower.tail= TRUE),4)

round(pnorm(1.96, lower.tail= FALSE),4)
round(pt(1.96, df= Inf, lower.tail= FALSE),4)
round(1-pt(1.96, df= Inf),4)
round(1-pt(1.96, df= Inf, lower.tail= TRUE),4)


# ChiSquare-분포의 그래프
# ChiSquare-분포 자유도 5까지
par(mfrow= c(1,2))
X <- seq(0, 20, length=100)
Hx <- dchisq(X,3)

Degf <- c(1,2,4,5)
Colors1 <- c("red", "blue", "darkgreen", "gold" )
Colors2 <- c("red", "blue", "black", "darkgreen", "gold" )
Labels <- c("df=1", "df=2", "df=3", "df=4", "df=5")

plot(X, Hx, type= "l", lwd= 2, lty= 1, xlab= "X^2 value", col= "black",
     ylab= "Density", main= "자유도에 따른 ")

for (i in 1:4){lines(X, dchisq(X, Degf[i]), lwd= 2, col= Colors1[i])}

legend("topright", inset= .1, title= "Distributions",
       Labels, lwd= 2, lty= c(1, 1, 1, 1, 1), col= Colors2)


# ChiSquare-분포 자유도 5이상
X <- seq(0, 99, length=100)
Hx <- dchisq(X,5)

Degf <- c(10,20,30,40,50)
Colors <- c("red", "blue", "black", "darkgreen", "gold" )
Labels <- c("df=5", "df=10", "df=20", "df=30", "df=40")

plot(X, Hx, type= "l", lwd= 2, lty= 1, xlab= "X^2 value", col= "red",
     ylab= "Density", main= "여러가지 ChiSquare-분포들")

for (i in 2:5){lines(X, dchisq(X, Degf[i]), lwd= 2, col= Colors[i])}

legend("topright", inset= .1, title= "Distributions",
       Labels, lwd= 2, lty= c(1, 1, 1, 1, 1), col= Colors)

par(mfrow= c(1,1))


#chisq_alpha(d.f.)
round(qchisq(0.05,5,lower.tail= FALSE),2)
round(qchisq(0.95,5),2)
round(qchisq(0.95,5,lower.tail= TRUE),2)

round(qchisq(0.95,5,lower.tail= FALSE),3)
round(qchisq(0.05,5),3)
round(qchisq(0.05,5,lower.tail= TRUE),3)


# 분위수
round(pchisq(11.07,5,lower.tail= FALSE),2)
round(1-pchisq(11.07,5),2)
round(1-pchisq(11.07,5,lower.tail= TRUE),2)

round(pchisq(1.145,5,lower.tail= FALSE),2)
round(1-pchisq(1.145,5),2)
round(1-pchisq(1.145,5,lower.tail= TRUE),2)



# F-분포의 그래프
install.packages(ggplot2)
library(ggplot2)
ggplot(data.frame(X= c(0, 5)), aes(x= X)) +
        stat_function(fun= df, args= list(df1=   5, df2=  10), 
                      colour= "blue",   size= 1) +
        stat_function(fun= df, args= list(df1=  10, df2=  30), 
                      colour= "red",    size= 1) +
        stat_function(fun= df, args= list(df1=  50, df2=  50), 
                      colour= "black",  size= 1) +
        stat_function(fun= df, args= list(df1= 100, df2= 100), 
                      colour= "yellow", size= 1) +
        annotate("segment", x= 3, xend= 3.5, y=1.4, yend= 1.4, 
                 colour= "blue",   size= 1) +
        annotate("segment", x= 3, xend= 3.5, y=1.2, yend= 1.2, 
                 colour= "red",    size= 1) + 
        annotate("segment", x= 3, xend= 3.5, y=1.0, yend= 1.0, 
                 colour= "black",  size= 1) +
        annotate("segment", x= 3, xend= 3.5, y=0.8, yend= 0.8, 
                 colour= "yellow", size= 1) + 
        annotate("text", x= 4.3, y= 1.4, label= "F(df1=5, df2=10)") +
        annotate("text", x= 4.3, y= 1.2, label= "F(df1=10, df2=30)") + 
        annotate("text", x= 4.3, y= 1.0, label= "F(df1=50, df2=5)") +
        annotate("text", x= 4.3, y= 0.8, label= "F(df1=100, df2=100)") +
        ggtitle("F-분포")


#F_alpha(d.f.)
round(qf(0.05,7,9,lower.tail= FALSE),2)
round(qf(0.95,7,9),2)
round(qf(0.95,7,9,lower.tail= TRUE),2)


# 분위수
round(pf(3.29,7,9,lower.tail= FALSE),2)
round(1-pf(3.29,7,9),2)
round(1-pf(3.29,7,9,lower.tail= TRUE),2)


# F-분포의 성질
round(qf(0.05,7,9,lower.tail= FALSE),2)
round(1/qf(0.95,9,7,lower.tail= FALSE),2)

round(qf(0.95,7,9),2)
round(1/qf(0.05,9,7),2)




##6장. 추정
#6.2 구간추정
#6.2.1 모평균의 구간추정

#[예제 6.3] 모분산을 아는 경우
X <- c(175, 176, 173, 175, 174, 173, 173, 176, 173, 179)
Sd = 1.5           #기지의 표준편차
Num = length(X)    #자료의 수
Mean = mean(X)     #평균
z_0.025 = round(qnorm(0.975),2)
# 하한과 상한의 계산 
round(Mean - z_0.025*Sd/sqrt(Num),2)
round(Mean + z_0.025*Sd/sqrt(Num),2)

# 검정 함수 이용
install.packages("TeachingDemos")
library(TeachingDemos)
Z.test <- z.test(X, 175, sd= Sd, alternative= c("two.sided"))
names(Z.test)
Z.test$conf.int[1]; Z.test$conf.int[2]
Z.test$conf.int


#[예제 6.4] 모분산을 모르는 경우
X <- c(17.2, 16.9, 17.6, 18.0, 17.4, 16.3, 15.8, 17.2, 17.3, 16.0)
Num = length(X)       #자료의 수
D.F. = Num - 1        # 자유도 
Mean = mean(X)        #평균
Sd = round(sd(X),3)   #표준편차

# 하한과 상한의 계산
t_0.025 = round(qt(0.975,D.F.),3)
round(Mean - t_0.025*Sd/sqrt(Num),2)
round(Mean + t_0.025*Sd/sqrt(Num),2)

# 검정 함수 이용
T.test <- t.test(X)
names(T.test)
T.test$conf.int[1]; T.test$conf.int[2]
T.test$conf.int


##[예제 6.5] 모분산을 모르는 경우
X <- c(175, 176, 173, 175, 174, 173, 173, 176, 173, 179)
Num = length(X)      #자료의 수
D.F. = Num - 1       #자유도 
Mean = mean(X)       #평균
Sd = round(sd(X),2)  #표준편차

# 하한과 상한의 계산
t_0.025 = round(qt(0.975,D.F.),3)
round(Mean - t_0.025*Sd/sqrt(Num),1)
round(Mean + t_0.025*Sd/sqrt(Num),1)

# 검정 함수 이용
T.test <- t.test(X)
names(T.test)
T.test$conf.int[1]; T.test$conf.int[2]
T.test$conf.int




#6.2.2 모비율의 구간추정

#[예제 6.6] 
Data <- read.csv(file='6장_예제6.6.csv', header = TRUE)
attach(Data)
str(Data)
# 하한과 상한의 계산
Num = length(Lot)
Num_F = length(Lot[Lot=="F"]); 
P.hat <- Num_F/Num
z_0.025 = round(qnorm(0.975),2)
Lower <- P.hat - z_0.025*sqrt(P.hat*(1-P.hat)/Num)
if(Lower < 0){Lower = 0} else{ Lower = Lower }
Lower
Upper <- P.hat + z_0.025*sqrt(P.hat*(1-P.hat)/Num)
Upper


# 검정 함수 이용(0) : z.test
install.packages("TeachingDemos")
library(TeachingDemos)
x=0
for(i in 1:100){if(Lot[i]=="F"){X[i]=1} else{X[i]=0}}
Sd = sqrt(0.02*0.98)
z.test(X, sd = Sd)

# 검정 함수 이용(1)
Prop.test <- prop.test(Num_F, Num)
Prop.test$conf.int[1]; Prop.test$conf.int[2]
Prop.test$conf.int

# 검정 함수 이용(2)
Prop.test <- prop.test(Num_F, Num, correct = FALSE)
Prop.test$conf.int[1]; Prop.test$conf.int[2]
Prop.test$conf.int

# 검정 함수 이용(3)
Binom.test <- binom.test( c(Num_F, Num-Num_F))
Binom.test$conf.int[1]; Binom.test$conf.int[2]
Binom.test$conf.int

detach(Data)



#[예제 6.7] 
Data <- read.csv(file='6장_예제6.7.csv', header = TRUE)
attach(Data)
str(Data)

# 하한과 상한의 계산
Num = length(Preference)
Num_F = length(Preference[Preference=="favor"]); 
P.hat <- Num_F/Num
z_0.025 = round(qnorm(0.975),2)
Lower <- P.hat - z_0.025*sqrt(P.hat*(1-P.hat)/Num)
if(Lower < 0){Lower = 0} else{ Lower = Lower }
Lower
Upper <- P.hat + z_0.025*sqrt(P.hat*(1-P.hat)/Num)
Upper


# 검정 함수 이용(0) : z.test & t.test
install.packages("TeachingDemos")
library(TeachingDemos)
X=0
for(i in 1:100){if(Preference[i]=="favor"){X[i]=1} else{X[i]=0}}
Sd = sqrt(0.42*0.58)
z.test(X, sd = Sd)
t.test(X)


# 검정 함수 이용(1)
Prop.test <- prop.test(Num_F, Num)
Prop.test$conf.int[1]; Prop.test$conf.int[2]
Prop.test$conf.int

# 검정 함수 이용(2)
Prop.test <- prop.test(Num_F, Num, correct = FALSE)
Prop.test$conf.int[1]; Prop.test$conf.int[2]
Prop.test$conf.int

# 검정 함수 이용(3)
Binom.test <- binom.test( c(Num_F, Num-Num_F))
Binom.test$conf.int[1]; Binom.test$conf.int[2]
Binom.test$conf.int

detach(Data)



#6.2.3 모분산의 구간추정

#[예제 6.8] 
# 하한과 상한의 계산
X <- c(45, 47, 44, 46, 45, 47, 48)
Num = length(X)    #자료의 수
D.F. = Num - 1     #자유도 
Mean = mean(X)     #표본평균
Var = var(X)       #표본분산 
Sd = sd(X)         #표준편차

# 하한과 상한의 계산
chi_0.025 = round(qchisq(0.975,D.F.),2)
chi_0.975 = round(qchisq(0.025,D.F.),2)
round(D.F.*Var/chi_0.025,2)
round(D.F.*Var/chi_0.975,2)

# 검정 함수 이용
install.packages("TeachingDemos")
library(TeachingDemos)
Sigma.test <- sigma.test(X)
names(Sigma.test)
Sigma.test$conf.int[1]; Sigma.test$conf.int[2]
Sigma.test$conf.int



#6.2.5 모평균의 차이에 대한 구간추정

#[예제 6.14]
X_1 = c(13, 14, 9, 11, 13)
X_2 = c(9, 10, 12, 11, 8)
Num_1 = length(X_1); D.F._1 = Num_1 - 1;
Num_2 = length(X_2); D.F._2 = Num_2 - 1;
Pool.Var = (D.F._1*var(X_1)+D.F._2*var(X_2))/(D.F._1+D.F._2)      #공동분산

# 하한과 상한의 계산
t_0.025 = round(qt(0.975,D.F._1+D.F._2),3)
round((mean(X_1)-mean(X_2))-t_0.025*sqrt(Pool.Var)*sqrt(1/Num_1+1/Num_2),3)
round((mean(X_1)-mean(X_2))+t_0.025*sqrt(Pool.Var)*sqrt(1/Num_1+1/Num_2),3)

# 검정 함수 이용 - 두 모분산이 같은 경우
T.test <- t.test(X_1,X_2, var.equal = TRUE)
names(T.test)
T.test$conf.int[1]; T.test$conf.int[2]
T.test$conf.int
T.test$parameter

# 검정 함수 이용 - 두 모분산이 다른 경우
T.test <- t.test(X_1,X_2, var.equal = FALSE)
names(T.test)
T.test$conf.int[1]; T.test$conf.int[2]
T.test$conf.int
T.test$parameter


#[예제 6.15]
X_1 = c(68, 82, 80, 88)
X_2 = c(60, 74, 84, 84)
X = X_1 - X_2
Num = length(X); D.F. = Num - 1;
Var = var(X)
Sd = sd(X)

# 하한과 상한의 계산
t_0.025 = round(qt(0.975,D.F.),3)
round(mean(X)-t_0.025*Sd/sqrt(Num),3)
round(mean(X)+t_0.025*Sd/sqrt(Num),3)

# 검정 함수 이용
T.test <- t.test(X_1, X_2,paired = TRUE)
T.test$conf.int[1]; T.test$conf.int[2]
T.test$conf.int



#6.2.6 모비율의 차이에 대한 구간추정

#[예제 6.16]
Buy = c(40, 20)
Total = c(1000, 1000)
Prob = Buy/Total

# 하한과 상한의 계산
z_0.025 = round(qnorm(0.975),2)
Se = sqrt(Prob[1]*(1-Prob[1])/Total[1] + Prob[2]*(1-Prob[2])/Total[2])
round((Prob[1]-Prob[2])-z_0.025*Se ,3)
round((Prob[1]-Prob[2])+z_0.025*Se ,3)

# 검정 함수 이용(1)
Prop.test <- prop.test(Buy, Total)
Prop.test$conf.int[1]; Prop.test$conf.int[2]
Prop.test$conf.int

# 검정 함수 이용(2)
Prop.test <- prop.test(Buy, Total, correct = FALSE)
Prop.test$conf.int[1]; Prop.test$conf.int[2]
Prop.test$conf.int



#6.2.7 모분산 비율에 대한 구간추정

#[예제 6.17]
Data <- read.csv(file='6장_예제6.17.csv', header = TRUE)
attach(Data)
str(Data)

A = subset(Data, Class == "A")
B = subset(Data, Class == "B")

Num_A = length(A$Nicotin);         Num_B = length(B$Nicotin)
D.F._A = Num_A - 1;                D.F._B = Num_B - 1    
Mean_A = mean(A$Nicotin);          Mean_B = mean(B$Nicotin)
Sd_A = round(sd(A$Nicotin),1);     Sd_B = round(sd(B$Nicotin),1)


# 하한과 상한의 계산
f_0.025 = round(qf(0.975,D.F._A,D.F._B),2)
f_0.975 = round(qf(0.975,D.F._B,D.F._A),2)   # or 
#f_0.975 = round(1/qf(0.025,D.F._A,D.F._B),2)
round(Sd_A^2/Sd_B^2/f_0.025,4)
round(Sd_A^2/Sd_B^2*f_0.975,4)

# 검정 함수 이용
Var.test <- var.test(A$Nicotine, B$Nicotine)
Var.test$conf.int[1]; Var.test$conf.int[2]
Var.test$conf.int

detach(Data)




##7장. 검정
#7.2 모평균의 검정
#7.2.1 단일집단의 모평균의 검정

#[예제 7.2] 
Data <- read.csv(file='7장_예제7.2.csv', header = TRUE)
attach(Data)
mean(Tar)
sd(Tar)    #교재의 예는 0.14로 한 예임!! 
D.F. <- length(Tar)-1
Alpha = 0.05
T.test <- t.test(Data$Tar, mu=4, alternative = "less")
T.test
t_0.95 = round(qt(Alpha, D.F.),3)
if (T.test$statistic < t_0.95) {"Reject Ho"} else{"Ho:  Accpet"}
if (T.test$p.value < Alpha) {"Reject Ho"} else{"Ho:  Accpet"}


#7.2.2 두 집단의 모평균의 동일성에 대한 검정

#[예제 7.5] 
New <- c(15, 10, 13, 7, 9 , 8, 21, 9, 14, 8)
Placebo <- c(15, 14, 12, 8, 14, 7, 16, 10, 15, 12)
t.test(New, Placebo, var.equal = TRUE)


#[예제 7.6] 
New <- c(15, 10, 13, 7, 9 , 8, 21, 9, 14, 8)
Placebo <- c(15, 14, 12, 8, 14, 7, 16, 10, 15, 12)
t.test(New, Placebo, var.equal = FALSE)


#[예제 7.7] 
X <- c(50, 45, 55, 60, 63, 48, 50, 70, 56, 44)
Y <- c(48, 45, 50, 60, 60, 50, 49, 60, 55, 48)
t.test(X, Y, alternative= "greater", paired= TRUE)




#7.3 모비율의 검정
#7.3.1 단일집단의 모비율에 대한 검정

#[예제 7.8] 
Data <- read.csv(file='7장_예제7.8.csv', header = TRUE)
attach(Data)
str(Data)
# 기초통계량 계산
Num = length(Product)
Num_D = length(Product[Product=="D"]); 

# 검정 함수 이용(0) : z.test 
install.packages("TeachingDemos")
library(TeachingDemos)
for(i in 1:100){if(Product[i]=="D"){X[i]=1} else{X[i]=0}}
Sd = sqrt(0.05*0.95)
z.test(X, 0.05, sd = Sd, alternative = "less")

# 검정 함수 이용(1)
prop.test(Num_D, Num, p=0.05, alternative = "less")

# 검정 함수 이용(2)
prop.test(Num_D, Num, p=0.05, alternative = "less", correct = FALSE)

# 검정 함수 이용(3)
binom.test( c(Num_D, Num-Num_D), p=0.05, alternative = "less")

detach(Data)





#7.3.2 두 집단의 모비율의 동일성에 대한 검정

#[예제 7.9] 
Data <- read.csv(file='7장_예제7.9.csv', header = TRUE)
attach(Data)
str(Data)

A=0; B=0;
for(i in 1:100){if(ProductA[i]=="D"){A[i]=1} else{A[i]=0}}
for(i in 1:100){if(ProductB[i]=="D"){B[i]=1} else{B[i]=0}}

sum(A); sum(B)
Defect = c(sum(A), sum(B))
Total = c(length(A), length(B))
Prob = Defect/Total

# 검정 함수 이용(1)
prop.test(Defect, Total, correct = FALSE)

# 검정 함수 이용(2)
prop.test(Defect, Total)

detach(Data)



#7.4. 모분산의 검정
#7.4.1. 단일집단의 모분산의 검정

#[예제 7.10] 
Data <- read.csv(file='7장_예제7.10.csv', header = TRUE)
attach(Data)
str(Data)

install.packages("TeachingDemos")
library(TeachingDemos)
Num= length(Data$IQ)
D.F.= Num - 1
SigmaSq= 225
Alpha = 0.05
Sigma.test <- sigma.test(IQ, sigmasq= 225, alternative= c("two.sided"))
Sigma.test
names(Sigma.test)
Sigma.test$statistic    # 검정통계량의 값

# 기각 기준값의 하한과 상한
Chi_Top= round(qchisq(1-Alpha/2, D.F.),2)
Chi_Bottom= round(qchisq(Alpha/2, D.F.),2)

if ( Sigma.test$statistic > Chi_Bottom & Sigma.test$statistic < Chi_Top ) 
{ cat('\n 귀무가설 Ho 채택 !!')} else{ cat('\n 귀무가설 Ho 기각!!')}

detach(Data)




#7.4.2. 두 집단의 모분산의 동일성에 대한 검정

#[예제 7.12] 
New <- c(15, 10, 13, 7, 9 , 8, 21, 9, 14, 8)
Placebo <- c(15, 14, 12, 8, 14, 7, 16, 10, 15, 12)

Num1= length(New);       D.F.1= Num1 - 1
Num2= length(Placebo);   D.F.2= Num2 - 1
Alpha= 0.05
Var.test= var.test(New, Placebo, alternative = c("two.sided"))
Var.test

# 기각 기준값의 하한과 상한
F_Top= round(qf(1-Alpha/2, D.F.1, D.F.2),2)
F_Bottom= round(qf(Alpha/2, D.F.1, D.F.2),2)

if ( Var.test$statistic > F_Bottom & Var.test$statistic < F_Top ) 
{ cat('\n 귀무가설 Ho 채택 !!')} else{ cat('\n 귀무가설 Ho 기각!!')}

                   


##8장. 상관분석
#[표 8.1]의 처리
Data <- read.csv(file='8장_[표8.1].csv', header = TRUE)
attach(Data)
str(Data)

length(Age); length(Weight);   length(Time)
mean(Age);   mean(Weight);     mean(Time)
sd(Age);     sd(Weight);       sd(Time)
sum(Age);    sum(Weight);      sum(Time)
min(Age);    min(Weight);      min(Time)
max(Age);    max(Weight);      max(Time)

par(mfrow=c(1,3))
plot(Age, Weight, xlab="나이", ylab="몸무게" )
plot(Age, Time, xlab="나이", ylab="소요시간" )
plot(Time, Weight, xlab="쇼요시간", ylab="몸무게" )

detach(Data)


#[예제 8.1]
Weight <- c(83, 67, 92, 38, 95, 75, 70, 74, 100, 73)
Height <- c(170, 168, 170, 142, 152, 145, 150, 163, 145, 163)
plot(Height, Weight, xlab="키", ylab="몸무게")
cor(Height, Weight)


#[예제 8.2]
Temp <- c(13.9, 31.1, 11.7, 23.9, 32.2, 12.8, 15, 7.8, 17.2, 21.1)
Rain <- c(796, 525, 864, 770, 948, 804, 897, 1118, 762, 838)
plot(Temp, Rain, xlab="온도", ylab="강수량")
cor(Temp, Rain)


#[예제 8.3]
X <- c(50, 70, 70, 40,90, 70, 60, 50, 80, 60)
Y <- c(60, 60, 80, 50, 80, 60, 60, 60, 80, 70)
plot(X, Y, xlab="통계학 점수", ylab="영어 점수")

Num= length(X)
D.F.= Num-2
Alpha = 0.05
Cor.test <- cor.test(X,Y,alternative= c("two.sided"))
Cor.test
names(Cor.test)
Cor.test$statistic    # 검정통계량의 값


# 기각 기준값의 하한과 상한
T_Top= round(qt(1-Alpha/2, D.F.),4)
T_Bottom= round(qt(Alpha/2, D.F.),4)

if ( Cor.test$statistic > T_Bottom & Cor.test$statistic < T_Top ) 
{ cat('\n 귀무가설 Ho 채택 !!')} else{ cat('\n 귀무가설 Ho 기각!!')}




##9장. 회귀분석
#9.1 회귀의 이해

#[예제 9.1]
Year <- c(96, 97, 98, 99, 00, 01)
Outcome <- c(16, 20, 27, 39, 56, 63)
Income <- c(2, 3, 4, 7, 11, 11)
plot(Outcome, Income, xlab="수출액", ylab="수입액")

Num= length(Year)
D.F.= Num-2
Alpha = 0.01
Cor.test <- cor.test(Outcome, Income, alternative= c("two.sided"))
Cor.test
names(Cor.test)
Cor.test$statistic    # 검정통계량의 값  - 자리수 반올림에 따라 차이 발생!! 

# 기각 기준값의 하한과 상한
T_Top= round(qt(1-Alpha/2, D.F.),3)
T_Bottom= round(qt(Alpha/2, D.F.),3)

if ( Cor.test$statistic > T_Bottom & Cor.test$statistic < T_Top ) 
{ cat('\n 귀무가설 Ho 채택 !!')} else{ cat('\n 귀무가설 Ho 기각!!')}

Reg = lm(Income ~ Outcome)   #선형회귀
names(Reg)
abline(Reg)



#[예제 9.2]
X <- c(1, 2, 3, 6, 8, 10)
Y <- c(1800, 1400, 1300, 1000, 600, 500)
plot(X, Y)

Reg = lm(Y ~ X)   #선형회귀
names(Reg)
abline(Reg)


#[예제 9.3]
Data <- read.csv(file='9장_예제9.3.csv', header = TRUE)
attach(Data)
str(Data)

Lm <- lm(Weight ~ Height, Data)
Lm

plot(Height, Weight, main = "자료와 회귀직선" )
abline(coef(Lm))

summary(Lm)
anova(Lm)

deviance(Lm)    # 또는 sum((Weight - predict(Lm))^2)

str(anova(Lm))
anova(Lm)$Df            # Degree of Freedom
anova(Lm)$Sum           # SSR SSE  
anova(Lm)$Mean          # MSR MSE
anova(Lm)$F             # f-value
anova(Lm)$Pr            # p-value

confint(Lm)[2,]        # 또는 confint(Lm, level = 0.95)

detach(Data)



#[예제 9.4]
Age <- c( 18, 23, 25, 35, 65, 54, 34, 56, 72, 19, 23, 42, 18, 39, 37)
Max <- c(202,186,187,180,156,169,174,172,153,199,193,174,198,183,178)

Data <- data.frame(Age, Max)

MaxAge <- lm(Max ~ Age, Data)
MaxAge

plot(Age, Max, main = "자료와 회귀직선" )
abline(coef(MaxAge))

summary(MaxAge)
anova(MaxAge)

deviance(MaxAge)       # 또는 
sum((Max - predict(MaxAge))^2)

str(anova(MaxAge))

anova(MaxAge)$Df            # Degree of Freedom
anova(MaxAge)$Sum           # SSR SSE  
anova(MaxAge)$Mean          # MSR MSE
anova(MaxAge)$F             # f-value
anova(MaxAge)$Pr            # p-value

confint(MaxAge)        # 또는 confint(MaxAge, level = 0.95)

fitted.values(MaxAge)  # 또는 fitted(MaxAge)
residuals(MaxAge)

# 교재와 비교 위해 level= 0.90 사용
predict(MaxAge)        #  또는 predict(MaxAge, interval= "none")  
predict(MaxAge, interval= "confidence", level= 0.90)     #  E(y)
predict(MaxAge, interval= "prediction", level= 0.90)     #  y_p  


predict(MaxAge, data.frame(Age= 25))
predict(MaxAge, data.frame(Age= 25), interval= "confidence", level= 0.90)
predict(MaxAge, data.frame(Age= 25), interval= "prediction", level= 0.90)

Age <- seq(min(Age), max(Age), 0.1)
Pc <- predict(MaxAge, data.frame(Age), interval= "confidence", level= 0.90)
Pp <- predict(MaxAge, data.frame(Age), interval= "prediction", level= 0.90)
matplot(Age, Pc, ylab= "Max", main = " 예측값의 신뢰구간[ E(y) 및 y_p ]",
        type= 'l', lty= 1, col= "black")
matlines(Age, Pp, type= 'l', lty= 2, col= c("black", "red", "red"))

Opar <- par(mfrow= c(2,3))
plot(MaxAge, which= c(1:6), las= 1) 
par(Opar)

##las(the style of axis labels) : 축 라벨 방향 옵션
# 0: 축과 언제나 평행[기본 옵션] 
# 1: 언제나 가로 방향 
# 2: 언제나 축과 직각 방향 
# 3: 언제나 세로 방향

plot(MaxAge, which= c(1), las= 0) 
#1 Residuals vs Fitted 
#   * X-축 : 선형 회귀로 예측된 Y 값, Y-축 : 잔차.
#   * 선형회귀에서의 가정 :  오차의 평균 0, 분산이 일정한 정규 분포
#     => 예측된 Y 값과 무관하게 잔차의 평균은 0, 분산은 일정해
#     => 이 그래프에서는 기울기 0인 직선이 관측되는 것이 이상적.

plot(MaxAge, which= c(2), las= 2) 
#2 Normal Q-Q 
#   * 잔차가 정규분포를 따르는지 확인하기 위한 QQ-Plot

plot(MaxAge, which= c(3), las= 3) 
#3 Scale-Location
#    * X-축 : 선형회귀로 예측된 Y 값, Y-축 : 표준화잔차(Standardized Residual)
#      * 이 경우도 기울기가 0인 직선이 이상적이다. 
#      * 특정위치에서 0에서 멀리 떨어진 값이 관찰  
#         => 표준화 잔차가 크다
#         => i.e. 회귀직선이 해당 Y를 잘 적합하지 못한다는 의미
#         => 이런 점들은 이상치 outlier일 가능성이 높다

plot(MaxAge, which= c(4), las= 1) 
#4 Cook's distance(회귀직선의 모양(기울기나 절편 등)에 키치는 영향력)
#  관측값의 순서별 쿡의 거리
#    * 쿡의 거리 : 회귀 직선의 모양(기울기/절편등)에 크게 영향을 끼치는 점들을 찾는 방법

plot(MaxAge, which= c(5), las= 1) 
#5 Residuals vs Leverage
#  * X-축 :레버리지Leverage, Y-축: 표준화 잔차
#  * 우측 상단/하단에 선으로 쿡의 거리(Cook’s Distance)가 표시
#    * 쿡의 거리 : 레버리지와 잔차에 비례 
#                  i.e. 두 값이 크다면 우측상단과 우측하단에 위치
#    * 레버리지: 설명변수가 얼마나 극단에 치우쳐 있는지를 의미
#                ex) 특정 데이터만 유독 다르다면 해당 데이터의 레버리지는 큰 값.
#                   => 이런 데이터는 입력이 잘못되었거나, 
#                   => 해당 범위의 설명변수값을 가지는 자료를 보충해야 하는 작업 등 필요

plot(MaxAge, which= c(6), las= 1) 
#6 Cook's dist vs Leverage W/ h_ii/(1-h_ii)

detach(Data)




##10장. 분산분석
#10.2 일원분류 분산분석

#[예제 10.1]
Data <- read.csv(file='10장_예제10.1.csv', header = TRUE)
attach(Data)
str(Data)

boxplot(Score ~ Type, horizontal= TRUE)

## 등분산 가정 없이
oneway.test(Score ~ Type)

## 등분사 가정하에서
oneway.test(Score ~ Type, var.equal= TRUE)

## 등분산 가정과 같은 결과
anova(lm(Score ~ Type))

AovOut= aov(Score ~ Type, data= Data)
summary(AovOut)

detach(Data)


#[예제 10.2]
Data <- read.csv(file='10장_예제10.2.csv', header = TRUE)
attach(Data)
str(Data)

boxplot(Conentration ~ Treatment, horizontal= TRUE)

## 등분산 가정 없이
oneway.test(Conentration ~ Treatment)

## 등분사 가정하에서
oneway.test(Conentration ~ Treatment, var.equal= TRUE)

## 등분산 가정과 같은 결과
anova(lm(Conentration ~ Treatment))

AovOut= aov(Conentration ~ Treatment, data= Data)
summary(AovOut)

detach(Data)



#10.3. 이원분류 분산분석
#10.3.1. 반복측정이 없는 경우

#[예제 10.3]
Product= c(42.8, 38.6, 50.2, 48.2, 52.3, 43.5, 
           58.7, 50.8, 48.2, 40.3, 53.5, 51.2 )
Region= factor(rep(rep(1:4, c(1,1,1,1)), 3))
Fertile= factor(rep(1:3, c(4,4,4)))
anova(lm(Product ~ Region + Fertile))
aov(Product ~ Region + Fertile)
summary(aov(Product ~ Region + Fertile))


#[예제 10.4]
Time= c(8.4, 12.8, 9.6, 9.8, 8.4, 8.6, 8.9, 7.9,
           9.4, 15.2, 9.1, 8.8, 8.2, 9.9, 9.0, 8.1,
           9.8, 12.9, 11.2, 9.9, 8.5, 9.8, 9.2, 8.2, 
           12.2, 14.4, 9.8, 12.0, 8.5, 10.9, 10.4, 10.0)
Experimenter= factor(rep(rep(1:8, c(1,1,1,1,1,1,1,1)), 4))
Treatment= factor(rep(1:4, c(8,8,8,8)))
anova(lm(Time ~ Experimenter + Treatment))
aov(Time ~ Experimenter + Treatment)
summary(aov(Time ~ Experimenter + Treatment))


#10.3.1. 반복측정이 없는 경우

#[예제 10.5]
Sell= c( 23, 20, 21, 22, 19, 20, 19, 18, 21, 
         22, 20, 19, 20, 21, 22, 20, 19, 22,
         18, 18, 16, 21, 23, 20, 20, 22, 24)
Design= factor(rep( rep(1:3, c(3,3,3)), 3))
Size= factor(rep( 1:3, c(9,9,9)))
anova(lm(Sell ~ Size * Design))
summary(aov(Sell ~ Size * Design))




##11장. 범주형 자료 분석
#11.2 다항 실험

#[예제 11.2]
X <- c(A = 95, B = 62, C = 83)
X
Xsq <- chisq.test(X)   # 또는    Xsq <- chisq.test(as.table(X))
Xsq
names(Xsq)
Xsq$observed            # 관찰 도수
Xsq$expected            # 귀무가설하에서의 기대 도수
Xsq$residuals           # Pearson's 잔차
Xsq$statistic
sum(Xsq$residuals^2)    # 검정 통계량 값
Xsq$stdres              # 표준화 잔차


#[예제 11.3]
X <- c(R= 64, W= 14, G=38, B=49, BR=6, Y=29)
Prob <- c(30, 10, 15, 25, 5, 15)
chisq.test(X, p= Prob, rescale.p= TRUE)
#또는  chisq.test(as.table(X), p= Prob, rescale.p= TRUE)


#[예제 11.4]
X <- c(22, 21, 22, 27, 22, 36)
Prob <- c(1, 1, 1, 1, 1, 1)
chisq.test(X, p= Prob, rescale.p= TRUE)
#또는  chisq.test(as.table(X), p= Prob, rescale.p= TRUE)


#[예제 11.5]
X <- c(100, 110, 80, 55, 14)
Prob <- c(29, 21, 17, 17, 16)
chisq.test(X, p= Prob, rescale.p= TRUE)
#또는  chisq.test(as.table(X), p= Prob, rescale.p= TRUE)


#[예제 11.6]
Job = matrix( c(8, 18, 11, 10, 16, 12), nrow=2,
              dimnames = list ( "시행" = c("Male", "Female"),     
                                "결과" = c("Unemploy", "Employ", "Condㄷmploy")))
Job
Chisqt.test <- chisq.test(Job)
Chisqt.test
names(Chisqt.test)

Chisqt.test$statistic
Chisqt.test$observed
Chisqt.test$expected


#[예제 11.7]
Obs = matrix( c(12813, 65963, 647, 4000, 359, 2642, 42, 303), nrow= 2,
              dimnames = list ( "안전벨트" = c("착용", "미착용"),
                                "부상정도" = c("부상없음", "경상", "중경상", "중상"))) 
Obs
Chisqt.test <- chisq.test(Obs)
Chisqt.test
names(Chisqt.test)

Chisqt.test$statistic
Chisqt.test$observed
Chisqt.test$expected


#[예제 11.8]
Obs = matrix( c(65, 59, 48, 43, 53, 76, 42, 49), nrow= 4,
              dimnames = list ( "Region" = c("서울", "부산", "광주", "대전"),
                                "Can" = c("CanA", "CanB"))) 
Obs
Chisqt.test <- chisq.test(Obs)
Chisqt.test
names(Chisqt.test)

Chisqt.test$statistic
Chisqt.test$observed
Chisqt.test$expected


#[예제 11.9]
Dices = matrix( c(38, 12, 26, 4, 26, 17, 34, 17, 31, 18, 45, 32), nrow=2,
                dimnames = list ( "시행" = c("시행 1", "시행 2"),     
                                  "결과" = c("1", "2", "3", "4", "5", "6")))
Dices
Chisqt.test <- chisq.test(Dices)
Chisqt.test
names(Chisqt.test)

Chisqt.test$statistic
Chisqt.test$observed
Chisqt.test$expected


#[예제 11.10]
Obs = matrix( c(120, 10, 10, 30, 75, 30, 50, 15, 60), nrow= 3,
              dimnames = list ( "Age" = c("30세 이하", "30~44", "45세 이상"),
                                "TvProgram" = c("Ch_A", "Ch_B", "Ch_C"))) 
Obs
Chisqt.test <- chisq.test(Obs)
Chisqt.test
names(Chisqt.test)

Chisqt.test$statistic
Chisqt.test$observed
Chisqt.test$expected




##12장. 비모수통계학
#12.2 부호 검정

#[예제 12.1]
PreIQ= c(96, 110, 98, 113, 88, 92, 106, 119, 100, 97)
PostIQ= c(99, 112, 107, 110, 88, 101, 107, 123, 91, 99)
Diff= PostIQ - PreIQ

Num= length(PreIQ)
Zero= sum(Diff==0)
Size= Num - Zero
T <- sum(Diff>0)
P_value <- pbinom(T-1, size= Size, prob=0.5, lower.tail= FALSE)

install.packages("BSDA")
library(BSDA)
BSDA::SIGN.test(Diff, md = 0, alternative = "greater")


#[예제 12.2]
Data <- read.csv(file='12장_예제12.2.csv', header = TRUE)
attach(Data)
str(Data)

Size= length(IQ)
Num <- sum(IQ<100)
P_value <- pbinom(Num-1, size= Size, prob=0.5, lower.tail= FALSE)
P_value 

#install.packages("BSDA")
library(BSDA)
BSDA::SIGN.test(IQ, md = 100, alternative = "less")

detach(Data)



#12.3 윌콕슨의 부호순위검정

#[예제 12.3]
Data <- read.csv(file='12장_예제12.3.csv', header = TRUE)
attach(Data)
str(Data)

wilcox.test(DrinkA, DrinkB, paired= TRUE, alternative= "greater")   

detach(Data)



#12.4 윌콕슨의 순위합 검정

#[예제 12.4]
A <- c(3, 5)
B <- c(1, 2, 4)
SumRankX= sum(rank(c(A,B))[1:2]) 
SumRankX
SumRankX-(length(A)*(length(A)+1))/2
wilcox.test(A, B, alternative= "greater")
wilcox.test(A, B, exact= TRUE, correct= TRUE, alternative= "greater")
wilcox.test(A, B, exact= FALSE, correct= TRUE, alternative= "greater")



#[예제 12.5]
A <- c(4.7, 6.4, 4.1, 3.7, 3.9)
B <- c(7.6, 11.1, 6.8, 9.8, 4.9, 6.1, 15.1)
SumRankX= sum(rank(c(A,B))[1:5]) 
SumRankX
SumRankX-(length(A)*(length(A)+1))/2
wilcox.test(A, B, alternative= "less")
wilcox.test(A, B, exact= TRUE, correct= TRUE, alternative= "less")
wilcox.test(A, B, exact= FALSE, correct= TRUE, alternative= "less")



#[예제 12.6]
Data <- read.csv(file='12장_예제12.6.csv', header = TRUE)
attach(Data)
str(Data)

SumRankX= sum(rank(c(A,B))[1:11]) 
SumRankX
SumRankX-(length(A)*(length(A)+1))/2
wilcox.test(A, B)   
wilcox.test(A, B, correct= FALSE)
wilcox.test(A, B, exact= FALSE, correct= FALSE )

detach(Data)



#12.5 런 검정

#[예제 12.7]
install.packages("DescTools")
library(DescTools)
Order = c("m", "m", "m", "w", "m", "m", "m", "m", "w", "w", "w", "m")
RunsTest(Order)


#[예제 12.8]
#install.packages("DescTools")
library(DescTools)
Data <- read.csv(file='12장_예제12.8.csv', header = TRUE)
attach(Data)
str(Data)

RunsTest(DVD)

detach(Data)

