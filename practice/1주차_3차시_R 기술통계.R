# 작업 디렉토리 변경(각자의 환경에 따라 다르므로 유의)
Data <- read.csv('1주차_3차시_R 기술통계.csv')
Data
str(Data)

# 평균과 분산 구하기
Mean_D <- mean(Data$RegionD)
Var_D <- var(Data$RegionD)

Mean_D
Var_D

min(Data$RegionD)
max(Data$RegionD)

Bottom= round(min(Data$RegionD), digits= 0)
Bottom

Top = round(max(Data$RegionD), digits= 0)
Top


# 도수분포표 작성
table(Data$RegionD)    # ???????


Boundaries <- seq(Bottom-5, Top+5, 5)
Boundaries

table(cut(Data$RegionD, Boundaries))


# 상대도수분포표 작성
Length <- length(Data$RegionD)
Length

table(cut(Data$RegionD, Boundaries)) / Length


# 히스토그램 작성
hist(Data$RegionD)

hist(Data$RegionD, prob= TRUE)

hist(Data$RegionA, breaks= Boundaries, prob= T, 
     main= "상대도수 히스토그램", ylab= "상대도수", xlab= "D지역")


# 상자그림 작성
boxplot(Data$RegionD)

boxplot(Data)