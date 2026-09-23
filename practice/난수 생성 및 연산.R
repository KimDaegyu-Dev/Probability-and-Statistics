#===================================
# 정규분포로부터의 난수1
mu1 = 5; sigma1 = 5;
norm_1 = rnorm(1000, mu1, sigma1)
mean(norm_1)
var(norm_1)


# 정규분포로부터의 난수2
mu2 = 5; sigma2 = 5;
norm_2 = rnorm(1000, mu2, sigma2)
mean(norm_2)
var(norm_2)

Sum = norm_1 + norm_2#X+Y
mean(Sum)
var(Sum)

Diff = norm_1 - norm_2#X-Y
mean(Diff)
var(Diff)

Square = norm_1 * norm_1#X^2
mean(Square)
var(Square)

XmulY = norm_1 * norm_2#X*Y
mean(XmulY)
var(XmulY)

XdivY = norm_1 / norm_2
mean(XdivY)
var(XdivY)

# Sum_square = norm_1 * norm_1 + norm_2 * norm_2#X^2+Y^2
# mean(Sum_square)
# var(Sum_square)




par( mfrow=c(1,1) )
 
hist(norm_1,freq = FALSE,breaks=20,col="gray",xlab="",main="정규분포 난수X")
lines(density(norm_1),col="blue", lty=1, lwd =2)

hist(norm_2,freq = FALSE,breaks=20,col="gray",xlab="",main="정규분포 난수Y")
lines(density(norm_2),col="blue", lty=1, lwd =2)

hist(Sum,freq = FALSE,breaks=20,col="gray",xlab="",main="정규분포 합(X+Y)")
lines(density(Sum),col="blue", lty=1, lwd =2)

hist(Diff,freq = FALSE,breaks=20,col="gray",xlab="",main="정규분포 차(X-Y)")
lines(density(Diff),col="blue", lty=1, lwd =2)

hist(Square,freq = FALSE,breaks=20,col="gray",xlab="",main="정규분포 제곱(X^2)")
lines(density(Square),col="blue", lty=1, lwd =2)

hist(XmulY,freq = FALSE,breaks=20,col="gray",xlab="",main="정규분포 X*Y")
lines(density(Sum_square),col="blue", lty=1, lwd =2)

hist(XdivY,freq = FALSE,breaks=20,col="gray",xlab="",main="정규분포 X/Y")
lines(density(Sum_square),col="blue", lty=1, lwd =2)




##==========================================

Bi1 = rbinom(1000, 10, 0.5)#이항분포
mean(Bi1)
var(Bi1)

Bi2 = rbinom(1000, 100, 0.05)#이항분포
mean(Bi2)
var(Bi2)

Bi3 = rbinom(1000, 1000, 0.005)
mean(Bi3)
var(Bi3)

Po = rpois(1000,5)#poisson distribution
mean(Po)
var(Po)

par( mfrow=c(2,2) )
hist(Bi1,freq = FALSE,col="gray",xlab="",main="이항분포(10, 0.5)")
lines(density(Bi1),col="blue", lty=1, lwd =3)

hist(Bi2,freq = FALSE,breaks=10,col="gray",xlab="",main="이항분포(100, 0.05)")
lines(density(Bi2),col="blue", lty=1, lwd =3)

hist(Bi3,freq = FALSE,breaks=20,col="gray",xlab="",main="이항분포(1000, 0.005)")
lines(density(Bi3),col="blue",lty=1, lwd =3)

hist(Po,freq = FALSE,breaks=10,col="gray",xlab="",main="포아송 분포(5)")
lines(density(Po),col="blue", lty=1, lwd =3)



###===============================================

mu=0; sigma=1;  Df=1
X =rnorm(1000,mu,sigma)#정규분포
mean(X)
var(X)

Y = rchisq(1000, Df)
mean(Y)
var(Y)

par( mfrow=c(1,2) )
hist(X*X,freq = FALSE,col="gray",breaks=20,xlab="",main="정규분포의 제곱(Y=X^2)")
lines(density(X*X),col="blue", lty=1, lwd =3)

hist(Y,freq = FALSE,col="gray",breaks=20,xlab="",main="Chi-Square(자유도=1)")
lines(density(Y),col="blue", lty=1, lwd =3)

####=======================================================================

Df1=25; Df2=20
X = rchisq(1000, Df1)
mean(X)
var(X)

Y = rchisq(1000, Df2)
mean(Y)
var(Y)

F1 = (X/Df1)/(Y/Df2)
mean(F1)
var(F1)

F2 = rf(1000, Df1 ,Df2)
mean(F2)
var(F2)


par( mfrow=c(1,1) )
hist(F1,freq = FALSE,col="gray",breaks=20,xlab="",main="Chi-square 분포의 비(F=(X/Df1)/(Y/Df2)")
lines(density(F1),col="blue", lty=1, lwd =3)

hist(F2,freq = FALSE,col="gray",breaks=20,xlab="",main="F분포")
lines(density(F2),col="blue", lty=1, lwd =3)





















