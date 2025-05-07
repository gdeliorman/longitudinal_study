##from the sas results

##for fin 1 bprs (setting 1) seperate
var_placebo<- matrix(c(83.1577, 121.63, 121.63, 208.64), nrow=2, ncol=2)
var_exp<- matrix(c(133.66, 186.73,186.73, 307.02 ), nrow=2, ncol=2)

##for fin 1 bprs (setting 1) joint
var_joint<- matrix(c(92.0857, NA,134.17, NA,
                     NA, 106.16, NA, 151.52,
                     134.17, NA,229.21, NA,
                     NA, 151.52, NA, 252.57), nrow=4, ncol=4)

Sigma<- var_joint
corr<- cov2cor(var_joint)


##for fin 1 bprs (setting 2 3 trials bprs) joint
var_joint<- matrix(c(156.39, NA,245.3, NA,
                     NA, 146.90, NA,233.52 ,
                     245.3, NA,440.51, NA,
                     NA, 233.52, NA, 422.73), nrow=4, ncol=4)

Sigma<- var_joint
corr<- cov2cor(var_joint)

##for fin 1 cgi (setting 2)
var_placebo<- matrix(c(83.1577, 121.63, 121.63, 208.64), nrow=2, ncol=2)
var_exp<- matrix(c(162.99, 221.84,221.84, 354.38 ), nrow=2, ncol=2)

##for 3-trials bprs(setting 3)
var_placebo<- matrix(c(158.58,248.70, 248.70, 446.60), nrow=2, ncol=2)
var_exp<- matrix(c(144.79, 230.24,230.24, 416.94 ), nrow=2, ncol=2)

##for 3-trials bprs(setting 3) joint
# var_joint<- matrix(c(158.58,248.70, 248.70, 446.60), nrow=2, ncol=2)
# var_exp<- matrix(c(144.79, 230.24,230.24, 416.94 ), nrow=2, ncol=2)

##for 3-trials cgi (setting 4)
var_placebo<- matrix(c(158.58,248.70, 248.70, 446.60), nrow=2, ncol=2)
var_exp<- matrix(c(144.79, 230.24,230.24, 416.94 ), nrow=2, ncol=2)


##estimable sigma
Sigma<- matrix(c( var_placebo[1,1], NA, var_placebo[2,1],NA,
                  NA, var_exp[1,1], NA, var_exp[1,2],
                  var_placebo[1,2], NA, var_placebo[2,2], NA,
                  NA, var_exp[1,2], NA, var_exp[2,2]), nrow=4, ncol=4)

##estimable corr
corr<- matrix(c( 1, NA, (var_placebo[2,1]/sqrt(var_placebo[1,1]*var_placebo[2,2])),NA,
                 NA, 1, NA, var_exp[2,1]/(sqrt(var_exp[1,1])*sqrt(var_exp[2,2])),
                 (var_placebo[2,1]/sqrt(var_placebo[1,1]*var_placebo[2,2])), NA, 1, NA,
                 NA,  var_exp[2,1]/(sqrt(var_exp[1,1])*sqrt(var_exp[2,2])), NA, 1), nrow=4, ncol=4)

corr<- cov2cor(Sigma)
library(Surrogate)
ICA<- ICA.ContCont(T0S0=corr[1,3], T1S1=corr[2,4], T0T0=Sigma[1,1], T1T1=Sigma[2,2], S0S0=Sigma[3,3], S1S1=Sigma[4,4], T0T1=seq(-1, 1, by=.1), 
                   T0S1=seq(-1, 1, by=.1), T1S0=seq(-1, 1, by=.1), S0S1=seq(-1, 1, by=.1))


ICA_joint<- ICA.ContCont(T0S0=corr[1,3], T1S1=corr[2,4], T0T0=Sigma[1,1], T1T1=Sigma[2,2], S0S0=Sigma[3,3], S1S1=Sigma[4,4], T0T1=seq(-1, 1, by=.1), 
                         T0S1=seq(-1, 1, by=.1), T1S0=seq(-1, 1, by=.1), S0S1=seq(-1, 1, by=.1))



ICA<- ICA_joint
rho<- ICA$ICA
summary(rho)
summary(rho^2)
#p<-5 ##for fin (settin 1 and 2)
#p<-6 #(0,1,2,4, and 6, 8 ) for 3 trials setting (3-4)
RH_square<- 1-(1-rho^2)^p
summary(RH_square)
min(RH_square)
quantile(RH_square)
mean(RH_square)
median(RH_square)


##fin 1 setting 1 
hist(RH_square, main="", xlab=bquote("R"[Lambda]^2),  breaks = 15, labels = TRUE, ylim = c(0,1600), xlim = c(0,1))
grid()
box()

margin<- qt(0.95,df=length(RH_square)-1)*sd(RH_square)/sqrt(length(RH_square))
lowerinterval <- mean(RH_square) - margin
upperinterval <- mean(RH_square) + margin

hist(RH_square, main="", xlab=bquote("R"[Lambda]^2),  breaks = 15, labels = TRUE, ylim = c(0,1100), xlim = c(0,1))
grid()
box()

hist(RH_square, main="", xlab=bquote("R"[Lambda]^2),  breaks = 15,prob = TRUE)
hist(RH_square, main=bquote("Histogram of" ~"R"[Lambda]^2), xlab=bquote("R"[Lambda]^2),  breaks = 15, labels = TRUE, ylim = c(0,3500))
hist(RH_square, main="", xlab=bquote("R"[Lambda]^2),  breaks = 15, labels = TRUE, ylim = c(0,1200))
hist(RH_square, main="", xlab=bquote("R"[Lambda]^2),  breaks = 15, labels = TRUE, ylim = c(0,1600))

grid()
box()
summary(RH_square)


##FOR i=1

corr[1,2]<- corr[2,1]<- corr[4,1]<- corr[1,4]<- corr[2,3]<- corr[3,2] <- corr[4,3] <- corr[3,4]<- -0.9
Sigma[1,2]<- Sigma[2,1] <- sqrt(Sigma[1,1]*Sigma[2,2])*-0.9
Sigma[1,4]<- Sigma[4,1] <- sqrt(Sigma[1,1]*Sigma[4,4])*-0.9
Sigma[2,3]<- Sigma[3,2] <- sqrt(Sigma[2,2]*Sigma[3,3])*-0.9
Sigma[4,3]<- Sigma[3,4] <- sqrt(Sigma[4,4]*Sigma[3,3])*-0.9

library(matrixcalc)

is.positive.definite(Sigma)
##double check the formula using joint model 
rho<- 0.6921
RH_square[1]
first_corr<- ICA$Pos.Def[1,]
u_11<- as.numeric(Sigma[1,1]+ Sigma[2,2]- 2*first_corr[1]* sqrt(Sigma[1,1]*Sigma[2,2]))
u_22<- as.numeric(Sigma[3,3]+ Sigma[4,4]-2*first_corr[6]* sqrt(Sigma[3,3]*Sigma[4,4]))
u_12<- as.numeric(Sigma[1,3]+ Sigma[2,4]- first_corr[4]* sqrt(Sigma[2,2]*Sigma[3,3])- first_corr[3]* sqrt(Sigma[1,1]*Sigma[4,4]) )

delta_u<- matrix(c(u_11, u_12,u_12,u_22), nrow=2, ncol=2)
ar1_cor <- function(n, rho) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - 
                    (1:n - 1))
  rho^exponent
}
r<-as.matrix(ar1_cor(5, rho), nrow=5, ncol=5)
sigma_delta<- kronecker(delta_u,r)
delta_t<-u_11*r
delta_s<-u_22*r

#delta_sigma<- matrix(c(u_11*rho, u_12*rho,u_12*rho,u_22*rho), nrow=2, ncol=2)
rh_square_check_1 <- 1- (det(sigma_delta) /(det(sigma_delta[1:5,1:5])*det(sigma_delta[6:10,6:10])))
rh_square_check_2 <- 1- (det(sigma_delta) /(det(delta_t)*det(delta_s)))