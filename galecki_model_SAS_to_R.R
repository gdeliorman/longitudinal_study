##from the sas results

var_placebo<- matrix(c(91.9693, 137.29, 137.29, 238.90), nrow=2, ncol=2)
#var_exp<- matrix(c(969.56, 69.6311,69.6311,962.97 ), nrow=2, ncol=2)
var_exp<- matrix(c(149.17, 205.39,205.39, 319.71 ), nrow=2, ncol=2)
var_exp<- matrix(c(162.99, 221.84,221.84, 354.38 ), nrow=2, ncol=2)
var_exp<- matrix(c(121.05, 170.45,170.45, 283.04 ), nrow=2, ncol=2)

var_placebo_2<- matrix(c(158.18,248.30, 248.30, 446.22), nrow=2, ncol=2)
var_exp_2<- matrix(c(144.58, 230.06,230.06, 416.81 ), nrow=2, ncol=2)


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

library(Surrogate)
ICA<- ICA.ContCont(T0S0=corr[1,3], T1S1=corr[2,4], T0T0=Sigma[1,1], T1T1=Sigma[2,2], S0S0=Sigma[3,3], S1S1=Sigma[4,4], T0T1=seq(-1, 1, by=.1), 
             T0S1=seq(-1, 1, by=.1), T1S0=seq(-1, 1, by=.1), S0S1=seq(-1, 1, by=.1))

rho<- ICA$ICA
summary(rho)
summary(rho^2)
p<-4
RH_square<- 1-(1-rho^2)^p
summary(RH_square)
hist(rho , main=bquote("Histogram of" ~"rho"[u]), xlab=bquote("rho"[u]))
hist(rho^2, main=bquote("Histogram of" ~"rho"[u]^2), xlab=bquote("rho"[u]^2))

hist(RH_square, main=bquote("Histogram of" ~"R"[lambda]^2), xlab=bquote("R"[lambda]^2), col="darkmagenta", breaks = 15)
hist(RH_square, main=bquote("Histogram of" ~"R"[lambda]^2), xlab=bquote("R"[lambda]^2), xlim=c(0,1), breaks = 15)
hist(RH_square, main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2), xlim=c(0,1), breaks = 15)
box()


len <- length(RH_square) 
hist_init <- hist(RH_square, plot = FALSE) 
rounded <- round(hist_init$counts / len * 100, 2) 

hist(RH_square, main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),  breaks = 15, labels = paste0(rounded , "%"), ylim = c(0,1100), cex.main=0.1)
hist(RH_square, main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),  breaks = 15, labels = TRUE, ylim = c(0,1100))
hist(RH_square, main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),  breaks = 15, ylim = c(0,1100))
hist(RH_square, main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),  breaks = 15,freq = FALSE)
hist(RH_square, main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),  breaks = 15,prob = TRUE)
hist(RH_square, main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),  breaks = 15, labels = TRUE, ylim = c(0,1100))

grid()
box()
summary(RH_square)


var_placebo_2<- matrix(c(78.63,115.96, 115.96, 198.14), nrow=2, ncol=2)
var_exp_2<- matrix(c(94.8169, 128.88,128.88, 215.13 ), nrow=2, ncol=2)


Sigma2<- matrix(c( var_placebo_2[1,1], NA, var_placebo_2[2,1],NA,
                  NA, var_exp_2[1,1], NA, var_exp_2[1,2],
                  var_placebo_2[1,2], NA, var_placebo_2[2,2], NA,
                  NA, var_exp_2[1,2], NA, var_exp_2[2,2]), nrow=4, ncol=4)

##estimable corr
corr2<- matrix(c( 1, NA, (var_placebo_2[2,1]/sqrt(var_placebo_2[1,1]*var_placebo_2[2,2])),NA,
                 NA, 1, NA, var_exp_2[2,1]/(sqrt(var_exp_2[1,1])*sqrt(var_exp_2[2,2])),
                 (var_placebo_2[2,1]/sqrt(var_placebo_2[1,1]*var_placebo_2[2,2])), NA, 1, NA,
                 NA,  var_exp_2[2,1]/(sqrt(var_exp_2[1,1])*sqrt(var_exp_2[2,2])), NA, 1), nrow=4, ncol=4)

library(Surrogate)
ICA2<- ICA.ContCont(T0S0=corr2[1,3], T1S1=corr2[2,4], T0T0=Sigma2[1,1], T1T1=Sigma2[2,2], S0S0=Sigma2[3,3], S1S1=Sigma2[4,4], T0T1=seq(-1, 1, by=.1), 
                   T0S1=seq(-1, 1, by=.1), T1S0=seq(-1, 1, by=.1), S0S1=seq(-1, 1, by=.1))

rho<- ICA2$ICA
summary(rho)
summary(rho^2)
p<-4
RH_square<- 1-(1-rho^2)^p
summary(RH_square)
hist(RH_square, main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),  breaks = 15)
grid()
box()
summary(RH_square)
