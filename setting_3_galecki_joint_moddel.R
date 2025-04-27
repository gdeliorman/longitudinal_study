##setting 3 model 2 (galecki+random intercept) linear model with random intercept

##libraries
library(Surrogate)

##from the sas joint model V
Sigma<- V<- matrix(c(156.39 , NA, 245.30,NA,
                     NA, 146.90, NA, 233.52,
                     245.30, NA, 440.51, NA,
                     NA, 233.52, NA, 422.73), nrow=4, ncol=4)

##estimable corr
corr<- cov2cor(Sigma)


ICA_1<- ICA.ContCont(T0S0=corr[1,3], T1S1=corr[2,4], T0T0=Sigma[1,1], T1T1=Sigma[2,2], S0S0=Sigma[3,3], S1S1=Sigma[4,4], T0T1=seq(-1, 1, by=.1), 
                     T0S1=seq(-1, 1, by=.1), T1S0=seq(-1, 1, by=.1), S0S1=seq(-1, 1, by=.1))

rho_delta_1<- ICA_1$ICA
pos_def_1<-ICA_1$Pos.Def

RH_lambda_square<- 1-(1-rho_delta_1^2)^p

p<-6
AR<- 0.8230
Q<- matrix(c(-1, 0,1,0,
             0, -1,0,1), nrow=2, ncol=4)


ar1_cor <- function(n, rho) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - 
                    (1:n - 1))
  rho^exponent
}
r<-as.matrix(ar1_cor(6, AR), nrow=6, ncol=6)
p1 <- matrix(c(1,1,1,1,1,1), nrow=1, ncol=6)


##to check 
RH_square<- data.frame()
for (i in 1:length(rho_delta_1)) {
  
    
    ##covariances
    cov_sigma_T0T1<-  pos_def_1[i,1]* sqrt(Sigma[1,1]*Sigma[2,2])
    cov_sigma_T0S1<-  pos_def_1[i,3]* sqrt(Sigma[1,1]*Sigma[4,4])
    cov_sigma_T1S0<-  pos_def_1[i,4]* sqrt(Sigma[2,2]*Sigma[3,3])
    cov_sigma_S0S1<-  pos_def_1[i,6]* sqrt(Sigma[3,3]*Sigma[4,4])
    
    
    V_1<- sigma_u <- matrix (c((Sigma[1,1])+(Sigma[2,2])-2*cov_sigma_T0T1,
                               (Sigma[1,3])+(Sigma[2,4])- cov_sigma_T1S0-cov_sigma_T0S1,
                               (Sigma[1,3])+(Sigma[2,4])- cov_sigma_T1S0-cov_sigma_T0S1,
                               (Sigma[3,3])+(Sigma[4,4])-2*cov_sigma_S0S1), nrow=2, ncol=2)
    
    
    Sigma[1,2]<- Sigma[2,1]<- cov_sigma_T0T1
    Sigma[1,4]<- Sigma[4,1]<- cov_sigma_T0S1
    Sigma[3,2]<- Sigma[2,3]<- cov_sigma_T1S0
    Sigma[3,4]<- Sigma[4,3]<- cov_sigma_S0S1
    
    
    if (is.positive.definite(V_1)) {
      
      sigma_delta<- kronecker(V_1, r)
      rh_square_check<- 1- (det(sigma_delta)/(det(sigma_delta[1:6, 1:6])*det(sigma_delta[7:12,7:12])))
      RH_square<- c(RH_square, rh_square_check)
      
    }

}


summary(as.numeric(RH_square))
min(as.numeric(RH_square))
quantile(as.numeric(RH_square))
mean(as.numeric(RH_square))
median(as.numeric(RH_square))
length(as.numeric(RH_square))
quantile(as.numeric(RH_square), 0.25)
quantile(as.numeric(RH_square), 0.75)
hist(as.numeric(RH_square), main="", xlab=bquote("R"[Lambda]^2),  breaks = 15, labels = TRUE,ylim = c(0,1200))
hist(as.numeric(RH_lambda_square), main="", xlab=bquote("R"[Lambda]^2),  breaks = 15, labels = TRUE,ylim = c(0,1200))

grid()
box()



