##linear model with random intercept

##libraries
library(Surrogate)

##from the sas results two models
var_placebo<- matrix(c(55.1252, 81.5565, 81.5565, 138.94), nrow=2, ncol=2)
var_exp<- matrix(c(45.7899, 60.9193,60.9193, 96.188), nrow=2, ncol=2)
d_placebo<- matrix(c(23.0890, 34.6807, 34.6807, 65.5558), nrow=2, ncol=2)
d_exp<- matrix(c(82.8980, 135.46,135.46, 243.68 ), nrow=2, ncol=2)


##from the sas one model
var_placebo<- matrix(c(55.3676, 81.8897, 81.8897, 139.48), nrow=2, ncol=2)
var_exp<- matrix(c(48.4042, 64.4072,64.4072, 101.82), nrow=2, ncol=2)
d_placebo<- matrix(c(22.8810, 34.3872, 34.3872, 65.0816), nrow=2, ncol=2)
d_exp<- matrix(c(87.0843, 142.25,142.25, 255.92 ), nrow=2, ncol=2)

##estimable sigma
Sigma<- matrix(c( var_placebo[1,1], NA, var_placebo[2,1],NA,
                  NA, var_exp[1,1], NA, var_exp[1,2],
                  var_placebo[1,2], NA, var_placebo[2,2], NA,
                  NA, var_exp[1,2], NA, var_exp[2,2]), nrow=4, ncol=4)

##estimable corr
corr<- cov2cor(Sigma)
D_matrix<- matrix(c( d_placebo[1,1], NA, d_placebo[2,1],NA,
                     NA, d_exp[1,1], NA, d_exp[1,2],
                     d_placebo[1,2], NA, d_placebo[2,2], NA,
                     NA, d_exp[1,2], NA, d_exp[2,2]), nrow=4, ncol=4)

d_corr<- cov2cor(D_matrix)

ICA_1<- ICA.ContCont(T0S0=corr[1,3], T1S1=corr[2,4], T0T0=Sigma[1,1], T1T1=Sigma[2,2], S0S0=Sigma[3,3], S1S1=Sigma[4,4], T0T1=seq(-1, 1, by=.1), 
                   T0S1=seq(-1, 1, by=.1), T1S0=seq(-1, 1, by=.1), S0S1=seq(-1, 1, by=.1))

rho_delta_1<- ICA_1$ICA
pos_def_1<-ICA_1$Pos.Def

#gs<- ICA_1$GoodSurr

ICA_2<- ICA.ContCont(T0S0=d_corr[1,3], T1S1=d_corr[2,4], T0T0=D_matrix[1,1], T1T1=D_matrix[2,2], S0S0=D_matrix[3,3], S1S1=D_matrix[4,4], T0T1=seq(-1, 1, by=.1), 
                     T0S1=seq(-1, 1, by=.1), T1S0=seq(-1, 1, by=.1), S0S1=seq(-1, 1, by=.1))

pos_def_2<-ICA_2$Pos.Def
rho_delta_2<- ICA_2$ICA



##add for loop
cov_sigma_T0T1<-  pos_def_1[1,1]* sqrt(Sigma[1,1]*Sigma[2,2])
cov_sigma_T0S1<-  pos_def_1[1,3]* sqrt(Sigma[1,1]*Sigma[4,4])
cov_sigma_T1S0<-  pos_def_1[1,4]* sqrt(Sigma[2,2]*Sigma[3,3])
cov_sigma_S0S1<-  pos_def_1[1,6]* sqrt(Sigma[3,3]*Sigma[4,4])


cov_d_T0T1<-  pos_def_2[1,1]* sqrt(D_matrix[1,1]*D_matrix[2,2])
cov_d_T0S1<-  pos_def_2[1,3]* sqrt(D_matrix[1,1]*D_matrix[3,3])
cov_d_T1S0<-  pos_def_2[1,4]* sqrt(D_matrix[2,2]*D_matrix[3,3])
cov_d_S0S1<-  pos_def_2[1,6]* sqrt(D_matrix[3,3]*D_matrix[4,4])


V_1<- sigma_u <- matrix (c((Sigma[1,1])+(Sigma[2,2])-2*cov_sigma_T0T1,
                           (Sigma[1,3])+(Sigma[2,4])- cov_sigma_T1S0-cov_sigma_T0S1,
                           (Sigma[1,3])+(Sigma[2,4])- cov_sigma_T1S0-cov_sigma_T0S1,
                           (Sigma[3,3])+(Sigma[4,4])-2*cov_sigma_S0S1), nrow=2, ncol=2)



D_1<- sigma_d <- matrix (c((D_matrix[1,1])+(D_matrix[2,2]) - 2*cov_d_T0T1,
                           (D_matrix[1,3])+(D_matrix[2,4])- cov_d_T1S0-cov_d_T0S1,
                           (D_matrix[1,3])+(D_matrix[2,4])- cov_d_T1S0-cov_d_T0S1,
                           (D_matrix[3,3])+(D_matrix[4,4])- 2*cov_d_S0S1), nrow=2, ncol=2)



p<-5
# AR1_placebo<- 0.4226
# AR2_exp<- 0.4314
AR<- 0.4259

# alpha1<- (p-(p*AR1_placebo)+2*AR1_placebo)/(1+AR1_placebo)
# alpha2<- (p-(p*AR2_exp)+2*AR2_exp)/(1+AR2_exp)
alpha<- (p-(p*AR)+2*AR)/(1+AR)

rho_ud_square<- (V_1[1,2]+ alpha*D_1[1,2])^2/ ((V_1[1,1]+alpha*D_1[1,1])*(V_1[2,2]+alpha*D_1[2,2]))

rho_u<- V_1[1,2]/ sqrt(V_1[1,1]*V_1[2,2])
rho_u_square<- V_1[1,2]^2/ (V_1[1,1]*V_1[2,2])

RH_square<- 1-((1-rho_delta_1[1]^2)^(p-1))*(1-rho_ud_square)
RH_square<- 1-((1-rho_u_square)^(p-1))*(1-rho_ud_square)
RH_square<- data.frame()
##for loop pdm 1 times pdm 2 
# for (i in 1:8) {
#   for (j in 1:5) {
    
for (i in 1:length(rho_delta_1)) {
  for (j in 1:length(rho_delta_2)) {
   
    cov_sigma_T0T1<-  pos_def_1[i,1]* sqrt(Sigma[1,1]*Sigma[2,2])
    cov_sigma_T0S1<-  pos_def_1[i,3]* sqrt(Sigma[1,1]*Sigma[4,4])
    cov_sigma_T1S0<-  pos_def_1[i,4]* sqrt(Sigma[2,2]*Sigma[3,3])
    cov_sigma_S0S1<-  pos_def_1[i,6]* sqrt(Sigma[3,3]*Sigma[4,4])
    
    
    cov_d_T0T1<-  pos_def_2[j,1]* sqrt(D_matrix[1,1]*D_matrix[2,2])
    cov_d_T0S1<-  pos_def_2[j,3]* sqrt(D_matrix[1,1]*D_matrix[3,3])
    cov_d_T1S0<-  pos_def_2[j,4]* sqrt(D_matrix[2,2]*D_matrix[3,3])
    cov_d_S0S1<-  pos_def_2[j,6]* sqrt(D_matrix[3,3]*D_matrix[4,4])
    
    
    V_1<- sigma_u <- matrix (c((Sigma[1,1])+(Sigma[2,2])-2*cov_sigma_T0T1,
                               (Sigma[1,3])+(Sigma[2,4])- cov_sigma_T1S0-cov_sigma_T0S1,
                               (Sigma[1,3])+(Sigma[2,4])- cov_sigma_T1S0-cov_sigma_T0S1,
                               (Sigma[3,3])+(Sigma[4,4])-2*cov_sigma_S0S1), nrow=2, ncol=2)
    
    
    
    D_1<- sigma_d <- matrix (c((D_matrix[1,1])+(D_matrix[2,2]) - 2*cov_d_T0T1,
                               (D_matrix[1,3])+(D_matrix[2,4])- cov_d_T1S0-cov_d_T0S1,
                               (D_matrix[1,3])+(D_matrix[2,4])- cov_d_T1S0-cov_d_T0S1,
                               (D_matrix[3,3])+(D_matrix[4,4])- 2*cov_d_S0S1), nrow=2, ncol=2)
    
    rho_ud_square<- (V_1[1,2]+ alpha*D_1[1,2])^2/ ((V_1[1,1]+alpha*D_1[1,1])*(V_1[2,2]+alpha*D_1[2,2]))
    rho_u_square<- V_1[1,2]^2/ (V_1[1,1]*V_1[2,2])
    RH_square_comb<- 1-((1-rho_delta_1[1]^2)^(p-1))*(1-rho_ud_square)
    RH_square<- c(RH_square, RH_square_comb)
  }
}

RH_square_2<- as.data.frame(as.numeric(RH_square))

summary(RH_square_2$`as.numeric(RH_square_2)`)
hist(RH_square_2$`as.numeric(RH_square_2)`)

hist( as.numeric(RH_square_2$`as.numeric(RH_square)`), main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),  breaks = 15, labels = TRUE)
hist(as.numeric(RH_square_2$`as.numeric(RH_square)`), main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),  breaks = 15)
hist(as.numeric(RH_square_2$`as.numeric(RH_square)`), main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),  breaks = 15, labels = TRUE, ylim = c(0,3500))


hist( as.numeric(RH_square_2$`as.numeric(RH_square)`), main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),   labels = FALSE, xlim=c(0.999980,1))

hist( as.numeric(RH_square_2$`as.numeric(RH_square)`), main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),   labels = FALSE)

grid()
box()
write.csv(RH_square_2,"~/Downloads/RH_square_2_fin1_random.csv", row.names = FALSE)

RH_square_2<- read.csv("/Users/gokcedeliorman/Downloads/rh2_square_fin1_26march.txt")

summary(RH_square_2$X0.9999974)
hist( as.numeric(RH_square_2$X0.9999974), main="", xlab=bquote("R"[H]^2),  breaks = 8, labels = TRUE, xlim=c(0,1), ylim=c(0,1999999))
hist( as.numeric(RH_square_2$X0.9999974), main="", xlab=bquote("R"[Lambda]^2),  breaks = 15, xlim=c(0,1), ylim=c(0,1999999))
grid()
box()
quantile(RH_square_2$X0.9999974)
margin<- qt(0.95,df=length(RH_square_2$X0.9999974)-1)*sd(RH_square_2$X0.9999974)/sqrt(length(RH_square_2$X0.9999974))
lowerinterval <- mean(RH_square_2$X0.9999974) - margin
upperinterval <- mean(RH_square_2$X0.9999974) + margin


##for 3 trials bprs
#RH_square_2<- read.csv("/Users/gokcedeliorman/Downloads/RH_square_setting3_model2.txt")
RH_square_2<- read.csv("/Users/gokcedeliorman/Downloads/RH_square_check_setting3_model2.txt")
summary(RH_square_2$X1)
hist( as.numeric(RH_square_2$X1), main="", xlab=bquote("R"[Lambda]^2),  breaks = 8, labels = TRUE, xlim=c(0,1), ylim=c(0,1200000))
hist( as.numeric(RH_square_2$X1), main="", xlab=bquote("R"[Lambda]^2),  breaks = 15, xlim=c(0,1), ylim=c(0,1190000))
grid()

box()
quantile(RH_square_2$X1)
mean(RH_square_2$X1)
margin<- qt(0.95,df=length(RH_square_2$X1)-1)*sd(RH_square_2$X1)/sqrt(length(RH_square_2$X1))
lowerinterval <- mean(RH_square_2$X1) - margin
upperinterval <- mean(RH_square_2$X1) + margin


##change the code with text to take less time

##double check the formula and theory
##for i 1 
corr[1,2]<- corr[2,1]<- corr[4,1]<- corr[1,4]<- corr[2,3]<- corr[3,2] <- corr[4,3] <- corr[3,4]<- -0.9
Sigma[1,2]<- Sigma[2,1] <- sqrt(Sigma[1,1]*Sigma[2,2])*-0.9
Sigma[1,4]<- Sigma[4,1] <- sqrt(Sigma[1,1]*Sigma[4,4])*-0.9
Sigma[2,3]<- Sigma[3,2] <- sqrt(Sigma[2,2]*Sigma[3,3])*-0.9
Sigma[4,3]<- Sigma[3,4] <- sqrt(Sigma[4,4]*Sigma[3,3])*-0.9


d_corr[1,2]<- d_corr[2,1]<- d_corr[4,1]<- d_corr[1,4]<- d_corr[2,3]<- d_corr[3,2] <- d_corr[4,3] <- d_corr[3,4]<- -0.9
D_matrix[1,2]<- D_matrix[2,1] <- sqrt(D_matrix[1,1]*D_matrix[2,2])*-0.9
D_matrix[1,4]<- D_matrix[4,1] <- sqrt(D_matrix[1,1]*D_matrix[4,4])*-0.9
D_matrix[2,3]<- D_matrix[3,2] <- sqrt(D_matrix[2,2]*D_matrix[3,3])*-0.9
D_matrix[4,3]<- D_matrix[3,4] <- sqrt(D_matrix[4,4]*D_matrix[3,3])*-0.9


ar1_cor <- function(n, rho) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - 
                    (1:n - 1))
  rho^exponent
}
r<-as.matrix(ar1_cor(5, AR), nrow=5, ncol=5)
p1 <- matrix(c(1,1,1,1,1), nrow=1, ncol=5)
sigma_delta<- kronecker(D_1, t(p1)%*%(p1))+  kronecker(V_1, r)
rh_square_check_1<- 1- det(sigma_delta)/(det(sigma_delta[1:5, 1:5])*det(sigma_delta[6:10,6:10]))


