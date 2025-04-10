##linear model with random intercept + random slope 
##galecki's model +random intercept + random slope


##libraries
library(Surrogate)
library(matrixcalc)

## V matrixfrom the sas results two models placebo and experimental
var_placebo<- matrix(c(50.6904, 71.9325, 71.9325, 119.10), nrow=2, ncol=2)
var_exp<- matrix(c(47.7391, 64.6109,64.6109, 104.34), nrow=2, ncol=2)


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


##joint 
var_placebo<- matrix(c(52.2064, 74.0262, 74.0262, 122.54), nrow=2, ncol=2)
var_exp<- matrix(c(46.2546, 62.7348,62.7348, 101.33), nrow=2, ncol=2)


Sigma_joint<- matrix(c( var_placebo[1,1], NA, var_placebo[2,1],NA,
                  NA, var_exp[1,1], NA, var_exp[1,2],
                  var_placebo[1,2], NA, var_placebo[2,2], NA,
                  NA, var_exp[1,2], NA, var_exp[2,2]), nrow=4, ncol=4)

##estimable corr
corr_joint<- matrix(c( 1, NA, (var_placebo[2,1]/sqrt(var_placebo[1,1]*var_placebo[2,2])),NA,
                 NA, 1, NA, var_exp[2,1]/(sqrt(var_exp[1,1])*sqrt(var_exp[2,2])),
                 (var_placebo[2,1]/sqrt(var_placebo[1,1]*var_placebo[2,2])), NA, 1, NA,
                 NA,  var_exp[2,1]/(sqrt(var_exp[1,1])*sqrt(var_exp[2,2])), NA, 1), nrow=4, ncol=4)

r<- 0.4212


##D matrix 8x8 from the sas one model experimental and placebo 
d_placebo<- matrix(c(23.3244, 34.8638, -0.2096, -0.4222,
                     34.8638, 62.3625, -1.309, -1.0660,
                     -0.2096, -1.309,  0.7662, 1.9290,
                     -0.4222, -1.0660, 1.9290, 4.1996), nrow=4, ncol=4)


d_exp<- matrix(c(84.6033, 125.53,-0.4772, 2.1359,
                 125.53, 216.45,4.1294, 9.0339,
                 -0.4772, 4.1294,0.5283, 0.3696,
                 2.1359, 9.0339,0.3696, 0), nrow=4, ncol=4)



D_matrix<- matrix( c(d_placebo[1,1], d_placebo[2,1], NA, NA, d_placebo[3,1],d_placebo[4,1], NA,NA,
                     d_placebo[2,1], d_placebo[2,2], NA, NA, d_placebo[4,1],d_placebo[4,2], NA,NA,
                     NA,NA, d_exp[1,1], d_exp[2,1],  NA, NA, d_exp[3,1], d_exp[4,1],
                     NA,NA, d_exp[1,2], d_exp[2,2], NA, NA, d_exp[1,4], d_exp[4,2],
                     d_placebo[3,1], d_placebo[4,1], NA, NA, d_placebo[3,3],d_placebo[4,3], NA,NA,
                     d_placebo[4,1], d_placebo[4,2], NA, NA, d_placebo[3,4],d_placebo[4,4], NA,NA,           
                     NA,NA, d_exp[3,1], d_exp[4,1],  NA, NA, d_exp[3,3], d_exp[4,3],
                     NA,NA, d_exp[4,1], d_exp[4,2], NA, NA, d_exp[3,4], d_exp[4,4]), nrow=8, ncol=8)



d_corr<- matrix(c( 1, (d_placebo[2,1]/sqrt(d_placebo[1,1]*d_placebo[2,2])),NA,NA, (d_placebo[3,1]/sqrt(d_placebo[1,1]*d_placebo[3,3])), (d_placebo[4,1]/sqrt(d_placebo[1,1]*d_placebo[4,4])), NA,NA,
                   (d_placebo[2,1]/sqrt(d_placebo[1,1]*d_placebo[2,2])),1, NA,NA, (d_placebo[4,1]/sqrt(d_placebo[1,1]*d_placebo[4,4])), (d_placebo[4,2]/sqrt(d_placebo[2,2]*d_placebo[4,4])), NA,NA,
                   NA,NA, 1, (d_exp[2,1]/sqrt(d_exp[1,1]*d_exp[2,2])), NA,NA, (d_exp[3,1]/sqrt(d_exp[1,1]*d_exp[3,3])), (d_exp[4,1]/sqrt(d_exp[1,1]*d_exp[4,4])),
                   NA,NA, (d_exp[2,1]/sqrt(d_exp[1,1]*d_exp[2,2])), 1, NA,NA, (d_exp[4,1]/sqrt(d_exp[1,1]*d_exp[4,4])),(d_exp[2,4]/sqrt(d_exp[4,4]*d_exp[2,2])),
                   
                   (d_placebo[3,1]/sqrt(d_placebo[1,1]*d_placebo[3,3])),(d_placebo[4,1]/sqrt(d_placebo[1,1]*d_placebo[4,4])),NA,NA, 1, (d_placebo[4,3]/sqrt(d_placebo[3,3]*d_placebo[4,4])), NA,NA,
                   (d_placebo[4,1]/sqrt(d_placebo[1,1]*d_placebo[4,4])),(d_placebo[4,2]/sqrt(d_placebo[2,2]*d_placebo[4,4])), NA,NA, (d_placebo[4,3]/sqrt(d_placebo[3,3]*d_placebo[4,4])), 1, NA,NA,
                   
                   NA,NA, (d_exp[3,1]/sqrt(d_exp[1,1]*d_exp[3,3])), (d_exp[4,1]/sqrt(d_exp[1,1]*d_exp[4,4])), NA,NA, 1, (d_exp[4,3]/sqrt(d_exp[3,3]*d_exp[4,4])),
                   NA,NA, (d_exp[4,1]/sqrt(d_exp[1,1]*d_exp[4,4])), (d_exp[4,2]/sqrt(d_exp[2,2]*d_exp[4,4])), NA,NA, (d_exp[4,3]/sqrt(d_exp[3,3]*d_exp[4,4])), 1), nrow=8, ncol=8)


###joint D matrix
##D matrix 8x8 from the sas one model experimental and placebo 
d_placebo<- matrix(c(21.6273, 32.4239, -0.2096, -0.4222,
                     32.4239, 62.3625, -1.309, -1.0660,
                     -0.2096, -1.309,  0.7662, 1.9290,
                     -0.4222, -1.0660, 1.9290, 4.1996), nrow=4, ncol=4)


d_exp<- matrix(c(86.3973, 125.53,-0.4772, 2.1359,
                 125.53, 216.45,4.1294, 9.0339,
                 -0.4772, 4.1294,0.5283, 0.3696,
                 2.1359, 9.0339,0.3696, 3.5), nrow=4, ncol=4)



D_matrix_joint<- matrix( c(d_placebo[1,1], d_placebo[2,1], NA, NA, d_placebo[3,1],d_placebo[4,1], NA,NA,
                     d_placebo[2,1], d_placebo[2,2], NA, NA, d_placebo[4,1],d_placebo[4,2], NA,NA,
                     NA,NA, d_exp[1,1], d_exp[2,1],  NA, NA, d_exp[3,1], d_exp[4,1],
                     NA,NA, d_exp[1,2], d_exp[2,2], NA, NA, d_exp[1,4], d_exp[4,2],
                     d_placebo[3,1], d_placebo[4,1], NA, NA, d_placebo[3,3],d_placebo[4,3], NA,NA,
                     d_placebo[4,1], d_placebo[4,2], NA, NA, d_placebo[3,4],d_placebo[4,4], NA,NA,           
                     NA,NA, d_exp[3,1], d_exp[4,1],  NA, NA, d_exp[3,3], d_exp[4,3],
                     NA,NA, d_exp[4,1], d_exp[4,2], NA, NA, d_exp[3,4], d_exp[4,4]), nrow=8, ncol=8)



d_corr_joint<- matrix(c( 1, (d_placebo[2,1]/sqrt(d_placebo[1,1]*d_placebo[2,2])),NA,NA, (d_placebo[3,1]/sqrt(d_placebo[1,1]*d_placebo[3,3])), (d_placebo[4,1]/sqrt(d_placebo[1,1]*d_placebo[4,4])), NA,NA,
                   (d_placebo[2,1]/sqrt(d_placebo[1,1]*d_placebo[2,2])),1, NA,NA, (d_placebo[4,1]/sqrt(d_placebo[1,1]*d_placebo[4,4])), (d_placebo[4,2]/sqrt(d_placebo[2,2]*d_placebo[4,4])), NA,NA,
                   NA,NA, 1, (d_exp[2,1]/sqrt(d_exp[1,1]*d_exp[2,2])), NA,NA, (d_exp[3,1]/sqrt(d_exp[1,1]*d_exp[3,3])), (d_exp[4,1]/sqrt(d_exp[1,1]*d_exp[4,4])),
                   NA,NA, (d_exp[2,1]/sqrt(d_exp[1,1]*d_exp[2,2])), 1, NA,NA, (d_exp[4,1]/sqrt(d_exp[1,1]*d_exp[4,4])),(d_exp[2,4]/sqrt(d_exp[4,4]*d_exp[2,2])),
                   
                   (d_placebo[3,1]/sqrt(d_placebo[1,1]*d_placebo[3,3])),(d_placebo[4,1]/sqrt(d_placebo[1,1]*d_placebo[4,4])),NA,NA, 1, (d_placebo[4,3]/sqrt(d_placebo[3,3]*d_placebo[4,4])), NA,NA,
                   (d_placebo[4,1]/sqrt(d_placebo[1,1]*d_placebo[4,4])),(d_placebo[4,2]/sqrt(d_placebo[2,2]*d_placebo[4,4])), NA,NA, (d_placebo[4,3]/sqrt(d_placebo[3,3]*d_placebo[4,4])), 1, NA,NA,
                   
                   NA,NA, (d_exp[3,1]/sqrt(d_exp[1,1]*d_exp[3,3])), (d_exp[4,1]/sqrt(d_exp[1,1]*d_exp[4,4])), NA,NA, 1, (d_exp[4,3]/sqrt(d_exp[3,3]*d_exp[4,4])),
                   NA,NA, (d_exp[4,1]/sqrt(d_exp[1,1]*d_exp[4,4])), (d_exp[4,2]/sqrt(d_exp[2,2]*d_exp[4,4])), NA,NA, (d_exp[4,3]/sqrt(d_exp[3,3]*d_exp[4,4])), 1), nrow=8, ncol=8)




##corr for V 4 undefiniable corr
ICA_1<- ICA.ContCont(T0S0=corr[1,3], T1S1=corr[2,4], T0T0=Sigma[1,1], T1T1=Sigma[2,2], S0S0=Sigma[3,3], S1S1=Sigma[4,4], T0T1=seq(-1, 1, by=.1), 
                     T0S1=seq(-1, 1, by=.1), T1S0=seq(-1, 1, by=.1), S0S1=seq(-1, 1, by=.1))

rho_delta_1<- ICA_1$ICA
pos_def_1<-ICA_1$Pos.Def
sigma_t<- ICA_1$GoodSurr[,2]
sigma_s<- Sigma[3,3]+Sigma[4,4]-2*pos_def_1[,6]*sqrt(Sigma[3,3]*Sigma[4,4])
sigma_t_control<- Sigma[1,1]+Sigma[2,2]-2*pos_def_1[,1]*sqrt(Sigma[1,1]*Sigma[2,2])

sigma_ts<- Sigma[1,3]+Sigma[2,4]-pos_def_1[i,3]* sqrt(Sigma[1,1]*Sigma[4,4])- pos_def_1[i,4]* sqrt(Sigma[2,2]*Sigma[3,3])
sigma_u1<- matrix(c(sigma_t[1], sigma_ts[1],sigma_ts[1],sigma_s[1]), nrow = 2, ncol=2)

##kronocker product:
rho_ar1<- matrix(c(1,r,r,1), nrow=2, ncol=2)
H<- matrix(c(1,0,0,0,
             0,1,0,0,
             0,0,1,0,
             0,0,0,1), nrow=4, ncol=4)
for (i in 1:length(rho_delta_1)) {
    cov_sigma_T0T1<-  pos_def_1[i,1]* sqrt(Sigma[1,1]*Sigma[2,2])
    cov_sigma_T0S1<-  pos_def_1[i,3]* sqrt(Sigma[1,1]*Sigma[4,4])
    cov_sigma_T1S0<-  pos_def_1[i,4]* sqrt(Sigma[2,2]*Sigma[3,3])
    cov_sigma_S0S1<-  pos_def_1[i,6]* sqrt(Sigma[3,3]*Sigma[4,4])
    
    V_1<- sigma_u <- matrix (c((Sigma[1,1])+(Sigma[2,2])-2*cov_sigma_T0T1,
                               (Sigma[1,3])+(Sigma[2,4])- cov_sigma_T1S0-cov_sigma_T0S1,
                               (Sigma[1,3])+(Sigma[2,4])- cov_sigma_T1S0-cov_sigma_T0S1,
                               (Sigma[3,3])+(Sigma[4,4])-2*cov_sigma_S0S1), nrow=2, ncol=2)
    
    v_times_r<- kronecker(sigma_u, rho_ar1)
    delta_sigma<- v_times_r+H
    
    RH_square<- det(delta_sigma)/(det(delta_sigma[1:2,1:2])*det(delta_sigma[3:4,3:4]) )
    write(as.numeric(RH_square), file="~/Downloads/rh2_square_lme_8april.txt", append=TRUE)
    
    delta_sigma_2<- matrix(c(V_1[1,1]*r+1, V_1[1,2]*r,
                             V_1[2,1]*r,V_1[2,2]*r+1), nrow = 2, ncol=2)
    
    
    RH_square_2<- det(delta_sigma_2)/((delta_sigma_2[1,1])*(delta_sigma_2[2,2]) )
    
    write(as.numeric(RH_square_2), file="~/Downloads/rh2_square_2_lme_8april.txt", append=TRUE)
    
    
  }


RH_square_2_txt<-read.delim(file= '~/Downloads/rh2_square_lme_8april.txt', header = FALSE, sep = "\t", dec = ".")
min(RH_square_2_txt$V1)
max(RH_square_2_txt$V1)
sd(RH_square_2_txt$V1)
mean(RH_square_2_txt$V1)
median(RH_square_2_txt$V1)
quantile(RH_square_2_txt$V1)
hist( as.numeric(RH_square_2_txt$V1), main=bquote("Histogram of" ~"R"[H]^2), xlab=bquote("R"[H]^2),   labels = FALSE)

RH_square_22<- read.delim(file= '~/Downloads/rh2_square_2_lme_8april.txt', header = FALSE, sep = "\t", dec = ".")
min(RH_square_22$V1)
max(RH_square_22$V1)
sd(RH_square_22$V1)
mean(RH_square_22$V1)
median(RH_square_22$V1)
quantile(RH_square_22$V1)
hist( as.numeric(RH_square_22$V1), main=bquote("Histogram of" ~"R"[lambda]^2), xlab=bquote("R"[lambda]^2),   labels = TRUE, ylim=c(0,800))
box()
grid()



##undefined correlatiosn:
b_T0T1=seq(-1, 1, by=.1) 
a_T0T1=seq(-1, 1, by=.1) 
ab_T0T1=seq(-1, 1, by=.1) 

b_T0S1=seq(-1, 1, by=.1)
a_T0S1=seq(-1, 1, by=.1)
ab_T0S1=seq(-1, 1, by=.1)

b_T1S0=seq(-1, 1, by=.1)
a_T1S0=seq(-1, 1, by=.1)
ab_T1S0=seq(-1, 1, by=.1)

b_S0S1=seq(-1, 1, by=.1)
a_S0S1=seq(-1, 1, by=.1)
ab_S0S1=seq(-1, 1, by=.1)


grid_chunk1 <- expand.grid(b_T0T1, a_T0T1, ab_T0T1, b_T0S1)
grid_chunk2 <- expand.grid(a_T0S1, ab_T0S1, b_T1S0, a_T1S0)
grid_chunk3 <- expand.grid(ab_T1S0, b_S0S1, a_S0S1, ab_S0S1)

combins <- rbind(grid_chunk1, grid_chunk2, grid_chunk3)


combins <- expand.grid(b_T0T1, a_T0T1, ab_T0T1, 
                       b_T0S1, a_T0S1, ab_T0S1,
                       b_T1S0, a_T1S0, ab_T1S0,
                       b_S0S1, a_S0S1, ab_S0S1)

a<- c(0.2,0.3, 0.1, 
  0.9,1, 0.2, 
  0.5, 0.85, 0.4,
  0.2, 0.25, 0.6)
b<-c(0.1,0.3, 0.1, 
  0.9,1, 0.2, 
  0.5, 0.5, 0.4,
  0.2, 0.5, 0.6)
combins <- rbind(a,b)
  
for(i in 1:nrow(combins))
{ d_corr[1,4]<- d_corr[4,1]<- d_corr[3,2]<- d_corr[2,3]<- combins[i, 3] 
  d_corr[3,1]<- d_corr[1,3]<- combins[i, 1] 
  d_corr[4,2]<- d_corr[2,4]<- combins[i, 2] 
  
  d_corr[1,8]<- d_corr[8,1]<- d_corr[7,2]<- d_corr[2,7]<- combins[i, 6] 
  d_corr[7,1]<- d_corr[1,7]<- combins[i, 4] 
  d_corr[8,2]<- d_corr[2,8]<- combins[i, 5] 
  
  d_corr[4,5]<- d_corr[5,4]<- d_corr[6,3]<- d_corr[3,6]<- combins[i, 9] 
  d_corr[3,5]<- d_corr[5,3]<- combins[i, 7] 
  d_corr[6,4]<- d_corr[4,6]<- combins[i, 8] 
  
  d_corr[6,7]<- d_corr[7,6]<- d_corr[8,5]<- d_corr[5,8]<- combins[i, 12] 
  d_corr[5,7]<- d_corr[7,5]<- combins[i, 10] 
  d_corr[6,8]<- d_corr[8,6]<- combins[i, 11] 
  
  isSymmetric(d_corr) 
  is.positive.definite(d_corr)
  if(is.positive.definite(d_corr)==TRUE)
  {
    
    Sigma_delta_d<- matrix(NA, nrow=4, ncol=4)
    Sigma_delta_d[1,1]<- D_matrix[1,1]+ D_matrix[2,2]-2*d_corr[2,1]*sqrt(D_matrix[1,1]*D_matrix[2,2])
    Sigma_delta_d[2,2]<- D_matrix[3,3]+ D_matrix[4,4]-2*d_corr[3,4]*sqrt(D_matrix[3,3]*D_matrix[4,4])
    Sigma_delta_d[3,3]<- D_matrix[5,5]+ D_matrix[6,6]-2*d_corr[5,6]*sqrt(D_matrix[5,5]*D_matrix[6,6])
    Sigma_delta_d[4,4]<- D_matrix[7,7]+ D_matrix[8,8]-2*d_corr[7,8]*sqrt(D_matrix[7,7]*D_matrix[8,8])
    
    Sigma_delta_d[1,3]<-Sigma_delta_d[3,1]<- D_matrix[1,5]+ D_matrix[6,2]-2*d_corr[6,1]*sqrt(D_matrix[1,5]*D_matrix[6,2])
    Sigma_delta_d[2,4]<-Sigma_delta_d[4,2]<- D_matrix[3,7]+ D_matrix[8,4]-2*d_corr[8,3]*sqrt(D_matrix[3,7]*D_matrix[8,4])
    
    sigma_b_T0T1<-  combins[i,1]*sqrt(D_matrix[1,1]*D_matrix[3,3])
    sigma_a_T0T1<-  combins[i,2]*sqrt(D_matrix[2,2]*D_matrix[4,4])
    sigma_ab_T0T1<- combins[i,3]*sqrt(D_matrix[1,2]*D_matrix[3,4])
    
    sigma_b_T0S1<-  combins[i,4]*sqrt(D_matrix[1,1]*D_matrix[7,7])
    sigma_a_T0S1<-  combins[i,5]*sqrt(D_matrix[2,2]*D_matrix[8,8])
    sigma_ab_T0S1<- combins[i,6]*sqrt(D_matrix[1,2]*D_matrix[7,8])
    
    sigma_b_T1S0 <- combins[i,7]*sqrt(D_matrix[3,3]*D_matrix[5,5])
    sigma_a_T1S0 <- combins[i,8]*sqrt(D_matrix[4,4]*D_matrix[6,6])
    sigma_ab_T1S0<- combins[i,9]*sqrt(D_matrix[3,4]*D_matrix[5,6])
    
    sigma_b_S0S1<- combins[i,10]*sqrt(D_matrix[5,5]*D_matrix[7,7])
    sigma_a_S0S1<- combins[i,11]*sqrt(D_matrix[6,6]*D_matrix[8,8])
    sigma_ab_S0S1<- combins[i,12]*sqrt(D_matrix[5,6]*D_matrix[7,8])
    
    Sigma_delta_d[1,2]<- Sigma_delta_d[2,1]<- sigma_b_T0T1+sigma_a_T0T1-2*sigma_ab_T0T1
    Sigma_delta_d[1,4]<- Sigma_delta_d[4,1]<- sigma_b_T0S1+sigma_a_T0S1-2*sigma_ab_T0S1
    Sigma_delta_d[2,3]<- Sigma_delta_d[3,2]<- sigma_b_T1S0+sigma_a_T1S0-2*sigma_ab_T1S0
    Sigma_delta_d[3,4]<- Sigma_delta_d[4,3]<- sigma_b_S0S1+sigma_a_S0S1-2*sigma_ab_S0S1
  }
}

    
  





##cor for D 16 undefiniable corr
ICA_2<- ICA.ContCont_2 (T0S0=d_corr[1,3], T1S1=d_corr[2,4], T0T0=D_matrix[1,1], T1T1=D_matrix[2,2], S0S0=D_matrix[3,3], S1S1=D_matrix[4,4], T0T1=seq(-1, 1, by=.1), 
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


##change the code with text to take less time
