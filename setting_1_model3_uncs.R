V<- matrix(c(39.9046,NA,57.8688,NA,
             NA,32.4907,NA,43.8348,
             57.8688,NA,96.3282, NA,
             NA,43.8348,NA,70.4793), nrow = 4, ncol = 4)

corr_V<- cov2cor(V)
corr<- 0.1805

library(fastmatrix)
cor_matrix<-corCS(corr, p = 5)
cor_matrix<- matrix(c(1,corr,corr,corr,corr,
                      corr,1,corr,corr,corr,
                      corr,corr,1,corr,corr,
                      corr,corr,corr,1,corr,
                      corr,corr,corr,corr,1), nrow=5, ncol=5)

##estimate V matrix

library(Surrogate)
ICA_V<- ICA.ContCont(T0S0=corr_V[1,3], T1S1=corr_V[2,4], T0T0=V[1,1], T1T1=V[2,2], S0S0=V[3,3], S1S1=V[4,4], T0T1=seq(-1, 1, by=.1), 
                   T0S1=seq(-1, 1, by=.1), T1S0=seq(-1, 1, by=.1), S0S1=seq(-1, 1, by=.1))

##1461 pdf V matrices
length(ICA_V$ICA)

##complete V matrix
complete_V<- list()
for (i in 1:length(ICA_V$ICA)) {
  V[1,2]<-V[2,1]<- sqrt(V[1,1]*V[2,2])* ICA_V$Pos.Def[i,1]
  V[4,3]<-V[3,4]<- sqrt(V[4,4]*V[3,3])* ICA_V$Pos.Def[i,6]
  V[1,4]<-V[4,1]<- sqrt(V[1,1]*V[4,4])* ICA_V$Pos.Def[i,3]
  V[2,3]<-V[3,2]<- sqrt(V[2,2]*V[3,3])* ICA_V$Pos.Def[i,4]
  complete_V[[i]]<- V}
length(unique(complete_V))
length(complete_V)


D_template<- matrix(c(
  38.6455, 57.4401,    NA,    NA, -3.3569, -5.4102,      NA,     NA,
  57.4401, 105.47,     NA,    NA, -5.9486, -10.0573,     NA,     NA,
  NA,      NA,      100.88, 154.02,  NA,    NA,     -5.0632, -3.6366,
  NA,      NA,       154.02,  258.51,  NA,    NA,      -2.2534,  3.9227,
  -3.3569, -5.9486,   NA,    NA,  2.0368,  0.4484,    NA,     NA,
  -5.4102, -10.0573,  NA,    NA,  0.4484,  2.6058,    NA,     NA,
  NA,      NA,      -5.0632,  -2.2534,  NA,    NA,     7.8797,  3.3763,
  NA,      NA,      -3.6366,  3.9227,    NA,    NA,     3.3763 ,  4.8466
), nrow = 8, byrow = TRUE)

D_template<- matrix(c(
  38.6455, 57.4401,    0,    0, -3.3569, -5.4102,      0,     0,
  57.4401, 105.47,     0,    0, -5.9486, -10.0573,     0,     0,
  0,      0,      100.88, 154.02,  0,    0,     -5.0632, -3.6366,
  0,      0,       154.02,  258.51,  0,    0,      -2.2534,  0.4484,
  -3.3569, -5.9486,   0,    0,  2.0368,  3.9227,    0,     0,
  -5.4102, -10.0573,  0,    0,  3.9227,  2.6058,    0,     0,
  0,      0,      -5.0632,  -2.2534,  0,    0,     7.8797,  3.3763,
  0,      0,      -3.6366,  0.4484,    0,    0,     3.3763 ,  4.8466
), nrow = 8, byrow = TRUE)

is.symmetric.matrix(D_template)


R_template<- cov2cor(D_template)

ica_mpc<-ICA.ContCont.MultS.MPC(M=10000,N=568,Sigma=D_template,prob = NULL,Seed=123,
                                Save.Corr=TRUE, Show.Progress=FALSE)

ica_mpc<-ICA.ContCont.MultS.MPC(M=1000000,N=568,Sigma=D_template,prob = NULL,Seed=123,
                                Save.Corr=TRUE, Show.Progress=FALSE)


valid_rows <- which(complete.cases(ica_mpc$Lower.Dig.Corrs.All))
complete_D<- list()
for (i in valid_rows) {
  
  R<- R_template
  Sigma_D<- D_template
  
  ##fill R matrix
  R[3,1]<- R[1,3]<-  ica_mpc$Lower.Dig.Corrs.All[i,2]
  R[4,1]<- R[1,4]<-  ica_mpc$Lower.Dig.Corrs.All[i,3]
  R[7,1]<- R[1,7]<-  ica_mpc$Lower.Dig.Corrs.All[i,6]
  R[8,1]<- R[1,8]<-  ica_mpc$Lower.Dig.Corrs.All[i,7]
  
  R[2,3]<- R[3,2]<-  ica_mpc$Lower.Dig.Corrs.All[i,8]
  R[2,4]<- R[4,2]<-  ica_mpc$Lower.Dig.Corrs.All[i,9]
  R[2,7]<- R[7,2]<-  ica_mpc$Lower.Dig.Corrs.All[i,12]
  R[2,8]<- R[8,2]<-  ica_mpc$Lower.Dig.Corrs.All[i,13]
  
  R[3,5]<- R[5,3]<-  ica_mpc$Lower.Dig.Corrs.All[i,15]
  R[3,6]<- R[6,3]<-  ica_mpc$Lower.Dig.Corrs.All[i,16]
  R[4,5]<- R[5,4]<-  ica_mpc$Lower.Dig.Corrs.All[i,19]
  R[4,6]<- R[6,4]<-  ica_mpc$Lower.Dig.Corrs.All[i,20]
  
  R[7,5]<- R[5,7]<-  ica_mpc$Lower.Dig.Corrs.All[i,24]
  R[8,5]<- R[5,8]<-  ica_mpc$Lower.Dig.Corrs.All[i,25]
  R[7,6]<- R[6,7]<-  ica_mpc$Lower.Dig.Corrs.All[i,26]
  R[8,6]<- R[6,8]<-  ica_mpc$Lower.Dig.Corrs.All[i,27]
  
  if( is.positive.definite(R) ==TRUE){
    Sigma_D[3,1]<- Sigma_D[1,3]<-  R[1,3]*sqrt(Sigma_D[1,1]*Sigma_D[3,3])
    Sigma_D[4,1]<- Sigma_D[1,4]<-  R[1,4]*sqrt(Sigma_D[1,1]*Sigma_D[4,4])
    Sigma_D[7,1]<- Sigma_D[1,7]<-  R[1,7]*sqrt(Sigma_D[1,1]*Sigma_D[7,7])
    Sigma_D[8,1]<- Sigma_D[1,8]<-  R[1,8]*sqrt(Sigma_D[1,1]*Sigma_D[8,8])
    
    Sigma_D[2,3]<- Sigma_D[3,2]<-  R[2,3]*sqrt(Sigma_D[2,2]*Sigma_D[3,3])
    Sigma_D[2,4]<- Sigma_D[4,2]<-  R[2,4]*sqrt(Sigma_D[2,2]*Sigma_D[4,4])
    Sigma_D[2,7]<- Sigma_D[7,2]<-  R[2,7]*sqrt(Sigma_D[2,2]*Sigma_D[7,7])
    Sigma_D[2,8]<- Sigma_D[8,2]<-  R[2,8]*sqrt(Sigma_D[2,2]*Sigma_D[8,8])
    
    Sigma_D[3,5]<- Sigma_D[5,3]<-  R[3,5]*sqrt(Sigma_D[5,5]*Sigma_D[3,3])
    Sigma_D[3,6]<- Sigma_D[6,3]<-  R[3,6]*sqrt(Sigma_D[6,6]*Sigma_D[3,3])
    Sigma_D[4,5]<- Sigma_D[5,4]<-  R[4,5]*sqrt(Sigma_D[5,5]*Sigma_D[4,4])
    Sigma_D[4,6]<- Sigma_D[6,4]<-  R[4,6]*sqrt(Sigma_D[6,6]*Sigma_D[4,4])
    
    Sigma_D[7,5]<- Sigma_D[5,7]<-  R[7,5]*sqrt(Sigma_D[5,5]*Sigma_D[7,7])
    Sigma_D[8,5]<- Sigma_D[5,8]<-  R[8,5]*sqrt(Sigma_D[5,5]*Sigma_D[8,8])
    Sigma_D[7,6]<- Sigma_D[6,7]<-  R[7,6]*sqrt(Sigma_D[6,6]*Sigma_D[7,7])
    Sigma_D[8,6]<- Sigma_D[6,8]<-  R[8,6]*sqrt(Sigma_D[6,6]*Sigma_D[8,8])
    complete_D[[i]]<- Sigma_D
  }
}

complete_D<-complete_D[!sapply(complete_D,is.null)]
##check D are unique?
length(unique(complete_D))
length(complete_D)

Q<- matrix(c(-1, 0,1,0,
             0, -1,0,1), nrow=2, ncol=4)
I5<- diag(5)
z_1<- matrix(c(1,1), nrow = 1, ncol = 2)
za<-zb<- direct.sum(z_1,z_1)
z<- direct.sum(za,zb)

e_1<- matrix(c(1,0,0,0,0), nrow = 5, ncol = 1)
e_2<- matrix(c(0,1,0,0,0), nrow = 5, ncol = 1)
e_3<- matrix(c(0,0,1,0,0), nrow = 5, ncol = 1)
e_4<- matrix(c(0,0,0,1,0), nrow = 5, ncol = 1)
e_5<- matrix(c(0,0,0,0,1), nrow = 5, ncol = 1)

z<- kr<-kronecker(e_1,z)+kronecker(e_2,z)+kronecker(e_3,z)+kronecker(e_4,z)+kronecker(e_5,z)
A<- (kronecker(Q,I5) %*%z)

#RH<- data.frame()
##calculate R_H^2 using combination of V and D matrices.  1461*533 combination
for(i in 1:length(complete_V)){
  for(j in 1:length(complete_D)){
    u_11<- as.numeric(complete_V[[i]][1,1]+ complete_V[[i]][2,2]- complete_V[[i]][1,2])
    u_22<- as.numeric(complete_V[[i]][3,3]+ complete_V[[i]][4,4]- complete_V[[i]][3,4])
    u_12<- as.numeric(complete_V[[i]][1,3]+ complete_V[[i]][2,4]- complete_V[[i]][2,3]-complete_V[[i]][1,4] )
    delta_u<- matrix(c(u_11, u_12,u_12,u_22), nrow=2, ncol=2)
    delta_sigma<- A  %*% complete_D[[j]] %*% t(A) + (kronecker(delta_u,cor_matrix))
    
    if (is.positive.definite(delta_sigma)==TRUE){
      rh_square<- 1- ( (det(delta_sigma) / (det(delta_sigma[1:5,1:5])*det(delta_sigma[6:10,6:10]) ) ))
      write(mean(as.numeric(rh_square)), file="~/Downloads/rh_square_25april_uncs_setting1.txt", append=TRUE)
      
    }
    
    
  }
}

RH_square_2_txt<-read.delim(file= '~/Downloads/rh_square_25april_uncs_setting1.txt', header = FALSE, sep = "\t", dec = ".")

min(RH_square_2_txt$V1)
max(RH_square_2_txt$V1)
sd(RH_square_2_txt$V1)
mean(RH_square_2_txt$V1)
median(RH_square_2_txt$V1)
quantile(RH_square_2_txt$V1)


hist( as.numeric(RH_square_2_txt$V1), main="", xlab=bquote("R"[Lambda]^2),   labels = TRUE,breaks = 15, ylim=c(0,210000))
grid()
box()
