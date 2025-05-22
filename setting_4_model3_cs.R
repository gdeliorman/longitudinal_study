##setting 3 model 3 type=un@cs p=6 (6 time points)

library(matrixcalc)

V<- matrix(c(0.01365,NA,0.009203,NA,
             NA,0.01379,NA,0.008759,
             0.009203,NA,0.01819	, NA,
             NA,0.008759,NA,0.01854), nrow = 4, ncol = 4)

corr_V<- cov2cor(V)
corr<-0.000236

library(fastmatrix)
cor_matrix<-corCS(corr, p = 6)
cor_matrix<- matrix(c(1,corr,corr,corr,corr,corr,
                      corr,1,corr,corr,corr,corr,
                      corr,corr,1,corr,corr,corr,
                      corr,corr,corr,1,corr,corr,
                      corr,corr,corr,corr,1,corr,
                      corr,corr,corr,corr,corr,1), nrow=6, ncol=6)

##estimate V matrix

library(Surrogate)
ICA_V<- ICA.ContCont(T0S0=corr_V[1,3], T1S1=corr_V[2,4], T0T0=V[1,1], T1T1=V[2,2], S0S0=V[3,3], S1S1=V[4,4], T0T1=seq(-1, 1, by=.1), 
                     T0S1=seq(-1, 1, by=.1), T1S0=seq(-1, 1, by=.1), S0S1=seq(-1, 1, by=.1))


ICA_V<- ICA.ContCont(T0S0=corr_V[1,3], T1S1=corr_V[2,4], T0T0=V[1,1], T1T1=V[2,2], S0S0=V[3,3], S1S1=V[4,4], T0T1=seq(-1, 1, by=.2), 
                     T0S1=seq(-1, 1, by=.2), T1S0=seq(-1, 1, by=.2), S0S1=seq(-1, 1, by=.2))

## 1031 pdf V matrices
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

##The correspondence of the SAS output to the D matrix

D<- matrix(NA, nrow = 8, ncol=8)

un11<- D[1,1] <- 0.04162
un22<- D[3,3] <- 0.04140
un31<- D[5,1] <- D[1,5] <- 0.02306
un33<- D[5,5] <- 0.03991
un42<- D[7,3] <- D[3,7] <- 0.02896
un44<- D[7,7] <- 0.04811
un51<- D[1,2] <- D[2,1] <- 0.001000
un53<- D[2,5] <- D[5,2] <- 0.001224
un55<- D[2,2] <- 0.001086
un62<- D[3,4] <- D[4,3] <- 0.000439
un64<- D[4,7] <- D[7,4] <- 0.000025
un66<- D[4,4] <- 0.001452
un71<- D[1,6] <- D[6,1] <- 0.001114
un73<- D[5,6] <- D[6,5] <- 0.000374
un75<- D[2,6] <- D[6,2] <- 0.000684
un77<- D[6,6] <- 0.000919
un82<- D[3,8] <- D[8,3] <- 0.000918
un84<- D[7,8] <- D[8,7] <- -0.00053
un86<- D[4,8] <- D[8,4] <- 0.001472
un88<- D[8,8] <- 0.002100


#R_template<- cov2cor(D_template)
R_template<- cov2cor(D)

##7162 D pdf matrices
ica_mpc<-ICA.ContCont.MultS.MPC(M=100000,N=568,Sigma=D,prob = NULL,Seed=123,
                                Save.Corr=TRUE, Show.Progress=FALSE)

ica_mpc<-ICA.ContCont.MultS.MPC(M=10000,N=568,Sigma=D,prob = NULL,Seed=123,
                                Save.Corr=TRUE, Show.Progress=FALSE)

#ica_mpc<-ICA.ContCont.MultS.MPC(M=1000000,N=568,Sigma=D,prob = NULL,Seed=123, Save.Corr=TRUE, Show.Progress=FALSE)


valid_rows <- which(complete.cases(ica_mpc$Lower.Dig.Corrs.All))
complete_D<- list()
for (i in valid_rows) {
  
  R<- R_template
  Sigma_D<- D
  
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
  
  #R2<- matrix( c(as.numeric(formatC(R,  digits = 15))), ncol=8, nrow=8)
  R2<- round(R, digits = 15)
  
  if( is.positive.definite(R2) ==TRUE){
    Sigma_D[3,1]<- Sigma_D[1,3]<-  R2[1,3]*sqrt(Sigma_D[1,1]*Sigma_D[3,3])
    Sigma_D[4,1]<- Sigma_D[1,4]<-  R2[1,4]*sqrt(Sigma_D[1,1]*Sigma_D[4,4])
    Sigma_D[7,1]<- Sigma_D[1,7]<-  R2[1,7]*sqrt(Sigma_D[1,1]*Sigma_D[7,7])
    Sigma_D[8,1]<- Sigma_D[1,8]<-  R2[1,8]*sqrt(Sigma_D[1,1]*Sigma_D[8,8])
    
    Sigma_D[2,3]<- Sigma_D[3,2]<-  R2[2,3]*sqrt(Sigma_D[2,2]*Sigma_D[3,3])
    Sigma_D[2,4]<- Sigma_D[4,2]<-  R2[2,4]*sqrt(Sigma_D[2,2]*Sigma_D[4,4])
    Sigma_D[2,7]<- Sigma_D[7,2]<-  R2[2,7]*sqrt(Sigma_D[2,2]*Sigma_D[7,7])
    Sigma_D[2,8]<- Sigma_D[8,2]<-  R2[2,8]*sqrt(Sigma_D[2,2]*Sigma_D[8,8])
    
    Sigma_D[3,5]<- Sigma_D[5,3]<-  R2[3,5]*sqrt(Sigma_D[5,5]*Sigma_D[3,3])
    Sigma_D[3,6]<- Sigma_D[6,3]<-  R2[3,6]*sqrt(Sigma_D[6,6]*Sigma_D[3,3])
    Sigma_D[4,5]<- Sigma_D[5,4]<-  R2[4,5]*sqrt(Sigma_D[5,5]*Sigma_D[4,4])
    Sigma_D[4,6]<- Sigma_D[6,4]<-  R2[4,6]*sqrt(Sigma_D[6,6]*Sigma_D[4,4])
    
    Sigma_D[7,5]<- Sigma_D[5,7]<-  R2[7,5]*sqrt(Sigma_D[5,5]*Sigma_D[7,7])
    Sigma_D[8,5]<- Sigma_D[5,8]<-  R2[8,5]*sqrt(Sigma_D[5,5]*Sigma_D[8,8])
    Sigma_D[7,6]<- Sigma_D[6,7]<-  R2[7,6]*sqrt(Sigma_D[6,6]*Sigma_D[7,7])
    Sigma_D[8,6]<- Sigma_D[6,8]<-  R2[8,6]*sqrt(Sigma_D[6,6]*Sigma_D[8,8])
    complete_D[[i]]<- Sigma_D
  }
}

complete_D<-complete_D[!sapply(complete_D,is.null)]
##check D are unique?
length(unique(complete_D))
length(complete_D)

Q<- matrix(c(-1,0,1,0,
             0, -1,0,1), nrow=2, ncol=4)
I6<- diag(6)
z_1<- matrix(c(1,1), nrow = 1, ncol = 2)
za<-zb<- direct.sum(z_1,z_1)
z1<- direct.sum(za,zb)

e_1<- matrix(c(1,0,0,0,0,0), nrow = 6, ncol = 1)
e_2<- matrix(c(0,1,0,0,0,0), nrow = 6, ncol = 1)
e_3<- matrix(c(0,0,1,0,0,0), nrow = 6, ncol = 1)
e_4<- matrix(c(0,0,0,1,0,0), nrow = 6, ncol = 1)
e_5<- matrix(c(0,0,0,0,1,0), nrow = 6, ncol = 1)
e_6<- matrix(c(0,0,0,0,0,1), nrow = 6, ncol = 1)


z<- kr<-kronecker(e_1,z1)+kronecker(e_2,z1)+kronecker(e_3,z1)+kronecker(e_4,z1)+kronecker(e_5,z1)+kronecker(e_6,z1)
A<- (kronecker(Q,I6) %*%z)

##calculate R_H^2 using combination of V and D matrices.  1461*4952 combination
for(i in 1:length(complete_V)){
  #for(i in 955:length(complete_V)){
  for(j in 1:length(complete_D)){
    
    u_11<- as.numeric(complete_V[[i]][1,1]+ complete_V[[i]][2,2]- complete_V[[i]][1,2])
    u_22<- as.numeric(complete_V[[i]][3,3]+ complete_V[[i]][4,4]- complete_V[[i]][3,4])
    u_12<- as.numeric(complete_V[[i]][1,3]+ complete_V[[i]][2,4]- complete_V[[i]][2,3]-complete_V[[i]][1,4] )
    delta_u<- matrix(c(u_11, u_12,u_12,u_22), nrow=2, ncol=2)
    delta_u<- matrix( c(as.numeric(formatC(delta_u,  digits = 15))), ncol=2, nrow=2)
    
    complete_D[[j]]<- round(complete_D[[j]], digits = 15)
    first_part<- A  %*% complete_D[[j]] %*% t(A) 
    first_part<-  round(first_part, digits = 4)
    
    delta_sigma<- first_part+ (kronecker(delta_u,cor_matrix))
    delta_sigma_2<- round(delta_sigma, digits = 6)
    
    if ( is.positive.definite(delta_sigma_2)==TRUE & is.positive.definite(delta_u)==TRUE){
      rh_square<- 1- ( (det(delta_sigma_2) / (det(delta_sigma_2[1:6,1:6])*det(delta_sigma_2[7:12,7:12]) ) ))
      write(mean(as.numeric(rh_square)), file="~/Downloads/rh_square_22may_uncs_setting4_small.txt", append=TRUE)
      
    }
    
    
  }
}

RH_square_2_txt<-read.delim(file= '~/Downloads/rh_square_22may_uncs_setting4.txt', header = FALSE, sep = "\t", dec = ".")

RH_square_2_txt<-read.delim(file= '~/Downloads/rh_square_22may_uncs_setting4_small.txt', header = FALSE, sep = "\t", dec = ".")


min(RH_square_2_txt$V1)
max(RH_square_2_txt$V1)
sd(RH_square_2_txt$V1)
mean(RH_square_2_txt$V1)
median(RH_square_2_txt$V1)
quantile(RH_square_2_txt$V1)

margin<- qt(0.95,df=length(RH_square_2_txt$V1)-1)*sd(RH_square_2_txt$V1)/sqrt(length(RH_square_2_txt$V1))
lowerinterval <- mean(RH_square_2_txt$V1) - margin
upperinterval <- mean(RH_square_2_txt$V1) + margin


#hist( as.numeric(RH_square_2_txt$V1), main="", xlab=bquote("R"[Lambda]^2),   labels = TRUE,breaks = 15, ylim=c(0,5100000))
hist( as.numeric(RH_square_2_txt$V1), main="", xlab=bquote("R"[Lambda]^2),  breaks = 5, ylim=c(0,6000000), xlim=c(0,1))

grid()
box()
