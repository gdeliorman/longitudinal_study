##setting 3 model 3 type=un@cs p=6 (6 time points)

library(matrixcalc)

V<- 0.01557
corr_V<- cov2cor(V)
V<- matrix(0, nrow = 12, ncol=12)
diag(V)<-0.01557


##The correspondence of the SAS output to the D matrix

D<- matrix(NA, nrow = 8, ncol=8)

un11<- D[1,1] <- 0.04105
un22<- D[3,3] <- 0.04105
un31<- D[5,1] <- D[1,5] <- 0.02716
un33<- D[5,5] <- 0.04118
un42<- D[7,3] <- D[3,7] <- 0.03323
un44<- D[7,7] <- 0.04995
un51<- D[1,2] <- D[2,1] <- 0.000955
un53<- D[2,5] <- D[5,2] <- 0.000311
un55<- D[2,2] <- 0.001209
un62<- D[3,4] <- D[4,3] <- 0.000270
un64<- D[4,7] <- D[7,4] <- -0.00103
un66<- D[4,4] <- 0.001660
un71<- D[1,6] <- D[6,1] <- 0.000437
un73<- D[5,6] <- D[6,5] <- 0.000069
un75<- D[2,6] <- D[6,2] <- 0.001119
un77<- D[6,6] <- 0.001245
un82<- D[3,8] <- D[8,3] <- -0.00012
un84<- D[7,8] <- D[8,7] <- -0.00122
un86<- D[4,8] <- D[8,4] <- 0.002010
un88<- D[8,8] <- 0.002595


#R_template<- cov2cor(D_template)
R_template<- cov2cor(D)

##D pdf matrices
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
#for(i in 1:length(complete_V)){
  #for(i in 955:length(complete_V)){
  for(j in 1:length(complete_D)){
    
    
    first_part<- A  %*% complete_D[[j]] %*% t(A) 
    first_part<-  round(first_part, digits = 6)
    
    delta_sigma<- first_part+ V
    #delta_sigma<- first_part
    
    delta_sigma_2<- round(delta_sigma, digits = 6)
    
    if ( is.positive.definite(delta_sigma_2)==TRUE){
      rh_square<- 1- ( (det(delta_sigma_2) / (det(delta_sigma_2[1:6,1:6])*det(delta_sigma_2[7:12,7:12]) ) ))
      write(mean(as.numeric(rh_square)), file="~/Downloads/rh_square_22may_without_rep_setting4.txt", append=TRUE)
      
    }
    
    
  }


RH_square_2_txt<-read.delim(file= '~/Downloads/rh_square_22may_without_rep_setting4.txt', header = FALSE, sep = "\t", dec = ".")



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
hist( as.numeric(RH_square_2_txt$V1), main="", xlab=bquote("R"[Lambda]^2),  breaks = 3, ylim=c(0,23000), xlim=c(0,1))

grid()
box()
