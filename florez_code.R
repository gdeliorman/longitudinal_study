D_template<- matrix(c(
  38.6455, 57.4401,    NA,    NA, -3.3569, -5.4102,      NA,     NA,
  57.4401, 105.47,     NA,    NA, -5.9486, -10.0573,     NA,     NA,
  NA,      NA,      100.88, 154.02,  NA,    NA,     -5.0632, -3.6366,
  NA,      NA,       154.02,  258.51,  NA,    NA,      -2.2534,  3.9227,
  -3.3569, -5.9486,   NA,    NA,  2.0368,  0.4484,    NA,     NA,
  -5.4102, -10.0573,  NA,    NA,  0.4484,  2.6058,    NA,     NA,
  NA,      NA,      -5.0632,  -3.6366,  NA,    NA,     7.8797,  3.3763,
  NA,      NA,      -2.2534, 3.9227,    NA,    NA,     3.3763 ,  4.8466
), nrow = 8, byrow = TRUE)

R_template<- cov2cor(D_template)



ica_mpc<-ICA.ContCont.MultS.MPC(M=10000,N=568,Sigma=D_template,prob = NULL,Seed=123,
                                Save.Corr=TRUE, Show.Progress=FALSE)


# ica_mpc$Lower.Dig.Corrs.All[1,]
# ncol(ica_mpc$Lower.Dig.Corrs.All)
# nrow(ica_mpc$Lower.Dig.Corrs.All)

R<- R_template
##fill R matrix
R[3,1]<- R[1,3]<-  ica_mpc$Lower.Dig.Corrs.All[1,2]
R[4,1]<- R[1,4]<-  ica_mpc$Lower.Dig.Corrs.All[1,3]
R[7,1]<- R[1,7]<-  ica_mpc$Lower.Dig.Corrs.All[1,6]
R[8,1]<- R[1,8]<-  ica_mpc$Lower.Dig.Corrs.All[1,7]

R[2,3]<- R[3,2]<-  ica_mpc$Lower.Dig.Corrs.All[1,8]
R[2,4]<- R[4,2]<-  ica_mpc$Lower.Dig.Corrs.All[1,9]
R[2,7]<- R[7,2]<-  ica_mpc$Lower.Dig.Corrs.All[1,12]
R[2,8]<- R[8,2]<-  ica_mpc$Lower.Dig.Corrs.All[1,13]

R[3,5]<- R[5,3]<-  ica_mpc$Lower.Dig.Corrs.All[1,15]
R[3,6]<- R[6,3]<-  ica_mpc$Lower.Dig.Corrs.All[1,16]
R[4,5]<- R[5,4]<-  ica_mpc$Lower.Dig.Corrs.All[1,19]
R[4,6]<- R[6,4]<-  ica_mpc$Lower.Dig.Corrs.All[1,20]

R[7,5]<- R[5,7]<-  ica_mpc$Lower.Dig.Corrs.All[1,24]
R[8,5]<- R[5,8]<-  ica_mpc$Lower.Dig.Corrs.All[1,25]
R[7,6]<- R[6,7]<-  ica_mpc$Lower.Dig.Corrs.All[1,26]
R[8,6]<- R[6,8]<-  ica_mpc$Lower.Dig.Corrs.All[1,27]

is.positive.definite(R)

valid_rows <- which(complete.cases(ica_mpc$Lower.Dig.Corrs.All))
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
   write.table(i, file = "~/Downloads/i_24april.csv", row.names = FALSE, col.names = FALSE, append = TRUE)
   write.table(R, file = "~/Downloads/R_24april.csv", row.names = FALSE, col.names = FALSE, append = TRUE)
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
   write.table(Sigma_D, file = "~/Downloads/Sigma_D_24april.csv", row.names = FALSE, col.names = FALSE, append = TRUE)
   
}
}

##When i is 173 then D is pdf
i<-649
is.positive.definite(Sigma_D)

##read tables
sigma_d<- read.table("~/Downloads/Sigma_D_24april.csv", header = FALSE, sep = "")
sigma_d[1:8,1:8]
is.positive.definite(sigma_d[1:8,1:8])


sigma_d_list<- list()
for (i in 1:(nrow(sigma_d)/8)) {
  sigma_d_sub<- sigma_d[((i-1)*8+1):(i*8),1:8]
  sigma_d_list[[i]]<- sigma_d_sub
}

