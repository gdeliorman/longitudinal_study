##read the data
library(readr)

experimental_case_2_logcgi<- read_csv("/Users/gokcedeliorman/Downloads/experimental_case_4_log_cgi.csv", col_names=TRUE)
placebo_case_2_logcgi<- read_csv("/Users/gokcedeliorman/Downloads/placebo_case_4_log_cgi.csv", col_names=TRUE)

##/Users/gokcedeliorman/Downloads/case_1_fin1_cgi_transform.csv full data
experimental_case_1_fin1_logcgi<- read_csv("/Users/gokcedeliorman/Downloads/experimental_case_1_fin_1cgi_log.csv", col_names=TRUE)
placebo_case_1_fin1_logcgi<- read_csv("/Users/gokcedeliorman/Downloads/placebo_case_1_fin1_cgi_log.csv", col_names=TRUE)

#seperate as surrogate and true endpoint CGI FIN1
TZ1<- experimental_case_1_fin1_logcgi[experimental_case_1_fin1_logcgi$type2 == "CGI_SEV",]
TZ0<- placebo_case_1_fin1_logcgi[placebo_case_1_fin1_logcgi$type2 == "CGI_SEV",]
SZ1<- experimental_case_1_fin1_logcgi[experimental_case_1_fin1_logcgi$type2 == "PANSS",]
SZ0<- placebo_case_1_fin1_logcgi[placebo_case_1_fin1_logcgi$type2 == "PANSS",]


#seperate as surrogate and true endpoint CGI FIN1
TZ1_2<- experimental_case_2_logcgi[experimental_case_2_logcgi$type2 == "CGI_SEV",]
TZ0_2<- placebo_case_2_logcgi[placebo_case_2_logcgi$type2 == "CGI_SEV",]
SZ1_2<- experimental_case_2_logcgi[experimental_case_2_logcgi$type2 == "PANSS",]
SZ0_2<- placebo_case_2_logcgi[placebo_case_2_logcgi$type2 == "PANSS",]



## TREATMENT=1 PANSS z=1 SZ1
plot(1,1, pch=16,  xlab = "time", 
     ylab="log PANSS", ylim=c(3.5,5),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(0,6))
for (i in 3:71) {
  new_data<- SZ1[SZ1$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6),labels=c("0","1", "2", "3",  "4","5", "6"))

points(0, mean( SZ1[SZ1$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( SZ1[SZ1$XTIME == "1",]$resp_log ),  pch=16) #0
points(2, mean( SZ1[SZ1$XTIME == "2",]$resp_log ),  pch=16) #1
points(4, mean( SZ1[SZ1$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( SZ1[SZ1$XTIME == "6",]$resp_log ),  pch=16) #6

mean_x<- c(0,1,2,4,6)
mean_y<-  c(mean( SZ1[SZ1$XTIME == "0",]$resp_log ),mean( SZ1[SZ1$XTIME == "1",]$resp_log ),mean( SZ1[SZ1$XTIME == "2",]$resp_log ),
            mean( SZ1[SZ1$XTIME == "4",]$resp_log ), mean( SZ1[SZ1$XTIME == "6",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0



## TREATMENT=0 PANSS z=0 SZ0
plot(1,1, pch=16,  xlab = "time", 
     ylab="log PANSS", ylim=c(3.5,5),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(0,6))
for (i in 1:71) {
  new_data<- SZ0[SZ0$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6),labels=c("0","1", "2", "3",  "4","5", "6"))

points(0, mean( SZ0[SZ0$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( SZ0[SZ0$XTIME == "1",]$resp_log ),  pch=16) #0
points(2, mean( SZ0[SZ0$XTIME == "2",]$resp_log ),  pch=16) #1
points(4, mean( SZ0[SZ0$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( SZ0[SZ0$XTIME == "6",]$resp_log ),  pch=16) #6

mean_x<- c(0,1,2,4,6)
mean_y<-  c(mean( SZ0[SZ0$XTIME == "0",]$resp_log ),mean( SZ0[SZ0$XTIME == "1",]$resp_log ),mean( SZ0[SZ0$XTIME == "2",]$resp_log ),
            mean( SZ0[SZ0$XTIME == "4",]$resp_log ), mean( SZ0[SZ0$XTIME == "6",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0




## TREATMENT=1 CGI z=1 TZ1
plot(1,1, pch=16,  xlab = "time", 
     ylab="log CGI", ylim=c(0.5,2),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(0,6))
for (i in 3:71) {
  new_data<- TZ1[TZ1$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6),labels=c("0","1", "2", "3",  "4","5", "6"))

points(0, mean( TZ1[TZ1$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( TZ1[TZ1$XTIME == "1",]$resp_log ),  pch=16) #0
points(2, mean( TZ1[TZ1$XTIME == "2",]$resp_log ),  pch=16) #1
points(4, mean( TZ1[TZ1$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( TZ1[TZ1$XTIME == "6",]$resp_log ),  pch=16) #6

mean_x<- c(0,1,2,4,6)
mean_y<-  c(mean( TZ1[TZ1$XTIME == "0",]$resp_log ),mean( TZ1[TZ1$XTIME == "1",]$resp_log ),mean( TZ1[TZ1$XTIME == "2",]$resp_log ),
            mean( TZ1[TZ1$XTIME == "4",]$resp_log ), mean( TZ1[TZ1$XTIME == "6",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0


## TREATMENT=0 CGI z=1 TZ0
plot(1,1, pch=16,  xlab = "time", 
     ylab="log CGI", ylim=c(0.5,2),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(0,6))
for (i in 1:70) {
  new_data<- TZ0[TZ0$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6),labels=c("0","1", "2", "3",  "4","5", "6"))

points(0, mean( TZ0[TZ0$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( TZ0[TZ0$XTIME == "1",]$resp_log ),  pch=16) #0
points(2, mean( TZ0[TZ0$XTIME == "2",]$resp_log ),  pch=16) #1
points(4, mean( TZ0[TZ0$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( TZ0[TZ0$XTIME == "6",]$resp_log ),  pch=16) #6

mean_x<- c(0,1,2,4,6)
mean_y<-  c(mean( TZ0[TZ0$XTIME == "0",]$resp_log ),mean( TZ0[TZ0$XTIME == "1",]$resp_log ),mean( TZ0[TZ0$XTIME == "2",]$resp_log ),
            mean( TZ0[TZ0$XTIME == "4",]$resp_log ), mean( TZ0[TZ0$XTIME == "6",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0



##second data 3 trials
## TREATMENT=1 PANSS z=1 SZ1_2
plot(1,1, pch=16,  xlab = "time", 
     ylab="log PANSS", ylim=c(3.39,5.1),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(0,8))
for (i in 1:600) {
  new_data<- SZ1_2[SZ1_2$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8),labels=c("0","1", "2", "3",  "4","5", "6", "7", "8"))

points(0, mean( SZ1_2[SZ1_2$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( SZ1_2[SZ1_2$XTIME == "1",]$resp_log ),  pch=16) #0
points(2, mean( SZ1_2[SZ1_2$XTIME == "2",]$resp_log ),  pch=16) #1
points(4, mean( SZ1_2[SZ1_2$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( SZ1_2[SZ1_2$XTIME == "6",]$resp_log ),  pch=16) #6
points(8, mean( SZ1_2[SZ1_2$XTIME == "8",]$resp_log ),  pch=16) #6

mean_x<- c(0,1,2,4,6,8)
mean_y<-  c(mean( SZ1_2[SZ1_2$XTIME == "0",]$resp_log ),mean( SZ1_2[SZ1_2$XTIME == "1",]$resp_log ),mean( SZ1_2[SZ1_2$XTIME == "2",]$resp_log ),
            mean( SZ1_2[SZ1_2$XTIME == "4",]$resp_log ), mean( SZ1_2[SZ1_2$XTIME == "6",]$resp_log ), mean( SZ1_2[SZ1_2$XTIME == "8",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0



## TREATMENT=1 PANSS z=0 SZ0_2
plot(1,1, pch=16,  xlab = "time", 
     ylab="log PANSS", ylim=c(3.39,5.1),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(0,8))
for (i in 2:599) {
  new_data<- SZ0_2[SZ0_2$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8),labels=c("0","1", "2", "3",  "4","5", "6", "7", "8"))

points(0, mean( SZ0_2[SZ0_2$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( SZ0_2[SZ0_2$XTIME == "1",]$resp_log ),  pch=16) #0
points(2, mean( SZ0_2[SZ0_2$XTIME == "2",]$resp_log ),  pch=16) #1
points(4, mean( SZ0_2[SZ0_2$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( SZ0_2[SZ0_2$XTIME == "6",]$resp_log ),  pch=16) #6
points(8, mean( SZ0_2[SZ0_2$XTIME == "8",]$resp_log ),  pch=16) #6

mean_x<- c(0,1,2,4,6,8)
mean_y<-  c(mean( SZ0_2[SZ0_2$XTIME == "0",]$resp_log ),mean( SZ0_2[SZ0_2$XTIME == "1",]$resp_log ),mean( SZ0_2[SZ0_2$XTIME == "2",]$resp_log ),
            mean( SZ0_2[SZ0_2$XTIME == "4",]$resp_log ), mean( SZ0_2[SZ0_2$XTIME == "6",]$resp_log ), mean( SZ0_2[SZ0_2$XTIME == "8",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0


## TREATMENT=1 CGI z=1 TZ1_2
plot(1,1, pch=16,  xlab = "time", 
     ylab="log CGI", ylim=c(0,2),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(0,8))
for (i in 1:600) {
  new_data<- TZ1_2[TZ1_2$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8),labels=c("0","1", "2", "3",  "4","5", "6", "7", "8"))

points(0, mean( TZ1_2[TZ1_2$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( TZ1_2[TZ1_2$XTIME == "1",]$resp_log ),  pch=16) #0
points(2, mean( TZ1_2[TZ1_2$XTIME == "2",]$resp_log ),  pch=16) #1
points(4, mean( TZ1_2[TZ1_2$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( TZ1_2[TZ1_2$XTIME == "6",]$resp_log ),  pch=16) #6
points(8, mean( TZ1_2[TZ1_2$XTIME == "8",]$resp_log ),  pch=16) #6

mean_x<- c(0,1,2,4,6,8)
mean_y<-  c(mean( TZ1_2[TZ1_2$XTIME == "0",]$resp_log ),mean( TZ1_2[TZ1_2$XTIME == "1",]$resp_log ),mean( TZ1_2[TZ1_2$XTIME == "2",]$resp_log ),
            mean( TZ1_2[TZ1_2$XTIME == "4",]$resp_log ), mean( TZ1_2[TZ1_2$XTIME == "6",]$resp_log ), mean( TZ1_2[TZ1_2$XTIME == "8",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0

## TREATMENT=0 CGI z=0 TZ0_2
plot(1,1, pch=16,  xlab = "time", 
     ylab="log CGI", ylim=c(0,2),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(0,8))
for (i in 2:599) {
  new_data<- TZ0_2[TZ0_2$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8),labels=c("0","1", "2", "3",  "4","5", "6", "7", "8"))

points(0, mean( TZ0_2[TZ0_2$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( TZ0_2[TZ0_2$XTIME == "1",]$resp_log ),  pch=16) #0
points(2, mean( TZ0_2[TZ0_2$XTIME == "2",]$resp_log ),  pch=16) #1
points(4, mean( TZ0_2[TZ0_2$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( TZ0_2[TZ0_2$XTIME == "6",]$resp_log ),  pch=16) #6
points(8, mean( TZ0_2[TZ0_2$XTIME == "8",]$resp_log ),  pch=16) #6

mean_x<- c(0,1,2,4,6,8)
mean_y<-  c(mean( TZ0_2[TZ0_2$XTIME == "0",]$resp_log ),mean( TZ0_2[TZ0_2$XTIME == "1",]$resp_log ),mean( TZ0_2[TZ0_2$XTIME == "2",]$resp_log ),
            mean( TZ0_2[TZ0_2$XTIME == "4",]$resp_log ), mean( TZ0_2[TZ0_2$XTIME == "6",]$resp_log ), mean( TZ0_2[TZ0_2$XTIME == "8",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0
