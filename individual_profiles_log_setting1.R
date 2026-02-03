##individual profiles for log transformed setting 1 data

##read data
log_setting_1_data<- read.csv("/Users/gokcedeliorman/Downloads/setting_1_data_log_transform.csv")

##divide data as treatment and placebo
treatment<- log_setting_1_data[log_setting_1_data$TREAT == "1",]
placebo<- log_setting_1_data[log_setting_1_data$TREAT == "0",]


###seperate as surrogate and true endpoint bprs
TZ1<- treatment[treatment$type2 == "PANSS",]
TZ0<- placebo[placebo$type2 == "PANSS",]
SZ1<- treatment[treatment$type2 == "BPRS",]
SZ0<- placebo[placebo$type2 == "BPRS",]


## TREATMENT=0 log panss z=0 TZ0
plot(1,1, pch=16,  xlab = "time", 
     ylab="log PANSS", ylim=c(3.4,5.1),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(0,8))
for (i in 3:653) {
  new_data<- TZ0[TZ0$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8),labels=c("0","1", "2", "3",  "4","5", "6","7", "8"))


points(0, mean( TZ0[TZ0$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( TZ0[TZ0$XTIME == "1",]$resp_log ),  pch=16) #1
points(2, mean( TZ0[TZ0$XTIME == "2",]$resp_log ),  pch=16) #2
points(4, mean( TZ0[TZ0$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( TZ0[TZ0$XTIME == "6",]$resp_log ),  pch=16) #6
points(8, mean( TZ0[TZ0$XTIME == "8",]$resp_log ),  pch=16) #8

mean_x<- c(0,1,2,4,6,8)
mean_y<-  c(mean( TZ0[TZ0$XTIME == "0",]$resp_log ),mean( TZ0[TZ0$XTIME == "1",]$resp_log ),mean( TZ0[TZ0$XTIME == "2",]$resp_log ),
            mean( TZ0[TZ0$XTIME == "4",]$resp_log ), mean( TZ0[TZ0$XTIME == "6",]$resp_log ), mean( TZ0[TZ0$XTIME == "8",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0




## TREATMENT=1 log panss z=1 TZ1
plot(1,1, pch=16,  xlab = "time", 
     ylab="log PANSS", ylim=c(3.4,5.1),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(0,8))
for (i in 1:652) {
  new_data<- TZ1[TZ1$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8),labels=c("0","1", "2", "3",  "4","5", "6","7", "8"))


points(0, mean( TZ1[TZ1$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( TZ1[TZ1$XTIME == "1",]$resp_log ),  pch=16) #1
points(2, mean( TZ1[TZ1$XTIME == "2",]$resp_log ),  pch=16) #2
points(4, mean( TZ1[TZ1$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( TZ1[TZ1$XTIME == "6",]$resp_log ),  pch=16) #6
points(8, mean( TZ1[TZ1$XTIME == "8",]$resp_log ),  pch=16) #8

mean_x<- c(0,1,2,4,6,8)
mean_y<-  c(mean( TZ1[TZ1$XTIME == "0",]$resp_log ),mean( TZ1[TZ1$XTIME == "1",]$resp_log ),mean( TZ1[TZ1$XTIME == "2",]$resp_log ),
            mean( TZ1[TZ1$XTIME == "4",]$resp_log ), mean( TZ1[TZ1$XTIME == "6",]$resp_log ), mean( TZ1[TZ1$XTIME == "8",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0




## TREATMENT=0 bprs z=0 SZ0
plot(1,1, pch=16,  xlab = "time", 
     ylab="log BPRS", ylim=c(2.7,4.8),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(0,8))
for (i in 3:653) {
  new_data<- SZ0[SZ0$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8),labels=c("0","1", "2", "3",  "4","5", "6","7", "8"))

points(0, mean( SZ0[SZ0$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( SZ0[SZ0$XTIME == "1",]$resp_log ),  pch=16) #1
points(2, mean( SZ0[SZ0$XTIME == "2",]$resp_log ),  pch=16) #2
points(4, mean( SZ0[SZ0$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( SZ0[SZ0$XTIME == "6",]$resp_log ),  pch=16) #6
points(8, mean( SZ0[SZ0$XTIME == "8",]$resp_log ),  pch=16) #6


mean_x<- c(0, 1,2,4,6,8)
mean_y<-  c(mean( SZ0[SZ0$XTIME == "0",]$resp_log ),mean( SZ0[SZ0$XTIME == "1",]$resp_log ),mean( SZ0[SZ0$XTIME == "2",]$resp_log ),
            mean( SZ0[SZ0$XTIME == "4",]$resp_log ), mean( SZ0[SZ0$XTIME == "6",]$resp_log), mean( SZ0[SZ0$XTIME == "8",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0


## TREATMENT=1 log bprs z=1 SZ1
plot(1,1, pch=16,  xlab = "time", 
     ylab="log BPRS", ylim=c(2.7,4.6),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(0,8))
for (i in 1:652) {
  new_data<- SZ1[SZ1$id == i,]
  lines(new_data$XTIME, new_data$resp_log, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8),labels=c("0","1", "2", "3",  "4","5", "6","7", "8"))

points(0, mean( SZ1[SZ1$XTIME == "0",]$resp_log ),  pch=16) #0
points(1, mean( SZ1[SZ1$XTIME == "1",]$resp_log ),  pch=16) #1
points(2, mean( SZ1[SZ1$XTIME == "2",]$resp_log ),  pch=16) #2
points(4, mean( SZ1[SZ1$XTIME == "4",]$resp_log ),  pch=16) #4
points(6, mean( SZ1[SZ1$XTIME == "6",]$resp_log ),  pch=16) #6
points(8, mean( SZ1[SZ1$XTIME == "8",]$resp_log ),  pch=16) #6


mean_x<- c(0, 1,2,4,6,8)
mean_y<-  c(mean( SZ1[SZ1$XTIME == "0",]$resp_log ),mean( SZ1[SZ1$XTIME == "1",]$resp_log ),mean( SZ1[SZ1$XTIME == "2",]$resp_log ),
            mean( SZ1[SZ1$XTIME == "4",]$resp_log ), mean( SZ1[SZ1$XTIME == "6",]$resp_log), mean( SZ1[SZ1$XTIME == "8",]$resp_log )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0
