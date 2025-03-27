##read the data
library(readr)

experimental_case_2_bprs<- read_csv("/Users/gokcedeliorman/Downloads/experimental_case_2_bprs.csv", col_names=TRUE)
placebo_case_2_bprs<- read_csv("/Users/gokcedeliorman/Downloads/placebo_case_2_bprs.csv", col_names=TRUE)

experimental_case_2_cgi<- read_csv("/Users/gokcedeliorman/Downloads/experimental_case_2_cgi.csv", col_names=TRUE)
placebo_case_2_cgi<- read_csv("/Users/gokcedeliorman/Downloads/placebo_case_2_cgi.csv", col_names=TRUE)

experimental_case_1_fin1_bprs<- read_csv("/Users/gokcedeliorman/Downloads/experimental_case_1_fin1_bprs.csv", col_names=TRUE)
placebo_case_1_fin1_bprs<- read_csv("/Users/gokcedeliorman/Downloads/placebo_case_1_fin1_bprs.csv", col_names=TRUE)

experimental_case_1_fin1_cgi<- read_csv("/Users/gokcedeliorman/Downloads/experimental_case_1_fin1_cgi.csv", col_names=TRUE)
placebo_case_1_fin1_cgi<- read_csv("/Users/gokcedeliorman/Downloads/placebo_case_1_fin1_cgi.csv", col_names=TRUE)


#seperate as surrogate and true endpoint bprs
TZ1<- experimental_case_2_bprs[experimental_case_2_bprs$type2 == "PANSS",]
TZ0<- placebo_case_2_bprs[placebo_case_2_bprs$type2 == "PANSS",]
SZ1<- experimental_case_2_bprs[experimental_case_2_bprs$type2 == "BPRS",]
SZ0<- placebo_case_2_bprs[placebo_case_2_bprs$type2 == "BPRS",]

#seperate as surrogate and true endpoint bprs
TZ1<- experimental_case_2_cgi[experimental_case_2_cgi$type2 == "CGI_SEV",]
TZ0<- placebo_case_2_cgi[placebo_case_2_cgi$type2 == "CGI_SEV",]
SZ1<- experimental_case_2_cgi[experimental_case_2_cgi$type2 == "PANSS",]
SZ0<- placebo_case_2_cgi[placebo_case_2_cgi$type2 == "PANSS",]

#seperate as surrogate and true endpoint
TZ1<- experimental_case_1_fin1_bprs[experimental_case_1_fin1_bprs$type2 == "PANSS",]
TZ0<- placebo_case_1_fin1_bprs[placebo_case_1_fin1_bprs$type2 == "PANSS",]
SZ1<- experimental_case_1_fin1_bprs[experimental_case_1_fin1_bprs$type2 == "BPRS",]
SZ0<- placebo_case_1_fin1_bprs[placebo_case_1_fin1_bprs$type2 == "BPRS",]

#seperate as surrogate and true endpoint CGI FIN1
TZ1<- experimental_case_1_fin1_cgi[experimental_case_1_fin1_cgi$type2 == "CGI_SEV",]
TZ0<- placebo_case_1_fin1_cgi[placebo_case_1_fin1_cgi$type2 == "CGI_SEV",]
SZ1<- experimental_case_1_fin1_cgi[experimental_case_1_fin1_cgi$type2 == "PANSS",]
SZ0<- placebo_case_1_fin1_cgi[placebo_case_1_fin1_cgi$type2 == "PANSS",]


## TREATMENT=1 PANSS z=1
#plot(1,1, pch=16,  xlab = "time", 
 #    ylab="PANSS", ylim=c(10,150),main="Z=1", type="l", xaxt="n", xlim=c(1,7))
plot(1,1, pch=16,  xlab = "time", 
     ylab="PANSS", ylim=c(20,150),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(1,7))
for (i in 3:71) {
  new_data<- TZ1[TZ1$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7),labels=c("","0","1", "2", "3",  "4","5", "6"))

points(1, mean( TZ1[TZ1$XTIME == "0",]$resp ), col=2,  pch=16) #0
points(2, mean( TZ1[TZ1$XTIME == "1",]$resp ), col=2,  pch=16) #1
points(3, mean( TZ1[TZ1$XTIME == "2",]$resp ), col=2,  pch=16) #2
points(5, mean( TZ1[TZ1$XTIME == "4",]$resp ), col=2,  pch=16) #4
points(7, mean( TZ1[TZ1$XTIME == "6",]$resp ), col=2,  pch=16) #6

points(1, mean( TZ1[TZ1$XTIME == "0",]$resp ),  pch=16) #0
points(2, mean( TZ1[TZ1$XTIME == "1",]$resp ),  pch=16) #1
points(3, mean( TZ1[TZ1$XTIME == "2",]$resp ),  pch=16) #2
points(5, mean( TZ1[TZ1$XTIME == "4",]$resp ),  pch=16) #4
points(7, mean( TZ1[TZ1$XTIME == "6",]$resp ),  pch=16) #6

mean_x<- c(1,2,3,5,7)
mean_y<-  c(mean( TZ1[TZ1$XTIME == "0",]$resp ),mean( TZ1[TZ1$XTIME == "1",]$resp ),mean( TZ1[TZ1$XTIME == "2",]$resp ),
            mean( TZ1[TZ1$XTIME == "4",]$resp ), mean( TZ1[TZ1$XTIME == "6",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0
#legend("bottomright",  col=c("black", "red"), c("Z=0", "BPRS"), cex=0.4,
 #      , lty= 1)


## TREATMENT=1 BPRS z=1
plot(1,1, pch=16,  xlab = "time", 
     ylab="BPRS", ylim=c(20,80),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(1,7))
for (i in 1:70) {
  new_data<- SZ1[SZ1$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7),labels=c("","0","1", "2", "3",  "4","5", "6"))

points(1, mean( SZ1[SZ1$XTIME == "0",]$resp ),  pch=16) #0
points(2, mean( SZ1[SZ1$XTIME == "1",]$resp ),  pch=16) #1
points(3, mean( SZ1[SZ1$XTIME == "2",]$resp ),  pch=16) #2
points(5, mean( SZ1[SZ1$XTIME == "4",]$resp ),  pch=16) #4
points(7, mean( SZ1[SZ1$XTIME == "6",]$resp ),  pch=16) #6

mean_x<- c(1,2,3,5,7)
mean_y<-  c(mean( SZ1[SZ1$XTIME == "0",]$resp ),mean( SZ1[SZ1$XTIME == "1",]$resp ),mean( SZ1[SZ1$XTIME == "2",]$resp ),
            mean( SZ1[SZ1$XTIME == "4",]$resp ), mean( SZ1[SZ1$XTIME == "6",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0


## TREATMENT=0 panss z=0
plot(1,1, pch=16,  xlab = "time", 
     ylab="PANSS", ylim=c(20,150),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(1,7))
for (i in 1:70) {
  new_data<- TZ0[TZ0$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7),labels=c("","0","1", "2", "3",  "4","5", "6"))

points(1, mean( TZ0[TZ0$XTIME == "0",]$resp ),  pch=16) #0
points(2, mean( TZ0[TZ0$XTIME == "1",]$resp ),  pch=16) #1
points(3, mean( TZ0[TZ0$XTIME == "2",]$resp ),  pch=16) #2
points(5, mean( TZ0[TZ0$XTIME == "4",]$resp ),  pch=16) #4
points(7, mean( TZ0[TZ0$XTIME == "6",]$resp ),  pch=16) #6

mean_x<- c(1,2,3,5,7)
mean_y<-  c(mean( TZ0[TZ0$XTIME == "0",]$resp ),mean( TZ0[TZ0$XTIME == "1",]$resp ),mean( TZ0[TZ0$XTIME == "2",]$resp ),
            mean( TZ0[TZ0$XTIME == "4",]$resp ), mean( TZ0[TZ0$XTIME == "6",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0


## TREATMENT=0 bprs z=0
plot(1,1, pch=16,  xlab = "time", 
     ylab="BPRS", ylim=c(20,80),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(1,7))
for (i in 1:70) {
  new_data<- SZ0[SZ0$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7),labels=c("","0","1", "2", "3",  "4","5", "6"))

points(1, mean( SZ0[SZ0$XTIME == "0",]$resp ),  pch=16) #0
points(2, mean( SZ0[SZ0$XTIME == "1",]$resp ),  pch=16) #1
points(3, mean( SZ0[SZ0$XTIME == "2",]$resp ),  pch=16) #2
points(5, mean( SZ0[SZ0$XTIME == "4",]$resp ),  pch=16) #4
points(7, mean( SZ0[SZ0$XTIME == "6",]$resp ),  pch=16) #6

mean_x<- c(1,2,3,5,7)
mean_y<-  c(mean( SZ0[SZ0$XTIME == "0",]$resp ),mean( SZ0[SZ0$XTIME == "1",]$resp ),mean( SZ0[SZ0$XTIME == "2",]$resp ),
            mean( SZ0[SZ0$XTIME == "4",]$resp ), mean( SZ0[SZ0$XTIME == "6",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0




##for trial 3
## TREATMENT=1 PANSS z=1
#plot(1,1, pch=16,  xlab = "time", 
#    ylab="PANSS", ylim=c(10,150),main="Z=1", type="l", xaxt="n", xlim=c(1,7))
plot(1,1, pch=16,  xlab = "time", 
     ylab="PANSS", ylim=c(20,150),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(1,8))
for (i in 1:652) {
  new_data<- TZ1[TZ1$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8,9),labels=c("","0","1", "2", "3",  "4","5", "6","8", "8"))

points(1, mean( TZ1[TZ1$XTIME == "0",]$resp ),  pch=16) #0
points(2, mean( TZ1[TZ1$XTIME == "1",]$resp ),  pch=16) #1
points(3, mean( TZ1[TZ1$XTIME == "2",]$resp ),  pch=16) #2
points(5, mean( TZ1[TZ1$XTIME == "4",]$resp ),  pch=16) #4
points(7, mean( TZ1[TZ1$XTIME == "6",]$resp ),  pch=16) #6
points(8, mean( TZ1[TZ1$XTIME == "8",]$resp ),  pch=16) #6


mean_x<- c(1,2,3,5,7,8)
mean_y<-  c(mean( TZ1[TZ1$XTIME == "0",]$resp ),mean( TZ1[TZ1$XTIME == "1",]$resp ),mean( TZ1[TZ1$XTIME == "2",]$resp ),
            mean( TZ1[TZ1$XTIME == "4",]$resp ), mean( TZ1[TZ1$XTIME == "6",]$resp ), mean( TZ1[TZ1$XTIME == "8",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0
#legend("bottomright",  col=c("black", "red"), c("Z=0", "BPRS"), cex=0.4,
#      , lty= 1)


## TREATMENT=1 BPRS z=1
plot(1,1, pch=16,  xlab = "time", 
     ylab="BPRS", ylim=c(15,90),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(1,8))
for (i in 1:652) {
  new_data<- SZ1[SZ1$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8,9),labels=c("","0","1", "2", "3",  "4","5", "6","8", "8"))

points(1, mean( SZ1[SZ1$XTIME == "0",]$resp ),  pch=16) #0
points(2, mean( SZ1[SZ1$XTIME == "1",]$resp ),  pch=16) #1
points(3, mean( SZ1[SZ1$XTIME == "2",]$resp ),  pch=16) #2
points(5, mean( SZ1[SZ1$XTIME == "4",]$resp ),  pch=16) #4
points(7, mean( SZ1[SZ1$XTIME == "6",]$resp ),  pch=16) #6
points(8, mean( SZ1[SZ1$XTIME == "8",]$resp ),  pch=16) #6


mean_x<- c(1,2,3,5,7,8)
mean_y<-  c(mean( SZ1[SZ1$XTIME == "0",]$resp ),mean( SZ1[SZ1$XTIME == "1",]$resp ),mean( SZ1[SZ1$XTIME == "2",]$resp ),
            mean( SZ1[SZ1$XTIME == "4",]$resp ), mean( SZ1[SZ1$XTIME == "6",]$resp ), mean( SZ1[SZ1$XTIME == "8",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0


## TREATMENT=0 panss z=0
plot(1,1, pch=16,  xlab = "time", 
     ylab="PANSS", ylim=c(20,165),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(1,8))
for (i in 3:653) {
  new_data<- TZ0[TZ0$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8,9),labels=c("","0","1", "2", "3",  "4","5", "6","8", "8"))

points(1, mean( TZ0[TZ0$XTIME == "0",]$resp ),  pch=16) #0
points(2, mean( TZ0[TZ0$XTIME == "1",]$resp ),  pch=16) #1
points(3, mean( TZ0[TZ0$XTIME == "2",]$resp ),  pch=16) #2
points(5, mean( TZ0[TZ0$XTIME == "4",]$resp ),  pch=16) #4
points(7, mean( TZ0[TZ0$XTIME == "6",]$resp ),  pch=16) #6
points(8, mean( TZ0[TZ0$XTIME == "8",]$resp ),  pch=16) #6

mean_x<- c(1,2,3,5,7,8)
mean_y<-  c(mean( TZ0[TZ0$XTIME == "0",]$resp ),mean( TZ0[TZ0$XTIME == "1",]$resp ),mean( TZ0[TZ0$XTIME == "2",]$resp ),
            mean( TZ0[TZ0$XTIME == "4",]$resp ), mean( TZ0[TZ0$XTIME == "6",]$resp ), mean( TZ0[TZ0$XTIME == "8",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0


## TREATMENT=0 bprs z=0
plot(1,1, pch=16,  xlab = "time", 
     ylab="BPRS", ylim=c(15,100),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(1,8))
for (i in 3:653) {
  new_data<- SZ0[SZ0$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8,9),labels=c("","0","1", "2", "3",  "4","5", "6","8", "8"))

points(1, mean( SZ0[SZ0$XTIME == "0",]$resp ),  pch=16) #0
points(2, mean( SZ0[SZ0$XTIME == "1",]$resp ),  pch=16) #1
points(3, mean( SZ0[SZ0$XTIME == "2",]$resp ),  pch=16) #2
points(5, mean( SZ0[SZ0$XTIME == "4",]$resp ),  pch=16) #4
points(7, mean( SZ0[SZ0$XTIME == "6",]$resp ),  pch=16) #6
points(8, mean( SZ0[SZ0$XTIME == "8",]$resp ),  pch=16) #6


mean_x<- c(1,2,3,5,7,8)
mean_y<-  c(mean( SZ0[SZ0$XTIME == "0",]$resp ),mean( SZ0[SZ0$XTIME == "1",]$resp ),mean( SZ0[SZ0$XTIME == "2",]$resp ),
            mean( SZ0[SZ0$XTIME == "4",]$resp ), mean( SZ0[SZ0$XTIME == "6",]$resp), mean( SZ0[SZ0$XTIME == "8",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0



##for trial 1. t=cgi s=panss
#plot(1,1, pch=16,  xlab = "time", 
#    ylab="PANSS", ylim=c(10,150),main="Z=1", type="l", xaxt="n", xlim=c(1,7))
plot(1,1, pch=16,  xlab = "time", 
     ylab="CGI", ylim=c(0,8),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(0,6))
for (i in 3:71) {
  new_data<- TZ1[TZ1$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0,1,2,3,4,5,6),labels=c("0","1", "2", "3",  "4","5", "6"))

points(0, mean( TZ1[TZ1$XTIME == "0",]$resp ),  pch=16) #0
points(1, mean( TZ1[TZ1$XTIME == "1",]$resp ),  pch=16) #1
points(2, mean( TZ1[TZ1$XTIME == "2",]$resp ),  pch=16) #2
points(4, mean( TZ1[TZ1$XTIME == "4",]$resp ),  pch=16) #4
points(6, mean( TZ1[TZ1$XTIME == "6",]$resp ),  pch=16) #6

mean_x<- c(0,1,2,4,6)
mean_y<-  c(mean( TZ1[TZ1$XTIME == "0",]$resp ),mean( TZ1[TZ1$XTIME == "1",]$resp ),mean( TZ1[TZ1$XTIME == "2",]$resp ),
            mean( TZ1[TZ1$XTIME == "4",]$resp ), mean( TZ1[TZ1$XTIME == "6",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0

### cgi fin 1
## TREATMENT=1 PANSS z=1
plot(1,1, pch=16,  xlab = "time", 
     ylab="PANSS", ylim=c(20,150),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(0,6))
for (i in 3:71) {
  new_data<- SZ1[SZ1$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6),labels=c("0","1", "2", "3",  "4","5", "6"))

points(0, mean( SZ1[SZ1$XTIME == "0",]$resp ),  pch=16) #0
points(1, mean( SZ1[SZ1$XTIME == "1",]$resp ),  pch=16) #1
points(2, mean( SZ1[SZ1$XTIME == "2",]$resp ),  pch=16) #2
points(4, mean( SZ1[SZ1$XTIME == "4",]$resp ),  pch=16) #4
points(6, mean( SZ1[SZ1$XTIME == "6",]$resp ),  pch=16) #6

mean_x<- c(0,1,2,4,6)
mean_y<-  c(mean( SZ1[SZ1$XTIME == "0",]$resp ),mean( SZ1[SZ1$XTIME == "1",]$resp ),mean( SZ1[SZ1$XTIME == "2",]$resp ),
            mean( SZ1[SZ1$XTIME == "4",]$resp ), mean( SZ1[SZ1$XTIME == "6",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0

### fin 1 cgi
## TREATMENT=0 panss z=0
plot(1,1, pch=16,  xlab = "time", 
     ylab="CGI", ylim=c(0,8),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(0,6))
for (i in 1:70) {
  new_data<- TZ0[TZ0$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6),labels=c("0","1", "2", "3",  "4","5", "6"))

points(0, mean( TZ0[TZ0$XTIME == "0",]$resp ),  pch=16) #0
points(1, mean( TZ0[TZ0$XTIME == "1",]$resp ),  pch=16) #1
points(2, mean( TZ0[TZ0$XTIME == "2",]$resp ),  pch=16) #2
points(4, mean( TZ0[TZ0$XTIME == "4",]$resp ),  pch=16) #4
points(6, mean( TZ0[TZ0$XTIME == "6",]$resp ),  pch=16) #6

mean_x<- c(0,1,2,4,6)
mean_y<-  c(mean( TZ0[TZ0$XTIME == "0",]$resp ),mean( TZ0[TZ0$XTIME == "1",]$resp ),mean( TZ0[TZ0$XTIME == "2",]$resp ),
            mean( TZ0[TZ0$XTIME == "4",]$resp ), mean( TZ0[TZ0$XTIME == "6",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0



##for 3 trial cgi 
##for trial 1. t=cgi s=panss
#plot(1,1, pch=16,  xlab = "time", 
#    ylab="PANSS", ylim=c(10,150),main="Z=1", type="l", xaxt="n", xlim=c(1,7))
plot(1,1, pch=16,  xlab = "time", 
     ylab="CGI", ylim=c(0,8),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(0,6))
for (i in 3:71) {
  new_data<- TZ1[TZ1$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0,1,2,3,4,5,6),labels=c("0","1", "2", "3",  "4","5", "6"))

points(0, mean( TZ1[TZ1$XTIME == "0",]$resp ),  pch=16) #0
points(1, mean( TZ1[TZ1$XTIME == "1",]$resp ),  pch=16) #1
points(2, mean( TZ1[TZ1$XTIME == "2",]$resp ),  pch=16) #2
points(4, mean( TZ1[TZ1$XTIME == "4",]$resp ),  pch=16) #4
points(6, mean( TZ1[TZ1$XTIME == "6",]$resp ),  pch=16) #6

mean_x<- c(0,1,2,4,6)
mean_y<-  c(mean( TZ1[TZ1$XTIME == "0",]$resp ),mean( TZ1[TZ1$XTIME == "1",]$resp ),mean( TZ1[TZ1$XTIME == "2",]$resp ),
            mean( TZ1[TZ1$XTIME == "4",]$resp ), mean( TZ1[TZ1$XTIME == "6",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0

### cgi fin 1
## TREATMENT=1 PANSS z=1
plot(1,1, pch=16,  xlab = "time", 
     ylab="CGI", ylim=c(0,8),main="Z=1, mean profile", type="l", xaxt="n", xlim=c(0,8))
for (i in 1:600) {
  new_data<- TZ1[TZ1$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8),labels=c("0","1", "2", "3",  "4","5", "6", "7", "8"))

points(0, mean( TZ1[TZ1$XTIME == "0",]$resp ),  pch=16) #0
points(1, mean( TZ1[TZ1$XTIME == "1",]$resp ),  pch=16) #1
points(2, mean( TZ1[TZ1$XTIME == "2",]$resp ),  pch=16) #2
points(4, mean( TZ1[TZ1$XTIME == "4",]$resp ),  pch=16) #4
points(6, mean( TZ1[TZ1$XTIME == "6",]$resp ),  pch=16) #6
points(8, mean( TZ1[TZ1$XTIME == "8",]$resp ),  pch=16) #6


mean_x<- c(0,1,2,4,6,8)
mean_y<-  c(mean( TZ1[TZ1$XTIME == "0",]$resp ),mean( TZ1[TZ1$XTIME == "1",]$resp ),mean( TZ1[TZ1$XTIME == "2",]$resp ),
            mean( TZ1[TZ1$XTIME == "4",]$resp ), mean( TZ1[TZ1$XTIME == "6",]$resp ),mean( TZ1[TZ1$XTIME == "8",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0


##### TREATMENT=1 PANSS z=1
plot(1,1, pch=16,  xlab = "time", 
     ylab="CGI", ylim=c(0,8),main="Z=0, mean profile", type="l", xaxt="n", xlim=c(0,8))
for (i in 2:599) {
  new_data<- TZ0[TZ0$id == i,]
  lines(new_data$XTIME, new_data$resp, col="hotpink",  pch=16)
}
axis(1, at=c(0, 1,2,3,4,5,6,7,8),labels=c("0","1", "2", "3",  "4","5", "6", "7", "8"))

points(0, mean( TZ0[TZ0$XTIME == "0",]$resp ),  pch=16) #0
points(1, mean( TZ0[TZ0$XTIME == "1",]$resp ),  pch=16) #1
points(2, mean( TZ0[TZ0$XTIME == "2",]$resp ),  pch=16) #2
points(4, mean( TZ0[TZ0$XTIME == "4",]$resp ),  pch=16) #4
points(6, mean( TZ0[TZ0$XTIME == "6",]$resp ),  pch=16) #6
points(8, mean( TZ0[TZ0$XTIME == "8",]$resp ),  pch=16) #6


mean_x<- c(0,1,2,4,6,8)
mean_y<-  c(mean( TZ0[TZ0$XTIME == "0",]$resp ),mean( TZ0[TZ0$XTIME == "1",]$resp ),mean( TZ0[TZ0$XTIME == "2",]$resp ),
            mean( TZ0[TZ0$XTIME == "4",]$resp ), mean( TZ0[TZ0$XTIME == "6",]$resp ),mean( TZ0[TZ0$XTIME == "8",]$resp )) 
lines(mean_x, mean_y, col="black", lwd=2 ) #0


