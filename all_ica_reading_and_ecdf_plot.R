##3 models ICA results

ICA_1_galecki<- read.csv("/Users/gokcedeliorman/Downloads/ICA_galecki005_ica2_3may.csv")
ICA_2_ri<- read.csv("/Users/gokcedeliorman/Downloads/ICA_ri_7may.csv")
ICA_3_shared_ri<- read.csv("/Users/gokcedeliorman/Downloads/Ica_sri_01001_6may.csv")

ica_1<-ICA_1_galecki$R2_Lambda
ica_2<-ICA_2_ri$x
ica_3<- ICA_3_shared_ri$x

mean(ica_1)
mean(ica_2)
mean(ica_3)


## Replace these object names with your retained models
ica_list <- list(
  Galecki = ica_1,
 `Endpoint-specific random intercept` = ica_2,
  `Shared random intercept` = ica_3
)

## Keep only finite values
ica_list <- lapply(ica_list, function(x) x[is.finite(x)])

## Thresholds
thresholds <- c(0.20, 0.30, 0.50, 0.70, 0.80, 0.90, 0.95)

## Proportions: P(ICA < threshold)
prop_mat <- t(sapply(ica_list, function(x) {
  sapply(thresholds, function(th) mean(x < th))
}))

colnames(prop_mat) <- paste0("P(ICA < ", thresholds, ")")
prop_table <- data.frame(
  Model = rownames(prop_mat),
  round(prop_mat, 4),
  row.names = NULL,
  check.names = FALSE
)

prop_table

graphics.off()
par(mfrow = c(1,1))

#cols <- c("#1b9e77", "#d95f02", "#7570b3")
#ltys <- c(1, 2, 3)  # 1=solid, 2=dashed, 3=dotted


cols <- c("#0072B2",  "#009E73","#D55E00")
ltys <- c(4, 1, 2)
lwd  <- 2.5


#par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
# png("/Users/gokcedeliorman/Downloads/ecdf_plot_7may.png",
#         width = 6,      # inch
#         height = 4,
#         units = "in",
#         res = 600)
## Full ECDF
plot(ecdf(ica_list[[1]]),
     #do.points = FALSE, verticals = TRUE,
     col = cols[1], lwd = 2,
     main = "ECDF of ICA",
     #main = "",
     xlab = expression(R[Lambda]^2),
     ylab = "ECDF",
     xlim = c(0, 1),
     lty= ltys[1])

if (length(ica_list) > 1) {
  for (k in 2:length(ica_list)) {
    plot(ecdf(ica_list[[k]]),
         add = TRUE,
         do.points = FALSE, verticals = TRUE,
         col = cols[k], lwd = 2,
         lty = ltys[k])
  }
}

abline(v = thresholds, col = "grey85", lty = 3)

legend("topleft",
       legend = names(ica_list),
       col = cols[seq_along(ica_list)],
       lwd = 2,
       lty = ltys[seq_along(ica_list)],
       bty = "n",
       cex= 1)


#dev.off()

# dev.list()
# graphics.off()
# while(dev.cur() > 1) dev.off()
# dev.list()

## Lower-tail zoom

# png("/Users/gokcedeliorman/Downloads/zoom_7may.png",
#     width = 6,      # inch
#     height = 4,
#     units = "in",
#     res = 600)

plot(ecdf(ica_list[[1]]),
     do.points = FALSE, verticals = TRUE,
     col = cols[1], lwd = 2, lty = ltys[1],
     main = "Lower-tail zoom",
     xlab = expression(R[Lambda]^2),
     ylab = "ECDF",
     xlim = c(0.02, 0.7),   # adjust if needed
     ylim = c(0, 0.005))  # adjust if needed

if (length(ica_list) > 1) {
  for (k in 2:length(ica_list)) {
    plot(ecdf(ica_list[[k]]),
         add = TRUE,
         do.points = FALSE, verticals = TRUE,
         col = cols[k], lwd = 2, lty = ltys[k])
  }
}

abline(v = thresholds, col = "grey85", lty = 3)
legend("topleft", legend = names(ica_list),
       col = cols[seq_along(ica_list)], lwd = 2, bty = "n",
       lty = ltys[seq_along(ica_list)],
       cex=1)


#dev.off()
