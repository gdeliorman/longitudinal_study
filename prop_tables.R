
R2_galecki_05_ST <-read.csv("/Users/gokcedeliorman/Downloads/ICA_galecki005_ica2_29april.csv")
R2_galecki_05_TS <-read.csv("/Users/gokcedeliorman/Downloads/ICA_galecki005_ica2_3may.csv")

R2_galecki_ri_0105<- read.csv("/Users/gokcedeliorman/Downloads/ICA_galecki_ri05_ica2_29april.csv")
R2_galecki_ri_shared_01001<- read.csv("/Users/gokcedeliorman/Downloads/Ica_sri_0105_29april.csv")

ica1_st<- R2_galecki_05_ST$R2_Lambda
ica1_ts<- R2_galecki_05_TS$R2_Lambda

ica2<- R2_galecki_ri_0105$x
ica3<- R2_galecki_ri_shared_01001$x


## Replace these object names with your retained models
ica_list <- list(
  Galecki = ica1_ts,
  #Galecki2 = ica1_st,
  
  `Endpoint-specific random intercept` = ica2,
  `Shared random intercept` = ica3
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


cols <- c("#1b9e77", "#d95f02", "#7570b3")

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

## Full ECDF
plot(ecdf(ica_list[[1]]),
     do.points = FALSE, verticals = TRUE,
     col = cols[1], lwd = 2,
     main = "ECDF of ICA values",
     xlab = expression(ICA~(R[Lambda]^2)),
     ylab = "ECDF",
     xlim = c(0, 1))

if (length(ica_list) > 1) {
  for (k in 2:length(ica_list)) {
    plot(ecdf(ica_list[[k]]),
         add = TRUE,
         do.points = FALSE, verticals = TRUE,
         col = cols[k], lwd = 2)
  }
}

abline(v = thresholds, col = "grey85", lty = 3)
legend("bottomright", legend = names(ica_list),
       col = cols[seq_along(ica_list)], lwd = 2, bty = "n")

## Lower-tail zoom
plot(ecdf(ica_list[[1]]),
     do.points = FALSE, verticals = TRUE,
     col = cols[1], lwd = 2,
     main = "Lower-tail zoom",
     xlab = expression(ICA~(R[Lambda]^2)),
     ylab = "ECDF",
     xlim = c(0, 0.5),   # adjust if needed
     ylim = c(0, 0.15))  # adjust if needed

if (length(ica_list) > 1) {
  for (k in 2:length(ica_list)) {
    plot(ecdf(ica_list[[k]]),
         add = TRUE,
         do.points = FALSE, verticals = TRUE,
         col = cols[k], lwd = 2)
  }
}

abline(v = thresholds, col = "grey85", lty = 3)
legend("bottomright", legend = names(ica_list),
       col = cols[seq_along(ica_list)], lwd = 2, bty = "n")

