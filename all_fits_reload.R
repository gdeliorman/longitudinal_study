##all fits reloaded
##first run fit_all stats .R file to use unpack_par function

extract_fit_stats <- function(fit) {
  model_spec <- attr(fit, "model_spec")
  logLik <- -fit$objective
  k <- length(fit$par)
  AIC <- 2 * k - 2 * logLik
  
  data.frame(
    model       = model_spec$name,
    npar        = k,
    logLik      = logLik,
    neglogLik   = fit$objective,
    AIC         = AIC,
    convergence = fit$convergence,
    stringsAsFactors = FALSE
  )
}

p<-24

fit_galecki_reloaded<-readRDS(file = "/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/fit_galecki_2may.rds")
unpack_par(fit_galecki_reloaded$par, p, model_spec = spec_galecki)
est_galecki <- unpack_par(fit_galecki_reloaded$par, p = p, model = "galecki")
extract_fit_stats(fit_galecki_reloaded)

fit_ri_reloaded<-readRDS(file = "/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/fit_ri_2may.rds")
unpack_par(fit_ri_reloaded$par, p, model_spec = spec_ri)
extract_fit_stats(fit_ri_reloaded)


fit_rislope_reloaded<-readRDS(file = "/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/fit_rislope_2may.rds")
unpack_par(fit_rislope_reloaded$par, p, model_spec = spec_rislope)
extract_fit_stats(fit_rislope_reloaded)


fit_rislope_cs_reloaded<-readRDS( file = "/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/fit_rislope_cs_2may.rds")
unpack_par(fit_rislope_cs_reloaded$par, p, model_spec = spec_rislope_cs)
extract_fit_stats(fit_rislope_cs_reloaded)



fit_shared_ri_reloaded<-readRDS(file = "/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/fit_shared_ri_2may.rds")
unpack_par(fit_shared_ri_reloaded$par, p, model_spec = spec_shared_ri)
extract_fit_stats(fit_shared_ri_reloaded)


fit_shared_ri_slope_reloaded<-readRDS( file = "/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/fit_shared_rislope_2may.rds")
unpack_par(fit_shared_ri_slope_reloaded$par, p, model_spec = spec_shared_rislope)
extract_fit_stats(fit_shared_ri_slope_reloaded)



##others
fit_lm_reloaded<- readRDS(file = "/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/fit_lm_2may.rds")
fit_mixed_ex_reloaded<-readRDS(file = "/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/fit_mixed_example_2may.rds")
fit_alls_reloaded<-readRDS(file = "/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/fits_all_2may.rds")
fit_stats_all_reloaded<-readRDS(file = "/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/fitstats_all_2may.rds")


##TABLES 
diag_table_reloaded<- read.csv("/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/diag_table.csv")
diag_print_reloaded<- read.csv("/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/diag_print.csv")
diag_matrix_diag_reloaded<- read.csv("/Users/gokcedeliorman/Desktop/3rd_paper_fit_all_models/matrix_diagnostics_2x2_all_models.csv")


