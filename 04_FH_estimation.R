
# Direct estimation -----------------------------------------------------------------

# NOTE: this code relies to the R-file "8 eusilc - FH example.R" 
#       (Direct estimation and Fay-Herriot)                  

# How are the sample observations distributed in the regions?
describe(df_rent_unit$bezirk_name)
df_rent_unit %>%
  group_by(bezirk_name) %>%
  summarise(n = mean(samplesize_in_bezirk), rent_unweighted = mean(rent_unweighted), 
            rent_unweighted_by_bezirk = mean(rent_unweighted_by_bezirk),
            rent_weighted_by_bezirk = mean(rent_weighted_by_bezirk))


# samplesize, Direct, SD, CV, var
fit_rent_Direct <- subset(df_rent_unit, select = c("bezirk_name", "bezirk", "samplesize_in_bezirk", 
                                                   "rent_weighted_by_bezirk", "var_rent_weighted",
                                                   "Direct_sd", "Direct_cv"))
fit_rent_Direct <- aggregate(fit_rent_Direct,
                               by = list(fit_rent_Direct$bezirk_name),
                               FUN = mean, na.rm = F)
# clean, rename
fit_rent_Direct$bezirk_name <- NULL
colnames(fit_rent_Direct)[1] <- "bezirk_name"
colnames(fit_rent_Direct)[4] <- "Direct"
fit_rent_Direct$Direct_var <- fit_rent_Direct$Direct_sd^2
fit_rent_Direct

# doublecheck: Merge rent-variables form df_rent_area
fit_rent_Direct <- plyr::join(fit_rent_Direct, df_rent_area, 
                              by = 'bezirk_name', type = 'left', match = 'all')

# We use domsize according to hh_weights from SOEP
bezirk_size <- data.frame(df_rent_area$bezirk_name,
                          round(
                            aggregate(df_rent_unit$hh_weight,
                                      by = list(bezirk = df_rent_unit$bezirk), FUN = sum),
                                digits = 0),
                          aggregate(df_rent_unit$samplesize_in_bezirk,
                                    by = list(df_rent_unit$bezirk), 
                                    FUN = mean))
bezirk_size[,4] <- NULL
colnames(bezirk_size) <- c("bezirk_name", "bezirk", "Anzahl_hh", "sampSize")
bezirk_size

fit_rent_Direct2 <- sae::direct(y = rent_unweighted, dom = bezirk, sweight = hh_weight,
                           domsize = bezirk_size[,2:3], data = df_rent_unit)

colnames(fit_rent_Direct2)[1] <- "bezirk"
colnames(fit_rent_Direct2)[4] <- "Direct_sd"
colnames(fit_rent_Direct2)[5] <- "Direct_cv"
fit_rent_Direct2$Direct_var <- fit_rent_Direct2$Direct_cv^2
fit_rent_Direct2

fit_rent_Direct2 <- plyr::join(fit_rent_Direct2, df_rent_area, 
                              by = 'bezirk', type = 'left', match = 'all')


# compare both calculations of Direct estimator (manual vs. direct()-function)
fit_rent_Direct[,c(1:4,6:7)]
fit_rent_Direct2[,c(1:6)]

## both calculations are the same, so we replace them
fit_rent_Direct <- fit_rent_Direct2
rm(fit_rent_Direct2)


# estimation of FH ------------------------------------------------------------------

# check covariates
cor_FH <- cor(df_rent_area[,c("priv_hh_with_children", "alq_mean", "shareForeigners",
                              "shareCriminalCharchPop", "quoted_net_rent_EUR_per_m2", 
                              "building_intensity", "shareElderly", "shareFamHighHIncHH", 
                              "shareSchoolsPop")], 
              method = c("pearson"))
round(cor_FH, 2)

# estimation FH
fit_rent_FH <- eblupFH(formula = Direct ~ shareFamChildrHH + shareFamHighHIncHH + 
                         shareForeigners + west + quoted_net_rent_EUR_per_m2 + 
                         building_intensity + shareElderly + alq_mean,
                         vardir = Direct_var, data = fit_rent_Direct)
fit_rent_FH

# residuals FH
df <- as.data.frame(fit_rent_FH$eblup)
fit_rent_FH_residuals <- fit_rent_Direct$Direct - df

colnames(fit_rent_FH_residuals) <- c("fit_rent_FH_residuals")
fit_rent_FH_residuals

# random effects FH eblub - Xb
X <- as.matrix(cbind(rep(1,length(fit_rent_Direct$building_intensity)), subset(fit_rent_Direct, select = c( "shareFamChildrHH", "alq_mean" , 
                                                      "shareForeigners","west" , "quoted_net_rent_EUR_per_m2", 
                                                      "building_intensity" , "shareElderly" , "shareFamHighHIncHH"))))
b <- fit_rent_FH$fit$estcoef$beta
X_b <- X %*% b

fit_rent_FH_ref <- fit_rent_FH$eblup - X_b

# MSE
fit_rent_FH_mse <- mseFH(formula = Direct ~ shareFamChildrHH + alq_mean + 
                           shareForeigners + west + #shareSinglesPop + shareTouristsPop +
                           quoted_net_rent_EUR_per_m2 + building_intensity + shareElderly + 
                           shareFamHighHIncHH, 
                         vardir = Direct_var,
                         data = fit_rent_Direct)
fit_rent_FH_mse$mse
fit_rent_FH_sqrtmse <- sqrt(fit_rent_FH_mse$mse)

# MSE BOOTSTRAP
fit_rent_FH_mse_B <- mseFH(formula = Direct ~ shareFamChildrHH + alq_mean + 
                           shareForeigners + west + #shareSinglesPop + shareTouristsPop +
                           quoted_net_rent_EUR_per_m2 + building_intensity + shareElderly + 
                           shareFamHighHIncHH, 
                         vardir = Direct_var,
                         B = 1000,
                         data = fit_rent_Direct
                         )
fit_rent_FH_mse_B$mse
fit_rent_FH_sqrtmse_B <- sqrt(fit_rent_FH_mse_B$mse)


# mse_FH ~ Direct_var: dominant Modellbias (Münnich, p. 162) - check with monte carlo simulation how biased we are


# Comparison CV of direct and FH
fit_rent_FH_cv <- 100*sqrt(fit_rent_FH_mse$mse) / fit_rent_FH$eblup
fit_rent_FH_cv

# save results (Direct, FH)
df_rent_results <- data.frame(fit_rent_Direct[,1:6], 
                              eblup_FH = fit_rent_FH[1], 
                              cv_FH = fit_rent_FH_cv, 
                              mse_FH = fit_rent_FH_mse$mse,
                              sqrtmse_FH = fit_rent_FH_sqrtmse)
colnames(df_rent_results)[7] <- "eblup_FH"

# Merge bezirk_name 
df_rent_results <- plyr::join(df_rent_results, bezirk_size, 
                              by = 'bezirk', type = 'left', match = 'all')
df_rent_results[,12:13] <- NULL

## goodness of fit
# absolute bias
mean(Metrics::ae(df_rent_results$Direct, df_rent_results$eblup_FH))
mean(df_rent_results$eblup_FH - df_rent_results$Direct)
# relative bias
mean(((df_rent_results$eblup_FH - df_rent_results$Direct) / df_rent_results$Direct) * 100)
# root mean squared error
mean(Metrics::rmse(df_rent_results$Direct, df_rent_results$eblup_FH))
# relative root mean squared error
(sqrt(mean((df_rent_results$eblup_FH - df_rent_results$Direct)^2)) * 100) / sum(df_rent_results$Direct)




# clean up
rm(list = c("fit_rent_FH_cv", "fit_rent_FH_mse", "cor_FH",
            "fit_rent_FH_sqrtmse"))


print("-- FH estimation done --")

###
