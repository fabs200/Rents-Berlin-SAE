
# BHF estimation --------------------------------------------------------------------

# NOTE: this code relies to the R-file "7 eusilc - EBLUP example.R"
#       (Direct estimation and Fay-Herriot)


# Imputation of unit level variables ------------------------------------------------

md.pattern(df_rent_unit)
# df_rent_unit$heat
# df_rent_unit$eqpinsul
# df_rent_unit$eqpfhea
# df_rent_unit$electr
# df_rent_unit$util 

# subset data 
df_rent_unit_BHF <- subset(df_rent_unit, select = c( "bezirk", "livingdur", "rent_unweighted",
                                                "size", "room", "hh_personen", "hh_kinder", 
                                                "hh_inc","heat", "eqpinsul","eqpnobar" ,
                                                "eqppark", "eqpmglass", "eqpnrj", "bgsampreg",
                                                "eqplif","eqpair" ,"eqpsol" ,"eqpalm"   ,
                                                "eqpgar" ,"eqpter", "eqpfhea", "electr"))

# imputation of NAs                                            
df_rent_unit_BHF_imp_mice <- mice(df_rent_unit_BHF, m=1, maxit=50, meth='pmm', seed=500)

df_rent_unit_BHF_imp <- complete(df_rent_unit_BHF_imp_mice, 1)

# check imputation
md.pattern(df_rent_unit_BHF_imp)

# Selection of area level variables -------------------------------------------------
# NOTE: we take the same Covariates as for FH estimation
Xmean_rent <- subset(df_rent_area, select = c("bezirk", "shareForeigners", "shareElderly", #"shareFamChildrHH",
                                              "quoted_net_rent_EUR_per_m2", "shareFamHighHIncHH"))
Popsize_rent <- bezirk_size[,2:3]


# Estimation of BHF -----------------------------------------------------------------

# NOTE: REML: Verzerrung der gesch?tzten Varianzkomponenten verringert (vgl. Harville (1977))
fit_rent_BHF <- eblupBHF(formula = as.numeric(rent_unweighted) ~ size + room + 
                            hh_kinder + hh_inc + heat + livingdur,
                         dom = bezirk, data = df_rent_unit_BHF_imp, 
                         meanxpop = Xmean_rent, popnsize = Popsize_rent, method = "REML")
fit_rent_BHF
fit_rent_BHF$eblup$eblup

fit_rent_BHF_mse <- pbmseBHF(formula = as.numeric(rent_unweighted) ~ size + room + 
                               hh_kinder + hh_inc + heat + livingdur,
                             dom = bezirk, data = df_rent_unit_BHF_imp, 
                             meanxpop = Xmean_rent, popnsize = Popsize_rent, method = "REML")

# NOTE: Before bringing together EBLUP + direct, reorder results of fit_rent_EBLUP according 
#       to order of bezirk_name in fit_rent_direct

# sqrt MSE 
fit_rent_BHF_sqrtmse <- sqrt(fit_rent_BHF_mse$mse$mse)

# cv
fit_rent_BHF_cv <- 100*sqrt(fit_rent_BHF_mse$mse$mse)/fit_rent_BHF$eblup$eblup

# putting above BHF results together
df_rent_results_BHF <- data.frame(
  bezirk = fit_rent_BHF$eblup$domain,
  sampsize = fit_rent_BHF$eblup$sampsize,
  eblup_BHF = fit_rent_BHF$eblup$eblup,
  mse_BHF = fit_rent_BHF_mse$mse$mse,
  sqrtmse_BHF = fit_rent_BHF_sqrtmse,
  cv_BHF = fit_rent_BHF_cv)

# merge bezirk_name
df_rent_results_BHF <- plyr::join(df_rent_results_BHF, bezirk_size[,1:2], 
                                  by = 'bezirk', type = 'left', match = 'all')
df_rent_results_BHF

# bring all results together (Direct, FH, BHF)
df_rent_results <- plyr::join(df_rent_results, df_rent_results_BHF, 
                              by = 'bezirk', type = 'left', match = 'all')
df_rent_results[,11:12] <- NULL # redundant
df_rent_results

## goodness of fit
# AIC
k = (length(fit_rent_BHF$fit$fixed) - 1)
lnL = fit_rent_BHF$fit$loglike
AIC = 2*k - 2*lnL
AIC
# BIC
ln_n = log(dim(df_rent_unit)[1])
BIC = ln_n*k - 2*lnL
BIC
# absolute bias
BHF_goodnessfit_help <- data.frame(df_rent_unit$rent_unweighted, df_rent_unit$bezirk)
colnames(BHF_goodnessfit_help) <- c("rent_unweighted", "bezirk")
BHF_goodnessfit <- plyr::join(BHF_goodnessfit_help, df_rent_results,
                                    by = 'bezirk', type = 'left', match = 'all')
names(BHF_goodnessfit)

# absolute bias
mean(Metrics::ae(BHF_goodnessfit$rent_unweighted, BHF_goodnessfit$eblup_BHF))
mean(BHF_goodnessfit$eblup_BHF - BHF_goodnessfit$rent_unweighted)
# relative bias
mean(((BHF_goodnessfit$eblup_BHF - BHF_goodnessfit$rent_unweighted) / BHF_goodnessfit$rent_unweighted) * 100)
# root mean squared error
mean(Metrics::rmse(BHF_goodnessfit$rent_unweighted, BHF_goodnessfit$eblup_BHF))
# relative root mean squared error
(sqrt(mean((BHF_goodnessfit$eblup_BHF - BHF_goodnessfit$rent_unweighted)^2)) * 100) / sum(BHF_goodnessfit$rent_unweighted)


# random effects BHF eblub 
fit_rent_BHF_ref <- fit_rent_BHF$fit$random
fit_rent_BHF_residuals <- fit_rent_BHF$fit$residuals

# clean up
rm(list = c("fit_rent_BHF_cv", "fit_rent_BHF_sqrtmse",
            "fit_rent_BHF_mse", "df_rent_unit_BHF_imp_mice",
            "df_rent_results_BHF", "df_rent_unit_BHF", "curWarnings",
            "BHF_goodnessfit_help", "BHF_goodnessfit"))


print("-- BHF estimation done --")


###
