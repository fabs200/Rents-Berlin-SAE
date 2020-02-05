
# Robustness Checks -----------------------------------------------------------------

#' Note: We run several robustness checks.
#' 1. OLS, 
#' 2. Direct estimation different area-level (west/east), 
#' 3. FH estimation different area-level (west/east)
#' 


# OLS -------------------------------------------------------------------------------
# "Mitte" as base category 
df_rent_unit$bezirk_d_1 <- ifelse(df_rent_unit$bezirk == 1, 1, 0)
df_rent_unit$bezirk_d_2 <- ifelse(df_rent_unit$bezirk == 2, 1, 0)
df_rent_unit$bezirk_d_3 <- ifelse(df_rent_unit$bezirk == 3, 1, 0)
df_rent_unit$bezirk_d_4 <- ifelse(df_rent_unit$bezirk == 4, 1, 0)
df_rent_unit$bezirk_d_5 <- ifelse(df_rent_unit$bezirk == 5, 1, 0)
df_rent_unit$bezirk_d_6 <- ifelse(df_rent_unit$bezirk == 6, 1, 0)
df_rent_unit$bezirk_d_7 <- ifelse(df_rent_unit$bezirk == 7, 1, 0)
df_rent_unit$bezirk_d_8 <- ifelse(df_rent_unit$bezirk == 8, 1, 0)
df_rent_unit$bezirk_d_9 <- ifelse(df_rent_unit$bezirk == 9, 1, 0)
df_rent_unit$bezirk_d_10 <- ifelse(df_rent_unit$bezirk == 10, 1, 0)
df_rent_unit$bezirk_d_11 <- ifelse(df_rent_unit$bezirk == 11, 1, 0)
df_rent_unit$bezirk_d_12 <- ifelse(df_rent_unit$bezirk == 12, 1, 0)

out <- lm( rent_unweighted ~ size  + eqpter + eqpbas + eqpalm + eqpsol +
             eqplif + eqpnrj + eqpfhea + eqpinsul + eqpmglass + eqppark + eqpnobar +
           bezirk_d_2 + bezirk_d_3 + bezirk_d_4 + bezirk_d_5 + bezirk_d_6 +
           bezirk_d_7 + bezirk_d_8 + bezirk_d_9 + bezirk_d_10 + bezirk_d_11 +
           bezirk_d_12,
           data = df_rent_unit)
summary(out)

qqnorm(out$residuals, ylab = "residuals", xlab = "normal scores", 
       main = "BHF resid") 
qqline(out$residuals)


# Specification Checks: fit with different area-level: west/east --------------------


# Different area-level: Direct ------------------------------------------------------
## 1. define new bezirk_size ('bgsampreg': variable indicating west/east-area in SOEP)
bezirk_size_we <- data.frame(aggregate(df_rent_unit$hh_weight, by = list(bezirk = df_rent_unit$bgsampreg), FUN = sum))
colnames(bezirk_size_we) <- c("bgsampreg", "Anzahl_hh")
bezirk_size_we

## Direct: area-level west/east-Dummy ('bgsampreg')
fit_rent_Direct_we <- sae::direct(y = rent_unweighted, dom = bgsampreg, sweight = hh_weight,
                                domsize = bezirk_size_we, data = df_rent_unit)
colnames(fit_rent_Direct_we)[1] <- "west"


# Different area-level: FH ----------------------------------------------------------
## FH: First, datapreparation, aggregate df_rent_area by west, then merge fit_rent_Direct_we
df_rent_area_we <- df_rent_area %>%
  group_by(west) %>%
  summarise(rent = mean(rent_unweighted), 
            rent_weighted_by_we = mean(rent_weighted_by_bezirk), 
            alq_mean = mean(alq_mean), 
            priv_hh_with_children = mean(priv_hh_with_children), 
            shareForeigners = mean(shareForeigners),
            quoted_net_rent_EUR_per_m2 = mean(quoted_net_rent_EUR_per_m2), 
            building_intensity = mean(building_intensity),
            shareElderly = mean(shareElderly), 
            shareFamHighHIncHH = mean(shareFamHighHIncHH))

fit_rent_Direct_we <- plyr::join(fit_rent_Direct_we, as.data.frame(df_rent_area_we), 
                               by = 'west', type = 'left', match = 'all')

fit_rent_Direct_we$Direct_var <- fit_rent_Direct_we$SD^2

# estimation FH west/east-area-level
fit_rent_FH_we <- eblupFH(formula = Direct ~ alq_mean,
                       vardir = Direct_var,
                       data = fit_rent_Direct_we)
fit_rent_FH_we
                    # testing covariates:
                    # priv_hh_with_children + alq_mean + shareForeigners +
                    #   shareForeigners + quoted_net_rent_EUR_per_m2 + 
                    #   building_intensity + shareElderly + shareFamHighHIncHH

# MSE
fit_rent_FH_we_mse <- mseFH(formula = Direct ~ alq_mean ,
                            vardir = Direct_var,
                            data = fit_rent_Direct_we)
fit_rent_FH_we_mse$mse
fit_rent_FH_we_sqrtmse <- sqrt(fit_rent_FH_we_mse$mse)


# Different area-level: BHF ---------------------------------------------------------

## First, datapreparation, generate Xmean and Popsize df on west-east-area-level
df_rent_unit_BHF_imp_we <- df_rent_unit_BHF_imp
colnames(df_rent_unit_BHF_imp_we)[15] <- "west"
df_rent_unit_BHF_imp_we

Xmean_rent_we <- df_rent_area %>%
  group_by(west) %>%
  summarise(shareForeigners = mean(shareForeigners), 
            shareElderly = mean(shareElderly), 
            quoted_net_rent_EUR_per_m2 = mean(quoted_net_rent_EUR_per_m2),
            shareFamHighHIncHH = mean(shareFamHighHIncHH))
Xmean_rent_we <- as.data.frame(Xmean_rent_we)
Popsize_rent_we <- bezirk_size_we[,-1]

Popsize_rent_we <- df_rent_unit %>%
  group_by(bgsampreg) %>%
  summarise(Anzahl_hh = sum(hh_weight))
colnames(Popsize_rent_we)[1] <- "west"

# fit_rent_BHF_we <- eblupBHF(formula = rent_unweighted ~ size + room + 
#                            hh_kinder + hh_inc + heat + livingdur,
#                          dom = west, data = df_rent_unit_BHF_imp_we, 
#                          meanxpop = Xmean_rent_we, popnsize = Popsize_rent_we, 
#                          method = "REML")
# fit_rent_BHF_we
# fit_rent_BHF_we$eblup$eblup
# 
# fit_rent_BHF_mse_we <- pbmseBHF(formula = rent_unweighted ~ size + room + 
#                               hh_kinder + hh_inc + heat + livingdur,
#                             dom = west, data = df_rent_unit_BHF_imp_we, 
#                             meanxpop = Xmean_rent_we, popnsize = Popsize_rent_we, 
#                             method = "REML")
# fit_rent_BHF_mse_we



# Test: Normality -------------------------------------------------------------------

# FH: area-level west-east (no residuals available -> generate)
# fit_rent_FH_we_residuals <- as.data.frame(fit_rent_FH_we$eblup)
# fit_rent_FH_we_residuals$residuals <- fit_rent_FH_we_residuals - fit_rent_Direct_we$Direct
# colnames(fit_rent_FH_we_residuals) <- c("eblupFH", "residualsFH")
# fit_rent_FH_we_residuals

# qqnorm(fit_rent_FH_we$eblup$residuals, ylab = "residuals", xlab = "normal scores", 
#        main = "FH resid") 
# qqline(fit_rent_FH_we$eblup$residuals)

# BHF: area-level west-east
# qqnorm(fit_rent_BHF_we$fit$residuals, ylab = "residuals", xlab = "normal scores", 
#        main = "BHF resid") 
# qqline(fit_rent_BHF_we$fit$residuals)

# FH: area-level bezirk
df <- data.frame(residuals = fit_rent_FH_residuals$fit_rent_FH_residuals)
gg1 <- ggplot(df, aes(sample = residuals)) + stat_qq(color = "#E69F00") + stat_qq_line() + theme_bw()
pdf(paste0(projectpath,"/output/graph10_qqplot_FH.pdf"), 
    width = 4, height = 3.5)
print(gg1)
dev.off()


# BHF: area-level bezirk
df <- data.frame(residuals = fit_rent_BHF$fit$residuals)
gg2 <- ggplot(df, aes(sample = residuals)) + stat_qq(color = "#56B4E9") + stat_qq_line() + theme_bw()
pdf(paste0(projectpath,"/output/graph11_qqplot_BHF.pdf"), 
    width = 4, height = 3.5)
print(gg2)
dev.off()


#definitions of ui ei BHF ui eij
fit_rent_FH_ref 
fit_rent_FH_residuals

fit_rent_BHF_ref 
fit_rent_BHF_residuals 


# TESTS --------------------------------------------------------
################################################
# FH
# ei normality
hist(fit_rent_FH_residuals$fit_rent_FH_residuals)

# Kolmogorov - Smirnov 
# Ist der Wert der Teststatistik gr??er als der entsprechende tabellierte kritische Wert, so wird die Nullhypothese verworfen
# Ho: Normality 
ks.test(fit_rent_FH_residuals$fit_rent_FH_residuals, "pnorm")
#D = 0.33217, p-value = 0.1109
#alternative hypothesis: two-sided
# cannot reject normality 

# Shapiro - Wilks
shapiro.test(fit_rent_FH_residuals$fit_rent_FH_residuals)
# Ho: Normality 
# die Nullhypothese in der Regel nicht abgelehnt, wenn der p-Wert gr??er ist als das festgelegte Signifikanzniveau
#data:  fit_rent_FH_residuals$residualsFH$V1
#W = 0.88695, p-value = 0.1076
# cannot reject normality 

#Ui
# create Ui random effects FH-Estimates - intercept
# FH_rand_effects <- fit_rent_FH$eblup[,1] - fit_rent_FH$fit$estcoef$beta[1]
hist(fit_rent_FH_ref)

ks.test(fit_rent_FH_ref , "pnorm")
#D = 0.58294, p-value = 0.0002104
#alternative hypothesis: two-sided
#rejcet normality 
shapiro.test(fit_rent_FH_ref )
#data:  FH_rand_effects
#W = 0.94778, p-value = 0.6048
# cannot rejcet normality 


##################################################
# BHF
# ei
hist(fit_rent_BHF_residuals , breaks = 200)
# Kolmogorov - Smirnov 
# Ho: Normality 
ks.test(fit_rent_BHF_residuals, "pnorm")
#D = 0.54072, p-value < 0.00000000000000022
#alternative hypothesis: two-sided
#reject normality highly significantly 

# Shapiro - Wilks
shapiro.test(fit_rent_BHF_residuals) 
# Ho: Normality 
# die Nullhypothese in der Regel nicht abgelehnt, wenn der p-Wert gr??er ist als das festgelegte Signifikanzniveau
#data:  fit_rent_BHF$fit$residuals
#W = 0.9279, p-value = 0.00000000000001008
#reject normality highly significantly !

# Ui
ks.test(fit_rent_BHF_ref, "pnorm")
#D = 0.40964, p-value = 0.02467
#alternative hypothesis: two-sided
# cannot reject normality 

shapiro.test(fit_rent_BHF_ref$`(Intercept)`)
#data:  fit_rent_BHF$fit$random[, 1]
#W = 0.95633, p-value = 0.7304
hist(fit_rent_BHF_ref$`(Intercept)`)
# cannot reject normality 



fit_rent_FH_ref 
fit_rent_FH_residuals

fit_rent_BHF_ref 
fit_rent_BHF_residuals 

################################################################

#create long dataset for correlation analysis of Ui and ei

#FH correlation
FH_rand_effects <- tibble::rowid_to_column(data.frame(fit_rent_FH_ref), "bezirk")
FH_errors <- tibble::rowid_to_column(data.frame(FH_errors = fit_rent_FH_residuals), "bezirk") 

df_area<-plyr::join(FH_rand_effects, FH_errors, 
                    by = 'bezirk', type = 'left', match = 'all')
cor(df_area$fit_rent_FH_ref, df_area$fit_rent_FH_residuals, method = c("pearson"))
cor(df_area$fit_rent_FH_ref, df_area$fit_rent_FH_residuals, method = c("spearman"))

#BHF correlation
df_unit <- data.frame(bezirk = df_rent_unit$bezirk, residuals = fit_rent_BHF_residuals)

BHF_rand_effects <- tibble::rowid_to_column(data.frame(fit_rent_BHF_ref), "bezirk")
colnames(BHF_rand_effects)[2] <- "rand_effects"

df_unit <-plyr::join(df_unit, BHF_rand_effects, 
                     by = 'bezirk', type = 'left', match = 'all')
cor(df_unit$residuals, df_unit$rand_effects, method = c("pearson"))
cor(df_unit$residuals, df_unit$rand_effects, method = c("spearman"))

##########################################################################################
#Shrinkage factor FH --> Variance U /(Variance U + (Variance e / nd))

fit_rent_FH_ref 
fit_rent_FH_residuals

#Variance residuals 
var_FH_residuals <- var(fit_rent_FH_residuals)
#variance random effects
var_FH_ref <- var(fit_rent_FH_ref)

Shrinkage_FH <-  var_FH_ref[1]/(var_FH_ref[1] + (var_FH_residuals[1]/bezirk_size$sampSize))


#Shrinkage factor FH --> Variance U /(Variance U + (Variance e / nd))
# Random effects:
#   Groups   Name        Variance Std.Dev.
# dom      (Intercept)   322.6   17.96  
# Residual             24826.6  157.56  
# Number of obs: 497, groups:  dom, 12

shrinkage_BHF <-  322.6 / (322.6 + (24826.6/bezirk_size$sampSize))

# check
#var(fit_rent_FH_residuals$residualsFH$V1) 
#variance random effects 
#var(FH_rand_effects$FH_rand_effects)
#Shrinkage Factor 
#fit_rent_BHF$fit$random[,1]
#fit_rent_BHF$fit$residuals


# clean up
rm(list = c("out", "gg1", "gg2", "df",
            "df_rent_area_we", "bezirk_size_we", "Xmean_rent_we", "Popsize_rent_we", 
            "fit_rent_FH_we", "fit_rent_Direct_we", "fit_rent_FH_we_mse",
            "df_rent_unit_BHF_imp_we"
            #,"fit_rent_BHF_mse_we", "fit_rent_BHF_we"
            ))

print("-- end of robustness checks --")

###
