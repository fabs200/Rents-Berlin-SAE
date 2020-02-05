
# descriptive statistics ------------------------------------------------------------

## unit level data
names(df_rent_unit)
describe(df_rent_unit)
describe(df_rent_unit$bezirk_name)

# unweighted mean vs. weighted mean
mean(df_rent_unit$rent_unweighted) # mean=563.0946
weighted.mean(df_rent_unit$rent_unweighted, w = df_rent_unit$hh_weight) # mean=502.308

# Weighted HCR vs. unweighted HCR
arpr(df_rent_unit$hh_inc, weights = NULL)
arpr(df_rent_unit$hh_inc, weights = df_rent_unit$hh_weight)

## area level data
names(df_rent_area)
describe(df_rent_area)
describe(df_rent_area$bezirk_name)


# graphs ----------------------------------------------------------------------------


# histogram: rent
p_rent <- ggplot(df_rent_unit, aes(x = rent_unweighted,  y = ..density..)) + 
  geom_histogram(position = "identity", color = "black", fill = "steelblue", bins = 28, alpha = .5) +
  geom_vline(aes(xintercept = mean(rent_unweighted)), color = "blue", linetype = "dashed", size = .5) +
  theme_bw() + xlab("rent in Euro")# + ggtitle("histogram of rent")
p_rent
ggsave(paste0(projectpath,"/output/histogram_rent.pdf"), width = 4, height = 3.25)

# histogram with density fct: rent
p_rent <- ggplot(df_rent_unit, aes(x = rent_unweighted, y = ..density..), aes(fill = ..count..)) + 
  geom_histogram(position = "identity", color = "black", fill = "steelblue", bins = 28, alpha = .5) +
  geom_density(aes(color = "Density")) +
  geom_vline(aes(xintercept = mean(rent_unweighted)), color = "blue", linetype = "dashed", size = .5) +
  theme_bw() + xlab("rent (in Euro)") + theme(legend.position = "none")# + ggtitle("histogram of rent") 
p_rent
ggsave(paste0(projectpath,"/output/histogram_density_rent.pdf"), width = 4, height = 3.25)

# export tex table
selected_vars = c("rent_unweighted", "size", "room", "moveyr", "heat", "electr", "hh_personen", "hh_kinder",
                  "cnstyrmin", "cnstyrmax", "eqpter", "eqpbas", "eqpgar", "eqpalm", "eqpsol")
# numeric vars
stargazer(subset(df_rent_unit, select = selected_vars), summary = TRUE, rownames = TRUE,  
          summary.stat = c("N", "mean","min","max"), float = FALSE,
          out = paste0(projectpath, "output/table_soep_descriptive.tex"))


selected_vars_area = c("shareFamChildrHH" , "alq_mean" , 
                         "shareForeigners" , "west" , #shareSinglesPop, shareTouristsPop,
                         "quoted_net_rent_EUR_per_m2" , "building_intensity", "shareElderly",
                         "shareFamHighHIncHH")

stargazer(subset(df_rent_area, select = selected_vars_area), summary = TRUE, rownames = TRUE,  
          summary.stat = c("N", "mean","min","max"), float = FALSE,
          out = paste0(projectpath, "output/table_descriptives_area.tex"))


# categorical vars
tb1 <- ftable(df_rent_unit$bezirk_name, df_rent_unit$eqpair, df_rent_unit$eqpbas, df_rent_unit$eqpgar)
tb1
tb1.xtable <- xtableFtable(tb1)
print.xtableFtable(tb1.xtable)

tb2 <- ftable(df_rent_unit$bezirk_name)
tb2
tb2.xtable <- xtableFtable(tb2, method = "compact")
print.xtableFtable(tb2.xtable)

# box-plots by bezirk: FH vs. BHF
ggplot(df_rent_unit, aes(x = rent_unweighted, y = (prediction - trueStat), fill = method)) + 
  geom_boxplot() + theme(legend.position = "bottom")
ggplot(df_rent_unit, aes(x = region, y = (prediction - trueStat)^2, fill = method)) + 
  geom_boxplot() + scale_y_log10() + theme(legend.position = "bottom")

# clean up
rm(list = c("p_rent", "tb1", "tb1.xtable", "tb2", "tb2.xtable", "selected_vars"))

print("-- end of descriptives --")

###
