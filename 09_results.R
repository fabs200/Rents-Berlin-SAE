
# Plot: Rent Prediction -------------------------------------------------------------
df_rent_results_pred_long <- reshape(df_rent_results, 
             varying = c("Direct", "eblup_FH", "eblup_BHF"), 
             v.names = "pred_rent",
             timevar = "model", 
             times = c("Direct", "eblup_FH", "eblup_BHF"), 
             direction = "long")

# comparison of models of all predictions
gg1 <- ggplot(df_rent_results_pred_long, aes(y = pred_rent, x = model)) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"),
                     labels = c("HT", "FH", "BHF")) +
  xlab("") + ylab("rent") + geom_boxplot(aes(color = model))  + theme_bw() +
  scale_x_discrete(limits = c("Direct","eblup_FH","eblup_BHF")) +
  theme(axis.line = element_blank(), axis.text.x = element_blank(),
        legend.title = element_blank())
pdf(paste0(projectpath,"/output/graph1_boxplot_rent.pdf"), 
    width = 4, height = 2.9)
print(gg1)
dev.off()

# barplot
bezirk_shortname = c("Mitt","FrKr","Pank", "ChWi", "Span", "StZe", "TSch", "Neuk", "TrKö", "MaHe", "Lich", "Rein")

gg2 <- ggplot(df_rent_results_pred_long, aes(fill = model, y = pred_rent, x = reorder(bezirk_name,bezirk))) + 
  geom_bar(position = "dodge", stat = "identity") + theme_bw() +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"), 
                    name = "", breaks = c("Direct", "eblup_FH", "eblup_BHF"),
                    labels = c("HT", "FH", "BHF")) +
  xlab("") + ylab("rent") +
  theme(axis.text.x = element_text(angle = 45, hjust = .945)) + 
  scale_x_discrete(labels = bezirk_shortname)
pdf(paste0(projectpath,"/output/graph2_barplot_rent.pdf"), 
    width = 4.55, height = 2.9)
print(gg2)
dev.off()

# Plot: CV --------------------------------------------------------------------------
df_rent_results_cv_long <- reshape(df_rent_results, 
                                varying = c("Direct_cv", "cv_FH", "cv_BHF"), 
                                v.names = "cv",
                                timevar = "model", 
                                times = c("Direct_cv", "cv_FH", "cv_BHF"), 
                                direction = "long")


gg3 <- ggplot(df_rent_results_cv_long, aes(x = model, y = cv)) +
  geom_boxplot(aes(color = model)) +
  scale_color_manual(values = c("#56B4E9", "#E69F00", "#999999"),
                     labels = c("BHF", "FH", "HT")) + theme_bw() +
  xlab("") + ylab("cv") +
  theme(axis.line = element_blank(), axis.text.x = element_blank(),
        legend.title = element_blank()) +
  scale_x_discrete(limits = c("Direct_cv","cv_FH","cv_BHF"))
pdf(paste0(projectpath,"/output/graph3_boxplot_cv.pdf"), 
    width = 4, height = 2.8)
print(gg3)
dev.off()


# Plot: sqrtMSE ---------------------------------------------------------------------
df_rent_results_sqrtmse_long <- reshape(df_rent_results, 
                                        varying = c("sqrtmse_FH", "sqrtmse_BHF"), 
                                        v.names = "sqrtmse",
                                        timevar = "model", 
                                        times = c("sqrtmse_FH", "sqrtmse_BHF"), 
                                        direction = "long")

gg4 <- ggplot(df_rent_results_sqrtmse_long, aes(x = model, y = sqrtmse)) +
  geom_boxplot(aes(color = model)) +
  scale_color_manual(values = c("#56B4E9", "#E69F00"),
                     labels = c("FH", "BHF")) +
  xlab("") + ylab(bquote(sqrt(MSE)[] )) +
  scale_x_discrete(limits = c("sqrtmse_FH","sqrtmse_BHF")) + theme_bw() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank())
pdf(paste0(projectpath,"/output/graph4_boxplot_sqrtMSE.pdf"), 
    width = 4, height = 2.9)
print(gg4)
dev.off()


# Plot: MSE -------------------------------------------------------------------------
df_rent_results_mse_long <- reshape(df_rent_results, 
                                        varying = c("mse_FH", "mse_BHF"), 
                                        v.names = "mse",
                                        timevar = "model", 
                                        times = c("mse_FH", "mse_BHF"), 
                                        direction = "long")

gg5 <- ggplot(df_rent_results_mse_long, aes(x = model, y = mse)) +
  geom_boxplot(aes(color = model)) +
  scale_color_manual(values = c("#56B4E9", "#E69F00"),
                     labels = c("BHF", "FH")) +
  xlab("") + ylab(bquote(MSE[])) +
  scale_x_discrete(limits = c("mse_FH","mse_BHF")) + theme_bw() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank())
pdf(paste0(projectpath,"/output/graph5_boxplot_MSE.pdf"), 
    width = 4.25, height = 2.95)
print(gg5)
dev.off()


# Also f?r Direct folgende Variablen:
#   bezirk_name, Direct, Direct_var, Direct_cv
# 
# F?r FH:
#   bezirk_name, eblup_FH, sqrtmse_FH, cv_FH
# 
# F?r BHF:
#   bezirk_name, eblup_BHF, sqrtmse_BHF, cv_BHF


# Table: Direct ---------------------------------------------------------------------
#use df_rent_results
results_dir <- subset(df_rent_results, select = c( "bezirk_name", "Direct", "Direct_var", "Direct_cv"))
colnames(results_dir)<-c("Domains", "Direct", "var", "CV" )
print(xtable(results_dir, type = "Latex"), file = paste0(projectpath, "output/results_dir.tex"))


# Table: FH -------------------------------------------------------------------------
#use df_rent_results
results_FH <- subset(df_rent_results, select = c("bezirk_name","eblup_FH",    "cv_FH" ,   "mse_FH", "sqrtmse_FH"))
colnames(results_FH)<-c("Domains", "eblup_FH", "cv", "mse", "sqrtmse" )
print(xtable(results_FH, type = "Latex"), file = paste0(projectpath, "output/results_FH.tex"))

# Table: BHF ------------------------------------------------------------------------
#use df_rent_results
results_BHF <- subset(df_rent_results, select = c("bezirk_name","eblup_BHF",    "cv_BHF" ,   "mse_BHF", "sqrtmse_BHF"))
colnames(results_BHF)<-c("Domains", "eblup_BHF", "cv", "mse", "sqrtmse" )
print(xtable(results_BHF, type = "Latex"), file = paste0(projectpath, "output/results_BHF.tex"))


df_rent_results$bezirk_short <- c("Mitt","FrKr","Pank", "ChWi","Span", "StZe", "TSch",
                                     "Neuk","TrK?", "MaHe","Lich","Rein")
  # 01  Mitte                       Mitt 
  # 02  Friedrichshain-Kreuzberg    FrKr
  # 03  Pankow                      Pank
  # 04  Charlottenburg-Wilmersdorf  ChWi
  # 05  Spandau                     Span
  # 06  Steglitz-Zehlendorf         StZe
  # 07  Tempelhof-Sch?neberg        TSch
  # 08  Neuk?lln                    Neuk
  # 09  Treptow-K?penick            TrK?
  # 10  Marzahn-Hellersdor          MaHe
  # 11  Lichtenberg                 Lich
  # 12  Reinickendorf               Rein    
  # https://www.statistik-berlin-brandenburg.de/produkte/verzeichnisse/RBSverzeichnisse20130621.pdf
  
results_all <- subset(df_rent_results, select = c( "bezirk_short", "Direct", "Direct_var", "Direct_cv",
                                    "eblup_FH",    "cv_FH" ,   "mse_FH", "sqrtmse_FH",
                                    "eblup_BHF",    "cv_BHF" ,   "mse_BHF", "sqrtmse_BHF"))
colnames(results_all)[1]<-c("Domains")

print(xtable(results_all, type = "Latex"), file = paste0(projectpath, "output/results_all.tex"))


print("-- DONE --")

###
