
# data datapreparation: area level data ---------------------------------------------
## trying to use similiar variables as Eilers (2017)
# OK: Share dwelling house
# OK: Share single- and two-familiy houses
# Share 3-9 family homes
# Share building blocks with 10 or more households Share low payment default
# Share medium payment default
# OK: Share high payment default
# Purchasing power per household
# OK: Share foreigners
# OK: Unemployment rate

# load, prepare correspondence-list: plz <-> bezirk
plz_bezirk <- read_excel(paste0(berlindata, "plz/RBSverzeichnisse20130621.xls"), 
                         sheet = "4.1 PLZ", range = "A10:C253", col_names = FALSE)
colnames(plz_bezirk) <- c("plz", "bezirk", "bezirk_name")
plz_bezirk$bezirk <- as.numeric(plz_bezirk$bezirk)
table(plz_bezirk$bezirk_name, plz_bezirk$bezirk)

# loading, prepare data: Unemployment rate
## source: https://statistik.arbeitsagentur.de/Navigation/Statistik/Statistik-nach-Regionen/SGBII-Traeger/Berlin/Charlottenburg-Wilmersdorf-Nav.html
alq_berlin <- as.data.frame(read_excel(paste0(berlindata, "berlin_arbeitslosenquote_bezirk_Jan2016-Dez2016.xlsx"), 
                         sheet = "2016", range = "A4:M16", col_names = TRUE))
alq_berlin$alq_mean <- rowMeans(alq_berlin[,2:13])
alq_berlin <- alq_berlin[,c(1,14)]

# loading, prepare data: population and Share foreigners
## source: https://www.statistik-berlin-brandenburg.de/Statistiken/statistik_SB.asp?Ptyp=700&Sageb=12041&creg=BBB
popshareforeig_berlin <- as.data.frame(read_excel(paste0(berlindata, "SB_A01-05-00_2016h01_BE.xlsx"), 
                                 sheet = "T7", range = "A40:M51", col_names = FALSE))
popshareforeig_berlin <- popshareforeig_berlin[,c(1,5,13)]
colnames(popshareforeig_berlin) <- c("bezirk_name", "shareGermans", "shareForeigners")

# loading, prepare data: share elderly
## source: https://www.statistik-berlin-brandenburg.de/produkte/produkte-langereihen.asp
# Define: "Elderly": age 55+, "Youth": age 15-27
shareelderly_berlin <- as.data.frame(read_excel(paste0(berlindata, "SB_A01-05-00_2016h01_BE.xlsx"), 
                                               sheet = "T11", range = "B4:L169", col_names = TRUE))
shareelderly_berlin <- shareelderly_berlin[-c(1,3:13,15,17:21,23,25:42,44,46:55,58:67,69,71:80,
                                              82,84:92,94,96:101,103,105:122,124,126:133,
                                              135,137:149,151,153:164),
                                           c(2:10)]

colnames(shareelderly_berlin)[1] <- "bezirk_name"
shareelderly_berlin$bezirk_name <- gsub("-","-",shareelderly_berlin$bezirk_name)
shareelderly_berlin$bezirk_pop <- NA
shareelderly_berlin$shareElderly <- NA
shareelderly_berlin$shareYouth <- NA

for (p in 1:12) {
  q = p + 1
  # assign values to associated bezirk
  shareelderly_berlin[p,c(2:9)] <- shareelderly_berlin[q,c(2:9)]
  shareelderly_berlin[p,10] <- shareelderly_berlin[q,1]
  shareelderly_berlin[p,1] <- substring(shareelderly_berlin[p,1], 5)
  shareelderly_berlin <- shareelderly_berlin[-q,]
  
  # calculate shareElderly
  shareelderly_berlin[p,11] <- (shareelderly_berlin[p,8] + shareelderly_berlin[p,9]) / as.numeric(shareelderly_berlin[p,10])
  # calculate shareYouth  
  shareelderly_berlin[p,12] <- (shareelderly_berlin[p,4] + shareelderly_berlin[p,5]) / as.numeric(shareelderly_berlin[p,10])
}

# loading, prepare data of own research: socio-economic infos (married, single, etc., hhs 
# with children, high income, etc.), Ubahn, crimes, neugebaute_Wohnungen, Bauintensität, 
# Tourismus_Gäste, Grundbuchumschreibungen, Wohnfläche_pro_Wohnung_m2, Einwohner_pro_Wohnung
# Angebotsmieten_nettokalt_EUR_pro_m2, Wohnbestand
furtherInfo_berlin <- as.data.frame(read_excel(paste0(berlindata, "Berlin_Recherche.xlsx"), 
                                                  sheet = "2016", range = "A2:AJ14", 
                                               col_names = TRUE))
for (i in 8:16) {
  furtherInfo_berlin[,i] <- furtherInfo_berlin[,i] * 1000
}

## Merge all dfs together
df_rent_area <- merge(x = furtherInfo_berlin, y = alq_berlin, by = c("bezirk_name"))
df_rent_area <- merge(x = df_rent_area, y = popshareforeig_berlin, by = c("bezirk_name"))
df_rent_area <- merge(x = df_rent_area, y = shareelderly_berlin, by = c("bezirk_name"))

## Merge dep. var. from df_rent_unit
# First: prepare df_rent for merge (subset, aggregate)
dep_vars <- c("rent_unweighted", "rent_unweighted_by_bezirk", "rent_weighted_by_bezirk", "var_rent_unweighted")
df_rent_unit_help <- df_rent_unit[,c("bezirk", "bezirk_name", dep_vars)]

df_rent_unit_help <- aggregate(df_rent_unit_help,
                               by = list(df_rent_unit$bezirk_name),
                               FUN = mean, na.rm = FALSE)
df_rent_unit_help <- df_rent_unit_help[,-3]
colnames(df_rent_unit_help)[1] <- "bezirk_name"

# Second: merge
df_rent_area <- merge(x = df_rent_area, y = df_rent_unit_help, by = "bezirk_name")

# cleaning
df_rent_area$bezirk_pop <- as.numeric(df_rent_area$bezirk_pop) # correction
colnames(df_rent_area)[2] <- "bezirk"
df_rent_area[,51] <- NULL # redundant var


attach(df_rent_area)
# calculate population related shares
df_rent_area$shareSinglesPop <-     100 * (pop_single / bezirk_pop)
df_rent_area$shareMarriedPop <-     100 * (pop_married / bezirk_pop)
df_rent_area$shareDivorcedPop <-    100 * (pop_divorced / bezirk_pop)
df_rent_area$shareWidowedPop <-     100 * (pop_widowed / bezirk_pop)
df_rent_area$sharePupilsPop <-      (pupils / bezirk_pop) * 100
df_rent_area$shareSchoolsPop <-     (all_schools / bezirk_pop) * 100
df_rent_area$sharePublSchoolsPop <- (public_schools / bezirk_pop) * 100
df_rent_area$sharePrivSchoolsPop <- (priv_schools / bezirk_pop) * 100
df_rent_area$shareCrimesPop <-      (crimes / bezirk_pop) * 100
df_rent_area$shareCriminalCharchPop <- (criminalcharches / bezirk_pop) * 100
df_rent_area$shareTouristsPop <- (tourists / bezirk_pop) * 100
df_rent_area$shareElderly <- 100 * df_rent_area$shareElderly

# calculate hh related shares
df_rent_area$shareFamChildrHH <-    100 * (priv_hh_with_children / Anzahl_hh)
df_rent_area$shareFamHighHIncHH <-  100 * (priv_hh_high_hhinc / Anzahl_hh)
df_rent_area$shareTotalHH <-        100 * (priv_hh_total / Anzahl_hh)
df_rent_area$shareSingleHH <-       100 * (priv_hh_single / Anzahl_hh)
df_rent_area$shareMultiHH <-        100 * (priv_hh_multiperson / Anzahl_hh)
df_rent_area$shareTwoPersHH <-      100 * (priv_hh_2pers / Anzahl_hh)
df_rent_area$shareThreePersHH <-    100 * (priv_hh_3pers / Anzahl_hh)
df_rent_area$shareFourMorePersHH <- 100 * (priv_hh_4morepers / Anzahl_hh)
detach(df_rent_area)

# Dummy West-Ost
colnames(df_rent_area)[21] <- "west"
df_rent_area$west <- ifelse(df_rent_area$west == "west", 1, 0)  # west==1, ost==0
df_rent_area[which(df_rent_area$west == "Mitte"),21] <- 0       # majority vote
df_rent_area[which(df_rent_area$west == "Friedrichshain-Kreuzberg"),21] <- 0

# finalize
df_rent_area <- df_rent_area[order(df_rent_area$bezirk),]

# check correlations
#cor(furtherInfo_berlin[,c(22:28)])
#corrplot::corrplot(cor(furtherInfo_berlin[,c(22:28)]), type="upper", order="hclust")

# clean up
rm(list = c("p", "q", "i", "alq_berlin", "furtherInfo_berlin", "popshareforeig_berlin", 
            "shareelderly_berlin", "df_rent_unit_help", "plz_bezirk"))

print("-- end of area-level data preparation --")

###
