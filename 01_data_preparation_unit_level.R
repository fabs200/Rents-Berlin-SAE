
# data datapreparation: unit level SOEP data ----------------------------------------

# read in soepdata
soepdata = c("bghgen", "bghbrutto", "bgpequiv", "bgkind")
for (i in soepdata) {
  print(paste0("reading in: ",soeppath,i,".dta"))
  assign(paste(i), read.dta(paste0(soeppath,i,".dta")))
}

# read in regional data
soepregion <- read.dta(paste0(regionpath, "sae12.dta"))

# count number of cases (hh-level data)
dim(bghgen)
length(which(with(bghgen, rent16 >= -5))) # 17,822
length(which(with(bghgen, rent16 <= 0))) # 10,078
length((unique(bghgen$hhnrakt))) # 17,822

# Merge hh level datasets
# (NOTE: bgpequiv dataset is not uniquely defined by hhnrakt. Therefore, make unique)
bgpequiv_help <- unique(subset(bgpequiv, select = c("hhnrakt","hhnr","bghhnr", "w1110216")))
df_rent_hh_prep0 <- merge(x = bghgen, y = bghbrutto, 
                          by = c("hhnrakt","hhnr","bghhnr"))
df_rent_hh_prep0 <- merge(x = df_rent_hh_prep0, y = bgpequiv_help, 
                          by = c("hhnrakt","hhnr","bghhnr"))
names(df_rent_hh_prep0)
describe(df_rent_hh_prep0$rent16) # n=17,822

# Merge regional data
# NOTE: soepregion contains already only obs. in Berlin, merging it restricts 
#   df_rent_hh_prep0 to Berlin
df_rent_hh_prep0a <- merge(x = df_rent_hh_prep0, y = soepregion, by = "hhnrakt")
describe(df_rent_hh_prep0a$rent16) # n=807

# load, prepare correspondence-list: plz <-> bezirk
plz_bezirk <- read_excel(paste0(berlindata, "plz/RBSverzeichnisse20130621.xls"), 
                         sheet = "4.1 PLZ", range = "A10:C253", col_names = FALSE)
colnames(plz_bezirk) <- c("plz", "bezirk", "bezirk_name")
plz_bezirk$bezirk <- as.numeric(plz_bezirk$bezirk)
table(plz_bezirk$bezirk_name, plz_bezirk$bezirk)

# Merge correspondence list: plz <-> districts
df_rent_hh_prep0b <- merge(x = df_rent_hh_prep0a, y = plz_bezirk, 
                           by = c("plz", "bezirk"), all.x = TRUE)
describe(df_rent_hh_prep0b$rent16) # n=807

# keep non-negative rent
df_rent_hh_prep1a <- subset(df_rent_hh_prep0b, rent16 >= 0) # n=499

# prepare and merge bgkind: hh_personen (=bghhgr), hh_kinder (=bgkzahl)
bgkind_help <- subset(bgkind, select = c("hhnrakt", "bghhnr", "bgkzahl"))
#df_rent_hh_prep1b <- merge(x = df_rent_hh_prep1a, y = bgkind_help, by = "hhnrakt", x.all = TRUE)
df_rent_hh_prep1b <- plyr::join(df_rent_hh_prep1a, bgkind_help, by = 'hhnrakt', type = 'left', match = 'all')
  # rename and if set hh_kinder==0 if NA
names(df_rent_hh_prep1b)[c(89, 117)] <- c("hh_personen", "hh_kinder")
df_rent_hh_prep1b[c("hh_kinder")][is.na(df_rent_hh_prep1b[c("hh_kinder")])] <- 0
  # rename, make unique
df_rent_hh_prep1b <- unique(df_rent_hh_prep1b)

# keep only values with valid owners (NOT: keine Eigentümer, keine Heimbewohner, keine -5, -1)
levels(bghgen$owner16)
summary(bghgen$owner16)
df_rent_hh_prep2 <- subset(df_rent_hh_prep1b , owner16 == "[2] Hauptmieter" | 
                             owner16 == "[3] Untermieter" | owner16 == "[4] Mieter")
# Note: category 4 "Mieter" was available in earlier waves, later on it was split into 
#        Hauptmieter + Untermieter for detailed information. Therefore 0 cases of "Mieter".

# check number of cases for rent
describe(df_rent_hh_prep0$rent16) # n=17822
describe(df_rent_hh_prep0a$rent16) # n=807
describe(df_rent_hh_prep0b$rent16) # n=807
describe(df_rent_hh_prep1a$rent16) # n=499
describe(df_rent_hh_prep1b$rent16) # n=499
describe(df_rent_hh_prep2$rent16) # n=497

# check number of cases for owner
describe(df_rent_hh_prep0$owner16) # n=17822
describe(df_rent_hh_prep1a$owner16) # n=499
describe(df_rent_hh_prep1b$owner16) # n=499
describe(df_rent_hh_prep2$owner16) # n=497

# select variables we use to explain 'rent'
rent_vars <- c("hhnrakt", "sample1", "w1110216", "rent16", "size16", "room16", "moveyr16",
               "heat16", "util16", "electr16", "gas16", "i1hinc16", "owner16", 
               "cnstyrmin16", "cnstyrmax16",  "bgsampreg", "nuts116",
               "hh_personen", "hh_kinder",
               # dwelling equipment
               "eqpkit16", "eqpshw16", "eqpiwc16", "eqphea16", "eqpter16", "eqpbas16", 
               "eqpgar16", "eqpwat16", "eqptel16", "eqpalm16", "eqpsol16", "eqpair16", 
               "eqplif16", "eqpnrj16", "eqpfhea16", "eqpinsul16", "eqpmglass16", 
               "eqppark16", "eqpnobar16",
               # info variables
               "norent16", "rentinfo16", "heatinfo16", "utilinfo16", "electrinfo16", "gasinfo16",
               # flag variables
               "fsize16", "froom16", "frent16", "f2rent16", "fheat16", "futil16", "felectr16",
               # typ dwelling, living
               "bgwum1", "bgwum2", "bgwum3",
               # regional variables
               "regbez", "gkz", "plz", "bezirk", "bezirk_name")

# non-zero and relevant vars
rent_estimates <- c("hhnrakt", "rent", "hh_weight", "owner", "nuts1", "bgsampreg", "hh_inc",
                    # house-specific continuous vars
                    "size", "room", "moveyr", "heat", "util", "electr", 
                    "cnstyrmin", "cnstyrmax", "cnstyr", "livingdur", 
                    "hh_personen", "hh_kinder",
                    # categorical equipment vars
                    "eqpter", "eqpbas", "eqpgar", "eqpalm", "eqpsol", "eqpair", 
                    "eqplif", "eqpnrj", "eqpfhea", "eqpinsul", "eqpmglass", 
                    "eqppark", "eqpnobar",
                    # typ dwelling, living
                    "bgwum1", "bgwum2", "bgwum3",
                    # regional variables
                    "regbez", "gkz", "plz", "bezirk", "bezirk_name")

# selection of relevant vars
df_rent_hh_prep3 <- subset(df_rent_hh_prep2, select = rent_vars)
unique(length(df_rent_hh_prep3$hhnrakt)) # n=497

# renaming
oldvarnames <- c(names(df_rent_hh_prep3))
newvarnames <- str_replace_all(oldvarnames, "16", "")
newvarnames <- gsub("w11102", "hh_weight", newvarnames)
newvarnames <- gsub("i1hinc", "hh_inc", newvarnames)
colnames(df_rent_hh_prep3) <- newvarnames

# prepare vars (moveyr -> years living in dwelling)
df_rent_hh_prep3$livingdur <- 2016 - df_rent_hh_prep3$moveyr
df_rent_hh_prep3[which(df_rent_hh_prep3$livingdur > 2016),60] <- NA # correction if -1
summary(df_rent_hh_prep3$livingdur)

# create 1 var out of cnstymin and cnstymax 
# (NOTE: if cnstyr==0, we define as cnstyr = cnstymax)
df_rent_hh_prep3$cnstyr <- round((df_rent_hh_prep3$cnstyrmin + df_rent_hh_prep3$cnstyrmax)/2)
df_rent_hh_prep3[which(df_rent_hh_prep3$cnstyrmin == 0),61] <- df_rent_hh_prep3[which(df_rent_hh_prep3$cnstyrmin == 0),15]
summary(df_rent_hh_prep3$cnstyr)

# replace all negative values with R's missings: NA
# numeric vars
for (i in -5:-1) {
  print(paste0("recoding missing value: ", i, " <- NA"))
  df_rent_hh_prep3[df_rent_hh_prep3 == i] <- NA
}
# categorical vars
na_categories = c("[-1] Keine Angabe", "[-1] keine Angabe", "[-2] Trifft nicht zu", "[-3] Nicht valide", 
               "[-4] Unzulaessige Mehrfachantwort", "[-5] In Fragebogenversion nicht enthalten",
               "[-6] Fragebogenversion mit geaenderter Filterfuehrung")
for (cat in na_categories) {
  print(paste0("recode category: ", cat, " <- NA"))
  df_rent_hh_prep3[df_rent_hh_prep3 == paste(cat)] <- NA
}

# subset dataset to selected variables
df_rent_hh_prep4 <- subset(df_rent_hh_prep3, select = rent_estimates)
unique(length(df_rent_hh_prep4$hhnrakt)) # n=497

# clean unused levels of factor vars
df_rent_hh_prep4 <- droplevels(df_rent_hh_prep4)

# sample size in bezirk
df_rent_hh_prep4$samplesize_in_bezirk <- 1
df_rent_hh_prep4$samplesize_in_bezirk <- ave(df_rent_hh_prep4$samplesize_in_bezirk, 
                                             by = df_rent_hh_prep4$bezirk, FUN = sum)

#Dummies erstellen !!!!
df_rent_hh_prep4$eqpnobar <- ifelse(df_rent_hh_prep4$eqpnobar  == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqppark  <- ifelse(df_rent_hh_prep4$eqppark   == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqpmglass<- ifelse(df_rent_hh_prep4$eqpmglass == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqpnrj   <- ifelse(df_rent_hh_prep4$eqpnrj    == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqplif   <- ifelse(df_rent_hh_prep4$eqplif    == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqpair   <- ifelse(df_rent_hh_prep4$eqpair    == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqpsol   <- ifelse(df_rent_hh_prep4$eqpsol    == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqpalm   <- ifelse(df_rent_hh_prep4$eqpalm    == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqpgar   <- ifelse(df_rent_hh_prep4$eqpgar    == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqpbas   <- ifelse(df_rent_hh_prep4$eqpbas    == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqpter   <- ifelse(df_rent_hh_prep4$eqpter    == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqpinsul   <- ifelse(df_rent_hh_prep4$eqpinsul    == "[1] Ja", 1, 0)
df_rent_hh_prep4$eqpfhea   <- ifelse(df_rent_hh_prep4$eqpfhea    == "[1] Ja", 1, 0)
df_rent_hh_prep4$bgsampreg   <- ifelse(df_rent_hh_prep4$bgsampreg    == "[1] Westdeutschland", 1, 0) #west==1, ost==0


# checks
length(unique(df_rent_hh_prep4$hhnrakt)) # n=497 (Hauptmieter, Untermieter)
length(unique(df_rent_hh_prep4$owner))  # n=2
names(df_rent_hh_prep4)

summary(df_rent_hh_prep4)
summary(df_rent_hh_prep4$rent) # mean=563.1
summary(df_rent_hh_prep4$owner) # Hauptmieter=482, Untermieter=15

# Dealing with SOEP weights ---------------------------------------------------------

# total hh population of Berlin that is tenant and pays rent
# NOTE: https://www.statistik-berlin-brandenburg.de/basiszeitreihegrafik/bas-zensus-haushalte.asp?Ptyp=300&Sageb=10025&creg=BBB&anzwer=7
# corresponds roughly to Amt für Statistik Berlin-Brandenburg (N=1,794,936)
sum(df_rent_hh_prep4$hh_weight) # N=1,601,853

# overall average rent (unweighted) vs. overal average rent (weighted)
df_rent_hh_prep4$rent_unweighted <- df_rent_hh_prep4$rent
mean(df_rent_hh_prep4$rent_unweighted) # mean=563.0946
weighted.mean(df_rent_hh_prep4$rent, w = df_rent_hh_prep4$hh_weight) # mean=502.308

## calculate unweighted average mean by bezirk: rent_unweighted_bezirk
df_rent_hh_prep4$rent_unweighted_by_bezirk <- ave(df_rent_hh_prep4$rent, 
                                                  by = df_rent_hh_prep4$bezirk, FUN = mean)
mean(df_rent_hh_prep4$rent_unweighted_by_bezirk) # mean=563.0946

## calculate weighted rent
df_rent_hh_prep4$rent_weighted <- sum(df_rent_hh_prep4$rent * df_rent_hh_prep4$hh_weight) /
  sum(df_rent_hh_prep4$hh_weight) # mean=502.3

## calculate unweighted average mean by bezirk: rent_weighted_by_bezirk
# Horvitz-Thomson estimator for mean of rent (slides chp.4, p.97)
  # First: calculate weighted rent -> rent_weighted_help
df_rent_hh_prep4$rent_weighted_help <- df_rent_hh_prep4$rent * df_rent_hh_prep4$hh_weight

  # Second: calculate mean of rent_weighted_help within bezirk (=numerator of eq. slides chp.4, p.97)
df_rent_hh_prep4$sum_rent_weighted_by_bezirk <- ave(df_rent_hh_prep4$rent_weighted_help, 
                                                by = df_rent_hh_prep4$bezirk, FUN = sum)

  # Third: calculate sum of weights by bezirk -> sum_weights_bezirk (=denominator of eq. slides chp.4, p.97)
df_rent_hh_prep4$sum_weights_by_bezirk <- ave(df_rent_hh_prep4$hh_weight, 
                                              by = df_rent_hh_prep4$bezirk, FUN = sum)
sum(unique(df_rent_hh_prep4$sum_weights_by_bezirk)) # N=1,601,853

  # Fourth: calculate avg. weighted rent by bezirk -> rent_weighted_bezirk (final quotient slides chp.4, p.97)
df_rent_hh_prep4$rent_weighted_by_bezirk <- df_rent_hh_prep4$sum_rent_weighted_by_bezirk / 
  df_rent_hh_prep4$sum_weights_by_bezirk
mean(unique(df_rent_hh_prep4$rent_weighted_by_bezirk)) # mean=519.3

## calculate weighted rent: rent * (n/N * weight)
df_rent_hh_prep4$rent_weighted <- df_rent_hh_prep4$rent * (length(unique(df_rent_hh_prep4$hhnrakt)) / sum(unique(df_rent_hh_prep4$sum_weights_by_bezirk)) * df_rent_hh_prep4$hh_weight)
summary(df_rent_hh_prep4$rent_weighted)

# checks by bezirk_name whether correcttly coded
df_rent_hh_prep4 %>% 
  group_by(bezirk_name) %>% 
  summarise(n = mean(samplesize_in_bezirk), rent_noW_a = mean(rent), 
            rent_noW_b = mean(rent_unweighted_by_bezirk),
            rent_W_a = mean(rent_weighted), rent_W_b = mean(rent_weighted_by_bezirk),
            var = var(rent), sd = sd(rent))


# Calculate within variances --------------------------------------------------------
df_rent_hh_prep4$var_rent_unweighted <- ave(df_rent_hh_prep4$rent, by = df_rent_hh_prep4$bezirk, FUN = var)
df_rent_hh_prep4$var_rent_weighted <- ave(df_rent_hh_prep4$rent_weighted, by = df_rent_hh_prep4$bezirk, FUN = var)

# Direct estimate: sd, cv -----------------------------------------------------------
df_rent_hh_prep4$Direct_sd <- sqrt(df_rent_hh_prep4$var_rent_weighted)
df_rent_hh_prep4$Direct_cv <- df_rent_hh_prep4$Direct_sd / df_rent_hh_prep4$rent_weighted_by_bezirk



# Test: box-cox transformation ------------------------------------------------------

#df_rent_hh_prep4$rent_unweighted <- boxcox(df_rent_hh_prep4$rent_unweighted)

# finalize --------------------------------------------------------------------------

# finalize dataset and clean up
df_rent_hh_prep4 <- df_rent_hh_prep4[,-which(names(df_rent_hh_prep4) %in% 
                                    c("rent_weighted_help", "sum_rent_weighted_by_bezirk", 
                                      "sum_weights_by_bezirk", "rent"))]
df_rent_unit <- df_rent_hh_prep4[order(df_rent_hh_prep4$bezirk, df_rent_hh_prep4$hhnrakt),]

rm(list = c("df_rent_hh_prep0", "df_rent_hh_prep0a", "df_rent_hh_prep0b", 
            "df_rent_hh_prep1a", "df_rent_hh_prep1b", "df_rent_hh_prep2" ,
            "df_rent_hh_prep3", "df_rent_hh_prep4", "bgkind_help",
            "bgpequiv_help", "soepdata", "plz_bezirk", "rent_vars",
            "bghgen", "bghbrutto", "bgpequiv", "bgkind", "soepregion",
            "cat", "i", "na_categories", "newvarnames", "oldvarnames"))

print("-- end of unit-level data preparation --")


###

