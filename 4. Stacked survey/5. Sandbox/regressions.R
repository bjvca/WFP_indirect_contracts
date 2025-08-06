# This script is use to run the regressions
rm(list = ls())
# Load the required libraries
library(chatR)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(reshape2)
library(forcats)
library(patchwork)
library(stargazer)
library(ggridges)
library(lavaan)
library(leaflet)
library(RColorBrewer)
library(geosphere)
library(cem)
library(AER)
library(dplyr)
# Set the working director"
# Check if the object 'pathA' exists
if (exists("pathA")) {
  dir <- pathA
} else {
  rm(list = ls())
  dir <- getwd() # Get the current working directory if 'pathA' does not exist
  dir <- strsplit(dir, "4. Stacked survey/5. Sandbox")[[1]]
}
dta_f <- read.csv(paste(dir, "4. Stacked survey/4. Data/data/DID_WFP_Farmer_data.csv",sep ="/"))
dta_t <- read.csv(paste(dir, "4. Stacked survey/4. Data/data/DID_WFP_Trader_data.csv",sep ="/"))
dta_t$strata[dta_t$strata == ""] <- "Conditional/Spillover"

###drop AMS group
dta_t <- subset(dta_t, strata != "AMS")
dta_f <- subset(dta_f, strata != "AMS")

###make controls - gender of HH head
dta_f$q15[dta_f$q15==""] <- dta_f$q8[dta_f$q15==""]
dta_f$male_head <- dta_f$q15 == "Male"
###make controls - age of HH head
dta_f$q16[dta_f$q16==""] <- dta_f$q10[dta_f$q16==""]
dta_f$age_head <- as.numeric(dta_f$q16)
###make controls - education of HH head
dta_f$q17[dta_f$q17==""] <- dta_f$q11[dta_f$q17==""]
dta_f$prim_head <- !(dta_f$q17=="No formal education" | dta_f$q17=="Some primary")
dta_f$hh_size <- dta_f$q19
##totland
dta_f$tot_land <- as.numeric(dta_f$q35a)
##cooperative memnership
dta_f$coop_mem <- dta_f$q33!="No"



dta_f$q203b <- as.numeric(dta_f$q203b)

dta_f$q108_1 <- dta_f$q108_1*dta_f$q203b
dta_f$q108_2 <- dta_f$q108_2*dta_f$q203b
dta_f$q108_3 <- dta_f$q108_3*dta_f$q203b
dta_f$q108_4 <- dta_f$q108_4*dta_f$q203b
dta_f$q108_5 <- dta_f$q108_5*dta_f$q203b
dta_f$q108_6 <- dta_f$q108_6*dta_f$q203b

dta_f$q109b_2 <- as.numeric(dta_f$q109b_2)

### look at transactions - this is for first season of 2023
### up to six separate transactions are recorded for each farmer
data_grph_1 <- data.frame(dta_f$farmer_id,dta_f$q107_1,dta_f$q108_1,dta_f$q109b_1,dta_f$strata,dta_f$q112_1)
names(data_grph_1) <- c("FarmerID","Month","VolumeSold","Price","strata","WFP_connect")
data_grph_2 <- data.frame(dta_f$farmer_id,dta_f$q107_2,dta_f$q108_2,dta_f$q109b_2,dta_f$strata,dta_f$q112_2)
names(data_grph_2) <- c("FarmerID","Month","VolumeSold","Price","strata","WFP_connect")
data_grph_3 <- data.frame(dta_f$farmer_id,dta_f$q107_3,dta_f$q108_3,dta_f$q109b_3,dta_f$strata,dta_f$q112_3)
names(data_grph_3) <- c("FarmerID","Month","VolumeSold","Price","strata","WFP_connect")
data_grph_4 <- data.frame(dta_f$farmer_id,dta_f$q107_4,dta_f$q108_4,dta_f$q109b_4,dta_f$strata,dta_f$q112_4)
names(data_grph_4) <- c("FarmerID","Month","VolumeSold","Price","strata","WFP_connect")
data_grph_5 <- data.frame(dta_f$farmer_id,dta_f$q107_5,dta_f$q108_5,dta_f$q109b_5,dta_f$strata,dta_f$q112_5)
names(data_grph_5) <- c("FarmerID","Month","VolumeSold","Price","strata","WFP_connect")
data_grph_6 <- data.frame(dta_f$farmer_id,dta_f$q107_6,dta_f$q108_6,dta_f$q109b_6,dta_f$strata,dta_f$q112_6)
names(data_grph_6) <- c("FarmerID","Month","VolumeSold","Price","strata","WFP_connect")

data_prices_23A <- rbind(data_grph_1,data_grph_2,data_grph_3,data_grph_4,data_grph_5,data_grph_6)

data_prices_23A$season <- "23A"


##now for second season of 2023
dta_f$q116_1 <- as.numeric(dta_f$q116_1)
dta_f$q117b_1 <- as.numeric(dta_f$q117b_1)

data_grph_1 <- data.frame(dta_f$farmer_id,dta_f$q115_1,dta_f$q116_1*dta_f$q203b,dta_f$q117b_1,dta_f$strata,dta_f$q120_1)
names(data_grph_1) <- c("FarmerID","Month","VolumeSold","Price","strata","WFP_connect")
data_grph_2 <- data.frame(dta_f$farmer_id,dta_f$q115_2,dta_f$q116_2*dta_f$q203b,dta_f$q117b_2,dta_f$strata,dta_f$q120_2)
names(data_grph_2) <- c("FarmerID","Month","VolumeSold","Price","strata","WFP_connect")
data_grph_3 <- data.frame(dta_f$farmer_id,dta_f$q115_3,dta_f$q116_3*dta_f$q203b,dta_f$q117b_3,dta_f$strata,dta_f$q120_3)
names(data_grph_3) <- c("FarmerID","Month","VolumeSold","Price","strata","WFP_connect")
data_grph_4 <- data.frame(dta_f$farmer_id,dta_f$q115_4,dta_f$q116_4*dta_f$q203b,dta_f$q117b_4,dta_f$strata,dta_f$q120_4)
names(data_grph_4) <- c("FarmerID","Month","VolumeSold","Price","strata","WFP_connect")

data_prices_23B <- rbind(data_grph_1,data_grph_2,data_grph_3,data_grph_4)
data_prices_23B$season <- "23B"
data_prices <- rbind(data_prices_23A,data_prices_23B)

data_prices$Price[data_prices$Price >1600] <- NA

data_prices$date <- as.Date(paste0("01 ", data_prices$Month), format = "%d %b %Y")
data_prices$date_month <- format(data_prices$date, "%b")
### pool strata

data_prices$strata2 <-  data_prices$strata == "Indirect" | data_prices$strata == "Spillover" 
data_prices$WFP_connect2 <- data_prices$WFP_connect == "Yes"


###merge in controls
## male_head
data_prices <- merge(data_prices, dta_f[c("farmer_id","male_head","age_head","prim_head","hh_size","tot_land","coop_mem","trader_numb")], by.x="FarmerID", by.y = "farmer_id" )



#exogenous but coarse

exo_model <- lm(Price~strata2+Month,data=data_prices)
coeftest(exo_model, vcov = vcovCL(exo_model, cluster = data_prices$FarmerID))



#endogenous

endo_model <- lm(Price~WFP_connect2+Month,data=data_prices)
coeftest(endo_model, vcov = vcovCL(endo_model, cluster = data_prices$FarmerID))

#first stage

first_stage <- lm(WFP_connect2~strata2+Month,data=data_prices)
coeftest(first_stage, vcov = vcovCL(first_stage, cluster = data_prices$FarmerID))
  
  
iv_model_nocontrols <- ivreg(Price~WFP_connect2+Month|
                        strata2+Month,
                      data = data_prices)
# Get the summary with diagnostics
iv_summary <- summary(iv_model_nocontrols, diagnostics = TRUE)

# Print the first-stage F-statistic (under "Weak instruments" test)
iv_summary$diagnostics


coeftest(iv_model_nocontrols, vcov = vcovCL(iv_model_nocontrols, cluster = data_prices$FarmerID))

###placebo test: Does living in a WFP-implementation area affect the price even for farmers who did not trade with WFP-linked traders?

# Subset the data to only those NOT connected to WFP-linked traders
placebo_data <- subset(data_prices, WFP_connect2 == FALSE)

# Regress price on strata2 and Month in this subgroup
placebo_model <- lm(Price ~ strata2 + Month, data = placebo_data)

# Clustered standard errors at the farmer level
placebo_se <- coeftest(placebo_model, vcov = vcovCL(placebo_model, cluster = placebo_data$FarmerID))

# Print the results
print(placebo_se)
### another placebo test: whether location has predictive power within the subset of farmers who did trade with WFP-linked traders
treated_data <- subset(data_prices, WFP_connect2 == TRUE)

treated_model <- lm(Price ~ strata2 + Month, data = treated_data)
coeftest(treated_model, vcov = vcovCL(treated_model, cluster = treated_data$FarmerID))

###add controls

iv_model_controls <- ivreg(Price~WFP_connect2+male_head+age_head+prim_head+hh_size+tot_land+coop_mem+VolumeSold+Month |
                    strata2+male_head+age_head+prim_head+hh_size+tot_land+coop_mem+VolumeSold+Month,
                  data = data_prices)

coeftest(iv_model_controls, vcov = vcovCL(iv_model_controls, cluster = data_prices$FarmerID))



# ###controls/matching
# Product Characteristics
# 
# Maize quality indicators (e.g., moisture content, impurities)
# 
# Variety (hybrid vs. local)
# 
# Grading (standardized or not)
# 
# Rationale: Higher-quality maize fetches higher prices and may also be more likely to be traded through formal contracts.
# 
# 2. Seller Characteristics
# 
# Gender, age, and education of the farmer
# 
# Farm size or output volume
# 
# Group membership or cooperative affiliation
# 
# Previous experience selling to traders/WFP
# 
# Rationale: Larger or more experienced farmers may negotiate better prices and be more likely to participate in specific contract types.
# 
# 3. Transaction Characteristics
# 
# Volume of transaction (price per kg often declines with scale)
# 
# Payment terms (cash vs. delayed)
# 
# Timing of sale (month or week of transaction; price seasonality is critical)
# 
# Rationale: Price differences may reflect liquidity terms or timing rather than contract effects.
# 
# 4. Buyer Characteristics
# 
# Trader type (local vs. regional, WFP-connected vs. not)
# 
# Distance between buyer and seller
# 
# Transport arrangements (who organizes transport?)
# 
# Rationale: Prices may differ systematically across buyer types regardless of contract modality.
# 
# 5. Geographic and Market Controls
# 
# Location fixed effects (district, subcounty, or even market-level dummies)
# 
# Market access (distance to town or paved road)
# 
# Local supply-demand conditions (e.g., rainfall, conflict, market density)
# 
# Rationale: Regional price variation can bias results if contract modalities are clustered geographically.
# 
# 6. Year/Season Fixed Effects
# 
# Include seasonal dummies or year fixed effects if your data spans multiple time periods.
# 
# Rationale: Controls for overall inflation, harvest cycles, and policy changes.


library(stargazer)
library(sandwich)
library(lmtest)

# Clustered standard errors
cl_vcov_exo         <- vcovCL(exo_model, cluster = data_prices$FarmerID)
cl_vcov_endo        <- vcovCL(endo_model, cluster = data_prices$FarmerID)
cl_vcov_first_stage <- vcovCL(first_stage, cluster = data_prices$FarmerID)
cl_vcov_iv1         <- vcovCL(iv_model_nocontrols, cluster = data_prices$FarmerID)
cl_vcov_iv2         <- vcovCL(iv_model_controls, cluster = data_prices$FarmerID)
sink(paste(dir,"4. Stacked survey/5. Sandbox/table_WFP_prices.tex", sep = ""))
stargazer(
  exo_model, endo_model, iv_model_nocontrols, iv_model_controls,
  se = list(
    sqrt(diag(cl_vcov_exo)),
    sqrt(diag(cl_vcov_endo)),
    sqrt(diag(cl_vcov_iv1)),
    sqrt(diag(cl_vcov_iv2))
  ),
  type = "latex",  # or "latex" for your paper
  title = "Effect of WFP connection on farm-gate prices",
  label = "tab:wfp_farmer_prices",
  column.labels = c("OLS Exogenous", "OLS Endogenous", "IV", "IV + Controls"),
  dep.var.caption = "Dependent variable: Price",
  covariate.labels = c("Strata (instrument)", "WFP connected (endogenous)", "Male head", "Age head", "Primary head", 
                       "HH size", "Land owned", "Coop member", "Volume sold"),
  keep = c("strata2", "WFP_connect2", "male_head", "age_head", "prim_head", 
           "hh_size", "tot_land", "coop_mem", "VolumeSold"),
  add.lines = list(
    c("Month FE", "Yes", "Yes", "Yes", "Yes"),
    c("Strata2 used as instrument", "No", "No", "Yes", "Yes"),
    c("Estimation", "OLS", "OLS", "2SLS", "2SLS")
  ),
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  keep.stat = c("n", "rsq")
)
sink()
### IV -  was WFP buying in the area?

#Now do this for traders - 
##dep var = price at which they sell -  weighted average of price with weights being how much they sold to WFP and not


dta_t$weight_23B <- ifelse(is.na(as.numeric(dta_t$q141)),0,as.numeric(dta_t$q141)/100) +  ifelse(is.na(as.numeric(dta_t$q150)),0,as.numeric(dta_t$q150)/100)
dta_t$weight_23B[dta_t$weight_23B>1] <- 1
dta_t$price_23B <- as.numeric(dta_t$q154)*dta_t$weight_23B + dta_t$q157*(1-dta_t$weight_23B)
dta_t$price_23B[dta_t$weight_23B == 0] <- dta_t$q157[dta_t$weight_23B == 0]
dta_t$price_23B[dta_t$weight_23B == 1] <- as.numeric(dta_t$q154[dta_t$weight_23B == 1])
dta_t$quant_23B <- as.numeric(dta_t$q155)*dta_t$weight_23B + dta_t$q158*(1-dta_t$weight_23B)
dta_t$quant_23B[dta_t$weight_23B == 0] <- dta_t$q158[dta_t$weight_23B == 0]
dta_t$quant_23B[dta_t$weight_23B == 1] <- as.numeric(dta_t$q155[dta_t$weight_23B == 1])


dta_t$weight_23A <- ifelse(is.na(as.numeric(dta_t$q167)),0,as.numeric(dta_t$q167)/100) +  ifelse(is.na(as.numeric(dta_t$q176)),0,as.numeric(dta_t$q176)/100)
dta_t$weight_23A[dta_t$weight_23A>1] <- 1
dta_t$price_23A <- as.numeric(dta_t$q180)*dta_t$weight_23A + dta_t$q183*(1-dta_t$weight_23A)
dta_t$price_23A[dta_t$weight_23A == 0] <- dta_t$q183[dta_t$weight_23A == 0]
dta_t$price_23A[dta_t$weight_23A == 1] <- as.numeric(dta_t$q180[dta_t$weight_23A == 1])
dta_t$quant_23A <- as.numeric(dta_t$q181)*dta_t$weight_23A + dta_t$q184*(1-dta_t$weight_23A)
dta_t$quant_23A[dta_t$weight_23A == 0] <- dta_t$q184[dta_t$weight_23A == 0]
dta_t$quant_23A[dta_t$weight_23A == 1] <- as.numeric(dta_t$q181[dta_t$weight_23A == 1])



##exo var = strata
dta_t$strata
## endo var =  sold to WFP
 
dta_t$WFP_23A <- dta_t$q165 == "Yes" | dta_t$q174 == "Yes"
dta_t$WFP_23B <- dta_t$q139 == "Yes" | dta_t$q148 == "Yes"

data_long_trader_B <- data.frame(dta_t$trader_id,dta_t$price_23B,dta_t$quant_23B,dta_t$strata,dta_t$WFP_23B)
names(data_long_trader_B) <- c("TraderID","Price","VolumeSold","strata","WFP_connect")
data_long_trader_B$season <- "B"

data_long_trader_A <- data.frame(dta_t$trader_id,dta_t$price_23A,dta_t$quant_23A,dta_t$strata,dta_t$WFP_23A)
names(data_long_trader_A) <- c("TraderID","Price","VolumeSold","strata","WFP_connect")
data_long_trader_A$season <- "A"

data_long_trader <- rbind(data_long_trader_A,data_long_trader_B)

##add controls
###make controls - gender of HH head
dta_t$q14[dta_t$q14==""] <- dta_t$q8[dta_t$q14==""]
dta_t$male_head <- dta_t$q14 == "Male"
###make controls - age of HH head
dta_t$age_head  <- dta_t$q10
dta_t$age_head[!is.na(dta_t$q15)] <- as.numeric(dta_t$q15)[!is.na(dta_t$q15)]
###make controls - education of HH head
dta_t$q16[dta_t$q16==""] <- dta_t$q11[dta_t$q16==""]
dta_t$prim_head <- !(dta_t$q16=="No formal education" | dta_t$q16=="Some primary")
dta_t$hh_size <- dta_t$q20


data_long_trader <- merge(data_long_trader, dta_t[c("trader_id","male_head","age_head","prim_head","hh_size")], by.x="TraderID", by.y = "trader_id" )


#exogenous but coarse
data_long_trader$strata <- relevel(factor(data_long_trader$strata), ref = "Control")

exo_model <- lm(Price~strata+season,data=data_long_trader)
coeftest(exo_model, vcov = vcovCL(exo_model, cluster = data_long_trader$TraderID))

#endogenous

endo_model <- lm(Price~WFP_connect+season,data=data_long_trader)
coeftest(endo_model, vcov = vcovCL(endo_model, cluster = data_long_trader$TraderID))

#first stage

first_stage <- lm(WFP_connect~strata+season,data=data_long_trader)
summary(first_stage)


iv_model_nocontrols <- ivreg(Price~WFP_connect+season|
                               strata+season,
                             data = data_long_trader)

coeftest(iv_model_nocontrols, vcov = vcovCL(iv_model_nocontrols, cluster = data_long_trader$TraderID))

# Get the summary with diagnostics
iv_summary <- summary(iv_model_nocontrols, diagnostics = TRUE)

# Print the first-stage F-statistic (under "Weak instruments" test)
iv_summary$diagnostics

###placebo test: Does living in a WFP-implementation area affect the price even for farmers who did not trade with WFP-linked traders?

# Subset the data to only those NOT connected to WFP-linked traders
placebo_data <- subset(data_long_trader, WFP_connect == FALSE)

# Regress price on strata2 and Month in this subgroup
placebo_model <- lm(Price ~ strata + season, data = placebo_data)

# Clustered standard errors at the farmer level
placebo_se <- coeftest(placebo_model, vcov = vcovCL(placebo_model, cluster = placebo_data$TraderID))

# Print the results
print(placebo_se)
### another placebo test: whether location has predictive power within the subset of farmers who did trade with WFP-linked traders
treated_data <- subset(data_long_trader, WFP_connect == TRUE)

treated_model <- lm(Price ~ strata + season, data = treated_data)
coeftest(treated_model, vcov = vcovCL(treated_model, cluster = treated_data$TraderID))



data_long_trader$VolumeSold[data_long_trader$VolumeSold > 50000000] <- NA
data_long_trader$VolumeSold <- data_long_trader$VolumeSold/1000

iv_model_controls <- ivreg(Price~WFP_connect+male_head+age_head+prim_head+hh_size+season+VolumeSold|
                               strata+male_head+age_head+prim_head+hh_size+season+VolumeSold,
                             data = data_long_trader)

coeftest(iv_model_controls, vcov = vcovCL(iv_model_controls, cluster = data_long_trader$TraderID))

# Clustered standard errors by TraderID
cl_vcov_exo_trader         <- vcovCL(exo_model, cluster = data_long_trader$TraderID)
cl_vcov_endo_trader        <- vcovCL(endo_model, cluster = data_long_trader$TraderID)
cl_vcov_iv1_trader         <- vcovCL(iv_model_nocontrols, cluster = data_long_trader$TraderID)
cl_vcov_iv2_trader         <- vcovCL(iv_model_controls, cluster = data_long_trader$TraderID)

# Save the LaTeX table
sink(paste0(dir, "4. Stacked survey/5. Sandbox/table_WFP_prices_trader.tex"))
stargazer(
  exo_model, endo_model, iv_model_nocontrols, iv_model_controls,
  se = list(
    sqrt(diag(cl_vcov_exo_trader)),
    sqrt(diag(cl_vcov_endo_trader)),
    sqrt(diag(cl_vcov_iv1_trader)),
    sqrt(diag(cl_vcov_iv2_trader))
  ),
  type = "latex",
  title = "Effect of WFP Connection on Trader-Level Selling Prices",
  label = "tab:wfp_trader_prices",
  column.labels = c("OLS Exogenous", "OLS Endogenous", "IV", "IV + Controls"),
  dep.var.caption = "Dependent variable: Selling Price (UGX/kg)",
  covariate.labels = c("Strata (instrument)", "WFP connected (endogenous)", "Male head", "Age head", "Prim head", "HH size", "VolumeSold"),
  keep = c("strata", "WFP_connect", "male_head", "age_head", "prim_head", "hh_size", "VolumeSold"),
  add.lines = list(
    c("Season FE", "Yes", "Yes", "Yes", "Yes"),
    c("Strata used as instrument", "No", "No", "Yes", "Yes"),
    c("Estimation", "OLS", "OLS", "2SLS", "2SLS")
  ),
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  keep.stat = c("n", "rsq")
)
sink()


#### We now turn to adoption, a key impact pathway.
#now for fertilizer
dta_f$organic_23A <- dta_f$q51 == "Yes"
dta_f$organic_23A[dta_f$q51 == "" | dta_f$q51 == "Don't know"] <- NA
tapply(dta_f$organic_23A, dta_f$strata, mean, na.rm=T)
#now for dap
dta_f$dap_23A <- dta_f$q52 == "Yes"
dta_f$dap_23A[dta_f$q52 == "" | dta_f$q52 == "Don't know"] <- NA
tapply(dta_f$dap_23A, dta_f$strata, mean, na.rm=T)
#now for urea
dta_f$urea_23A <- dta_f$q53 == "Yes"
dta_f$urea_23A[dta_f$q53 == "" | dta_f$q53 == "Don't know"] <- NA
tapply(dta_f$urea_23A, dta_f$strata, mean, na.rm=T)
#now for pesticides
dta_f$pesticides_23A <- dta_f$q55 == "Yes"
dta_f$pesticides_23A[dta_f$q55 == "" | dta_f$q55 == "Don't know"] <- NA
###improved seed use
dta_f$improved_seed_23A <- dta_f$q45 %in% c("Bazooka","DeKalb (DK) Monsanto","KH series","Longe 10H","Longe 10R/Kayongo-go","Longe 4 (OPV)","Longe 5 (nalongo-OPV)","Longe 5D (OPV)","Longe 6H", "Longe 7H","Longe 7R/Kayongo-go","MM3 (OPV)","Other hybrid","Other OPV", "Panner", "UH5051 (Gagawala)")

dta_f$improved_seed_23A[dta_f$q45 =="" | dta_f$q45 =="Don't know" ] <- NA
dta_f$improved_seed_23A[dta_f$q46 ==""] <- NA
dta_f$improved_seed_23A[dta_f$q46 =="Local Market"] <- FALSE
dta_f$improved_seed_23A[dta_f$q46 =="Other farmer/neigbor"] <- FALSE
dta_f$improved_seed_23A[dta_f$q46 =="Own stocks"] <- FALSE
tapply(dta_f$improved_seed_23A,dta_f$strata, mean,na.rm=T)

###now for second season of 2023

#now for fertilizer
dta_f$organic_23B <- dta_f$q75 == "Yes"
dta_f$organic_23B[dta_f$q75 == "" | dta_f$q75 == "Don't know"] <- NA
tapply(dta_f$organic_23B, dta_f$strata, mean, na.rm=T)
#now for dap
dta_f$dap_23B <- dta_f$q76 == "Yes"
dta_f$dap_23B[dta_f$q76 == "" | dta_f$q76 == "Don't know"] <- NA
tapply(dta_f$dap_23B, dta_f$strata, mean, na.rm=T)
#now for urea
dta_f$urea_23B <- dta_f$q77 == "Yes"
dta_f$urea_23B[dta_f$q77 == "" | dta_f$q77 == "Don't know"] <- NA
tapply(dta_f$urea_23B, dta_f$strata, mean, na.rm=T)
#now for pesticides
dta_f$pesticides_23B <- dta_f$q78 == "Yes"
dta_f$pesticides_23B[dta_f$q78 == "" | dta_f$q78 == "Don't know"] <- NA

###improved seed use
dta_f$improved_seed_23B <- dta_f$q45 %in% c("Bazooka","DeKalb (DK) Monsanto","KH series","Longe 10H","Longe 10R/Kayongo-go","Longe 4 (OPV)","Longe 5 (nalongo-OPV)","Longe 5D (OPV)","Longe 6H", "Longe 7H","Longe 7R/Kayongo-go","MM3 (OPV)","Other hybrid","Other OPV", "Panner", "UH5051 (Gagawala)")

dta_f$improved_seed_23B[dta_f$q69 =="" | dta_f$q69 =="Don't know" ] <- NA
dta_f$improved_seed_23B[dta_f$q70 ==""] <- NA
dta_f$improved_seed_23B[dta_f$q70 =="Local Market"] <- FALSE
dta_f$improved_seed_23B[dta_f$q70 =="Other farmer/neigbor"] <- FALSE
dta_f$improved_seed_23B[dta_f$q70 =="Own stocks"] <- FALSE
tapply(dta_f$improved_seed_23B,dta_f$strata, mean,na.rm=T)

library(andersonTools)
dta_f$strata2 <-  dta_f$strata == "Indirect" | dta_f$strata == "Spillover" 

season_A <- dta_f[,c("farmer_id","strata2","improved_seed_23A", "organic_23A", "dap_23A", "urea_23A", "pesticides_23A")]
season_B <- dta_f[,c("farmer_id","strata2","improved_seed_23B", "organic_23B", "dap_23B", "urea_23B", "pesticides_23B")]
season_A$season <- "23A"
season_B$season <- "23B"

### we also need a measure of direct connectivity to WFP - get from transaction level interactions
### if at least one transaction in season was with WFP, then connectivity is true, else false
farmer_level_data_23A <- data_prices_23A %>%
  group_by(FarmerID) %>%
  summarise(WFP_connect = any(WFP_connect == "Yes")) %>%
  mutate(WFP_connect = as.integer(WFP_connect))

season_A <- merge(season_A,farmer_level_data_23A, by.x="farmer_id", by.y="FarmerID")

farmer_level_data_23B <- data_prices_23B %>%
  group_by(FarmerID) %>%
  summarise(WFP_connect = any(WFP_connect == "Yes")) %>%
  mutate(WFP_connect = as.integer(WFP_connect))

season_B <- merge(season_B,farmer_level_data_23B, by.x="farmer_id", by.y="FarmerID")

#stack adoption variables
names(season_A) <- c("farmer_id", "strata2","improved_seed", "organic", "dap", "urea", "pesticides","season","WFP_connect")
names(season_B) <- c("farmer_id", "strata2","improved_seed", "organic", "dap", "urea", "pesticides","season","WFP_connect")

all <- rbind(season_A, season_B)

# Variables of interest
adoption_vars <- c("improved_seed", "organic", "dap", "urea", "pesticides")

all$adoption_index <- anderson_index(xmat= as.matrix(all[adoption_vars], revcols = 5))$index

all <- merge(all, dta_f[c("farmer_id","male_head","age_head","prim_head","hh_size","tot_land","coop_mem","trader_numb")], by.x="farmer_id", by.y = "farmer_id" )

#exogenous but coarse

adoption_vars <- c("improved_seed", "organic", "dap", "urea", "pesticides","adoption_index")

exo_model <- lm(adoption_index~strata2+season,data=all)
coeftest(exo_model, vcov = vcovCL(exo_model, cluster = all$farmer_id))

#endogenous

endo_model <- lm(adoption_index~WFP_connect+season,data=all)
coeftest(endo_model, vcov = vcovCL(endo_model, cluster = all$farmer_id))

#first stage

first_stage <- lm(WFP_connect~strata2+season,data=all)
coeftest(first_stage, vcov = vcovCL(first_stage, cluster = all$farmer_id))


iv_model_nocontrols <- ivreg(adoption_index~WFP_connect+season |
                               strata2+season,
                             data = all)
summary(iv_model_nocontrols, diagnostics = TRUE, vcov = vcovCL(iv_model_nocontrols, cluster = all$farmer_id))

##placebo test: Does living in a WFP-implementation area affect the price even for farmers who did not trade with WFP-linked traders?

# Subset the data to only those NOT connected to WFP-linked traders
placebo_data <- subset(all, WFP_connect == FALSE)

# Regress price on strata2 and Month in this subgroup
placebo_model <- lm(adoption_index ~ strata2 + season, data = placebo_data)

# Clustered standard errors at the farmer level
placebo_se <- coeftest(placebo_model, vcov = vcovCL(placebo_model, cluster =  placebo_data$farmer_id))

# Print the results
print(placebo_se)
### another placebo test: whether location has predictive power within the subset of farmers who did trade with WFP-linked traders
treated_data <- subset(all, WFP_connect == TRUE)

treated_model <- lm(adoption_index ~ strata2 + season, data = treated_data)
coeftest(treated_model, vcov = vcovCL(treated_model, cluster =  treated_data$farmer_id))


###add controls

iv_model_controls <- ivreg(adoption_index~WFP_connect+male_head+age_head+prim_head+hh_size+tot_land+coop_mem+season |
                             strata2+male_head+age_head+prim_head+hh_size+tot_land+coop_mem+season,
                           data = all)

summary(iv_model_controls, diagnostics = TRUE, vcov = vcovCL(iv_model_controls, cluster = all$farmer_id))

########

library(stargazer)
library(sandwich)
library(lmtest)

# Clustered standard errors
cl_vcov_exo         <- vcovCL(exo_model, cluster = all$farmer_id)
cl_vcov_endo        <- vcovCL(endo_model, cluster = all$farmer_id)
cl_vcov_first_stage <- vcovCL(first_stage, cluster = all$farmer_id)
cl_vcov_iv1         <- vcovCL(iv_model_nocontrols, cluster = all$farmer_id)
cl_vcov_iv2         <- vcovCL(iv_model_controls, cluster = all$farmer_id)
sink(paste(dir,"4. Stacked survey/5. Sandbox/adoption.tex", sep = ""))
stargazer(
  exo_model, endo_model, iv_model_nocontrols, iv_model_controls,
  se = list(
    sqrt(diag(cl_vcov_exo)),
    sqrt(diag(cl_vcov_endo)),
    sqrt(diag(cl_vcov_iv1)),
    sqrt(diag(cl_vcov_iv2))
  ),
  type = "latex",  # or "latex" for your paper
  title = "Effect of WFP connection on adoption",
  label = "tab:adoption",
  column.labels = c("OLS Exogenous", "OLS Endogenous", "IV", "IV + Controls"),
  dep.var.caption = "Dependent variable: Price",
  covariate.labels = c("Strata (instrument)", "WFP connected (endogenous)", "Male head", "Age head", "Primary head", 
                       "HH size", "Land owned", "Coop member"),
  keep = c("strata2", "WFP_connect", "male_head", "age_head", "prim_head", 
           "hh_size", "tot_land", "coop_mem"),
  add.lines = list(
    c("Month FE", "Yes", "Yes", "Yes", "Yes"),
    c("Strata2 used as instrument", "No", "No", "Yes", "Yes"),
    c("Estimation", "OLS", "OLS", "2SLS", "2SLS")
  ),
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  keep.stat = c("n", "rsq")
)
sink()


###


aggregated_data <- data_long_trader %>%
  group_by(TraderID) %>%
  summarise(
    Price = first(Price),  # or another numeric summary
    VolumeSold = first(VolumeSold),    # adjust as needed
    strata = first(strata),                # assuming it's the same within group
    WFP_connect = any(WFP_connect),                # boolean: TRUE if any TRUE
    season = first(season),
    male_head = first(male_head),            # another boolean
    age_head = first(age_head),
    prim_head = first(prim_head),          # another boolean
    hh_size = first(hh_size),
    .groups = "drop"
  )

table(dta_t$q114)
table(dta_t$q116)
table(dta_t$q117)
table(dta_t$q119)

dta_t$qual <- dta_t$q119 == "Yes, always" & dta_t$q114 == "Yes, always" & dta_t$q117 == "Yes" & (dta_t$q116 == "Yes, using screen/sieve" | dta_t$q116 == "Yes, using mechanical cleaner")
dta_t$inputs <- dta_t$q111 == "Yes" | dta_t$q104 =="Yes"

aggregated_data <- merge(aggregated_data, dta_t[c("trader_id","qual","inputs")],by.x ="TraderID",  by.y="trader_id", all.x=TRUE)

#### quality
#exogenous but coarse
aggregated_data$strata <- relevel(factor(aggregated_data$strata), ref = "Control")

exo_model <- lm(qual~strata,data=aggregated_data)
coeftest(exo_model, vcov = vcovCL(exo_model, cluster = aggregated_data$TraderID))

#endogenous

endo_model <- lm(qual~WFP_connect,data=aggregated_data)
coeftest(endo_model, vcov = vcovCL(endo_model, cluster = aggregated_data$TraderID))

#first stage

first_stage <- lm(WFP_connect~strata,data=aggregated_data)
summary(first_stage)


iv_model_nocontrols <- ivreg(qual~WFP_connect|
                               strata,
                             data = aggregated_data)

coeftest(iv_model_nocontrols, vcov = vcovCL(iv_model_nocontrols, cluster = aggregated_data$TraderID))

# Get the summary with diagnostics
iv_summary <- summary(iv_model_nocontrols, diagnostics = TRUE)

# Print the first-stage F-statistic (under "Weak instruments" test)
iv_summary$diagnostics

###placebo test: Does living in a WFP-implementation area affect the price even for farmers who did not trade with WFP-linked traders?

# Subset the data to only those NOT connected to WFP-linked traders
placebo_data <- subset(aggregated_data, WFP_connect == FALSE)

# Regress price on strata2 and Month in this subgroup
placebo_model <- lm(qual ~ strata , data = placebo_data)

# Clustered standard errors at the farmer level
placebo_se <- coeftest(placebo_model, vcov = vcovCL(placebo_model, cluster = placebo_data$TraderID))

# Print the results
print(placebo_se)
### another placebo test: whether location has predictive power within the subset of farmers who did trade with WFP-linked traders
treated_data <- subset(aggregated_data, WFP_connect == TRUE)

treated_model <- lm(qual ~ strata , data = treated_data)
coeftest(treated_model, vcov = vcovCL(treated_model, cluster = treated_data$TraderID))



data_long_trader$VolumeSold[data_long_trader$VolumeSold > 50000000] <- NA
data_long_trader$VolumeSold <- data_long_trader$VolumeSold/1000

iv_model_controls <- ivreg(qual~WFP_connect+male_head+age_head+prim_head+hh_size+VolumeSold|
                             strata+male_head+age_head+prim_head+hh_size+VolumeSold,
                           data = aggregated_data)

coeftest(iv_model_controls, vcov = vcovCL(iv_model_controls, cluster = aggregated_data$TraderID))

# Clustered standard errors by TraderID
cl_vcov_exo_trader         <- vcovCL(exo_model, cluster = aggregated_data$TraderID)
cl_vcov_endo_trader        <- vcovCL(endo_model, cluster = aggregated_data$TraderID)
cl_vcov_iv1_trader         <- vcovCL(iv_model_nocontrols, cluster = aggregated_data$TraderID)
cl_vcov_iv2_trader         <- vcovCL(iv_model_controls, cluster = aggregated_data$TraderID)

# Save the LaTeX table
sink(paste0(dir, "4. Stacked survey/5. Sandbox/table_WFP_qual_trader.tex"))
stargazer(
  exo_model, endo_model, iv_model_nocontrols, iv_model_controls,
  se = list(
    sqrt(diag(cl_vcov_exo_trader)),
    sqrt(diag(cl_vcov_endo_trader)),
    sqrt(diag(cl_vcov_iv1_trader)),
    sqrt(diag(cl_vcov_iv2_trader))
  ),
  type = "latex",
  title = "Effect of WFP Connection on Trader-Level Quality Investment",
  label = "tab:wfp_trader_qual",
  column.labels = c("OLS Exogenous", "OLS Endogenous", "IV", "IV + Controls"),
  dep.var.caption = "Dependent variable: Invests in quality (1=yes)",
  covariate.labels = c("Strata (instrument)", "WFP connected (endogenous)", "Male head", "Age head", "Prim head", "HH size", "VolumeSold"),
  keep = c("strata", "WFP_connect", "male_head", "age_head", "prim_head", "hh_size", "VolumeSold"),
  add.lines = list(
     c("Strata used as instrument", "No", "No", "Yes", "Yes"),
    c("Estimation", "OLS", "OLS", "2SLS", "2SLS")
  ),
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  keep.stat = c("n")
)
sink()

#### inputs
#exogenous but coarse
aggregated_data$strata <- relevel(factor(aggregated_data$strata), ref = "Control")

exo_model <- lm(inputs~strata,data=aggregated_data)
coeftest(exo_model, vcov = vcovCL(exo_model, cluster = aggregated_data$TraderID))

#endogenous

endo_model <- lm(inputs~WFP_connect,data=aggregated_data)
coeftest(endo_model, vcov = vcovCL(endo_model, cluster = aggregated_data$TraderID))

#first stage

first_stage <- lm(WFP_connect~strata,data=aggregated_data)
summary(first_stage)


iv_model_nocontrols <- ivreg(inputs~WFP_connect|
                               strata,
                             data = aggregated_data)

coeftest(iv_model_nocontrols, vcov = vcovCL(iv_model_nocontrols, cluster = aggregated_data$TraderID))

# Get the summary with diagnostics
iv_summary <- summary(iv_model_nocontrols, diagnostics = TRUE)

# Print the first-stage F-statistic (under "Weak instruments" test)
iv_summary$diagnostics

###placebo test: Does living in a WFP-implementation area affect the price even for farmers who did not trade with WFP-linked traders?

# Subset the data to only those NOT connected to WFP-linked traders
placebo_data <- subset(aggregated_data, WFP_connect == FALSE)

# Regress price on strata2 and Month in this subgroup
placebo_model <- lm(inputs ~ strata , data = placebo_data)

# Clustered standard errors at the farmer level
placebo_se <- coeftest(placebo_model, vcov = vcovCL(placebo_model, cluster = placebo_data$TraderID))

# Print the results
print(placebo_se)
### another placebo test: whether location has predictive power within the subset of farmers who did trade with WFP-linked traders
treated_data <- subset(aggregated_data, WFP_connect == TRUE)

treated_model <- lm(inputs ~ strata , data = treated_data)
coeftest(treated_model, vcov = vcovCL(treated_model, cluster = treated_data$TraderID))



iv_model_controls <- ivreg(inputs~WFP_connect+male_head+age_head+prim_head+hh_size+VolumeSold|
                             strata+male_head+age_head+prim_head+hh_size+VolumeSold,
                           data = aggregated_data)

coeftest(iv_model_controls, vcov = vcovCL(iv_model_controls, cluster = aggregated_data$TraderID))

# Clustered standard errors by TraderID
cl_vcov_exo_trader         <- vcovCL(exo_model, cluster = aggregated_data$TraderID)
cl_vcov_endo_trader        <- vcovCL(endo_model, cluster = aggregated_data$TraderID)
cl_vcov_iv1_trader         <- vcovCL(iv_model_nocontrols, cluster = aggregated_data$TraderID)
cl_vcov_iv2_trader         <- vcovCL(iv_model_controls, cluster = aggregated_data$TraderID)

# Save the LaTeX table
sink(paste0(dir, "4. Stacked survey/5. Sandbox/table_WFP_inputs_trader.tex"))
stargazer(
  exo_model, endo_model, iv_model_nocontrols, iv_model_controls,
  se = list(
    sqrt(diag(cl_vcov_exo_trader)),
    sqrt(diag(cl_vcov_endo_trader)),
    sqrt(diag(cl_vcov_iv1_trader)),
    sqrt(diag(cl_vcov_iv2_trader))
  ),
  type = "latex",
  title = "Effect of WFP Connection on Trader providing inputs",
  label = "tab:wfp_trader_inputs",
  column.labels = c("OLS Exogenous", "OLS Endogenous", "IV", "IV + Controls"),
  dep.var.caption = "Dependent variable: Invests in quality (1=yes)",
  covariate.labels = c("Strata (instrument)", "WFP connected (endogenous)", "Male head", "Age head", "Prim head", "HH size", "VolumeSold"),
  keep = c("strata", "WFP_connect", "male_head", "age_head", "prim_head", "hh_size", "VolumeSold"),
  add.lines = list(
    c("Strata used as instrument", "No", "No", "Yes", "Yes"),
    c("Estimation", "OLS", "OLS", "2SLS", "2SLS")
  ),
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  keep.stat = c("n")
)
sink()
