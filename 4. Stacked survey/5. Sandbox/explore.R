# This script is use to run the main analysis for the WFP project
rm(list = ls())
# Load the required libraries
library(chatR)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(reshape2)
library(forcats)
library(leaflet)
library(patchwork)
library(stargazer)
# Set the working directory
dir <- getwd()
dir <- strsplit(dir, "5. Sandbox")[[1]]
dta_f <- read.csv(paste(dir, "4. Data/data/DID_WFP_Farmer_data.csv",sep ="/"))
dta_t <- read.csv(paste(dir, "4. Data/data/DID_WFP_Trader_data.csv",sep ="/"))
dta_t$strata[dta_t$strata == ""] <- "Conditional/Spillover"
### one of the key identification strategies was to exploit within farmer differences with respect to who they sell to, so we start by looking if we have sufficient farmers that make more than one transaction (in the first season of 2023

dta_f$nr_of_transactions_23_1 <- as.numeric(dta_f$q106)
table(dta_f$nr_of_transactions_23_1) 

##only about 130 households recoded more than one transaction in first season of 2023, so the within regression is probably not worth exploring
##this would be about 300 observations
## can I run a decelare design analysis to look at MDE?
library(DeclareDesign)
M <- declare_model(N = 300, U = rnorm(N,mean(dta_f$q109b_1, na.rm=T),sd(dta_f$q109b_1, na.rm=TRUE)), potential_outcomes(Y ~ runif(n=1, min=0, max=.2)*mean(dta_f$q109b_1, na.rm=TRUE)*Z+U)) +
	declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
	declare_assignment(Z = complete_ra(N = N)) +
	declare_measurement(Y = reveal_outcomes(Y ~Z)) +
	declare_estimator(Y ~ Z,inquiry = "ATE")

diagnosis_M <- diagnose_design(M)

library(tidyverse)
simulations <-
	  diagnosis_M |>
	    get_simulations() |> 
	      mutate(significant = as.numeric(p.value <= 0.05))
library(scales)
library(rdss)
label_df <- 
	  tibble(
		     label = c("Conventional power target: 0.8", "Confidence region depicts\nsimulation error"),
		         x = c(0.1, 0.275),
		         y = c(0.85, 0.3),
			     color = rev(dd_palette("two_color_palette")),
			     hjust = c(0.5, 0)
			       )


g <-	  ggplot(simulations) + 
	    aes(bin, estimate) +
	      stat_smooth(aes(estimand, significant), method = 'loess', color = dd_palette("dd_dark_blue"), fill = dd_palette("dd_light_blue_alpha"), formula = 'y ~ x') +
	        geom_hline(yintercept = 0.8, color = dd_palette("dd_pink"), linetype = "dashed") +
		  geom_text(data = label_df, aes(label = label, x = x, y = y, color = color, hjust = hjust)) + 
		    scale_color_identity() + 
		      scale_y_continuous(breaks = seq(50, 150, 10)) +
		        theme_dd() + 
			  coord_cartesian(ylim = c(0, 1), xlim = c(50, 150)) +
			    labs(x = "Model parameter: true effect size",
				        y = "Diagnosand: statistical power")
###Notes: One of the ideas we had for identification was the exploit the fact that farmers may not sell to only one trader and maybe some would sell to WFP traders and normal traders. Unfortunately, most farmers report only a single transaction; only about 130 farmers report more than one transaction and it would be unlikely that all these farmers are selling to a WFP trader in at least one occasion. I ran a quick simulation to what the MDE for a price effect would look like and it seems we would only have reasonable power for effects that are larger than 16 percent, so I parked this idea for now...

#we only have 2 groups for traders but we can differentiate between indirect and spillover trader by checking if they sell to WFP - look at prices they get from WFP in seaon 1 and 2 
dta_t <- subset(dta_t, strata != "AMS")
# Prepare data for Season 23A
dta_t <- dta_t %>%
	  mutate(
	     share_women_23A = as.numeric(q78),
             share_men_23A = 100 - share_women_23A,
	     share_women_23B = as.numeric(q57),
	     share_men_23B = 100 - share_women_23B,
	     share_young_23A = as.numeric(q79),
	     share_old_23A = 100 - share_young_23A,
	     share_young_23B = as.numeric(q58),
	     share_old_23B = 100 - share_young_23B,
	     share_smallholder_23A = as.numeric(q80),
	     share_largeholder_23A = 100 - share_smallholder_23A,
	     share_smallholder_23B = as.numeric(q59),
	     share_largeholder_23B = 100 - share_smallholder_23B
			   )

# Prepare data for plotting Gender (Season 23A)
plot_data_gender_23A <- dta_t %>%
  select(strata, share_women_23A, share_men_23A) %>%
  pivot_longer(cols = -strata, names_to = "Group", values_to = "Share") %>%
  mutate(Group = ifelse(Group == "share_women_23A", "Women", "Men")) %>%
  group_by(strata, Group) %>%
  summarise(Share = mean(Share, na.rm = TRUE), .groups = "drop")

# Prepare data for plotting Gender (Season 23B)
plot_data_gender_23B <- dta_t %>%
  select(strata, share_women_23B, share_men_23B) %>%
  pivot_longer(cols = -strata, names_to = "Group", values_to = "Share") %>%
  mutate(Group = ifelse(Group == "share_women_23B", "Women", "Men")) %>%
  group_by(strata, Group) %>%
  summarise(Share = mean(Share, na.rm = TRUE), .groups = "drop")

# Prepare data for plotting Youth (Season 23A)
plot_data_youth_23A <- dta_t %>%
  select(strata, share_young_23A, share_old_23A) %>%
  pivot_longer(cols = -strata, names_to = "Group", values_to = "Share") %>%
  mutate(Group = ifelse(Group == "share_young_23A", "Youth", "Non-youth")) %>%
  group_by(strata, Group) %>%
  summarise(Share = mean(Share, na.rm = TRUE), .groups = "drop")

# Prepare data for plotting Youth (Season 23B)
plot_data_youth_23B <- dta_t %>%
  select(strata, share_young_23B, share_old_23B) %>%
  pivot_longer(cols = -strata, names_to = "Group", values_to = "Share") %>%
  mutate(Group = ifelse(Group == "share_young_23B", "Youth", "Non-youth")) %>%
  group_by(strata, Group) %>%
  summarise(Share = mean(Share, na.rm = TRUE), .groups = "drop")

# Prepare data for plotting Smallholders (Season 23A)
plot_data_smallholder_23A <- dta_t %>%
  select(strata, share_smallholder_23A, share_largeholder_23A) %>%
  pivot_longer(cols = -strata, names_to = "Group", values_to = "Share") %>%
  mutate(Group = ifelse(Group == "share_smallholder_23A", "Smallholder", "Non-smallholder")) %>%
  group_by(strata, Group) %>%
  summarise(Share = mean(Share, na.rm = TRUE), .groups = "drop")

# Prepare data for plotting Smallholders (Season 23B)
plot_data_smallholder_23B <- dta_t %>%
  select(strata, share_smallholder_23B, share_largeholder_23B) %>%
  pivot_longer(cols = -strata, names_to = "Group", values_to = "Share") %>%
  mutate(Group = ifelse(Group == "share_smallholder_23B", "Smallholder", "Non-smallholder")) %>%
  group_by(strata, Group) %>%
  summarise(Share = mean(Share, na.rm = TRUE), .groups = "drop")

# Create a function to generate plots
plot_function <- function(data, title) {
  ggplot(data, aes(x = Share, y = strata, fill = Group)) +
    geom_bar(stat = "identity", width = 0.6) +
    scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    labs(
      title = title,
      x = "Percentage",
      y = "Strata",
      fill = "Group"
    ) +
    scale_fill_manual(values = c("skyblue", "salmon")) +
    theme_minimal()
}

# Generate plots for Gender, Youth, and Smallholders
plot_gender_23A <- plot_function(plot_data_gender_23A, "Gender - Season 23A")
plot_gender_23B <- plot_function(plot_data_gender_23B, "Gender - Season 23B")

plot_youth_23A <- plot_function(plot_data_youth_23A, "Youth - Season 23A")
plot_youth_23B <- plot_function(plot_data_youth_23B, "Youth - Season 23B")

plot_smallholder_23A <- plot_function(plot_data_smallholder_23A, "Smallholder - Season 23A")
plot_smallholder_23B <- plot_function(plot_data_smallholder_23B, "Smallholder - Season 23B")

# Combine plots using patchwork
combined_plot <- plot_gender_23A / plot_youth_23A / plot_smallholder_23A
print(combined_plot)

ggsave("combined_plot.png", plot = combined_plot, width = 12, height = 8, dpi = 300)


dta_t$strata2 <- dta_t$strata
dta_t$strata2[dta_t$strata == "Conditional/Spillover" & (!is.na(dta_t$q154) | !is.na(dta_t$q180))] <- "Indirect"
dta_t$strata2[dta_t$strata == "Conditional/Spillover" & (is.na(dta_t$q154) & is.na(dta_t$q180))] <- "Spillover"


dta_f$nr_of_transactions_23_2 <- as.numeric(dta_f$q114)
table(dta_f$nr_of_transactions_23_2) ##only about 65 households recoded more than one transaction in first season of 2023, so the within regression is probably not worth exploring
dta_f$q108_1 <- dta_f$q108_1*dta_f$q203b
dta_f$q108_2 <- dta_f$q108_2*dta_f$q203b
dta_f$q108_3 <- dta_f$q108_3*dta_f$q203b
dta_f$q108_4 <- dta_f$q108_4*dta_f$q203b
dta_f$q108_5 <- dta_f$q108_5*dta_f$q203b
dta_f$q108_6 <- dta_f$q108_6*dta_f$q203b

### look at transactions - this is for first season of 2023
table(dta_f$nr_of_transactions_23_2) ##only about 65 households recoded more than one transaction in first season of 2023, so the within regression is probably not worth exploring
dta_f$q108_1 <- dta_f$q108_1*dta_f$q203b
dta_f$q108_2 <- dta_f$q108_2*dta_f$q203b
dta_f$q108_3 <- dta_f$q108_3*dta_f$q203b
dta_f$q108_4 <- dta_f$q108_4*dta_f$q203b
dta_f$q108_5 <- dta_f$q108_5*dta_f$q203b
dta_f$q108_6 <- dta_f$q108_6*dta_f$q203b

### look at transactions - this is for first season of 2023
### up to six separate transactions are recorded for each farmer
data_grph_1 <- data.frame(dta_f$farmer_id,dta_f$q107_1,dta_f$q108_1,dta_f$q109b_1,dta_f$strata)
names(data_grph_1) <- c("FarmerID","Month","VolumeSold","Price","strata")
data_grph_2 <- data.frame(dta_f$farmer_id,dta_f$q107_2,dta_f$q108_2,dta_f$q109b_2,dta_f$strata)
names(data_grph_2) <- c("FarmerID","Month","VolumeSold","Price","strata")
data_grph_3 <- data.frame(dta_f$farmer_id,dta_f$q107_3,dta_f$q108_3,dta_f$q109b_3,dta_f$strata)
names(data_grph_3) <- c("FarmerID","Month","VolumeSold","Price","strata")
data_grph_4 <- data.frame(dta_f$farmer_id,dta_f$q107_4,dta_f$q108_4,dta_f$q109b_4,dta_f$strata)
names(data_grph_4) <- c("FarmerID","Month","VolumeSold","Price","strata")
data_grph_5 <- data.frame(dta_f$farmer_id,dta_f$q107_5,dta_f$q108_5,dta_f$q109b_5,dta_f$strata)
names(data_grph_5) <- c("FarmerID","Month","VolumeSold","Price","strata")
data_grph_6 <- data.frame(dta_f$farmer_id,dta_f$q107_6,dta_f$q108_6,dta_f$q109b_6,dta_f$strata)
names(data_grph_6) <- c("FarmerID","Month","VolumeSold","Price","strata")
data_grph <- rbind(data_grph_1,data_grph_2,data_grph_3,data_grph_4,data_grph_5,data_grph_6)
#keep only transactions in from August to December
data_grph <- subset(data_grph, Month %in% c("Aug 2023","Sep 2023","Oct 2023","Nov 2023","Dec 2023"))

#data_grph <- subset(data_grph, Price <= 1750) ##based on boxplot
#drop transactions with volume sold greater than 100		    
#data_grph <- subset(data_grph, VolumeSold < 20000)

#dorp AMS strata
data_grph <- data_grph[data_grph$strata != "AMS",]
#max number of transaction by a farmer
max(table(data_grph$FarmerID))
#mean number of transaction by a farmer
mean(table(data_grph$FarmerID))
#share of farmers with only one transaction
table(table(data_grph$FarmerID) == 1)[2]/length(table(data_grph$FarmerID))
transactions_23A <- data_grph
# Summarize the data at the monthly level
monthly_data <- data_grph %>%
	  group_by(Month, strata) %>%
	    summarise(
		          Volume = mean(VolumeSold, na.rm=T),  # Total volume sold per month
			      Price = mean(Price, na.rm=T),  # Average price per month
			      .groups = "drop"
			        )

# Convert to Date format
monthly_data$Month <- as.Date(paste0("01-", monthly_data$Month), format="%d-%b %Y")

# Order by Date
monthly_data <- monthly_data[order(monthly_data$Month), ]
# Create the plot
ggplot(monthly_data, aes(x = Month, fill = strata)) +
  geom_bar(aes(y = Volume, group = strata), stat = "identity", position = "dodge", alpha = 0.7) +  # strataed bars for volume
  geom_line(aes(y = Price * max(Volume) / max(Price), color = strata, group = strata), size = 1) +  # Line for price
  geom_point(aes(y = Price * max(Volume) / max(Price), color = strata), size = 2) +  # Points on the line
  scale_y_continuous(
    name = "Volume Sold",
    sec.axis = sec_axis(~ . * max(monthly_data$Price) / max(monthly_data$Volume), name = "Price"),
  ) +  
  labs(title = "Monthly Sales Volume and Price Trends by Support Status - First season of 2023", x = "Month") +
  scale_fill_manual(values = brewer.pal(3, "Blues")) +  # Shades of blue for bar chart
  scale_color_manual(values = brewer.pal(3, "Blues")) +  # Shades of blue for line chart
  theme_minimal()

ggsave("first_seaon.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

data_grph_first_season_2023 <- data_grph
##now for second season of 2023

data_grph_1 <- data.frame(dta_f$farmer_id,dta_f$q115_1,dta_f$q116_1*dta_f$q203b,dta_f$q117b_1,dta_f$strata)
names(data_grph_1) <- c("FarmerID","Month","VolumeSold","Price","strata")
data_grph_2 <- data.frame(dta_f$farmer_id,dta_f$q115_2,dta_f$q116_2*dta_f$q203b,dta_f$q117b_2,dta_f$strata)
names(data_grph_2) <- c("FarmerID","Month","VolumeSold","Price","strata")
data_grph_3 <- data.frame(dta_f$farmer_id,dta_f$q115_3,dta_f$q116_3*dta_f$q203b,dta_f$q117b_3,dta_f$strata)
names(data_grph_3) <- c("FarmerID","Month","VolumeSold","Price","strata")
data_grph_4 <- data.frame(dta_f$farmer_id,dta_f$q115_4,dta_f$q116_4*dta_f$q203b,dta_f$q117b_4,dta_f$strata)
names(data_grph_4) <- c("FarmerID","Month","VolumeSold","Price","strata")

data_grph <- rbind(data_grph_1,data_grph_2,data_grph_3,data_grph_4)

data_grph <- subset(data_grph, Month %in% c("Jan 2024","Feb 2024","Mar 2024","Apr 2024","May 2024"))
		    

#dorp AMS strata
data_grph <- data_grph[data_grph$strata != "AMS",]
data_grph <- subset(data_grph, VolumeSold < 15000 ) ##based on boxplot
data_grph <- subset(data_grph, Price <= 1250) ##based on boxplot
#max number of transaction by a farmer
max(table(data_grph$FarmerID))
#mean number of transaction by a farmer
mean(table(data_grph$FarmerID))
#share of farmers with only one transaction
table(table(data_grph$FarmerID) == 1)[2]/length(table(data_grph$FarmerID))



# Summarize the data at the monthly level
monthly_data <- data_grph %>%
	  group_by(Month, strata) %>%
	    summarise(
		          Volume = mean(VolumeSold, na.rm=T),  # Total volume sold per month
			      Price = mean(Price, na.rm=T),  # Average price per month
			      .groups = "drop"
			        )
# Convert to Date format
monthly_data$Month <- as.Date(paste0("01-", monthly_data$Month), format="%d-%b %Y")

# Order by Date
monthly_data <- monthly_data[order(monthly_data$Month), ]

ggplot(monthly_data, aes(x = Month, fill = strata)) +
	  geom_bar(aes(y = Volume, group = strata), stat = "identity", position = "dodge", alpha = 0.7) +  # strataed bars for volume
	    geom_line(aes(y = Price * max(Volume) / max(Price), color = strata, group = strata), size = 1) +  # Line for price
	      geom_point(aes(y = Price * max(Volume) / max(Price), color = strata), size = 2) +  # Points on the line
	        scale_y_continuous(
				       name = "Volume Sold",
				           sec.axis = sec_axis(~ . * max(monthly_data$Price) / max(monthly_data$Volume), name = "Price"),
				         ) +  
  labs(title = "Monthly Sales Volume and Price Trends by Support Status - Second seaon of 2023", x = "Month") +
    scale_fill_manual(values = brewer.pal(3, "Blues")) +  # Shades of blue for bar chart
      scale_color_manual(values = brewer.pal(3, "Blues")) +  # Shades of blue for line chart
        theme_minimal()
ggsave("second_seaon.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

data_grph_second_season_2023 <- data_grph
### overall for first season of 2023
# Aggregate the total sales volume for each season and strata
# Aggregate the AVERAGE sales volume for each season and strata
overall_first_season <- data_grph_first_season_2023 %>%
	  group_by(strata) %>%
	    summarise(AvgVolume = mean(VolumeSold, na.rm = TRUE), .groups = "drop") %>%
	      mutate(Season = "Season 1")

      overall_second_season <- data_grph_second_season_2023 %>%
	        group_by(strata) %>%
		  summarise(AvgVolume = mean(VolumeSold, na.rm = TRUE), .groups = "drop") %>%
		    mutate(Season = "Season 2")

	    # Combine the data for both seasons
	    overall_seasons <- bind_rows(overall_first_season, overall_second_season)

	    # Reverse the order so that Season 1 appears on top
	    overall_seasons$Season <- factor(overall_seasons$Season, levels = c("Season 2", "Season 1"))

	    # Generate the horizontal stacked bar chart (using averages)
	    ggplot(overall_seasons, aes(y = Season, x = AvgVolume, fill = strata)) +
		      geom_bar(stat = "identity", position = "stack") +  # Stacked bars
		        scale_fill_brewer(palette = "Blues") +  # Use shades of blue
			  labs(title = "Average Sales Volume by Support Status - 2023",
			              x = "Average Volume Sold",
				             y = "Season") +
  theme_minimal()  # Clean theme
# Save the plot
ggsave("stacked_horizontal_bar_chart_reversed.png", width = 8, height = 2, dpi = 300)

### gender analysis
### merge in gender in transactions from first season of 2023
transactions_23A <- merge(transactions_23A,dta_f[c("farmer_id","q8")],by.x = "FarmerID", by.y = "farmer_id")

names(transactions_23A)[names(transactions_23A) == 'q8'] <- 'gender'
# Make sure your variables are in the right format
transactions_23A$strata <- as.factor(transactions_23A$strata)
transactions_23A$gender <- as.factor(transactions_23A$gender)

# Run the regressions
model1 <- lm(Price ~ strata, data = transactions_23A)
model2 <- lm(Price ~ gender, data = transactions_23A)
model3 <- lm(Price ~ strata * gender, data = transactions_23A)

# Generate the LaTeX table
stargazer(model1, model2, model3,
          type = "latex",
          title = "Regression Results: Price Analysis",
          label = "tab:regression_results",
          dep.var.labels = "Price",
          column.sep.width = "15pt",
          font.size = "small", out = "regression_results_gender.tex", float.env = "sidewaystable")


### price spread analysis - 3 groups and two seasons
data_grph_first_season_2023$season <- "Season 1"
data_grph_second_season_2023$season <- "Season 2"
all_transactions <- rbind(data_grph_first_season_2023,data_grph_second_season_2023)
all_transactions <- all_transactions %>%
	  mutate(strata = recode(strata, Spillover = "Conditional/Spillover", Indirect = "Conditional/Spillover"))

#calculate average price and volume by season and strata
avg_volume_price_by_season_strata <- all_transactions %>%
	  group_by(strata, season) %>%
	    summarise(AvgVolume = mean(VolumeSold, na.rm = TRUE), AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop")

season2 <- data.frame(cbind(dta_t$q154,dta_t$q157,dta_t$q155,dta_t$q158,dta_t$q64,dta_t$strata))
season1 <- data.frame(cbind(dta_t$q180,dta_t$q183,dta_t$q181,dta_t$q184,dta_t$q85,dta_t$strata))
season2$X1 <- as.numeric(season2$X1)
season1$X1 <- as.numeric(season1$X1)
season2$X2 <- as.numeric(season2$X2)
season1$X2 <- as.numeric(season1$X2)

season1$season <- "Season 1"
season2$season <- "Season 2"
all <- rbind(season1, season2)
names(all) <- c("Price_WFP","Price_non_WFP","Vol_WFP","Vol_non_WFP","price_farmer","strata","season")
all <- subset(all, strata != "AMS")

all$Price_WFP <- as.numeric(all$Price_WFP)
all$Price_non_WFP <- as.numeric(all$Price_non_WFP)
all$Vol_WFP <- as.numeric(all$Vol_WFP)
all$Vol_non_WFP <- as.numeric(all$Vol_non_WFP)
all$price_farmer <- as.numeric(all$price_farmer)

all$price_trader <- rowMeans(all[ , c("Price_WFP", "Price_non_WFP")],na.rm = TRUE)
all$volume <- rowMeans(all[ , c("Vol_WFP", "Vol_non_WFP")],na.rm = TRUE)

#calculate average price and volume by season and strata
avg_volume_price_by_season_strata_trader <- all %>%
	  group_by(strata, season) %>%
	    summarise(AvgVolume = mean(volume, na.rm = TRUE), Price = mean(price_trader, na.rm = TRUE), .groups = "drop")

names(avg_volume_price_by_season_strata_trader) <- c("strata","season","AvgVolume","AvgPrice")
all_merged <- merge(avg_volume_price_by_season_strata,avg_volume_price_by_season_strata_trader, by = c("strata","season"))

names(all_merged) <- c("strata","season","AvgVolume_Farmer","AvgPrice_Farmer","AvgVolume_Trader","AvgPrice_Trader")

# Define shades of blue for the farmer groups (similar to the uploaded image)
custom_colors <- c("Control" = "red",  # Light blue
				                         "Conditional/Spillover" = "green") # Dark blue

# Create Price Spread Analysis Plot with shades
#This graph compares the prices farmers receive at the farm gate vs. the prices traders sell at in markets. If traders always paid farmers fairly, all points would fall on the parity line (where farm gate price = trader price). Any deviations show the trader markup or inefficiencies.

ggplot(all_merged, aes(x = AvgPrice_Farmer, y = AvgPrice_Trader,, 
			                              color = strata, shape = season)) +
  geom_point(alpha = 0.85, size = 5) +  # Scatter plot with transparency
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1) +  # Parity Line (1:1)
      scale_color_manual(values = custom_colors) +  # Apply custom shades of blue
      #  scale_size_continuous(range = c(1, 3)) +
     xlim(600,850)+ylim(500,1000)+	# Scale for bubble sizes
	  labs(
		       x = "Farm Gate Price (UGX/kg)",
		           y = "Trader Sale Price (UGX/kg)",
		           color = "Farmer Group",
			       shape = "Season"
			         ) +
  theme_minimal()  # Clean theme
ggsave("price_spread.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

#this is an alternative plot based on the trader reported data
ggplot(all, aes(x = price_farmer, y = price_trader, color = strata)) +
  geom_point(alpha = 0.2, size = 2, position = position_jitter(width = 5, height = 5)) +  # Smaller points & jitter
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1) +  # Parity Line (1:1)
  geom_density_2d(aes(color = strata), size = 0.8, bins = 5) +  # Add contour lines with fewer bins
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  coord_cartesian(xlim=c(400, 1000) , ylim=c(500, 1100)) +  # Define axis limits
  labs(
    x = "Farm Gate Price (UGX/kg)",
    y = "Trader Sale Price (UGX/kg)",
    color = "Farmer Group"
  ) +
  theme_minimal()
ggsave("price_spread_alt.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
###########################sankey graphs########################################
library(networkD3)
##now for traders
   #create a wide dataset  - this is first harvest of 2023
  traders_23A <-  data.frame(cbind(dta_t$trader_id,dta_t$strata,dta_t$q162,dta_t$q164a,dta_t$q167, dta_t$q170,dta_t$q173,dta_t$q176))
  names(traders_23A) <- c("farmer_id","strata","broker","non_WFP_wholesaler","WFP_wholesaler","processor","govt","WFP") 
  traders_23A$non_WFP_wholesaler <- as.numeric(traders_23A$non_WFP_wholesaler)
  traders_23A$WFP_wholesaler <- as.numeric(traders_23A$WFP_wholesaler)
  traders_23A$processor <- as.numeric(traders_23A$processor)
  traders_23A$broker <- as.numeric(traders_23A$broker)
  traders_23A$govt <- as.numeric(traders_23A$govt)
  traders_23A$WFP <- as.numeric(traders_23A$WFP)
#  traders_23A$other <- as.numeric(traders_23A$other)

traders_23A[is.na(traders_23A)] <- 0
traders_23A$other_buyer <- 100 - rowSums(traders_23A[,c("broker","non_WFP_wholesaler","WFP_wholesaler","processor","govt","WFP")])
##drop AMS farmers
traders_23A <- traders_23A[traders_23A$strata != "AMS",]

#reshpate from wide to long
traders_23A <- melt(traders_23A, id.vars = c("farmer_id","strata"), variable.name = "Who", value.name = "VolumeSold")
#average trader sales by Who and strata
summary_dta_traders_23A <- traders_23A %>%
	  group_by(Who,strata) %>%
	    summarise(Mean_Value = mean(VolumeSold, na.rm = TRUE), .groups = "drop")
#create a wide dataset - this is for the second harvest of 2023
traders_23B <- data.frame(cbind(dta_t$trader_id,dta_t$strata,dta_t$q134,dta_t$q138,dta_t$q141, dta_t$q144,dta_t$q147,dta_t$q150))
  names(traders_23B) <- c("farmer_id","strata","broker","non_WFP_wholesaler","WFP_wholesaler","processor","govt","WFP") 
  traders_23B$non_WFP_wholesaler <- as.numeric(traders_23B$non_WFP_wholesaler)
  traders_23B$WFP_wholesaler <- as.numeric(traders_23B$WFP_wholesaler)
  traders_23B$processor <- as.numeric(traders_23B$processor)
  traders_23B$broker <- as.numeric(traders_23B$broker)
  traders_23B$govt <- as.numeric(traders_23B$govt)
  traders_23B$WFP <- as.numeric(traders_23B$WFP)
#  traders_23B$other <- as.numeric(traders_23B$other)

traders_23B[is.na(traders_23B)] <- 0
traders_23B$other_buyer <- 100 - rowSums(traders_23B[,c("broker","non_WFP_wholesaler","WFP_wholesaler","processor","govt","WFP")])
##drop AMS farmers
traders_23B <- traders_23B[traders_23B$strata != "AMS",]

#reshpate from wide to long
traders_23B <- melt(traders_23B, id.vars = c("farmer_id","strata"), variable.name = "Who", value.name = "VolumeSold")
#average trader sales by Who and strata
summary_dta_traders_23B <- traders_23B %>%
	  group_by(Who,strata) %>%
	    summarise(Mean_Value = mean(VolumeSold, na.rm = TRUE), .groups = "drop")

# express trader folows in kg
#summary_dta_traders$Mean_Value[summary_dta_traders$strata == "Control"] <- summary_dta_traders$Mean_Value[summary_dta_traders$strata == "Control"]/100*summary_dta_farmers$Mean_Value[summary_dta_farmers$strata == "Control" & summary_dta_farmers$Who == "Trader"]

#summary_dta_traders$Mean_Value[summary_dta_traders$strata == "Conditional/Spillover"] <- summary_dta_traders$Mean_Value[summary_dta_traders$strata == "Conditional/Spillover"]/100*summary_dta_farmers$Mean_Value[summary_dta_farmers$strata == "Conditional/Spillover" & summary_dta_farmers$Who == "Trader"]

#links_f <- data.frame(rep(0,dim(summary_dta_farmers)[1]),as.factor(summary_dta_farmers$Who), summary_dta_farmers$Mean_Value,summary_dta_farmers$strata)
#names(links_f) <- c("source","target","value","group")
#links_t <- data.frame(rep(2,dim(summary_dta_traders)[1]),as.factor(summary_dta_traders$Who), summary_dta_traders$Mean_Value,summary_dta_traders$strata)
#names(links_t) <- c("source","target","value","group")
#links <- rbind(links_f,links_t)
#links$target <- fct_recode(links$target,"processor" =  "Procressor")

#agents <- data.frame(c("farmer",levels(links$target)))
#names(agents) <- "name"


#links$target <- as.numeric(links$target)
#sank <- list(agents, links)
#names(sank) <- c("nodes","links")

#sankeyNetwork(Links = sank$links, Nodes = sank$nodes, Source = "source",LinkGroup="group",
#	                   Target = "target", Value = "value", NodeID = "name",
#			                units = "Litres ", fontSize = 24, nodeWidth = 30)

### seperate graphs
#sank <- list(agents, links[links$group=="Control",])
#names(sank) <- c("nodes","links")
#C_net <- sankeyNetwork(Links = sank$links, Nodes = sank$nodes, Source = "source",
#		                    Target = "target", Value = "value", NodeID = "name",
#				                  units = "kg", fontSize = 24, nodeWidth = 30)


#sank <- list(agents, links[links$group=="Conditional/Spillover",])
#names(sank) <- c("nodes","links")
#I_net <- sankeyNetwork(Links = sank$links, Nodes = sank$nodes, Source = "source",
#			             Target = "target", Value = "value", NodeID = "name",
#				                   units = "kg", fontSize = 24, nodeWidth = 30)

#saveNetwork(C_net,paste(dir, "C_flow.html", sep = "/"), selfcontained = TRUE)
#saveNetwork(SW_net, paste(dir, "I_flow.html",sep = "?"), selfcontained = TRUE)

### sankey only for traders
###we can still use traders for the sales data - do something similar for purchases

   #create a wide dataset - this is for the second season of 2023
  traders_buy_23B <-  data.frame(cbind(dta_t$trader_id,dta_t$strata,dta_t$q47,dta_t$q50,dta_t$q53, dta_t$q56))
  names(traders_buy_23B) <- c("farmer_id","strata","coop","market","trader_up","farmer") 
  traders_buy_23B$coop <- as.numeric(traders_buy_23B$coop)
  traders_buy_23B$market <- as.numeric(traders_buy_23B$market)
  traders_buy_23B$trader_up <- as.numeric(traders_buy_23B$trader_up)
  traders_buy_23B$farmer <- as.numeric(traders_buy_23B$farmer)

traders_buy_23B[is.na(traders_buy_23B)] <- 0
traders_buy_23B$other_seller <- 100 - rowSums(traders_buy_23B[,c("coop","market","trader_up","farmer")])

##drop AMS farmers
traders_buy_23B <- traders_buy_23B[traders_buy_23B$strata != "AMS",]

#reshpate from wide to long
traders_buy_23B <- melt(traders_buy_23B, id.vars = c("farmer_id","strata"), variable.name = "Who", value.name = "share_bought")
#
#average trader sales by Who and strata
summary_dta_traders_buy_23B <- traders_buy_23B %>%
	  group_by(Who,strata) %>%
	    summarise(Mean_Value = mean(share_bought, na.rm = TRUE), .groups = "drop")
summary_dta_traders_buy_23B <- summary_dta_traders_buy_23B %>%
	  bind_rows(
		        tail(summary_dta_traders_buy_23B, 2) %>%
				      mutate(Who = c("trader", "trader"),  # Replace with desired values
					                  Mean_Value = 0)  # Set Mean_Value to 0
			  )
   #create a wide dataset - this is for the first season of 2023
   traders_buy_23A <-  data.frame(cbind(dta_t$trader_id,dta_t$strata,dta_t$q68,dta_t$q71,dta_t$q74, dta_t$q77))
  names(traders_buy_23A) <- c("farmer_id","strata","coop","market","trader_up","farmer") 
  traders_buy_23A$coop <- as.numeric(traders_buy_23A$coop)
  traders_buy_23A$market <- as.numeric(traders_buy_23A$market)
  traders_buy_23A$trader_up <- as.numeric(traders_buy_23A$trader_up)
  traders_buy_23A$farmer <- as.numeric(traders_buy_23A$farmer)
traders_buy_23A[is.na(traders_buy_23A)] <- 0
traders_buy_23A$other_seller <- 100 - rowSums(traders_buy_23A[,c("coop","market","trader_up","farmer")])

##drop AMS farmers
traders_buy_23A <- traders_buy_23A[traders_buy_23A$strata != "AMS",]

#reshpate from wide to long
traders_buy_23A <- melt(traders_buy_23A, id.vars = c("farmer_id","strata"), variable.name = "Who", value.name = "share_bought")
#
#average trader sales by Who and strata
summary_dta_traders_buy_23A <- traders_buy_23A %>%
	  group_by(Who,strata) %>%
	    summarise(Mean_Value = mean(share_bought, na.rm = TRUE), .groups = "drop")
summary_dta_traders_buy_23A <- summary_dta_traders_buy_23A %>%
	  bind_rows(
		        tail(summary_dta_traders_buy_23A, 2) %>%
				      mutate(Who = c("trader", "trader"),  # Replace with desired values
					                  Mean_Value = 0)  # Set Mean_Value to 0
			  )


#average trader sales by Who and strata

summary_dta_traders_23A <- summary_dta_traders_23A %>%
	  mutate(Who = fct_recode(Who, trader_down = "broker"))
summary_dta_traders_23B <- summary_dta_traders_23B %>%
	  mutate(Who = fct_recode(Who, trader_down = "broker"))


summary_dta_traders_buy_23B$actor <- "farmer"
summary_dta_traders_buy_23A$actor <- "farmer"
summary_dta_traders_23A$actor <- "trader"
summary_dta_traders_23B$actor <- "trader"

summary_dta_traders_buy_23B$season <- "23B"
summary_dta_traders_buy_23A$season <- "23A"
summary_dta_traders_23A$season <- "23A"
summary_dta_traders_23B$season <- "23B"


all_g <- rbind(summary_dta_traders_buy_23A,summary_dta_traders_23A,summary_dta_traders_buy_23B,summary_dta_traders_23B)
all_g$Who <- as.factor(all_g$Who)

agents <- data.frame(c(levels(all_g$Who)))
all_g$Who_num <- as.numeric(all_g$Who)
ref <- 9
all_g$source <- ifelse(all_g$actor == "farmer",all_g$Who_num,ref)
all_g$target <- ifelse(all_g$actor == "trader",all_g$Who_num,ref)
all_g$Mean_Value[all_g$Mean_Value < 0] <- 0.01
links <- all_g[,c("source", "target", "Mean_Value", "strata", "season")]
links$Mean_Value[links$season == "23A" & links$strata == "Control" & links$target == 12] <- 0.01
# Function to normalize Mean_Value by season
normalize_by_season <- function(df, ref) {
	  df$Mean_Value[df$strata == "Control" & df$target == ref] <- 
		      df$Mean_Value[df$strata == "Control" & df$target == ref] / 
		          ave(df$Mean_Value[df$strata == "Control" & df$target == ref], df$season[df$strata == "Control" & df$target == ref], FUN = sum) * 100

		    df$Mean_Value[df$strata == "Conditional/Spillover" & df$target == ref] <- 
			        df$Mean_Value[df$strata == "Conditional/Spillover" & df$target == ref] / 
				    ave(df$Mean_Value[df$strata == "Conditional/Spillover" & df$target == ref], df$season[df$strata == "Conditional/Spillover" & df$target == ref], FUN = sum) * 100

			      df$Mean_Value[df$strata == "Control" & df$source == ref] <- 
				          df$Mean_Value[df$strata == "Control" & df$source == ref] / 
					      ave(df$Mean_Value[df$strata == "Control" & df$source == ref], df$season[df$strata == "Control" & df$source == ref], FUN = sum) * 100

				        df$Mean_Value[df$strata == "Conditional/Spillover" & df$source == ref] <- 
						    df$Mean_Value[df$strata == "Conditional/Spillover" & df$source == ref] / 
						        ave(df$Mean_Value[df$strata == "Conditional/Spillover" & df$source == ref], df$season[df$strata == "Conditional/Spillover" & df$source == ref], FUN = sum) * 100

						  return(df)
}

# Apply function
links <- normalize_by_season(links, ref)

# Rename columns
names(links) <- c("source", "target", "value", "group", "season")
names(agents) <- "name"


links$target <- as.numeric(links$target) - 1
links$source <- as.numeric(links$source) - 1

#links$value[links$group == "Control" & links$season == "23A" & links$target == 10 & links$source == 7] <- .01

# Function to create Sankey graph object
create_sankey <- function(df, season_filter, group_filter) {
  sank <- list(agents, df[df$season == season_filter & df$group == group_filter,])
  names(sank) <- c("nodes","links")
  
  sankeyNetwork(Links = sank$links, Nodes = sank$nodes, 
                Source = "source", Target = "target", 
                Value = "value", NodeID = "name",
                units = "kg", fontSize = 24, nodeWidth = 30)
}

# Generate objects for each season and group combination
C_net_23A <- create_sankey(links, "23A", "Control")
I_net_23A <- create_sankey(links, "23A", "Conditional/Spillover")
C_net_23B <- create_sankey(links, "23B", "Control")
I_net_23B <- create_sankey(links, "23B", "Conditional/Spillover")
#seasons are very similar

library(htmltools)
# Display both plots for Season 23A in RStudio Viewer
html_page_23A <- tagList(
  tags$h2("Control - Season 23A"), C_net_23A,
  tags$h2("Conditional/Spillover - Season 23A"), I_net_23A
)

# Render the HTML in RStudio Viewer
html_print(html_page_23A)
save_html(html_page_23A, "sankey_season_23A.html")
### now do sankey graphs using the farmer level data
### look at transactions - this is for first season of 2023
### up to six separate transactions are recorded for each farmer
data_grph_1 <- data.frame(dta_f$farmer_id,dta_f$q107_1,dta_f$q108_1,dta_f$q111_1,dta_f$q112_1,dta_f$strata)
names(data_grph_1) <- c("FarmerID","Month","VolumeSold","Who","WFP","strata")
data_grph_2 <- data.frame(dta_f$farmer_id,dta_f$q107_2,dta_f$q108_2,dta_f$q111_2,dta_f$q112_2,dta_f$strata)
names(data_grph_2) <- c("FarmerID","Month","VolumeSold","Who","WFP","strata")
data_grph_3 <- data.frame(dta_f$farmer_id,dta_f$q107_3,dta_f$q108_3,dta_f$q111_3,dta_f$q112_3,dta_f$strata)
names(data_grph_3) <- c("FarmerID","Month","VolumeSold","Who","WFP","strata")
data_grph_4 <- data.frame(dta_f$farmer_id,dta_f$q107_4,dta_f$q108_4,dta_f$q111_4,dta_f$q112_4,dta_f$strata)
names(data_grph_4) <- c("FarmerID","Month","VolumeSold","Who","WFP","strata")
data_grph_5 <- data.frame(dta_f$farmer_id,dta_f$q107_5,dta_f$q108_5,dta_f$q111_5,dta_f$q112_5,dta_f$strata)
names(data_grph_5) <- c("FarmerID","Month","VolumeSold","Who","WFP","strata")
data_grph_6 <- data.frame(dta_f$farmer_id,dta_f$q107_6,dta_f$q108_6,dta_f$q111_6,dta_f$q112_6,dta_f$strata)
names(data_grph_6) <- c("FarmerID","Month","VolumeSold","Who","WFP","strata")
data_grph <- rbind(data_grph_1,data_grph_2,data_grph_3,data_grph_4,data_grph_5,data_grph_6)
#keep only transactions in from August to December
data_grph <- subset(data_grph, Month %in% c("Aug 2023","Sep 2023","Oct 2023","Nov 2023","Dec 2023"))

data_grph <- subset(data_grph, VolumeSold <= 40000)
#dorp AMS 
data_grph <- data_grph[data_grph$strata != "AMS",]
#Average sales by Who and strata with counts
summary_dta_farmers <- data_grph %>%
	  group_by(Who, strata) %>%

# Get all unique combinations of Who and strata in your data
all_combinations <- expand.grid(
				  Who = unique(data_grph$Who),
				    strata = unique(data_grph$strata)
				  )

# Merge with your summary table
summary_dta_farmers_complete <- all_combinations %>%
	  left_join(summary_dta_farmers, by = c("Who", "strata")) %>%
	    mutate(
		       Mean_Value = ifelse(is.na(Mean_Value), 0.01, Mean_Value),  # Replace NA with 0.01
		           Count = ifelse(is.na(Count), 0, Count)  # Replace NA with 0 (no transactions)
		         )

# Calculate total Volume by strata
total_by_strata <- summary_dta_farmers_complete %>%
	  group_by(strata) %>%
	    summarise(Total_Value = sum(Mean_Value))

    # Join the totals back to the main table
    summary_dta_farmers_complete <- summary_dta_farmers_complete %>%
	      left_join(total_by_strata, by = "strata") %>%
	        mutate(Percentage = (Mean_Value / Total_Value) * 100) %>%
		  select(Who, strata, Mean_Value, Percentage, Count)  # Include Count in final table

# View the complete table with percentages and counts
print(summary_dta_farmers_complete)
# Create Sankey Graph - however, there are virtually no transactions apart from transactions with traders
# so the Sankey graph will not be very informative
agents <- data.frame(c("Farmer",levels(as.factor(summary_dta_farmers_complete$Who))))
summary_dta_farmers_complete$source <- 0
summary_dta_farmers_complete$target <- as.numeric(as.factor(summary_dta_farmers_complete$Who))
links <- summary_dta_farmers_complete[,c("source", "target", "Percentage", "strata")]
## Rename columns
names(links) <- c("source", "target", "value", "group")
names(agents) <- "name"

# Function to create Sankey graph object
create_sankey <- function(df, group_filter) {
  sank <- list(agents, df[df$group == group_filter,])
  names(sank) <- c("nodes","links")
  
  sankeyNetwork(Links = sank$links, Nodes = sank$nodes, 
                Source = "source", Target = "target", 
                Value = "value", NodeID = "name",
                units = "kg", fontSize = 24, nodeWidth = 30)
}

# Generate objects for each season and group combination
C_net_23A <- create_sankey(links, "Control")
I_net_23A <- create_sankey(links, "Indirect")
S_net_A23 <- create_sankey(links, "Spillover")

tapply(data_grph$WFP == "Yes", data_grph$strata, mean)*100
tapply(data_grph$VolumeSold, data_grph$strata, mean)
