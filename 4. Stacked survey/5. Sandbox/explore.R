# This script is use to run the main analysis for the WFP project
# It is called by the lyx file as well
#rm(list = ls())
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
### one of the key identification strategies was to exploit within farmer differences with respect to who they sell to, so we start by looking if we have sufficient farmers that make more than one transaction (in the first season of 2023

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
dta_f <- subset(dta_f, strata != "AMS")

dta_f$nr_of_transactions_23_1 <- as.numeric(dta_f$q106)
### indicator of market participation in 23A
table(dta_f$nr_of_transactions_23_1)
dta_f$market_participation_23A <- 0
dta_f$market_participation_23A[dta_f$nr_of_transactions_23_1 > 0] <- 1
dta_f$market_participation_23A[dta_f$q35h!="Yes"] <- NA

dta_f$nr_of_transactions_23_2 <- as.numeric(dta_f$q114)
table(dta_f$nr_of_transactions_23_2) ##only about 65 households recoded more than one transaction in first season of 2023, so the within regression is probably not worth exploring
dta_f$market_participation_23B <- 0
dta_f$market_participation_23B[dta_f$nr_of_transactions_23_2 > 0] <- 1
dta_f$market_participation_23B[dta_f$q59!="Yes"] <- NA

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

ggsave("combined_plot.png", plot = combined_plot, width = 12, height = 8, dpi = 300)


dta_t$strata2 <- dta_t$strata
dta_t$strata2[dta_t$strata == "Conditional/Spillover" & (!is.na(dta_t$q154) | !is.na(dta_t$q180))] <- "Indirect"
dta_t$strata2[dta_t$strata == "Conditional/Spillover" & (is.na(dta_t$q154) & is.na(dta_t$q180))] <- "Spillover"

### production at farmer level
dta_f$acres_23A <- rowSums(cbind(dta_f$q38_1,dta_f$q38_2,dta_f$q38_3,dta_f$q38_4),na.rm=TRUE)
dta_f$production_23A <- rowSums(cbind(as.numeric(dta_f$q39_1),dta_f$q39_2,dta_f$q39_3,dta_f$q39_4),na.rm=TRUE)*as.numeric(dta_f$q203b)
dta_f$acres_23A[dta_f$q35h!="Yes"]<- NA
dta_f$production_23A[dta_f$q35h!="Yes"]<- NA
dta_f$production_23A[dta_f$production_23A>50000] <- NA
dta_f$acres_23A[dta_f$acres_23A > 40 ]<- NA
dta_f$yield_23A <- dta_f$production_23A/dta_f$acres_23A
dta_f$yield_23A[dta_f$yield_23A > 3500] <- NA

dotplot1 <- data.frame(cbind(tapply(dta_f$acres_23A,dta_f$strata,mean,na.rm=TRUE),tapply((dta_f$q35h=="Yes"),dta_f$strata,mean, na.rm=TRUE),tapply(dta_f$production_23A,dta_f$strata,mean, na.rm=TRUE), tapply(dta_f$yield_23A,dta_f$strata,mean, na.rm=TRUE)))
dotplot1$strata <- rownames(dotplot1)
names(dotplot1) <- c("plotsize","share","production","yield","strata")
dotplot1$season <- "23A"

dta_f$q63_2[dta_f$q63_2 > 100] <- NA
dta_f$acres_23B <- rowSums(cbind(dta_f$q62_1,dta_f$q62_2,dta_f$q62_3,dta_f$q62_4,dta_f$q62_5),na.rm=TRUE)
dta_f$production_23B <- rowSums(cbind(as.numeric(dta_f$q63_1),dta_f$q63_2,dta_f$q63_3,dta_f$q63_4,dta_f$q63_5),na.rm=TRUE)*as.numeric(dta_f$q203b)
dta_f$acres_23B[dta_f$q59!="Yes"] <- NA
dta_f$production_23B[dta_f$q59!="Yes"] <- NA
dta_f$production_23B[dta_f$production_23B>50000] <- NA
dta_f$acres_23B[dta_f$acres_23B > 40 ]<- NA
dta_f$yield_23B <- dta_f$production_23B/dta_f$acres_23B
dta_f$yield_23B[dta_f$yield_23B > 3500] <- NA
dotplot2 <- data.frame(cbind(tapply(dta_f$acres_23B,dta_f$strata,mean,na.rm=TRUE),tapply((dta_f$q59=="Yes"),dta_f$strata,mean, na.rm=TRUE), tapply(dta_f$production_23B,dta_f$strata,mean, na.rm=TRUE),tapply(dta_f$yield_23B,dta_f$strata,mean, na.rm=TRUE)))
dotplot2$strata <- rownames(dotplot2)
names(dotplot2) <- c("plotsize","share","production","yield","strata")
dotplot2$season <- "23B"


dta_f$acres_24A <- rowSums(cbind(as.numeric(dta_f$q85_1),as.numeric(dta_f$q85_2),as.numeric(dta_f$q85_3),as.numeric(dta_f$q85_4)),na.rm=TRUE)
dta_f$production_24A <- rowSums(cbind(as.numeric(dta_f$q86_1),as.numeric(dta_f$q86_2),as.numeric(dta_f$q86_3),as.numeric(dta_f$q86_4)),na.rm=TRUE)*as.numeric(dta_f$q203b)
dta_f$acres_24A[dta_f$q82!="Yes"] <- NA
dta_f$production_24A[dta_f$q82!="Yes"] <- NA
dta_f$production_24A[dta_f$production_24A>50000] <- NA
dta_f$acres_24A[dta_f$acres_24A > 40 ]<- NA
dta_f$yield_24A <- dta_f$production_24A/dta_f$acres_24A
dta_f$yield_24A[dta_f$yield_24A > 3500] <- NA
dotplot3 <- data.frame(cbind(tapply(dta_f$acres_24A,dta_f$strata,mean, na.rm=TRUE),tapply((dta_f$q82=="Yes"),dta_f$strata,mean, na.rm=TRUE),tapply(dta_f$production_24A,dta_f$strata,mean, na.rm=TRUE),tapply(dta_f$yield_24A,dta_f$strata,mean, na.rm=TRUE)))
dotplot3$strata <- rownames(dotplot3)
names(dotplot3) <- c("plotsize","share","production","yield","strata")
dotplot3$season <- "24A"



alldot <- rbind(dotplot1,dotplot2,dotplot3)


### cleveand dot plot


# Creating a Cleveland dot plot with Season on the y-axis and color-coded Groups
p <- ggplot(alldot, aes(x = share, y = reorder(season, -share))) +
	  geom_point(aes(color = strata, size = plotsize), show.legend = TRUE) +
	    scale_color_manual(values = c("Control" = "red", "Spillover" = "blue", "Indirect" = "green")) +
	     scale_size(range = c(3,12))+ labs(x = "Share of Farmers Selling (%)", y = "Season") +
  theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
	          panel.grid.minor.x = element_blank(),
		          legend.position = "right")

  # Print the plot
  #print(p)

ggsave("cleveland.png", plot = p, width = 12, height = 4, dpi = 300)

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
dta_f$q116_1 <- as.numeric(dta_f$q116_1)
dta_f$q117b_1 <- as.numeric(dta_f$q117b_1)

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
#### volumes sold by farmer
sold_first_season <-aggregate(VolumeSold ~ FarmerID, data = data_grph_first_season_2023, FUN = sum, na.rm = TRUE)
sold_second_season <-aggregate(VolumeSold ~ FarmerID, data = data_grph_second_season_2023, FUN = sum, na.rm = TRUE)
names(sold_first_season)[names(sold_first_season) == 'VolumeSold'] <- 'VolumeSold_23A'
names(sold_second_season)[names(sold_second_season) == 'VolumeSold'] <- 'VolumeSold_23B'

### merge into dta_f
dta_f <- merge(dta_f,sold_first_season, by.x = "farmer_id",by.y="FarmerID", all.x = TRUE)
dta_f <- merge(dta_f,sold_second_season, by.x = "farmer_id",by.y="FarmerID", all.x = TRUE)
dta_f$VolumeSold_23A[dta_f$market_participation_23A == 0] <- 0
dta_f$VolumeSold_23B[dta_f$market_participation_23B == 0] <- 0
dta_f$VolumeSold_23A[dta_f$VolumeSold_23A > 50000] <- NA
dta_f$VolumeSold_23B[dta_f$VolumeSold_23B > 20000] <- NA

#### express as percentage of production
dta_f$share_sold_23A <- dta_f$VolumeSold_23A/dta_f$production_23A*100
dta_f$share_sold_23B <- dta_f$VolumeSold_23B/dta_f$production_23B*100
dta_f$share_sold_23A[dta_f$share_sold_23A>100] <- 100
dta_f$share_sold_23B[dta_f$share_sold_23B>100] <- 100


### delta production
dta_f$delta_production_1 <- dta_f$production_23B - dta_f$production_23A
dta_f$delta_production_2 <- dta_f$production_24A - dta_f$production_23B
#delta area
dta_f$delta_area_1 <- dta_f$acres_23B - dta_f$acres_23A
dta_f$delta_area_2 <- dta_f$acres_24A - dta_f$acres_23B
# Create summary dataframe with means per strata
dta_summary <- dta_f %>%
  group_by(strata) %>%
  summarise(
    mean_area_1 = mean(delta_area_1, na.rm = TRUE),
    mean_production_1 = mean(delta_production_1, na.rm = TRUE), 
    mean_area_2 = mean(delta_area_2, na.rm = TRUE),
    mean_production_2 = mean(delta_production_2, na.rm = TRUE)  
  )

# Plot only the group averages
ggplot(dta_summary, aes(x = mean_area_1, y = mean_production_1, color = strata)) +
  geom_point(shape = 18, size = 4, stroke = 1.5) +  # Only average points
  labs(x = "change in plot size", y = "change in production", title = "Intensive/extensive production change") +
  coord_cartesian(xlim = c(-0.5, 0.5), ylim = c(-300, 300)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  annotate("text", x = 0.25, y = 270, label = "Extensive production increase", size = 4, hjust = 0.5) +
  annotate("text", x = -0.25, y =270, label = "Intensive production increase", size = 4, hjust = 0.5) +
  annotate("text", x = 0.25, y = -270, label = "Intensive production decrease", size = 4, hjust = 0.5) +
  annotate("text", x = -0.25, y = -270, label = "Extensive production decrease", size = 4, hjust = 0.5) +
  theme_minimal()

ggsave("prod_change_1.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
# Plot only the group averages
ggplot(dta_summary, aes(x = mean_area_2, y = mean_production_2, color = strata)) +
  geom_point(shape = 18, size = 4, stroke = 1.5) +  # Only average points
  labs(x = "change in plot size", y = "change in production", title = "Intensive/extensive production change") +
  coord_cartesian(xlim = c(-0.5, 0.5), ylim = c(-3000, 3000)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  annotate("text", x = 0.25, y = 270, label = "Extensive production increase", size = 4, hjust = 0.5) +
  annotate("text", x = -0.25, y =270, label = "Intensive production increase", size = 4, hjust = 0.5) +
  annotate("text", x = 0.25, y = -270, label = "Intensive production decrease", size = 4, hjust = 0.5) +
  annotate("text", x = -0.25, y = -270, label = "Extensive production decrease", size = 4, hjust = 0.5) +
  theme_minimal()

ggsave("prod_change_2.png", plot = last_plot(), width = 8, height = 6, dpi = 300)



# Reshape to long format (manually match names to avoid confusion)
dta_summary_long <- dta_summary %>%
  select(strata, 
         area_1 = mean_area_1, production_1 = mean_production_1,
         area_2 = mean_area_2, production_2 = mean_production_2) %>%
  pivot_longer(
    cols = -strata,
    names_to = c("metric", "period"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = metric,
    values_from = value
  )

# Plot
ggplot(dta_summary_long, aes(x = area, y = production, color = strata, shape = period)) +
  geom_point(size = 4, stroke = 1.5) +
  labs(x = "Change in plot size", y = "Change in production", title = "Intensive/extensive production change") +
  coord_cartesian(xlim = c(-0.5, 0.5), ylim = c(-2000, 2000)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  annotate("text", x = 0.25, y = 270, label = "Extensive production increase", size = 4, hjust = 0.5) +
  annotate("text", x = -0.25, y = 270, label = "Intensive production increase", size = 4, hjust = 0.5) +
  annotate("text", x = 0.25, y = -270, label = "Intensive production decrease", size = 4, hjust = 0.5) +
  annotate("text", x = -0.25, y = -270, label = "Extensive production decrease", size = 4, hjust = 0.5) +
  scale_shape_manual(values = c("1" = 17, "2" = 19), labels = c("23A–23B", "23B–24A")) +
  theme_minimal()
ggsave("prod_change_both.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
### gender analysis
### merge in gender in transactions from first season of 2023
transactions_23A <- merge(transactions_23A,dta_f[c("farmer_id","q8","q10","q35b")],by.x = "FarmerID", by.y = "farmer_id")

names(transactions_23A)[names(transactions_23A) == 'q8'] <- 'gender'
names(transactions_23A)[names(transactions_23A) == 'q10'] <- 'age'
names(transactions_23A)[names(transactions_23A) == 'q35b'] <- 'area'
# Make sure your variables are in the right format
transactions_23A$strata <- as.factor(transactions_23A$strata)
transactions_23A$gender <- as.factor(transactions_23A$gender)
transactions_23A$youth <- as.numeric(transactions_23A$age)<35
transactions_23A$small <- as.numeric(transactions_23A$area)<2.5

# Run the regressions
model1 <- lm(Price ~ strata, data = transactions_23A)
model2 <- lm(Price ~ gender, data = transactions_23A)



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

season2 <- data.frame(cbind(dta_t$trader_id, dta_t$q154,dta_t$q157,dta_t$q155,dta_t$q158,dta_t$q64,dta_t$strata))
season1 <- data.frame(cbind(dta_t$trader_id, dta_t$q180,dta_t$q183,dta_t$q181,dta_t$q184,dta_t$q85,dta_t$strata))
season2$X1 <- as.numeric(season2$X1)
season1$X1 <- as.numeric(season1$X1)
season2$X2 <- as.numeric(season2$X2)
season1$X2 <- as.numeric(season1$X2)

season1$season <- "Season 1"
season2$season <- "Season 2"
all <- rbind(season1, season2)
names(all) <- c("trader_id","Price_WFP","Price_non_WFP","Vol_WFP","Vol_non_WFP","price_farmer","strata","season")
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
	    summarise(
		       Mean_Value = mean(VolumeSold, na.rm = TRUE),
		           Count = n(),
		           .groups = "drop"
		         )

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
#print(summary_dta_farmers_complete)
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
### is WFP a reliable buyer

### does indirect conditional contracts increase competiton?

dta_t$competition <- as.numeric(dta_t$q42)
tapply(dta_t$competition, dta_t$strata, mean, na.rm=TRUE)

#
### all has data of the trader level of prices paid to farmers and 
## we can merge in competition data here
##keep only season 1

all <- subset(all, season == "Season 1")

all <- merge(all, dta_t[c("trader_id","competition")], by.x = "trader_id", by.y = "trader_id")

#all$competition <- as.numeric(all$competition > median(all$competition, na.rm = TRUE))
all$competition[all$competition >= 50] <- NA
all$strata <- relevel(as.factor(all$strata), ref = "Control")

model1 <- (lm(competition ~ strata, data = all))
model2 <- (lm(price_farmer ~ strata + competition, data = all))

a <- coef(model1)["strataConditional/Spillover"]  # Path A coefficient
b <- coef(model2)["competition"]        # Path B coefficient
c_prime <- coef(model2)["strataConditional/Spillover"]  # Direct effect (Path C')

indirect_effect <- a * b
total_effect <- c_prime + indirect_effect

# Display the results
#cat("Indirect Effect: ", indirect_effect, "\n")
#cat("Direct Effect: ", c_prime, "\n")
#cat("Total Effect: ", total_effect, "\n")
###now do this with path analysis using lavaan
specmod <- "
price_farmer ~ c*strata
competition ~ a*strata
price_farmer ~ b*competition

ab := a*b
"

fitmod <- sem(specmod, data = all)
summary(fitmod, fit.measures = TRUE, rsquare = TRUE)

set.seed(12345)

fitmod <- sem(specmod,bootstrap = 500,se = 'bootstrap',data = all)
mediation_t <- parameterEstimates(fitmod,  ci=TRUE, level = .95, boot.ci.type ='perc')

### can we also do this with the farmer data? Lets try this for season 23 A
# merge in gps data using old_farmerid
dta_f_gps <- merge(dta_f,read.csv("/home/bjvca/IFPRI Dropbox/Data-UG-Conditional Contracts/Confidential/Data/farmer/raw/latest.csv")[c("Farmer_ID","check.maize._location_latitude","check.maize._location_longitude")],by.x="old_farmerid", by.y="Farmer_ID", all.x=T)
dta_t_gps <- merge(dta_t,read.csv("/home/bjvca/IFPRI Dropbox/Data-UG-Conditional Contracts/Confidential/Data/trader/raw/latest.csv")[c("Trader_ID","trader.check._location_latitude","trader.check._location_longitude")],by.x="old_traderid", by.y="Trader_ID", all.x=T)

#create a farmer level index of exposure to competition. We do this by taking the average of the number of other traders in the are as reported by the traders weighted by the inverse distance to the trader.
dta_f_gps$competition <- NA

for (i in 1:nrow(dta_f_gps)) {
        dist_vec <- rep(NA, nrow(dta_t_gps))
        for (j in 1:nrow(dta_t_gps)) {
                dist_vec[j] <- distHaversine(c(as.numeric(dta_f_gps$check.maize._location_longitude[i]), as.numeric(dta_f_gps$check.maize._location_latitude[i])), c(as.numeric(dta_t_gps$trader.check._location_longitude[j]), as.numeric(dta_t_gps$trader.check._location_latitude[j])))
        }
        dist_vec <- 1/dist_vec
        dist_vec <- dist_vec / sum(dist_vec, na.rm=T)
        dta_t_gps$weight <- dist_vec
        dta_f_gps$competition[i] <- sum(dta_t_gps$competition*dta_t_gps$weight, na.rm=T)
}
dta_f_gps$competition[dta_f_gps$competition == 0] <- NA
# now we need to calcuate average prices received during the first season of 2023

prices_23A <- aggregate(transactions_23A$Price, by = list(transactions_23A$FarmerID), FUN = mean, na.rm = TRUE)
names(prices_23A) <- c("Farmer_ID","price")
#merge in prices into dta_f_gps
dta_f_gps <- merge(dta_f_gps, prices_23A, by.x = "farmer_id", by.y = "Farmer_ID")

#dta_f_gps$competition <- as.numeric(dta_f_gps$competition > median(dta_f_gps$competition, na.rm = TRUE))
dta_f_gps$strata2 <- "Control"
dta_f_gps$strata2[dta_f_gps$strata == "Indirect" | dta_f_gps$strata == "Spillover"] <- "Indirect/Spillover"
model1 <- (lm(competition ~ strata2, data = dta_f_gps))
model2 <- (lm(price ~ strata2 + competition, data = dta_f_gps))

a <- coef(model1)[2]  # Path A coefficient
b <- coef(model2)[3]        # Path B coefficient
c_prime <- coef(model2)[2]  # Direct effect (Path C')

indirect_effect <- a * b
total_effect <- c_prime + indirect_effect

# Display the results


#cat("Indirect Effect: ", indirect_effect, "\n")
#cat("Direct Effect: ", c_prime, "\n")
#cat("Total Effect: ", total_effect, "\n")
###now do this with path analysis using lavaan
specmod <- "
price ~ c*strata2
competition ~ a*strata2
price ~ b*competition

ab := a*b
"

fitmod <- sem(specmod, data = dta_f_gps)
summary(fitmod, fit.measures = TRUE, rsquare = TRUE)

set.seed(12345)
# Display the results


#cat("Indirect Effect: ", indirect_effect, "\n")
#cat("Direct Effect: ", c_prime, "\n")
#cat("Total Effect: ", total_effect, "\n")
###now do this with path analysis using lavaan
specmod <- "
price ~ c*strata2
competition ~ a*strata2
price ~ b*competition

ab := a*b
"

fitmod <- sem(specmod, data = dta_f_gps)
summary(fitmod, fit.measures = TRUE, rsquare = TRUE)

set.seed(12345)

fitmod <- sem(specmod,bootstrap = 500,se = 'bootstrap',data = dta_f_gps)
mediation_f <- parameterEstimates(fitmod,  ci=TRUE, level = .95, boot.ci.type ='perc')

dta_t$purchase_23A <- dta_t$purchase_2023a/1000
dta_t$purchase_23B <- dta_t$purchase_2023b/1000
dta_t$purchase_23A[dta_t$purchase_23A > 1000] <- NA
dta_t$purchase_23B[dta_t$purchase_23B > 1000] <- NA

tapply(dta_t$purchase_23A, dta_t$strata, mean, na.rm=T)
tapply(dta_t$purchase_23B, dta_t$strata, mean, na.rm=T)

dta_f$improved_seed <- dta_f$q45 %in% c("Bazooka","DeKalb (DK) Monsanto","KH series","Longe 10H","Longe 10R/Kayongo-go","Longe 4 (OPV)","Longe 5 (nalongo-OPV)","Longe 5D (OPV)","Longe 6H", "Longe 7H","Longe 7R/Kayongo-go","MM3 (OPV)","Other hybrid","Other OPV", "Panner", "UH5051 (Gagawala)")

dta_f$improved_seed[dta_f$q45 =="" | dta_f$q45 =="Don't know" ] <- NA
dta_f$improved_seed[dta_f$q46 ==""] <- NA
dta_f$improved_seed[dta_f$q46 =="Local Market"] <- FALSE
dta_f$improved_seed[dta_f$q46 =="Other farmer/neigbor"] <- FALSE
dta_f$improved_seed[dta_f$q46 =="Own stocks"] <- FALSE
tapply(dta_f$improved_seed,dta_f$strata, mean,na.rm=T)

#now for fertilizer
dta_f$organic <- dta_f$q51 == "Yes"
dta_f$organic[dta_f$q51 == "" | dta_f$q51 == "Don't know"] <- NA
tapply(dta_f$organic, dta_f$strata, mean, na.rm=T)
#now for dap
dta_f$dap <- dta_f$q52 == "Yes"
dta_f$dap[dta_f$q52 == "" | dta_f$q52 == "Don't know"] <- NA
tapply(dta_f$dap, dta_f$strata, mean, na.rm=T)
#now for urea
dta_f$urea <- dta_f$q53 == "Yes"
dta_f$urea[dta_f$q53 == "" | dta_f$q53 == "Don't know"] <- NA
tapply(dta_f$urea, dta_f$strata, mean, na.rm=T)
#now for pesticides
dta_f$pesticides <- dta_f$q55 == "Yes"
dta_f$pesticides[dta_f$q55 == "" | dta_f$q55 == "Don't know"] <- NA
tapply(dta_f$pesticides, dta_f$strata, mean, na.rm=T)

# Variables of interest
adoption_vars <- c("improved_seed", "organic", "dap", "urea", "pesticides")

# Reshape and compute mean usage per strata and input
plot_data <- dta_f %>%
  select(strata, all_of(adoption_vars)) %>%
  pivot_longer(cols = all_of(adoption_vars), names_to = "input", values_to = "used") %>%
  group_by(strata, input) %>%
  summarize(mean_use = mean(used, na.rm = TRUE), .groups = "drop")

# Reorder input levels by overall mean use
plot_data <- plot_data %>%
  group_by(input) %>%
  mutate(overall_mean = mean(mean_use, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(input = reorder(input, overall_mean))

# Then re-run the same ggplot code
ggplot(plot_data, aes(x = mean_use, y = input, color = strata)) +
  geom_point(size = 3) +
  labs(
    x = "Proportion of farmers using input",
    y = "Input",
  color = "Strata"
  ) +
  theme_minimal()

plot_data_adoption <- plot_data
ggsave("adoption.png", plot = last_plot(), width = 8, height = 2, dpi = 300)



# make my own food insecurity scale
 
dta_f$food_insecurity  <- "food secure"
dta_f$food_insecurity[dta_f$q233 == "Yes" & dta_f$q234 =="Yes"  & dta_f$q235 =="Yes"  ] <- "mildly food insecure"
dta_f$food_insecurity[dta_f$q233 == "Yes" & dta_f$q234 =="Yes"  & dta_f$q235 =="Yes"  & dta_f$q236 =="Yes"  & dta_f$q237 =="Yes"  & dta_f$q238 =="Yes"  ] <- "moderately food insecure"
dta_f$food_insecurity[dta_f$q233 == "Yes" & dta_f$q234 =="Yes"  & dta_f$q235 =="Yes"  & dta_f$q236 =="Yes"  & dta_f$q237 =="Yes"  & dta_f$q238 =="Yes"    & dta_f$q239 =="Yes"  & dta_f$q240 =="Yes"     ] <- "severely food insecure"


df_prop <- dta_f %>%
  group_by(strata, food_insecurity) %>%
  summarise(count = n()) %>%
  group_by(strata) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

plot_f  <- ggplot(df_prop, aes(x = food_insecurity, y = prop, fill = strata)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Proportion", fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



dta_t$food_insecurity <- "food secure"
dta_t$food_insecurity[dta_t$q277 == "Yes" & dta_t$q278 =="Yes"  & dta_t$q279 =="Yes"   ] <- "mildly food insecure"
dta_t$food_insecurity[dta_t$q277 == "Yes" & dta_t$q278 =="Yes"  & dta_t$q279 =="Yes"  & dta_t$q280 =="Yes"  & dta_t$q281 =="Yes"  & dta_t$q282 =="Yes"  ] <- "moderately food insecure"
dta_t$food_insecurity[dta_t$q277 == "Yes" & dta_t$q278 =="Yes"  & dta_t$q279 =="Yes"  & dta_t$q280 =="Yes"  & dta_t$q281 =="Yes"  & dta_t$q282 =="Yes"    & dta_t$q283 =="Yes"  & dta_t$q284 =="Yes"     ] <- "severely food insecure"


df_prop <- dta_t %>%
  group_by(strata, food_insecurity) %>%
  summarise(count = n()) %>%
  group_by(strata) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

plot_t  <- ggplot(df_prop, aes(x = food_insecurity, y = prop, fill = strata)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Proportion", fill = "Group") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Combine plots side by side
combined_plot <- plot_f + plot_t  # patchwork syntax

# Save the combined plot
ggsave("food_security_comparison.png", plot = combined_plot, width = 8, height = 8, dpi = 300)

### food diversity score  
dta_f$A <-  (((as.numeric(dta_f$q212a)/7) >.5) +  ((as.numeric(dta_f$q213a)/7)>.5) + ((as.numeric(dta_f$q214a)/7)>.5)  + ((as.numeric(dta_f$q215a)/7)>.5) + ((as.numeric(dta_f$q216a)/7)>.5) + ((as.numeric(dta_f$q219a)/7)>.5)) >0
dta_f$B <- (((as.numeric(dta_f$q217a)/7) > .5) +  ((as.numeric(dta_f$q218a)/7)>.5))> 0
dta_f$C <- (as.numeric(dta_f$q223a)/7)>.5
dta_f$D <- (as.numeric(dta_f$q224a)/7)>.5
dta_f$E <- (((as.numeric(dta_f$q225a)/7)>.5) +  (as.numeric(dta_f$q226a)/7)>.5) >0
dta_f$F <- (as.numeric(dta_f$q228a)/7)>.5
dta_f$G <- (as.numeric(dta_f$q227a)/7)>.5
dta_f$H <- (((as.numeric(dta_f$q220a)/7)>.5) +  (as.numeric(dta_f$q221a)/7)>.5) >0
dta_f$I <- (as.numeric(dta_f$q229a)/7)>.5
dta_f$J <- (as.numeric(dta_f$q231a)/7)>.5
dta_f$K <- (as.numeric(dta_f$q230a)/7)>.5
dta_f$FDS <- dta_f$A + dta_f$B + dta_f$C + dta_f$D + dta_f$E + dta_f$F + dta_f$G + dta_f$H + dta_f$I + dta_f$J + dta_f$K + 1 ## assume everyone eats salt



df_prop <- dta_f %>%
  group_by(strata, FDS) %>%
  summarise(count = n()) %>%
  group_by(strata) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

plot_f  <- ggplot(df_prop, aes(x = FDS, y = prop, fill = strata)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = seq(floor(min(df_prop$FDS, na.rm=T)), ceiling(max(df_prop$FDS, na.rm=T)), by = 1)) +
  labs(x = "Number of Food Groups", y = "Proportion", fill = "Group") +
  theme_minimal()


#now for traders

### food diversity score  
dta_t$A <-  (((as.numeric(dta_t$q257a)/7) >.5) +  ((as.numeric(dta_t$q258a)/7)>.5) + ((as.numeric(dta_t$q259a)/7)>.5)  + ((as.numeric(dta_t$q260a)/7)>.5) + ((as.numeric(dta_t$q261a)/7)>.5) + ((as.numeric(dta_t$q264a)/7)>.5)) >0
dta_t$B <- (((as.numeric(dta_t$q262a)/7) > .5) +  ((as.numeric(dta_t$q262a)/7)>.5))> 0
dta_t$C <- (as.numeric(dta_t$q267a)/7)>.5
dta_t$D <- (as.numeric(dta_t$q268a)/7)>.5
dta_t$E <- (((as.numeric(dta_t$q269a)/7)>.5) +  (as.numeric(dta_t$q270a)/7)>.5) >0
dta_t$F <- (as.numeric(dta_t$q272a)/7)>.5
dta_t$G <- (as.numeric(dta_t$q271a)/7)>.5
dta_t$H <- (((as.numeric(dta_t$q265a)/7)>.5) +  (as.numeric(dta_t$q266a)/7)>.5) >0
dta_t$I <- (as.numeric(dta_t$q273a)/7)>.5
dta_t$J <- (as.numeric(dta_t$q274a)/7)>.5
dta_t$K <- (as.numeric(dta_t$q275a)/7)>.5
dta_t$L <- (as.numeric(dta_t$q276a)/7)>.5
dta_t$FDS <- dta_t$A + dta_t$B + dta_t$C + dta_t$D + dta_t$E + dta_t$F + dta_t$G + dta_t$H + dta_t$I + dta_t$J + dta_t$K + dta_t$L ## assume everyone eats salt

df_prop <- dta_t %>%
  group_by(strata, FDS) %>%
  summarise(count = n()) %>%
  group_by(strata) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

plot_t <- ggplot(df_prop, aes(x = FDS, y = prop, fill = strata)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = seq(floor(min(df_prop$FDS)), ceiling(max(df_prop$FDS)), by = 1)) +
  labs(x = "Number of Food Groups", y = "Proportion", fill = "Group") +
  theme_minimal()

# Combine plots side by side
combined_plot <- plot_f + plot_t  # patchwork syntax

# Save the combined plot
ggsave("diet_diversity_score.png", plot = combined_plot, width = 8, height = 8, dpi = 300)

###  Livelihood Coping Strategies – Food Security (LCS-FS) 
library(labelled)
#library(expss)

dta_f <- dta_f %>%
  mutate(across(c(m1:m16), ~ case_when(
    . == "No, because I did not need to" ~ 10,
    . == "No, we sold those assets or engaged in this activity within the last 12 months and can't continue" ~ 20,
    . == "Yes" ~ 30,
    . == "Not applicable (don’t have access to this strategy)" ~ 9999,
    TRUE ~ NA_real_  # handles missing or unmatched values
  )))

#value labels
dta_f <- dta_f %>%
  mutate(across(c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16), ~labelled(., labels = c(
    "No, because I did not need to" = 10,
    "No, we sold those assets or engaged in this activity within the last 12 months and can't continue" = 20,
    "Yes" = 30,
    "Not applicable (don’t have access to this strategy)" = 9999
  ))))

#create a variable to specify if the household used any of the strategies by severity
#stress
dta_f <- dta_f %>% mutate(stress_coping_FS = case_when(
  m1 == 20 | m1 == 30 ~ 1,
  m2 == 20 | m2 == 30 ~ 1,
  m3 == 20 | m3 == 30 ~ 1,
  m4 == 20 | m4 == 30 ~1,
  m5 == 20 | m5 == 30 ~ 1,
  m6 == 20 | m6 == 30 ~ 1,
    TRUE ~ 0))
var_label(dta_f$stress_coping_FS) <- "Did the HH engage in stress coping strategies"
#Crisis
dta_f <- dta_f %>% mutate(crisis_coping_FS = case_when(
  m7 == 20 | m7 == 30 ~ 1,
  m8 == 20 | m8 == 30 ~ 1,
  m9 == 20 | m9 == 30 ~ 1,
  m10 == 20 | m10 == 30 ~1,
  m11 == 20 | m11 == 30 ~ 1,
  m12 == 20 | m12 == 30 ~ 1,
  m13 == 20 | m13 == 30 ~ 1,
  TRUE ~ 0))
var_label(dta_f$crisis_coping_FS) <- "Did the HH engage in crisis coping strategies"
#Emergency
dta_f <- dta_f %>% mutate(emergency_coping_FS = case_when(
  m14 == 20 | m14 == 30 ~ 1,
  m15 == 20 | m15 == 30 ~ 1,
  m16 == 20 | m16 == 30 ~ 1,
  TRUE ~ 0))
var_label(dta_f$emergency_coping_FS) <- "Did the HH engage in emergency coping strategies"

#calculate Max_coping_behaviour
dta_f <- dta_f %>% mutate(Max_coping_behaviourFS = case_when(
  emergency_coping_FS == 1 ~ "emergency",
  crisis_coping_FS == 1 ~ "crisis",
  stress_coping_FS == 1 ~ "stress",
  TRUE ~ "none"))

df_prop <- dta_f %>%
  group_by(strata, Max_coping_behaviourFS) %>%
  summarise(count = n()) %>%
  group_by(strata) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

df_prop$Max_coping_behaviourFS <- factor(df_prop$Max_coping_behaviourFS,
                                         levels = c("none", "stress", "crisis", "emergency"))

plot_f <- ggplot(df_prop, aes(x = Max_coping_behaviourFS, y = prop, fill = strata)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Proportion", fill = "Group") +
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))

##now for traders
dta_t <- dta_t %>%
  mutate(across(c(m1:m16), ~ case_when(
    . == "No, because I did not need to" ~ 10,
    . == "No, we sold those assets or engaged in this activity within the last 12 months and can't continue" ~ 20,
    . == "Yes" ~ 30,
    . == "Not applicable (don’t have access to this strategy)" ~ 9999,
    TRUE ~ NA_real_  # handles missing or unmatched values
  )))

#value labels
dta_t <- dta_t %>%
  mutate(across(c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16), ~labelled(., labels = c(
    "No, because I did not need to" = 10,
    "No, we sold those assets or engaged in this activity within the last 12 months and can't continue" = 20,
    "Yes" = 30,
    "Not applicable (don’t have access to this strategy)" = 9999
  ))))

#create a variable to specify if the household used any of the strategies by severity
#stress
dta_t <- dta_t %>% mutate(stress_coping_FS = case_when(
  m1 == 20 | m1 == 30 ~ 1,
  m2 == 20 | m2 == 30 ~ 1,
  m3 == 20 | m3 == 30 ~ 1,
  m4 == 20 | m4 == 30 ~1,
  m5 == 20 | m5 == 30 ~ 1,
  m6 == 20 | m6 == 30 ~ 1,
  TRUE ~ 0))
var_label(dta_t$stress_coping_FS) <- "Did the HH engage in stress coping strategies"
#Crisis
dta_t <- dta_t %>% mutate(crisis_coping_FS = case_when(
  m7 == 20 | m7 == 30 ~ 1,
  m8 == 20 | m8 == 30 ~ 1,
  m9 == 20 | m9 == 30 ~ 1,
  m10 == 20 | m10 == 30 ~1,
  m11 == 20 | m11 == 30 ~ 1,
  m12 == 20 | m12 == 30 ~ 1,
  m13 == 20 | m13 == 30 ~ 1,
  TRUE ~ 0))
var_label(dta_t$crisis_coping_FS) <- "Did the HH engage in crisis coping strategies"
#Emergency
dta_t <- dta_t %>% mutate(emergency_coping_FS = case_when(
  m14 == 20 | m14 == 30 ~ 1,
  m15 == 20 | m15 == 30 ~ 1,
  m16 == 20 | m16 == 30 ~ 1,
  m17 == 20 | m17 == 30 ~ 1,
  TRUE ~ 0))
var_label(dta_t$emergency_coping_FS) <- "Did the HH engage in emergency coping strategies"

#calculate Max_coping_behaviour
dta_t <- dta_t %>% mutate(Max_coping_behaviourFS = case_when(
  emergency_coping_FS == 1 ~ "emergency",
  crisis_coping_FS == 1 ~ "crisis",
  stress_coping_FS == 1 ~ "stress",
  TRUE ~ "none"))

df_prop <- dta_t %>%
  group_by(strata, Max_coping_behaviourFS) %>%
  summarise(count = n()) %>%
  group_by(strata) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

df_prop$Max_coping_behaviourFS <- factor(df_prop$Max_coping_behaviourFS,
                                         levels = c("none", "stress", "crisis", "emergency"))

plot_t <- ggplot(df_prop, aes(x = Max_coping_behaviourFS, y = prop, fill = strata)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Proportion", fill = "Group") +
  theme_minimal()  +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Combine plots side by side
combined_plot <- plot_f + plot_t  # patchwork syntax

# Save the combined plot
ggsave("coping_score.png", plot = combined_plot, width = 8, height = 8, dpi = 300)

### regression analysis


# 
# dta_f$price
# dta_f$north <- dta_f$district_code %in% c("Hoima","Masindi","Kiryandongo")
# summary(lm(price~strata + north,dta_f))
# 
# ### formal analysis - look at difference between indirect and spillover withing farmer pair
# dta_f$pairFE <-  sapply(strsplit(dta_f$old_farmerid, "_"), `[`, 1)  
# dta_f$treat <-  sapply(strsplit(dta_f$old_farmerid, "_"), `[`, 2) 
# 
# summary(lm(price ~ treat + pairFE,dta_f[dta_f$treat !="C",]))
# summary(lm(price ~ treat,dta_f[dta_f$treat !="C",]))
