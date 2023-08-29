here::here("data", "colony.csv")
library(gtsummary)
library(readr)
library(dplyr)
colony <- read_csv("data/colony.csv")
View(colony)
tbl_summary(
	colony,
	by = months,
	include = c(colony_lost_pct, colony_added, colony_lost))
linear_model <- lm(colony_lost_pct ~ months + state + year + colony_added + colony_reno_pct, data = colony)
tbl_regression(
	linear_model,
	exponentiate = FALSE,
	label = list(
		months ~ "Months",
		state ~ "State",
		year ~ "Year",
		colony_added ~ "Colony Added",
		colony_reno_pct ~ "Colony Renovated (%)"
	)
)
hist(colony[["colony_lost_pct"]],
		 main = paste("Histogram of Colony Losses"),
		 xlab = "Percent of Colonies Lost",
		 ylab = "Frequency",
		 col = "lightblue",
		 border = "black")




