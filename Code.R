# Nama : Jackie Lim
# NIM : 2702271833
# Assignment 1

library(dplyr)
library(openxlsx)
library(factoextra)
library(ggplot2)

setwd("D:/Binus/Semester 2/Data Mining/Homework/Assignment 1 Jackie")
covid<- read.csv("covid.csv", header = TRUE, sep = ";")

#1a) label male as 0, female as 1
covid$Gender <- factor(covid$Gender, levels = c("Male","Female"), labels = c(0,1))

#1b) label Poor as 0, Fair = 1, Good = 2, Very good = 3, Excellent = 4
covid$GeneralHealth <- factor(covid$GeneralHealth, levels = c("Poor", "Fair", "Good", "Very good", "Excellent"), 
                                                              labels = c(0,1,2,3,4))

#1c) last check up 
covid$LastCheckupTime <- factor(covid$LastCheckupTime, levels = c("5 or more years ago", "Within past year (anytime less than 12 months ago)",
                                                                  "Within past 2 years (1 year but less than 2 years ago)","Within past 5 years (2 years but less than 5 years ago)"),
                                                                  labels = c(0,1,2,3))

#1d) physical activities
covid$PhysicalActivities <- factor(covid$PhysicalActivities,levels = c("No","Yes"), labels = c(0,1))

#1e) removed teeth
covid$RemovedTeeth <- factor(covid$RemovedTeeth,levels = c("None of them","1 to 5","6 or more, but not all","All"), labels = c(0,1,2,3))

#1f) hearth attack, angina, stroke, asthma, heart attack, had angina, had stroke, had asthma, skin cancer, COPD, 
#    depressive disorder, kidney disease, arthritis, diabetes, deaf or hard of hearing, blind or vision diciculty, diciculty  concentrating, 
#    diciculty walking, diciculty dressing bathing, diciculty  errands 
covid$HadHeartAttack <- factor(covid$HadHeartAttack, levels = c("No","Yes"), labels = c(0,1))
covid$HadAngina <- factor(covid$HadAngina, levels = c("No","Yes"), labels = c(0,1))
covid$HadStroke <- factor(covid$HadStroke, levels = c("No","Yes"), labels = c(0,1))
covid$HadAsthma <- factor(covid$HadAsthma, levels = c("No","Yes"), labels = c(0,1))
covid$HadSkinCancer <- factor(covid$HadSkinCancer, levels = c("No","Yes"), labels = c(0,1))
covid$HadCOPD <- factor(covid$HadCOPD, levels = c("No","Yes"), labels = c(0,1))
covid$HadDepressiveDisorder <- factor(covid$HadDepressiveDisorder, levels = c("No","Yes"), labels = c(0,1))
covid$HadKidneyDisease <- factor(covid$HadKidneyDisease, levels = c("No","Yes"), labels = c(0,1))
covid$HadArthritis <- factor(covid$HadArthritis, levels = c("No","Yes"), labels = c(0,1))
covid$HadDiabetes <- factor(covid$HadDiabetes, levels = c("No","No, pre-diabetes or borderline diabetes","Yes, but only during pregnancy (female)","Yes"), labels = c(0,1,2,3))
covid$DeafOrHardOfHearing <- factor(covid$DeafOrHardOfHearing, levels = c("No","Yes"), labels = c(0,1))
covid$BlindOrVisionDifficulty <- factor(covid$BlindOrVisionDifficulty, levels = c("No","Yes"), labels = c(0,1))
covid$DifficultyConcentrating <- factor(covid$DifficultyConcentrating, levels = c("No","Yes"), labels = c(0,1))
covid$DifficultyWalking <- factor(covid$DifficultyWalking, levels = c("No","Yes"), labels = c(0,1))
covid$DifficultyDressingBathing <- factor(covid$DifficultyDressingBathing, levels = c("No","Yes"), labels = c(0,1))
covid$DifficultyErrands <- factor(covid$DifficultyErrands, levels = c("No","Yes"), labels = c(0,1))

#1g) smoker status
covid$SmokerStatus <- factor(covid$SmokerStatus, levels = c("Never smoked","Former smoker","Current smoker - now smokes some days",
                                                            "Current smoker - now smokes every day"),labels = c(0,1,2,3))

#1h) E-cigarette
covid$ECigaretteUsage <- factor(covid$ECigaretteUsage, levels = c("Never used e-cigarettes in my entire life","Not at all (right now)","Use them some days","Use them every day"),
                                                       labels = c(0,1,2,3))

#1i) Chest scan
covid$ChestScan <- factor(covid$ChestScan, levels = c("No","Yes"), labels = c(0,1))

#1j) age category
covid$AgeCategory <- factor(covid$AgeCategory, levels = c("Age 18 to 24","Age 25 to 29","Age 30 to 34","Age 35 to 39","Age 40 to 44","Age 45 to 49","Age 50 to 54","Age 55 to 59",
                                                          "Age 60 to 64","Age 65 to 69","Age 70 to 74","Age 75 to 79","Age 80 or older"), labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12))

#1k) alcohol drinker
covid$AlcoholDrinkers <- factor(covid$AlcoholDrinkers, levels = c("No","Yes"), labels = c(0,1))

#convert to num
covid$WeightInKilograms <- as.numeric(gsub(",", ".", covid$WeightInKilograms))
covid$HeightInMeters <- as.numeric(gsub(",", ".", covid$HeightInMeters))
covid$BMI <- as.numeric(gsub(",", ".", covid$BMI))

# 2a) Check missing values
missing_values <- colSums(is.na(covid))
print("Missing Values:")
print(missing_values)

table(covid$GeneralHealth)

# 2b) Fill missing values based on CovidPos
subset_data_0 <- covid[covid$CovidPos == "No", ]
subset_data_1 <- covid[covid$CovidPos == "Yes", ]

# Mode for General Health when CovidPos == "No"
mode_general_health_0 <- names(sort(table(subset_data_0$GeneralHealth), decreasing = TRUE))[1]
numeric_values0 <- as.numeric(mode_general_health_0)

# Mode for General Health when CovidPos == "Yes"
mode_general_health_1 <- names(sort(table(subset_data_1$GeneralHealth), decreasing = TRUE))[1]
numeric_values1 <- as.numeric(mode_general_health_1)

# Fill missing values based on CovidPos
covid$GeneralHealth <- ifelse(is.na(covid$GeneralHealth) & covid$CovidPos == "Yes", 
                              mode_general_health_1, 
                              covid$GeneralHealth)

covid$GeneralHealth <- ifelse(is.na(covid$GeneralHealth) & covid$CovidPos == "No", 
                              mode_general_health_0, 
                              covid$GeneralHealth)

#Fill for the NA at weight
covid$WeightInKilograms <- ifelse(is.na(covid$WeightInKilograms),
                                  ifelse(covid$CovidPos == "No",
                                         mean(covid$WeightInKilograms[covid$CovidPos == "No"], na.rm = TRUE),
                                         ifelse(covid$CovidPos == "Yes",
                                                mean(covid$WeightInKilograms[covid$CovidPos == "Yes"], na.rm = TRUE),
                                                ifelse(covid$CovidPos == "Tested positive using home test without a health professional",
                                                       mean(covid$WeightInKilograms[covid$CovidPos == "Tested positive using home test without a health professional"], na.rm = TRUE),
                                                       covid$WeightInKilograms))),
                                  covid$WeightInKilograms)

missing_values <- colSums(is.na(covid))
print("Missing Values:")
print(missing_values)

# Convert the modified numeric column back to factor
covid$GeneralHealth <- factor(covid$GeneralHealth, levels = c("1", "2", "3", "4", "5"), 
                              labels = c(0, 1, 2, 3, 4))


#3a)
compute_num <- function(column) {
  # Compute mean
  mean_val <- mean(column, na.rm = TRUE)
  
  # Compute first quartile (Q1)
  Q1_val <- quantile(column, probs = 0.25, na.rm = TRUE)
  
  # Compute median
  median_val <- median(column, na.rm = TRUE)
  
  # Compute third quartile (Q3)
  Q3_val <- quantile(column, probs = 0.75, na.rm = TRUE)
  
  # Compute standard deviation
  std_dev_val <- sd(column, na.rm = TRUE)
  
  # Compute variance
  variance_val <- var(column, na.rm = TRUE)
  
  # Create a summary dataframe
  summary_df <- data.frame(
    Mean = mean_val,
    Q1 = Q1_val,
    Median = median_val,
    Q3 = Q3_val,
    Std_Dev = std_dev_val,
    Variance = variance_val
  )
  
  return(summary_df)
}

compute_num(covid$PhysicalHealthDays)
compute_num(covid$MentalHealthDays)
compute_num(covid$SleepHours)
compute_num(covid$HeightInMeters)
compute_num(covid$WeightInKilograms)
compute_num(covid$BMI)



#3b
find_modes <- function(vector) {
  # Compute frequencies of each unique element
  frequencies <- table(vector)
  
  # Find the maximum frequency
  max_frequency <- max(frequencies)
  
  # Extract mode(s)
  modes <- names(frequencies[frequencies == max_frequency])
  
  # Return mode(s) and their frequencies
  return(list(Modes = modes))
}

result <- find_modes(covid$State) 
print(result)

result <- find_modes(covid$Gender) 
print(result)

result <- find_modes(covid$GeneralHealth) 
print(result)

result <- find_modes(covid$LastCheckupTime) 
print(result)

result <- find_modes(covid$PhysicalActivities) 
print(result)

result <- find_modes(covid$HadHeartAttack) 
print(result)

result <- find_modes(covid$HadAngina) 
print(result)

result <- find_modes(covid$HadStroke) 
print(result)

result <- find_modes(covid$HadAsthma) 
print(result)

result <- find_modes(covid$HadSkinCancer) 
print(result)

result <- find_modes(covid$HadCOPD) 
print(result)

result <- find_modes(covid$HadDepressiveDisorder) 
print(result)

result <- find_modes(covid$HadKidneyDisease) 
print(result)

result <- find_modes(covid$HadArthritis) 
print(result)

result <- find_modes(covid$HadDiabetes) 
print(result)

result <- find_modes(covid$DeafOrHardOfHearing) 
print(result)

result <- find_modes(covid$BlindOrVisionDifficulty) 
print(result)

result <- find_modes(covid$DifficultyConcentrating) 
print(result)

result <- find_modes(covid$DifficultyWalking) 
print(result)

result <- find_modes(covid$DifficultyDressingBathing) 
print(result)

result <- find_modes(covid$DifficultyErrands) 
print(result)

result <- find_modes(covid$SmokerStatus) 
print(result)

result <- find_modes(covid$ECigaretteUsage) 
print(result)

result <- find_modes(covid$ChestScan) 
print(result)

result <- find_modes(covid$AlcoholDrinkers) 
print(result)

result <- find_modes(covid$CovidPos) 
print(result)


outliers <- function(column, title) {
  # Replace commas with periods
  column <- gsub(",", ".", column)
  
  # Convert the column to numeric (if it's not already)
  column <- as.numeric(column)
  
  # Check for non-numeric values
  column2 <- na.omit(column)
  
  # Create boxplot with title
  boxplot(column2, main = title,
          xlab = "Value",
          ylab = "Frequency",
          col = "skyblue",
          border = "brown")
  
  # Compute quartiles
  Q1 <- quantile(column2, probs = 0.25)
  Q3 <- quantile(column2, probs = 0.75)
  
  # Compute IQR (Interquartile Range)
  IQR_val <- Q3 - Q1
  
  # Compute upper and lower bounds for outliers
  upper_bound <- Q3 + 1.5 * IQR_val
  lower_bound <- Q1 - 1.5 * IQR_val
  
  # Identify outliers
  outliers <- column2[column2 > upper_bound | column2 < lower_bound]
  
  # Count the number of outliers
  num_outliers <- length(outliers)
  
  # Print the number of outliers
  cat("Number of outliers:", num_outliers, "\n")
  
  # Return the outliers (if any)
  return(outliers)
}

outliers(covid$PhysicalHealthDays, "Physical Health Days")
outliers(covid$MentalHealthDays, "Mental Health Days")
outliers(covid$SleepHours, "Sleep Hours")
outliers(covid$HeightInMeters, "Height in Meters")
outliers(covid$WeightInKilograms, "Weight in Kilograms")
outliers(covid$BMI, "BMI")


#3D)
# Define a function to standardize numeric columns
standar <- function(data, columns) {
  for(i in columns) {
    if(is.numeric(data[[i]])) { #if numeric then do
      # Standarize the column
      data[[i]] <- (data[[i]] - mean(data[[i]], na.rm = TRUE)) / sd(data[[i]], na.rm = TRUE)
    } else {
      # Warn if the column is not numeric
      warning(paste0("Skipping non-numeric column '", col, "'."))
    }
  }
  return(data)
}

# Columns to standardize
columns_to_standardize <- c("HeightInMeters", "WeightInKilograms", "BMI",
                            "PhysicalHealthDays", "MentalHealthDays", "SleepHours")

# Apply standardization function
standarized_data <- standar(covid, columns_to_standardize)

# Display the standardized data
standarized_data


## 4 A) Perform PCA
# Subset relevant columns for PCA
covid_sample <- covid[, c("HeightInMeters", "WeightInKilograms", "BMI", "PhysicalHealthDays", "MentalHealthDays", "SleepHours")]

# Perform PCA
covid_sample <- na.omit(covid_sample)
covid_pca <- prcomp(covid_sample, scale = TRUE)

# C) Summarize PCA results
summary(covid_pca)

# D) Visualize variance with Scree Plot
fviz_eig(covid_pca, addlabels = TRUE, 
         ylim = c(0 , 50))

# E) Determine the number of principal components based on cumulative proportion
# In my opinion, I will only take up to P3, because based on the scree plot, it can be seen that the positions of P3 and P4 are almost parallel, and 70% is already considered high


# Export processed data to an Excel file
write.xlsx(covid, "final_dataset.xlsx")