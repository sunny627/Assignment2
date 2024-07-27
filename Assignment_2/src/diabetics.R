#Reading the csv data
diabetes = read.csv("C:/Users/Admin/Desktop/Assignment_2/data_clean/diabetes.csv")
diabetes

# About the dataset 
summary(diabetes)

nrow(diabetes)

# cleaning data to check null values & missing values are present
any(is.null(diabetes))
any(is.na(diabetes))

#A)
#set a seed value
set.seed(200)


# random sample of 25 observations
sample_glucose_data <- diabetes[sample(nrow(diabetes), 25, replace = FALSE), ]

summary(sample_glucose_data)

nrow(sample_glucose_data)

#finding the mean of the samples and population
diabetics_mean <- mean(diabetes$Glucose)
diabetics_max <- max(diabetes$Glucose)
sample_glucose_data_mean <- mean(sample_glucose_data$Glucose)
sample_glucose_data_max <- max(sample_glucose_data$Glucose)

sprintf("%f is the Population glucose mean", diabetics_mean)
sprintf("%f is the Population glucose max ", diabetics_max)
sprintf("%f is the sample glucose data mean", sample_glucose_data_mean)
sprintf("%f is the sample glucose data max", sample_glucose_data_max)


# bar chart to compare the statistics
stats <- c(diabetics_mean, sample_glucose_data_mean, diabetics_max, sample_glucose_data_max)
names(stats) <- c("P.Mean", "S.Mean", "P.Max", "S.Max")
barplot(stats, main="Glucose Statistics", ylab="Glucose", col=c("blue", "red", "blue", "red"), ylim=c(0, 200))


#B)
#  98th percentile of BMI of your sample and the population

#extract BMI
BMI <- diabetes$BMI
BMI

BMI_data_98th_percentile <- quantile(BMI, 0.98)
BMI_sample_data_98th_percentile <- quantile(sample(BMI, length(BMI), replace=TRUE), 0.98)

BMI_data_98th_percentile
BMI_sample_data_98th_percentile

#PlotS
hist(BMI, breaks=10, xlab="BMI", main="Population BMI Distribution",col="red",border = "yellow")
abline(v=BMI_data_98th_percentile, col="blue", lty=2)
legend("topright", legend=c("98th percentile"), col="blue", lty=2)

hist(sample(BMI, length(BMI), replace=TRUE), breaks=10, xlab="BMI", main="sample BMI Distribution", col="pink",border = "black")
abline(v=BMI_sample_data_98th_percentile, col="orange", lty=2)
legend("topright", legend=c("98th percentile"), col="orange", lty=2)


# C)

# average mean, standard deviation and percentile for BloodPressure
mean_BloodPressure  <- mean(diabetes$BloodPressure)
sd_BloodPressure  <- sd(diabetes$BloodPressure)
percentile_BloodPressure  <- quantile(diabetes$BloodPressure, c(0.25, 0.5, 0.75))
sprintf("%f, %f, %f are the mean_BloodPressure , SD_BloodPressure , percentile_BloodPressure (25,50,75) of the population ",mean_BloodPressure ,sd_BloodPressure ,percentile_BloodPressure)

BloodPressure_mean_boot <- rep(NA, 500)
BloodPressure_sd_boot <- rep(NA, 500)
BloodPressure_quantity_boot <- matrix(NA, nrow = 3, ncol = 500)


for (i in 1:500) {
  sample_i <- sample(diabetes$BloodPressure, size = 150, replace = TRUE)
  BloodPressure_mean_boot[i] <- mean(sample_i)
  BloodPressure_sd_boot[i] <- sd(sample_i)
  BloodPressure_quantity_boot[, i] <- quantile(sample_i, probs = c(0.25, 0.5, 0.75))
}

par(mfrow = c(3, 2))
hist(diabetes$BloodPressure, main = "Population", xlab = "BloodPressure")
hist(BloodPressure_mean_boot, main = "Bootstrap Mean", xlab = "BloodPressure")
abline(v = mean_BloodPressure, col = "blue")
hist(BloodPressure_sd_boot, main = "Bootstrap Standard Deviation", xlab = "BloodPressure")
abline(v = sd_BloodPressure, col = "red")
matplot(BloodPressure_quantity_boot, type = "l", main = "Bootstrap Percentiles", xlab = "Sample",
        ylab = "BloodPressure", lty = 1, col = 1:5)
lines(percentile_BloodPressure, lty = 2, col = "red")
