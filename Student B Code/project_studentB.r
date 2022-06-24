rm(list = ls());

# Load useful functions and packages
source(file = "./hw5_functions.R")
source(file = "./rapper_functions.R")
source(file = "./gangsta_rapper_functions.R")
library(lpSolveAPI)

# Specify values for the independent variable
time <- seq(from = 0, to = 23, length = 24);

# Construct the known model with a maintentance respiration rate 
# of 2.83e-06 kJ* (umol biomass)^-1 * hr^-1
lpModelOriginal <- read.lp(file = "./AHwUptake.lp", verbose = "normal")
knownModelDO <- gangstaIterate(gangstaModel = aerobicHetwAssimModel, 
                               drivingValues = data.frame(row.names = as.character(time)),
                               lpModel = lpModelOriginal,
                               respRate = 2.83e-06,
                               findConstraintRowAndColumn = findConstraintRowAndColumn)

############ Standard deviation of Synthetic Error = 0.001 ##################

# Specify the number of realizations
totalRealizations <- 100

# Build an empty data frame to store parameter estimates
# from each realization 
respRateEst1 <- numeric(totalRealizations)

# Build an empty matrix to store model predictions
# for each realization
confidences1 <- matrix(nrow = totalRealizations, 
                      ncol = length(time))

# Build an empty matrix to store predictions
predictions1 <- matrix(nrow = totalRealizations, 
                      ncol = length(time))


# Iterate through realizations for a standard deviation in synthetic error of 0.001
for(realization in 1:totalRealizations){
  # Add normally distributed error to generate synthetic data
  synthData <- addSynthError(knownModelDO, sderr = 0.001)
  
  # Estimate parameters by minimizing sum of squared residuals
  nlsr <- nls(
    formula = synthData ~ gangstaIterate(gangstaModel = aerobicHetwAssimModel, 
                                         drivingValues = data.frame(row.names = as.character(time)),
                                         lpModel = read.lp(file = "./AHwUptake.lp", verbose = "normal"),
                                         respRate = respRate,
                                         findConstraintRowAndColumn = findConstraintRowAndColumn
                                         ),
    start = list(respRate = 2.83e-06)
  )
  
  # Get parameter estimate
  nlsrest <- summary(nlsr)$coefficients
  
  # Store parameter estimate in numeric vector
  respRateEst1[realization] <- nlsrest["respRate", "Estimate"]
  
  # Store model predictions for the estimated parameters
  confidences1[realization,] <- predict(nlsr)
  
  # Calculate the standard deviation of the residuals
  sderr <- sd(synthData - confidences1[realization,])
 
  # Store model predictions with added error to generate prediction intervals 
  predictions1[realization,] <- confidences1[realization,] + rnorm(length(time), mean = 0, sd = sderr)
}

# Calculate the 95% confidence interval of the respiration energy parameter estimate
confidenceResp1 <- quantile(respRateEst1, probs = c(0.025, 0.975))

# Calculate the the range of the 95% confidence interval,
# which is a metric of uncertainty in the paramter estimate.
uncert1 <- confidenceResp1["97.5%"] - confidenceResp1["2.5%"]


############ Standard deviation of Synthetic Error = 0.003 ##################

# Build an empty data frame to store parameter estimates
# from each realization 
respRateEst2 <- numeric(totalRealizations)

# Build an empty matrix to store model predictions
# for each realization
confidences2 <- matrix(nrow = totalRealizations, 
                       ncol = length(time))

# Build an empty matrix to store predictions
predictions2 <- matrix(nrow = totalRealizations, 
                       ncol = length(time))


# Iterate through realizations for a standard deviation in synthetic error of 0.003
for(realization in 92:totalRealizations){
  # Add normally distributed error to generate synthetic data
  synthData <- addSynthError(knownModelDO, sderr = 0.003)
  
  # Estimate parameters by minimizing sum of squared residuals
  nlsr <- nls(
    formula = synthData ~ gangstaIterate(gangstaModel = aerobicHetwAssimModel, 
                                         drivingValues = data.frame(row.names = as.character(time)),
                                         lpModel = read.lp(file = "./AHwUptake.lp", verbose = "normal"),
                                         respRate = respRate,
                                         findConstraintRowAndColumn = findConstraintRowAndColumn
    ),
    start = list(respRate = 2.83e-06)
  )
  
  # Get parameter estimate
  nlsrest <- summary(nlsr)$coefficients
  
  # Store parameter estimate in numeric vector
  respRateEst2[realization] <- nlsrest["respRate", "Estimate"]
  
  # Store model predictions for the estimated parameters
  confidences2[realization,] <- predict(nlsr)
  
  # Calculate the standard deviation of the residuals
  sderr <- sd(synthData - confidences2[realization,])
  
  # Store model predictions with added error to generate prediction intervals 
  predictions2[realization,] <- confidences2[realization,] + rnorm(length(time), mean = 0, sd = sderr)
}

# Calculate the 95% confidence interval of the respiration energy parameter estimate
confidenceResp2 <- quantile(respRateEst2, probs = c(0.025, 0.975))

# Calculate the the range of the 95% confidence interval,
# which is a metric of uncertainty in the paramter estimate.
uncert2 <- confidenceResp2["97.5%"] - confidenceResp2["2.5%"]

############ Standard deviation of Synthetic Error = 0.005 ##################

# Build an empty data frame to store parameter estimates
# from each realization 
respRateEst3 <- numeric(totalRealizations)

# Build an empty matrix to store model predictions
# for each realization
confidences3 <- matrix(nrow = totalRealizations, 
                       ncol = length(time))

# Build an empty matrix to store predictions
predictions3 <- matrix(nrow = totalRealizations, 
                       ncol = length(time))


# Iterate through realizations for a standard deviation in synthetic error of 0.005
for(realization in 21:totalRealizations){
  # Add normally distributed error to generate synthetic data
  synthData <- addSynthError(knownModelDO, sderr = 0.005)
  
  # Estimate parameters by minimizing sum of squared residuals
  nlsr <- nls(
    formula = synthData ~ gangstaIterate(gangstaModel = aerobicHetwAssimModel, 
                                         drivingValues = data.frame(row.names = as.character(time)),
                                         lpModel = read.lp(file = "./AHwUptake.lp", verbose = "normal"),
                                         respRate = respRate,
                                         findConstraintRowAndColumn = findConstraintRowAndColumn
    ),
    start = list(respRate = 2.83e-06)
  )
  
  # Get parameter estimate
  nlsrest <- summary(nlsr)$coefficients
  
  # Store parameter estimate in numeric vector
  respRateEst3[realization] <- nlsrest["respRate", "Estimate"]
  
  # Store model predictions for the estimated parameters
  confidences3[realization,] <- predict(nlsr)
  
  # Calculate the standard deviation of the residuals
  sderr <- sd(synthData - confidences3[realization,])
  
  # Store model predictions with added error to generate prediction intervals 
  predictions3[realization,] <- confidences3[realization,] + rnorm(length(time), mean = 0, sd = sderr)
}

# Calculate the 95% confidence interval of the respiration energy parameter estimate
confidenceResp3 <- quantile(respRateEst3, probs = c(0.025, 0.975))

# Calculate the the range of the 95% confidence interval,
# which is a metric of uncertainty in the paramter estimate.
uncert3 <- confidenceResp3["97.5%"] - confidenceResp3["2.5%"]


############ Plots ##################
# Plot Probabilty Densities and 95% confidence limits
quartz(width = 8, height = 5)
par(mai = c(1,1,0.5,1.8),
    oma = c(0, 0, 0, 0),
    xpd=F)
plot(density(respRateEst1), 
     xlab = "Estimated Maintenance Respiration rate (kJ per umol biomass per hour)", 
     las = 1, 
     main = "Respiration Rate Probability Density \n Known Respiration Rate = 2.8e-06, Sum of Squares Objective",
     col = "darkorchid4",
     xlim = c(2.0e-06, 3.5e-06),
     ylim = c(0,7e06)
)
confidenceResp1 <- quantile(respRateEst1, probs = c(0.025, 0.975))
abline(v = confidenceResp1["2.5%"], lty = "dashed", col = "darkorchid4")
abline(v = confidenceResp1["97.5%"], lty = "dashed", col = "darkorchid4")

lines(density(respRateEst2), 
      col = "deepskyblue")
confidenceResp2 <- quantile(respRateEst2, probs = c(0.025, 0.975))
abline(v = confidenceResp2["2.5%"], lty = "dashed", col = "deepskyblue")
abline(v = confidenceResp2["97.5%"], lty = "dashed", col = "deepskyblue")

lines(density(respRateEst3), 
      col = "firebrick1")
confidenceResp3 <- quantile(respRateEst3, probs = c(0.025, 0.975))
abline(v = confidenceResp3["2.5%"], lty = "dashed", col = "firebrick1")
abline(v = confidenceResp3["97.5%"], lty = "dashed", col = "firebrick1")

par(xpd = T)
legend("right",
       legend = c("0.001", "0.003","0.005"),
       col = c("darkorchid4", "deepskyblue", "firebrick1"),
       lty = 1,
       title = "Standard Deviation",
       inset = -0.32)

# Plot Uncertainty vs. Synthetic Error Standard Deviation
quartz(width = 8, height = 5)
plot(x = c(0.001,0.003,0.005),
     y = c(uncert1,uncert2,uncert3),
     type = "point",
     ylab = "Parameter Estimate Uncertainty",
     xlab = "Synthetic Error Standard Deviation",
     pch = 19,
     ylim = c(min(c(uncert1,uncert2,uncert3)),
              max(c(uncert1,uncert2,uncert3))),
     col = "darkseagreen2")


# Plot the known model
quartz(width = 8, height = 5)
par(mai = c(1,1,0.25,0.25),
    oma = c(0, 0, 0, 0),
    xpd=F)
plot(x = c(0, time+1),
     y = c(10,knownModelDO),
     xlab = "Time (h)",
     ylab = "Dissolved Oxygen (umols)",
     xlim = c(2,4),
     ylim = c(9.2,9.7),
     type = "l")

# Calculate and plot the 95% condfidence and prediction intervals 
# where the standard deviation of synthetic error was 0.005
confidenceInt3 <- apply(
  X = confidences3,
  MARGIN = 2,
  quantile,
  probs=c(0.025, 0.5, 0.975)
);
lines(x = time+1,
      y = confidenceInt3["2.5%",],
      col = "red",
      lty = "dashed")
lines(x = time+1,
      y = confidenceInt3["97.5%",],
      col = "red",
      lty = "dashed")


predictionInt3 <- apply(
  X = predictions3,
  MARGIN = 2,
  quantile,
  probs=c(0.025, 0.5, 0.975)
);
lines(x = time+1,
      y = predictionInt3["2.5%",],
      col = "blue",
      lty = "dashed")
lines(x = time+1,
      y = predictionInt3["97.5%",],
      col = "blue",
      lty = "dashed")


legend("topright",
       legend = c("Known Model", "95% Confidence \n Interval", "95% Prediction  \n Interval"),
       col = c("black","red","blue"),
       lty = c("solid", "dashed","dashed"))



