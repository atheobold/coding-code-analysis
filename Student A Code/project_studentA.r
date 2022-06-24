#upper anterior measurement
anterior <- lm(ProximateAnalysisData$PSUA~ProximateAnalysisData$Lipid)
summary(anterior)
with(ProximateAnalysisData, plot(PSUA ~ Lipid, las = 1))
abline(anterior)
plot(anterior)
posterior <- lm(ProximateAnalysisData$PSUP ~ ProximateAnalysisData$Lipid)


summary(posterior)
posterior
with(ProximateAnalysisData, plot(PSUP ~ Lipid, las = 1))
abline(posterior)
plot(posterior)
#OUTLIER REMOVED 
anterior2 <- lm(ProximateAnalysisDataOutlier$PSUA ~ ProximateAnalysisDataOutlier$Lipid)


summary(anterior2)
with(ProximateAnalysisDataOutlier, plot(PSUA ~ Lipid, las = 1,  xlab = "Whole-body Lipid Content (%)", ylab = "UA Fatmeter Reading"))


abline(anterior2)
plot(anterior2)
anterior2
posterior2 <- lm(ProximateAnalysisDataOutlier$PSUP ~ ProximateAnalysisDataOutlier$Lipid)
summary(posterior2)
with(ProximateAnalysisDataOutlier, plot(PSUP ~ Lipid, las = 1,  xlab = "Whole-body Lipid Content (%)", ylab = "UP Fatmeter Reading"))
abline(posterior2)
plot(posterior2)
posterior2
qt(.975,9)
#upper Middle measurements only
middle <- lm(ProximateAnalysisData$PSUM ~ ProximateAnalysisData$Lipid)
summary(middle)
with(ProximateAnalysisData, plot(PSUM ~ Lipid, las = 1, xlab = "Whole-body Lipid Content (%)", ylab = "UM Fatmeter Reading"))
abline(middle)
plot(middle)
middle
middleLog <- lm(ProximateAnalysisData$logPSUM ~ ProximateAnalysisData$Lipid)


summary(middleLog)
with(ProximateAnalysisData, plot(logPSUM ~ Lipid, las = 1))
abline(middleLog)
plot(middle)
with(ProximateAnalysisData, plot(Lipid ~ logPSUM, las = 1))
#Means and sd of data
mean(ProximateAnalysisData$Lipid)
sd(ProximateAnalysisData$Lipid)

