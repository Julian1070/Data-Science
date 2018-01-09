library(dplyr)
library(Matching)
library(randomForest)
library(rbounds)
load("/Users/Julian/Downloads/data.Rdata")

# Clean up data (exclude NAs and convert years:months to months)
mydata <- na.omit(x)
split <- t(data.frame(strsplit(mydata$yrs.mths, split = ':')))
mydata$yrs.mths <- as.numeric(split[,1])*12 + as.numeric(split[,2])
colnames(mydata)[2] <- "months"

# Divide the data into the three different groups (SLI = Specific Language Impairment, LLF = Low Language Functioning, T = Typical)
SLI <- filter(mydata, jcppgroup == "S")
LLF <- filter(mydata, jcppgroup == "LLF")
Typ <- filter(mydata, jcppgroup == "T")

# Calculate means and sd of the three groups
SLImean <- apply(SLI[,-1], 2, mean)
SLIsd <- apply(SLI[,-1], 2, sd)
LLFmean <- apply(LLF[,-1], 2, mean)
LLFsd <- apply(LLF[,-1], 2, sd)
Typmean <- apply(Typ[,-1], 2, mean)
Typsd <- apply(Typ[,-1], 2, sd)

tableMEANSD <- cbind(SLImean, SLIsd, LLFmean, LLFsd, Typmean, Typsd)
colnames(tableMEANSD)<-c("SLI_mean","SLI_sd","LLF_mean","LLF_sd","Typical_mean","Typical_sd")
rownames(tableMEANSD)<-c("Age in months","BAS-II verbal mental age (in months)","BAS-II non-verbal mental age (in months)",
                         "Recalling sentences (raw score)","Formulated sentences (raw score)","Word classes receptive (raw score)",
                         "Word classes expressive (raw score)","WMTBC-listening recall (raw score)","Odd-one-out total correct",
                         "Verbal fluency (log)","Design fluency (sqrt)","D-KEFS-Sorting task verbal (raw correct)",
                         "D-KEFS-Sorting task non-verbal (raw correct)","Vimi verbal (sqrt)","Vimi non-verbal (log)","Switch cost (log)",
                         "IED-Total errors","Dummy 1", "Dummy 2")

# calculating p-values
ttest_SLI_Typ <- c()
for (x in 2:length(SLI)){
  ttest_SLI_Typ <- c(ttest_SLI_Typ, t.test(SLI[,x], Typ[,x])[3])
}

ttest_SLI_LLF <- c()
for (x in 2:length(SLI)){
  ttest_SLI_LLF <- c(ttest_SLI_LLF, t.test(SLI[,x], LLF[,x])[3])
}

ttest_LLF_Typ <- c()
for (x in 2:length(LLF)){
  ttest_LLF_Typ <- c(ttest_LLF_Typ, t.test(LLF[,x], Typ[,x])[3])
}

# making table for p-values
frame <- data.frame(round(as.numeric(ttest_SLI_Typ), digits = 5), round(as.numeric(ttest_SLI_LLF[-18]), digits = 5), 
                    round(as.numeric(ttest_LLF_Typ), digits = 5))
colnames(frame)<-c("SLI & Typ", "SLI & LLF", "LLF & Typ")
rownames(frame)<-c("Age in months","BAS-II verbal mental age (in months)","BAS-II non-verbal mental age (in months)",
                   "Recalling sentences (raw score)","Formulated sentences (raw score)","Word classes receptive (raw score)",
                   "Word classes expressive (raw score)","WMTBC-listening recall (raw score)","Odd-one-out total correct",
                   "Verbal fluency (log)","Design fluency (sqrt)","D-KEFS-Sorting task verbal (raw correct)",
                   "D-KEFS-Sorting task non-verbal (raw correct)","Vimi verbal (sqrt)","Vimi non-verbal (log)","Switch cost (log)",
                   "IED-Total errors")
frame

# Matching
mydata["Has_SLI"] <- ifelse(mydata$jcppgroup == "S", 1, 0)
propmatch <- glm(Has_SLI ~ months + bas.vma + bas.nvma, family = binomial, data = mydata)
X=propmatch$fitted
Tr=mydata$Has_SLI
match_rs.raw <- Match(Y=mydata$rs.raw, Tr=Tr, X=X, M=1)
balance <- MatchBalance(Has_SLI ~ months + bas.vma + bas.nvma, data = mydata, match.out = match_rs.raw, nboots = 200)

propmatch2 <- glm(Has_SLI ~ months, family = binomial, data = mydata)
X2=propmatch2$fitted
Tr2=mydata$Has_SLI
match_rs.raw2 <- Match(Y=mydata$rs.raw, Tr=Tr2, X=X2, M=1)
balance2 <- MatchBalance(Has_SLI ~ months, data = mydata, match.out = match_rs.raw, nboots = 200)

genmatch <- GenMatch(Tr=Tr, X=X, M=1, pop.size = 5000)
genmatch_rs.raw <- Match(Y=mydata$rs.raw, Tr=Tr, X=X, M=1, Weight.matrix = genmatch$Weight.matrix)
genmatchbal <- MatchBalance(Has_SLI ~ months + bas.vma + bas.nvma, data = mydata, match.out = genmatch_rs.raw, nboots = 2000)

sensitivity_rs.raw <- psens(genmatch_rs.raw, Gamma = 3, GammaInc = .1)

# random forest to find decisive variables
mydata["Has_SLI"] <- ifelse(mydata$jcppgroup == "S", 1, 0)
forest <- randomForest(Has_SLI ~ months + bas.vma + bas.nvma + rs.raw + fs.raw + wc.rec.raw + wc.exp.raw + wmtbc.lr.raw + ooo.total + 
                         log.verbal.fluency + sqrt.design.fluency + dkef.st.v.raw + dkef.st.nv.raw + sqrt.vimi.V + log.vimi.NV + 
                         log.TM.switch.cost + ied.tot.err, data = mydata, importance = TRUE, ntrees = 2000)
varImpPlot(forest)
forest2 <- randomForest(Has_SLI ~ bas.vma + bas.nvma + rs.raw + fs.raw + wmtbc.lr.raw + ooo.total + log.verbal.fluency + 
                          sqrt.design.fluency + dkef.st.v.raw + dkef.st.nv.raw + sqrt.vimi.V + log.vimi.NV + log.TM.switch.cost + 
                          ied.tot.err, data = mydata, importance = TRUE, ntrees = 2000)
varImpPlot(forest2)

