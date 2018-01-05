# Import libraries
library(ggplot2)
library(dplyr)

# Load dataframe
df <- read.csv("sanctions.csv")

# Changing blanks to NAs
df[df==""] <- NA

# Creating new variables
df["Outcome for Sender"] <- as.factor(ifelse(df$settlementnaturesender>=7, "Good", 
                                             ifelse(df$settlementnaturesender<=4, "Poor", "Average")))
df["Outcome for Target"] <- as.factor(ifelse(df$settlementnaturetarget>=7, "Good", 
                                             ifelse(df$settlementnaturetarget<=4, "Poor", "Average")))
df["Sender Commitment"] <- as.factor(ifelse(df$scommit==1, "Weak", ifelse(df$scommit==2, "Moderate", "Strong")))
df["Length [in years]"] <- df$endyear - df$startyear
df["Length of Sactions"] <- as.factor(ifelse(df$`Length [in years]`<5, "< 5 years", ">= 5 years"))
# Sorting the factors
df$`Outcome for Sender` <- factor(df$`Outcome for Sender`, levels = c("Poor", "Average", "Good"))
df$`Outcome for Target` <- factor(df$`Outcome for Target`, levels = c("Poor", "Average", "Good"))
df$`Sender Commitment` <- factor(df$`Sender Commitment`, levels = c("Strong", "Moderate", "Weak"))
df$`Length of Sactions` <- factor(df$`Length of Sactions`, levels = c("< 5 years", ">= 5 years"))

# Length of sanctions
# Mean
sanction_mean <- mean(df$`Length [in years]`, na.rm = TRUE)
# Median
sanction_median <- median(df$`Length [in years]`, na.rm = TRUE)
# Standard Deviation
sanction_sd <- sd(df$`Length [in years]`, na.rm = TRUE)

# Print values
sanction_mean
sanction_median
sanction_sd

# Number of studies with length != NA
n <- length(na.omit(df$`Length [in years]`))

# Stat count of sanction lengths
sanction_length <- ggplot(df) +
  theme_bw() +
  stat_count(aes(x=`Length [in years]`), fill = "darkblue") +
  labs(title="Duration of Sanctions", subtitle="n = 961", x="Years", y="Case Studies", 
       caption = "Data from https://www.unc.edu/~bapat/TIES.htm")

# Print stat count
sanction_length

# Filter different lengths of sanctions
short_sanctions <- filter(df, df$`Length of Sactions`=="< 5 years")
long_sanctions <- filter(df, df$`Length of Sactions`==">= 5 years")

# Plot Sender Outcome for short sanctions, color coded with the commitment
outcome_short <- ggplot(short_sanctions) + 
  stat_count(aes(x=`Outcome for Sender`, fill=`Sender Commitment`)) +
  theme_bw() +
  labs(title="Sender Outcome by Commitment", subtitle="for sanctions shorter than 5 years", 
       x="Outcome for Sender", y="Case Studies", caption = "Data from https://www.unc.edu/~bapat/TIES.htm")

# Print it
outcome_short

# Plot Sender Outcome for long sanctions, color coded with the commitment
outcome_long <- ggplot(long_sanctions) + 
  stat_count(aes(x=`Outcome for Sender`, fill=`Sender Commitment`)) +
  theme_bw() +
  labs(title="Sender Outcome by Commitment", subtitle="for sanctions longer than or equal to 5 years", 
       x="Outcome for Sender", y="Case Studies", caption = "Data from https://www.unc.edu/~bapat/TIES.htm")

# Print it
outcome_long

# Print summary of linear model (outcome to length)
summary(lm(df$settlementnaturesender~df$`Length [in years]`))

# Print t-test comparing outcomes for short sanctions and outcomes for long sanctions
t.test(subset(df, df$`Length [in years]`<5)$settlementnaturesender, subset(df, df$`Length [in years]`>=5)$settlementnaturesender)
