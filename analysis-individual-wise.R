library(readr)
bias <- read_csv("./merged_normalized.csv")
View(bias)

bias.full <- lm(Proximity_Bias ~ 
                    Comm_Effectiveness + 
                    Info_Exchange + 
                    Social_Cohesion + 
                    Satisfaction + 
                    Trust + 
                    Subj_Connection + 
                    Viability + 
                    Psych_Safety +
                    as.factor(Condition),
                data = bias)
summary(bias.full)

bias.reduced <- lm(Proximity_Bias ~ 
                    Comm_Effectiveness + 
                    Info_Exchange + 
                    Subj_Connection + 
                    Viability + 
                    Psych_Safety +
                    as.factor(Condition),
                data = bias)
summary(bias.reduced)
