library(readr)
# bias <- read_csv("merged.csv")
bias <- read_csv("merged.csv")
bias$Environment_Work <- (bias$Environment_Work - min(bias$Environment_Work)) / (max(bias$Environment_Work) - min(bias$Environment_Work))
bias$Environment <- (bias$Environment - min(bias$Environment)) / (max(bias$Environment) - min(bias$Environment))
bias$Usability_VR <- (bias$Usability_VR - min(bias$Usability_VR)) / (max(bias$Usability_VR) - min(bias$Usability_VR))

catdog.avg["relationship"] = "cat/dog"
catdogmouse.avg["relationship"] = "mouse"
merged_catdogmouse = rbind(catdog.avg, catdogmouse.avg)

ggplot(merged_catdogmouse, aes(x=Condition, y=Connectedness, fill=relationship)) +
    geom_bar(stat="identity", position=position_dodge()) 

ggplot(data=merged_catdogmouse, aes(x=Condition, y=Info_Exchange, fill=relationship)) +
    geom_bar(position = "dodge", stat = "summary", fun.y = "mean")

ggplot(data=bias, aes(x=Condition, y=Info_Exchange)) +
    geom_bar(position = "dodge", stat = "summary", fun.y = "mean")

#summary(aov(Proximity_Bias ~ Condition, data=bias))

bias$Info_Exchange

bias.regress <- lm(bias$Proximity_Bias ~ 
                       bias$Comm_Effectiveness + 
                       bias$Info_Exchange + 
                       bias$Social_Cohesion + 
                       bias$Satisfaction + 
                       bias$Trust + 
                       bias$Subj_Connection + 
                       bias$Viability + 
                       bias$Psych_Safety + 
                       as.factor(bias$Condition) +
                       bias$Usability_VR
                       #bias$Environment_Work + 
                       #bias$Environment + 
                       
                   )
# Ra = .7161
summary(bias.regress)
vif(bias.regress)

plot(y=bias$Proximity_Bias, x=bias$Info_Exchange)
summary(lm(Proximity_Bias ~ as.factor(Condition), data=bias))

bias.reduced <- lm(bias$Proximity_Bias ~ 
                       bias$Comm_Effectiveness + 
                       bias$Info_Exchange + 
                       bias$Subj_Connection + 
                       bias$Viability + 
                       bias$Psych_Safety + 
                       as.factor(bias$Condition) +
                       bias$Usability_VR
                   )
# Ra = .7709
summary(bias.reduced)
# Variance inflation, ANOVA test on gender (same vs different gender)


#for (x in c("Comm_Effectiveness","Info_Exchange","Subj_Connection","Viability","Psych_Safety"))

ggplot(merged_catdogmouse, aes(x=Condition, y=Comm_Effectiveness, fill=relationship)) +
    geom_bar(stat="identity", position=position_dodge()) 

ggplot(merged_catdogmouse, aes(x=Condition, y=Info_Exchange, fill=relationship)) +
    geom_bar(stat="identity", position=position_dodge()) 

ggplot(merged_catdogmouse, aes(x=Condition, y=Subj_Connection, fill=relationship)) +
    geom_bar(stat="identity", position=position_dodge()) 

ggplot(merged_catdogmouse, aes(x=Condition, y=Viability, fill=relationship)) +
    geom_bar(stat="identity", position=position_dodge()) 

ggplot(merged_catdogmouse, aes(x=Condition, y=Psych_Safety, fill=relationship)) +
    geom_bar(stat="identity", position=position_dodge()) 

ggplot(bias, aes(x=Condition, y=Environment)) +
    geom_bar(stat="identity", position=position_dodge()) 

ggplot(bias, aes(x=Condition, y=Environment_Work)) +
    geom_bar(stat="identity", position=position_dodge()) 

ggplot(bias, aes(x=Condition, y=Usability_VR)) +
    geom_bar(stat="identity", position=position_dodge()) 
