# Created by Emily 2/25
# Modified by Hannah 3/1
library(tidyverse)

Clean_Data = read.delim("./FamCOVID_clean_summedscores.txt")
Clean_Data$FamChange_agg = factor(Clean_Data$FamChange_agg)

# Hypothesis Testing
### 1: Family change and mental health outcomes ---------------------------------------------

# depression
m1 = aov(PHQ_sum ~ FamChange_agg, Clean_Data)
summary(m1)
TukeyHSD(m1) #Tukey post-hoc comparison
tapply(Clean_Data$PHQ_sum, Clean_Data$FamChange_agg, mean) # group means

# anxiety
m2 = aov(GAD_sum ~ FamChange_agg, Clean_Data)
summary(m2)
TukeyHSD(m2) #Tukey post-hoc comparison
tapply(Clean_Data$GAD_sum, Clean_Data$FamChange_agg, mean) # group means

### 2: Family change and familism -------------------------------------------------------------

# familism (support)
m4 = aov(MACVS_support_sum ~ FamChange_agg, Clean_Data)
summary(m4)
TukeyHSD(m4) #Tukey post-hoc comparison
tapply(Clean_Data$MACVS_support_sum, Clean_Data$FamChange_agg, mean) # group means


# familism (obligation)
m5 = aov(MACVS_obligation_sum ~ FamChange_agg, Clean_Data)
summary(m5)
TukeyHSD(m5) #Tukey post-hoc comparison
tapply(Clean_Data$MACVS_obligation_sum, Clean_Data$FamChange_agg, mean) # group means


# familism (referent)
m6 = aov(MACVS_referent_sum ~ FamChange_agg, Clean_Data)
summary(m6)
TukeyHSD(m6) #Tukey post-hoc comparison
tapply(Clean_Data$MACVS_referent_sum, Clean_Data$FamChange_agg, mean) # group means

### 3: Familism and mental health outcomes ----------------------------------------------------

# familism (support) and depression
cor.test(Clean_Data$MACVS_support_sum, Clean_Data$GAD_sum)
# familism (support) and anxiety
cor.test(Clean_Data$MACVS_support_sum, Clean_Data$PHQ_sum)

# familism (obligation) and depression
cor.test(Clean_Data$MACVS_obligation_sum, Clean_Data$GAD_sum)
# familism (obligation) and anxiety
cor.test(Clean_Data$MACVS_obligation_sum, Clean_Data$PHQ_sum)

# familism (referent) and depression
cor.test(Clean_Data$MACVS_referent_sum, Clean_Data$GAD_sum)
# familism (referent) and anxiety
cor.test(Clean_Data$MACVS_referent_sum, Clean_Data$PHQ_sum)



