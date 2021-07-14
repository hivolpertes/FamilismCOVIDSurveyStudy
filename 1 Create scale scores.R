library(tidyverse)
library(psych)

# read in data
# make sure the path is correct
Clean_Data = read.delim("./FamCOVID_clean.txt")

# Depression
Clean_Data = Clean_Data %>% 
  mutate(PHQ_sum = rowSums(select(., PHQ_1, PHQ_2, PHQ_3, PHQ_4, PHQ_5 , PHQ_6 , PHQ_7 , PHQ_8), na.rm = TRUE))
#View(select(Clean_Data, PHQ_1:PHQ_8, PHQ_sum))
alpha(select(Clean_Data, PHQ_1:PHQ_8))

# Anxiety
Clean_Data = Clean_Data %>% 
  mutate(GAD_sum = rowSums(select(., GAD_1, GAD_2, GAD_3, GAD_4, GAD_5, GAD_6, GAD_7), na.rm = TRUE))
#View(select(Clean_Data, GAD_1:GAD_7, GAD_sum))
alpha(select(Clean_Data, GAD_1:GAD_7))

# Loneliness
Clean_Data = Clean_Data %>% 
  mutate(Loneliness_sum = rowSums(select(., Loneliness_1, Loneliness_2, Loneliness_3, Loneliness_4, 
                                         Loneliness_5, Loneliness_6, Loneliness_7, Loneliness_8, Loneliness_9, 
                                         Loneliness_10, Loneliness_11, Loneliness_12, Loneliness_13, Loneliness_14, 
                                         Loneliness_15, Loneliness_16, Loneliness_17, Loneliness_18, Loneliness_19, 
                                         Loneliness_20), na.rm = TRUE))
#View(select(Clean_Data, Loneliness_1:Loneliness_20, Loneliness_sum))

# Mexican American Culture Values
# sum of all scales / "family values"
Clean_Data = Clean_Data %>% 
  mutate(MACVS_sum = rowSums(select(., MACVS1_1, MACVS1_2, MACVS1_3, MACVS1_4, MACVS1_5, MACVS1_6, 
                                    MACVS1_7, MACVS1_8, MACVS1_9, MACVS1_10, MACVS1_11, MACVS1_12, 
                                    MACVS1_13, MACVS1_14, MACVS2_1, MACVS2_2, MACVS2_3, MACVS2_4, 
                                    MACVS2_5, MACVS2_6, MACVS2_7, MACVS2_8, MACVS2_9, MACVS2_10, 
                                    MACVS2_11, MACVS2_12, MACVS2_13, MACVS2_14), na.rm = TRUE))
#View(select(Clean_Data, MACVS1_1:MACVS2_14, MACVS_sum))

# support subscale 
Clean_Data = Clean_Data %>% 
  mutate(MACVS_support_sum = rowSums(select(., MACVS1_1, MACVS1_6, MACVS1_12, MACVS2_2, MACVS2_8, MACVS2_13),
                                     na.rm = T))
alpha(select(Clean_Data, MACVS1_1, MACVS1_6, MACVS1_12, MACVS2_2, MACVS2_8, MACVS2_13))

# Obligation subscale
Clean_Data = Clean_Data %>% 
  mutate(MACVS_obligation_sum = rowSums(select(., MACVS1_2, MACVS1_2 , MACVS1_7, MACVS1_13, MACVS2_3, MACVS2_9),
                                      na.rm = T))
alpha(select(Clean_Data, MACVS1_2, MACVS1_2 , MACVS1_7, MACVS1_13, MACVS2_3, MACVS2_9))


# referent subscale 
Clean_Data = Clean_Data %>% 
  mutate(MACVS_referent_sum = rowSums(select(., MACVS1_3, MACVS1_8, MACVS2_4, MACVS2_10, MACVS2_14),
                                      na.rm = T))
alpha(select(Clean_Data, MACVS1_3, MACVS1_8, MACVS2_4, MACVS2_10, MACVS2_14))

# Ethnic Idenity 
# all scores 
Clean_Data = Clean_Data %>%
  mutate(MEIM_sum = rowSums(select(., MEIM_1, MEIM_2, MEIM_3, MEIM_4, MEIM_5, MEIM_6),
                            na.rm = T))
# exploration subscale
Clean_Data = Clean_Data %>%
  mutate(MEIM_exp_sum = rowSums(select(., MEIM_1, MEIM_4, MEIM_5),
                                na.rm = T))
# commitment subscale 
Clean_Data = Clean_Data %>%
  mutate(MEIM_com_sum = rowSums(select(., MEIM_2, MEIM_3, MEIM_6),
                                na.rm = T))

# Discrimination
# total sum
Clean_Data = Clean_Data %>% 
  mutate(discrimination_sum = rowSums(select(., ExpInpers_1, ExpInpers_2, ExpOnline_1, ExpOnline_2, 
                                             VicInpers_1, VicInpers_2, VicOnline_1 , VicOnline_2, 
                                             Rum_1, Rum_2), na.rm = T))

# interpersonal subscale
Clean_Data = Clean_Data %>% 
  mutate(interpersonal_sum = rowSums(select(., ExpInpers_1, ExpInpers_2, ExpOnline_1, ExpOnline_2),
                                     na.rm = T))
### change scores (positive means increase as a result of pandemic)
Clean_Data = mutate(Clean_Data,
                    ExpInpers_change = ExpInpers_2 - ExpInpers_1)
Clean_Data = mutate(Clean_Data,
                    ExpOnline_change = ExpOnline_2 - ExpOnline_1)

# vicarious subscale
Clean_Data = Clean_Data %>% 
  mutate(vicarious_sum = rowSums(select(., VicInpers_1, VicInpers_2, VicOnline_1 , VicOnline_2), na.rm = T))
### change scores (positive means increase as a result of pandemic)
Clean_Data = mutate(Clean_Data, 
                    VicInpers_change = VicInpers_2 - VicInpers_1)
Clean_Data = mutate(Clean_Data, 
                    VicOnline_change = VicOnline_2 - VicOnline_1)

# interpersonal subscale
Clean_Data = Clean_Data %>% 
  mutate(rum_sum = rowSums(select(., Rum_1, Rum_2), na.rm = T))
### change scores (positive means increase as a result of pandemic)
Clean_Data = mutate(Clean_Data,
                    rum_change = Rum_2 - Rum_1)

# overall discrim (before and after)
Clean_Data = Clean_Data %>% 
  mutate(discrim_before = rowSums(select(., ExpInpers_1, ExpOnline_1, VicInpers_1, VicOnline_1, Rum_1)),
         discrim_after = rowSums(select(., ExpInpers_2, ExpOnline_2, VicInpers_2, VicOnline_2, Rum_2)))

# family change variables

unique(Clean_Data$FamChange_2)
unique(Clean_Data$FamChange_2.1)

# create new family change variable that aggregates
# -1 = negative change
# 0 = no change
# 1 = positive change

# agg = 0 when FamChange_2 is 0 OR FamChange_2 is 1 and FamChange_2.1 is 6
# agg = -1 when FamChange_2 is 1 AND FamChange_2.1 is 5
# agg = 1 when FamChange_2 is 1 AND FamChange_2.1 is 4

Clean_Data$FamChange_agg[Clean_Data$FamChange_2 == 0] = 0 
Clean_Data$FamChange_agg[Clean_Data$FamChange_2 == 1 & Clean_Data$FamChange_2.1 == 5] = -1
Clean_Data$FamChange_agg[Clean_Data$FamChange_2 == 1 & Clean_Data$FamChange_2.1 == 4] = 1
Clean_Data$FamChange_agg[Clean_Data$FamChange_2 == 1 & Clean_Data$FamChange_2.1 == 6] = 0

unique(Clean_Data$FamChange_agg)

count = count(Clean_Data, FamChange_agg)
count$percent = count$n/sum(count$n)
count

# create new data set

write.table(Clean_Data, "./FamCOVID_clean_summedscores.txt", sep="\t", row.names=F)


