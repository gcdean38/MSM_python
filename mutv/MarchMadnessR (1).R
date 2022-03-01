mmadness <- read.csv("https://raw.githubusercontent.com/gcdean38/MSM_python/main/mutv/MarchMadnessFullData.csv")
install.packages("tidyverse")
install.packages("caret")
install.packages("MASS")
library(tidyverse)
library(caret)        
set.seed(1)
testing <- sample(1:nrow(mmadness),.25*nrow(mmadness))
mm.test <- mmadness[testing, -c(1,2,3,6,7,8,9,10,41,42,44,45,46,47,48)]
mm.train <- mmadness[-testing, -c(1,2,3,6,7,8,9,10,41,42,44,45,46,47,48)]

library(MASS)

#Stepwise
mm_model <- glm(res ~ ., data = mm.train, family = binomial) %>% stepAIC(trace = FALSE)
summary(mm_model)
coef(mm_model)
mm.predict <- predict(mm_model,mm.test)
mm.result = rep("loss",length(mm.predict))
mm.result[mm.predict>0.5] = "win"

mm.cm <- table(mm.test$res, mm.result)
mm.cm
(53+13)/sum(53,13,10,12)

#MODEL 1
mm1 <- glm(res ~ FG + AST+lower_TotalWinPct, data = mm.train, family = binomial)
summary(mm1)
coef(mm1)
mm1.predict <- predict(mm1,mm.test)
mm1.result = rep("loss",length(mm1.predict))
mm1.result[mm1.predict>0.5] = "win"

mm1.cm <- table(mm.test$res, mm1.result)
mm1.cm
(49+5)/sum(49+5+18+16)
sum(49+5+18+16)
#MODEL 2
mm2 <- glm(res ~ FG + AST+lower_TotalWinPct+TotalWinPct+ConfW+AwayWinPct+Opp.+FT.
           +ORB+lower_ConfL+lower_HomeWinPct+lower_FT+lower_STL, data = mm.train, family = binomial)
summary(mm2)
coef(mm2)
mm2.predict <- predict(mm2,mm.test)
mm2.result = rep("loss",length(mm2.predict))
mm2.result[mm2.predict>0.5] = "win"

mm2.cm <- table(mm.test$res, mm2.result)
mm2.cm
(55+12)/sum(55+12+10+11)
sum(55+12+10+11)
#MODEL 3
mm3 <- glm(res ~ FG + AST+lower_TotalWinPct+lower_ConfL, data = mm.train, family = binomial)
summary(mm3)
coef(mm3)
mm3.predict <- predict(mm3,mm.test)
mm3.result = rep("loss",length(mm3.predict))
mm3.result[mm3.predict>0.5] = "win"

mm3.cm <- table(mm.test$res, mm3.result)
mm3.cm
(51+12)/sum(51+12+14+11)

#MODEL 4
mm4 <- glm(res ~ FG + AST+lower_TotalWinPct+lower_ConfL+Opp.+lower_FT, data = mm.train, family = binomial)
summary(mm4)
coef(mm4)
mm4.predict <- predict(mm4,mm.test)
mm4.result = rep("loss",length(mm4.predict))
mm4.result[mm4.predict>0.5] = "win"

mm4.cm <- table(mm.test$res, mm4.result)
mm4.cm

(55+11)/sum(55+11+10+12)
