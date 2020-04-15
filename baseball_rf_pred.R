library(dplyr)
library(randomForest)
dat <- read.csv("2010_2019_DET_w_weather.csv")
dat$X <- NULL
dat$DH_Game <- as.factor(dat$DH_Game)

#dat <- select(dat, c("DH_Game", "DOW", "Away", "H_Game_number", "H_Wins","A_Wins","Game_Time","Attendance", "PRCP", "TMAX", "TMIN",WT01,WT03,WT07, WT08, WT16,WT18))
dat$DH_Game <- as.factor(dat$DH_Game)
dat$WT01 <- as.factor(dat$WT01)
dat$WT03 <- as.factor(dat$WT03)
dat$WT07 <- as.factor(dat$WT07)
dat$WT08 <- as.factor(dat$WT08)
dat$WT16 <- as.factor(dat$WT16)
dat$WT18 <- as.factor(dat$WT18)



## 75% of the sample size
smp_size <- floor(0.8 * nrow(dat))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dat)), size = smp_size)

train <-dat[train_ind, ]
test <- dat[-train_ind, ]

rf <- randomForest(Attendance ~.,data = train)
rf

pred_values <- predict(rf,test[-21])
pred_values

agree_tab <- data.frame(test$Attendance, pred_values)
agree_tab$difference <- agree_tab$test.Attendance - agree_tab$pred_values

agree_tab$agreement <- ifelse(abs(agree_tab$difference) <= 1000, TRUE, FALSE)
prop.table(table(agree_tab$agreement))

