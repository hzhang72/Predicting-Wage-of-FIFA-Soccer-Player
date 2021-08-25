library(tidyverse)
library(car)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(leaps)
library(lubridate)
library(mice)

#######################################

TrainNew <- read.csv("FifaTrainNew.csv")
NoY <- read.csv("FifaNoY.csv")
head(TrainNew)

Train_na <- as.vector(TrainNew$Club[which(is.na(TrainNew$Club))]) 
na <- as.vector(TrainNew$Position[which(is.na(TrainNew$Position))]) 

TrainNew$Club <- as.vector(TrainNew$Club) 
TrainNew$Club[which(is.na(TrainNew$Club))] <- "None"

NoY$Club <- as.vector(NoY$Club) 
NoY$Club[which(is.na(NoY$Club))] <- "None"

club_list <- TrainNew %>% 
  group_by(Club) %>% 
  summarise(mean = mean(WageNew)) %>% 
  arrange(mean)

club_list <- data.frame(club_list, 'row_num'=c(1:652), 'wage'=club_list$mean, 'log_wage'=log(club_list$mean))

plot(club_list$wage)
abline(150000, 0 ,col = 2)
abline(110000, 0 ,col = 1)
abline(92000, 0 ,col = 3)
abline(70000, 0 ,col = 3)
abline(47000, 0 ,col = 4)
abline(32000, 0 ,col = 5)
abline(16000, 0 ,col = 6)
abline(8000, 0 ,col = 7)
abline(1000, 0, col = 8)

my_group= rep(" ", nrow(TrainNew))

Group_one <-club_list$Club[which(club_list$wage <= 1000)]
Group_two <-club_list$Club[which(club_list$wage > 1000 & club_list$wage <= 8000)]
Group_three <-club_list$Club[which(club_list$wage > 8000 & club_list$wage <= 16000)]
Group_four <- club_list$Club[which(club_list$wage > 16000 & club_list$wage <= 32000)]
Group_five <- club_list$Club[which(club_list$wage > 32000 & club_list$wage <= 47000)]
Group_six <- club_list$Club[which(club_list$wage > 47000 & club_list$wage <= 90000)]
Group_seven <- club_list$Club[which(club_list$wage > 90000 & club_list$wage <= 150000)]
Group_eight <-club_list$Club[which(club_list$wage > 150000)]

for(i in 1:nrow(TrainNew)){
  if (TrainNew$Club[i] %in% Group_one){
    my_group[i] <- 1
  } else if (TrainNew$Club[i] %in% Group_two){
    my_group[i] <- 2
  } else if (TrainNew$Club[i] %in% Group_three){
    my_group[i] <- 3
  } else if (TrainNew$Club[i] %in% Group_four){
    my_group[i] <- 4
  } else if (TrainNew$Club[i] %in% Group_five){
    my_group[i] <- 5
  } else if (TrainNew$Club[i] %in% Group_six){
    my_group[i] <- 6
  } else if (TrainNew$Club[i] %in% Group_seven){
    my_group[i] <- 7
  } else if (TrainNew$Club[i] %in% Group_eight){
    my_group[i] <- 8
  }
}

TrainNew$Club_levels <-  as.factor(my_group)

for(i in 1:nrow(NoY)){
  if (NoY$Club[i] %in% Group_one){
    my_group[i] <- 1
  } else if (NoY$Club[i] %in% Group_two){
    my_group[i] <- 2
  } else if (NoY$Club[i] %in% Group_three){
    my_group[i] <- 3
  } else if (NoY$Club[i] %in% Group_four){
    my_group[i] <- 4
  } else if (NoY$Club[i] %in% Group_five){
    my_group[i] <- 5
  } else if (NoY$Club[i] %in% Group_six){
    my_group[i] <- 6
  } else if (NoY$Club[i] %in% Group_seven){
    my_group[i] <- 7
  } else if (NoY$Club[i] %in% Group_eight){
    my_group[i] <- 8
  }
}

NoY$Club_levels <- as.factor(my_group[1:nrow(NoY)])

TrainNew <- TrainNew[TrainNew$WageNew >= 200, ]

TrainNew$age <- ifelse(TrainNew$Age  < 20, "young", "old")
NoY$age <- ifelse(NoY$Age  < 20, "young", "old")

TrainNew$attack <- ifelse(TrainNew$Position == "CAM" | TrainNew$Position == "RAM" | TrainNew$Position == "LAM", "attack", "not_attack")
NoY$attack <- ifelse(NoY$Position == "CAM" | NoY$Position == "RAM" | NoY$Position == "LAM", "attack", "not_attack")

TrainNew$position <- ifelse(TrainNew$Position == "GK", "yes", "no")
NoY$position <- ifelse(NoY$Position == "GK", "yes", "no")

TrainNew$GKSkill <- TrainNew$GKDiving + TrainNew$GKHandling + TrainNew$GKPositioning + TrainNew$GKReflexes + TrainNew$GKKicking
NoY$GKSkill <- NoY$GKDiving + NoY$GKHandling + NoY$GKPositioning + NoY$GKReflexes + NoY$GKKicking

attackSkill <- rep(0, nrow(TrainNew))
for (i in seq(47, 70)) {
  attackSkill  <- attackSkill +  TrainNew[, i]
}
TrainNew$AttackSkill <- attackSkill

attackSkill_noy <- rep(0, nrow(NoY))
for (i in seq(46, 69)) {
  attackSkill_noy  <- attackSkill_noy +  NoY[, i]
}
NoY$AttackSkill <- attackSkill_noy

final_train <- TrainNew %>%
  group_by(Club) %>%
  summarize(mean_wage = mean(WageNew)) %>%
  arrange(mean_wage) 

quantile_wage <- character()
quantile(final_train$mean_wage, c(.6, .7, .90), na.rm = TRUE)
for(i in 1:nrow(final_train)){
  if(final_train$mean_wage[i] < 6447.651){
    quantile_wage[i] <- "below 60%"
  } else if(final_train$mean_wage[i] >6447.651 & final_train$mean_wage[i] < 9553.919){
    quantile_wage[i] <- "60% - 70%"
  } else if(final_train$mean_wage[i] >9553.919 & final_train$mean_wage[i] < 25576.172){
    quantile_wage[i] <- "70% - 90%"
  } else{
    quantile_wage[i] <- "Above 90%"
  }
}

final_train$quantile_wage <- quantile_wage

quantile <- character()
for(i in 1 : nrow(TrainNew)){
  for(j in 1 : nrow(final_train)){
    if(TrainNew$Club[i] %in% final_train$Club[j]){
      quantile[i]<- final_train$quantile_wage[j]
      break
    }
  }
}

quantile_NoY <- character()
for(i in 1 : nrow(NoY)){
  for(j in 1 : nrow(final_train)){
    if(NoY$Club[i] %in% final_train$Club[j]){
      quantile_NoY[i]<- final_train$quantile_wage[j]
      break
    }
  }
}
TrainNew$quantile <- quantile
NoY$quantile <- quantile_NoY

TrainNew$over_95 <- ifelse(TrainNew$quantile == "Above 90%", "yes", "no")
TrainNew$over_70 <- ifelse(TrainNew$quantile == "70% - 90%" | TrainNew$quantile == "Above 95%", "yes", "no")
TrainNew$under_50 <- ifelse(TrainNew$quantile == "below 60%", "yes", "no")

NoY$over_95 <- ifelse(NoY$quantile == "Above 90%", "yes", "no")
NoY$over_70 <- ifelse(NoY$quantile == "70% - 90%" | NoY$quantile == "Above 95%", "yes", "no")
NoY$under_50 <- ifelse(NoY$quantile == "below 60%", "yes", "no")

club_ranks <- numeric()
club_wage <- numeric()
club_logwage <- numeric()

for (i in 1:nrow(TrainNew)){
  club_ranks[i] <-which(final_train == TrainNew[i,"Club"])
  club_wage[i] <- final_train[which(final_train == TrainNew[i,"Club"]), "mean_wage"][[1]]
  club_logwage[i] <- log(final_train[which(final_train == TrainNew[i,"Club"]), "mean_wage"][[1]])
}

TrainNew <- cbind(TrainNew,"ClubRank"=club_ranks)
TrainNew <- cbind(TrainNew,"ClubLogWage"=club_logwage)
TrainNew <- cbind(TrainNew,"ClubWage"=club_wage)

club_ranks_NoY <- numeric()
club_wage_NoY <- numeric()
club_logwage_NoY <- numeric()

for (i in 1:nrow(NoY)){
  club_ranks_NoY[i] <-which(final_train == NoY[i,"Club"])
  club_wage_NoY[i] <- final_train[which(final_train == NoY[i,"Club"]), "mean_wage"][[1]]
  club_logwage_NoY[i] <- log(final_train[which(final_train == NoY[i,"Club"]), "mean_wage"][[1]])
}

NoY <- cbind(NoY,"ClubRank"=club_ranks_NoY)
NoY <- cbind(NoY,"ClubLogWage"=club_logwage_NoY)
NoY <- cbind(NoY,"ClubWage"=club_wage_NoY)

nationality_eng <- ifelse(TrainNew$Nationality == "England", "Yes", "No")
TrainNew <- cbind(TrainNew, nationality_eng)
nationality_eng <- ifelse(NoY$Nationality == "England", "Yes", "No")
NoY <- cbind(NoY, nationality_eng)

TrainNew$Club <- as.factor(TrainNew$Club)
club_list <-club_list[order(club_list$Club),]
for (i in 1:length(levels(TrainNew$Club))) {
  levels(TrainNew$Club)[i] <-  club_list$row_num[i]
}
TrainNew$Club <- as.numeric(as.character(TrainNew$Club))

NoY$Club <- as.factor(NoY$Club)
for (i in 1:length(levels(NoY$Club))) {
  levels(NoY$Club)[i] <-  club_list$row_num[i]
}
NoY$Club <- as.numeric(as.character(NoY$Club))

TrainNew$Weight <- gsub(pattern = "lbs", replacement = "", TrainNew$Weight)
TrainNew$Weight <- as.numeric(as.character(TrainNew$Weight))

summary(powerTransform(cbind(WageNew, Overall, Potential, AttackSkill, Club) ~ 1, data = TrainNew))

char <- lm(log(WageNew) ~ 0 + I(Overall^2.34) + Club_levels + Real.Face + age:I(Potential^0.5) + nationality_eng +  attack:I(AttackSkill^3.23) + I(Club^0.83) + position + over_95 + over_70,  data = TrainNew, weight = Overall**2.34)

char_na <- lm(log(WageNew) ~ 0 + I(Overall^1.6)  + I(Age^-0.61) + Club_levels + I(Club^0.81)+ over_95 + over_70,  data = TrainNew, weight = Overall**1.6)

summary(char)
anova(char)

Newdata <- data.frame(NoY)
WageNew <- predict(char, newdata=Newdata)
WageNew_na <- predict(char_na, newdata=Newdata)
Ob <- 1:length(WageNew)
WageNew <- exp(WageNew)
WageNew_na <- exp(WageNew_na)
a <- which(is.na(WageNew))
WageNew[a] <- WageNew_na[a]
Ob <- data.frame(Ob,"WageNew" = WageNew)

#write.csv(Ob, file = "0320.csv", row.names = F)
