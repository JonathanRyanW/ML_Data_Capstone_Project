#Information about this data can be read in the readme file

#Reading the data and getting some feel to it
batting <- read.csv("./data/Batting.csv")

head(batting)
str(batting)
head(batting$AB)
head(batting$X2B)

#Creating Batting Average variable
batting$BA <- batting$H / batting$AB

#Creating On Base Percentage variable
batting$OBP <- (batting$H + batting$BB + batting$HBP) /
  (batting$AB + batting$BB + batting$HBP + batting$SF)

#Creating Singles variable
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

#Creating Slugging variable
batting$SLG <- (batting$X1B + 2 * batting$X2B + 3 * batting$X3B + 4 * batting$HR) / batting$AB

str(batting)
summary(batting)

#Taking only batting data from 1985
batting <- filter(batting, yearID >= 1985)
summary(batting)

#Reading the salary data
salary <- read.csv("./data/Salaries.csv")

#Merging the batting data and salary data
combo <- merge(batting, salary, c("playerID","yearID"))
summary(combo)

#Finding the data of the lost players
lost <- c("giambja01", "damonjo01", "saenzol01")
lost_data <- filter(combo, playerID == lost[1] | playerID == lost[2] | playerID == lost[3])

#Pake operator %in% lebih cepet daripada banyak OR
lost_data <- filter(combo, playerID %in% lost)

#Taking only 2001 data
lost_data <- filter(lost_data, yearID == 2001)

#Taking only the playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB columns
lost_data <- select(lost_data, playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB)

#Requirements
require <-
"The total combined salary of the three players can not exceed 15 million dollars.
Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
Their mean OBP had to equal to or greater than the mean OBP of the lost players"

ab_lost <- sum(lost_data$AB)
obp_lost <- sum(lost_data$OBP)

#Filtering and rearranging the available players data
combo_2001 <- filter(combo, yearID == 2001)
combo_2001 <- select(combo_2001, playerID, AB, OBP, salary)
combo_2001 <- arrange(combo_2001, desc(salary), desc(OBP), desc(AB))

"After looking at the data, we can pick 3 players without using any more code.
I picked cirilje01, gonzalu01, and surhobj01. These 3 players have met all the
requirements."

#Checking the requirements. Requirements met.
player <- c("cirilje01", "gonzalu01", "surhobj01")
picked <- filter(combo_2001, playerID %in% player)
sapply(picked[,2:4], sum)

