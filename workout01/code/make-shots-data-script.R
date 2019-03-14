#make-shots-data-script
#use data from a csv to compile data of made shots 
#inputs: CSV files of GSW player data
#outputs: a csv file with graph visualization variables
library(readr)
library(dplyr)

iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)

iguodala$name <- "Andre Iguodala"
green$name <- "Draymond Green"
durant$name <- "Kevin Durant"
thompson$name <- "Klay Thompson"
curry$name <- "Stephen Curry"

iguodala$shot_made_flag[iguodala$shot_made_flag =='n'] <- 'shot_no'
iguodala$shot_made_flag[iguodala$shot_made_flag =='y'] <- 'shot_yes'

green$shot_made_flag[green$shot_made_flag =='n'] <- 'shot_no'
green$shot_made_flag[green$shot_made_flag =='y'] <- 'shot_yes'

durant$shot_made_flag[durant$shot_made_flag =='n'] <- 'shot_no'
durant$shot_made_flag[durant$shot_made_flag =='y'] <- 'shot_yes'

thompson$shot_made_flag[thompson$shot_made_flag =='n'] <- 'shot_no'
thompson$shot_made_flag[thompson$shot_made_flag =='y'] <- 'shot_yes'

curry$shot_made_flag[curry$shot_made_flag =='n'] <- 'shot_no'
curry$shot_made_flag[curry$shot_made_flag =='y'] <- 'shot_yes'

iguodala$minute <- (12 * (iguodala$period-1)) + (12 - iguodala$minutes_remaining)
green$minute <- (12 * (green$period-1)) + (12 - green$minutes_remaining)
durant$minute <- (12 * (durant$period-1)) + (12 - durant$minutes_remaining)
thompson$minute <- (12 * (thompson$period-1)) + (12 - thompson$minutes_remaining)
curry$minute <- (12 * (curry$period-1)) + (12 - curry$minutes_remaining)

sink("../output/anre-iguodala-summary.txt")
summary(iguodala)
sink()

sink("../output/draymond-green-summary.txt")
summary(green)
sink()

sink("../output/kevin-durant-summary.txt")
summary(durant)
sink()

sink("../output/klay-thompson-summary.txt")
summary(thompson)
sink()

sink("../output/stephen-curry-summary.txt")
summary(curry)
sink()

all_shots <- rbind(iguodala, green, durant, thompson, curry)

tbl_df(all_shots)

save(all_shots, file = "../data/shots-data.csv")

sink("../output/shots-data-summary.txt")
summary(all_shots)
sink()

save.image("../code/shots-data-tables.RData")

two_point_shooting <- all_shots %>%
  group_by(name) %>%
  filter(shot_type == '2PT Field Goal') %>%
  summarise(total = n(),
            two_made = sum(shot_made_flag == 'shot_yes'),
            three_made = 0,
            two_perc_made = two_made/total,
            three_perc_made = 0) %>%
  arrange(desc(two_perc_made))

three_point_shooting <- all_shots %>% 
  group_by(name) %>%
  filter(shot_type == '3PT Field Goal') %>%
  summarise(total = n(),
            two_made = 0,
            three_made = sum(shot_made_flag == 'shot_yes'),
            two_perc_made = 0,
            three_perc_made = three_made/total) %>%
  arrange(desc(three_perc_made))

both <- rbind(two_point_shooting, three_point_shooting)

effective_shooting<- both %>%
  group_by(name) %>%
  summarise(total_shots = sum(total),
            two_made = sum(two_made),
            three_made = sum(three_made),
            total_made = two_made + three_made,
            perc_made = total_made/total_shots,
            eperc_made = (two_made + 1.5*three_made)/total_shots) %>%
  arrange(desc(eperc_made))

sink("../data/effective_shooting_table.csv")
effective_shooting
sink()

kevin_klay_andre <- filter(effective_shooting, name == "Kevin Durant" | name == "Andre Iguodala" | name == "Klay Thompson")

sink("../data/kevin_klay_andre.csv")
kevin_klay_andre
sink()

steph_draymond <- filter(effective_shooting, name == "Stephen Curry" | name == "Draymond Green")

sink("../data/steph_draymond.csv")
steph_draymond
sink()


