library(readxl)
library(tidyverse)
defense <- read_excel('C:/Users/lisa.mcdaniel/DS202/ds202lab4/cyclonesFootball2019.xlsx',
                      sheet="Defensive")

offense <- read_excel('C:/Users/lisa.mcdaniel/DS202/ds202lab4/cyclonesFootball2019.xlsx',
                      sheet="Offensive")

unique(offense$Name)

bio <- read_excel('C:/Users/lisa.mcdaniel/DS202/ds202lab4/cyclonesFootball2019.xlsx',
                      sheet="Biography")
str(defense)

##Clean the Data
## The player names and opponent names are factors.
def1 <- defense %>%
  mutate(Name = as.factor(Name), 
         Opponent_Opponent = as.factor(Opponent_Opponent))
str(def1)

off1 <- offense %>%
  mutate(Name = as.factor(Name), 
         Opponent_Opponent = as.factor(Opponent_Opponent))
str(off1)

bio1 <-  bio %>%
  mutate(Name = as.factor(Name))
str(bio1)

## All offensive and defensive statistics are numerical. The Weight column in 
##biogrpahy is also numeric. (Optional: Learn to mutate multiple columns 
##systematically using the ?across function in `dplyr)
library(dplyr)
?across

defClean <- def1 %>%
  mutate(across(.cols = (Tackles_Solo:Pass_PB), .fns = (as.numeric)))
str(defClean)

offClean <- off1 %>%
  separate(`Passing_CMP-ATT`, c('Passing_CMP', 'Passing_ATT')) %>%
  mutate(across(.cols = (Rushing_ATT:Passing_INT), .fns = (as.numeric)))
str(offClean)

bio2 <- bio1 %>%
  mutate(Weight = as.numeric(Weight))
str(bio2)

## Change the Height column in biogrpahy into numeric. (Hint: Use a unit that works well)

bioClean <- bio2 %>%
 separate(Height, c('feet', 'inches'), sep = '-') %>%
 mutate(feet = as.numeric(feet), inches = as.numeric(inches)) %>%
 mutate(Height_in = (12*feet + inches)) %>%
 select(-feet, -inches)

str(defClean)
str(offClean)
str(bioClean)

##Reformat the defClean data frame into a tidy format using pivot_longer. 
##The type of statistic (Tackles_Solo, Tackles_ASST, etc) is added as a new key
##column named stat.

defClean1 <- defClean %>%
  pivot_longer(`Tackles_Solo`:`Pass_PB`, names_to='Def_Skill', values_to='Stat')

head(defClean1)

##Compare the distributions of the defensive statistics. What defensive skills
##are rare?

skill <- c("Turnover_INT", "Turnover_FR", "Turnover_FF", "Tackles_Sack", 
           "Pass_QBH", "Pass_PB", "Tackles_TFL", "Tackles_ASST", "Tackles_Solo")

ggplot(defClean1, aes(x=Def_Skill, y=Stat, fill=Def_Skill)) + 
  geom_bar(stat= 'identity') + scale_x_discrete(limits = skill) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("Count") + 
  xlab("Defensive Skill")

##Did ISU have better defense against Iowa or Notre Dame? Answer this question
##by creating a scatterplot with x- and y-axes being the number of solo tackle
##(of each player). A large number of solo tackles is an indicator of good 
##defense.

?geom_point

defClean2 <- defClean1 %>%
  filter(Opponent_Opponent == c("Iowa", "Notre Dame"), 
         Def_Skill == "Tackles_Solo")

ggplot(data=defClean2, aes(y=Stat, x=Name, color=Opponent_Opponent)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90)) + 
  ylab("Solo Tackles") + xlab('Tackler') + labs(color="Opponent")

##Separate the Hometown column into two columns, namely the city and the state. 
##Print the first few rows of these two columns. (Hint: look at the sep= 
##argument of separate)

bioClean1 <- bioClean %>%
  separate(Hometown, c('City', 'State'), sep = ',')

head(bioClean1)
str(bioClean1)
head(bioClean1[, c("City", "State")], n=5)
  
##How many players are there from each state?
?count

str(bioClean2)

St1 <- bioClean1 %>%
  select(Name, State) %>%
  count(State)

head(St1)  

##Find from which states do the ISU players come. Create an appropriate 
##summary.
?group_nest
str(bioClean1)

St2 <- bioClean1 %>%
  select(Name, State) %>%
  group_nest(State) %>%
  as_tibble()
 
St2
head(St2, n=5)

ggplot(data=St1, aes(x=State,  y=n, fill=State)) + 
  geom_bar(stat= 'identity') +  ggtitle("Number of Players Per State") + 
  theme(plot.title = element_text(hjust = 0.5)) +  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) + ylab("Number of Players") + 
  xlab("State")

##Is there a relationship between the player statistics and their positions?
##NOT DONE!
bioClean3 <- bioClean %>%
  select(Name, Position)

bioClean3
##Total Rushing
TRush <- offClean %>%
  summarise(Rush1=sum(Rushing_YDS, na.rm=TRUE), Rush2=sum(Rushing_ATT, na.rm=TRUE), 
                                              Rush3 = sum(Rushing_TD, na.rm=TRUE)) %>%
  mutate(TRush = (Rush1/Rush2)+Rush3) %>%
  select(TRush)
TRush

##Total Receiving
TRec <- offClean %>%
  summarise(TRec=sum(((Receiving_YDS/Receiving_REC) + Receiving_TD), 
                     na.rm = TRUE))
TRec

offPos <- offClean %>%
  left_join(bioClean3) %>%
  select(-Opponent_Opponent, -Name) %>%
  group_by(Position) %>%
  filter(!is.na(Position)) %>%
  summarise_at(vars(Rushing_ATT:Passing_INT), mean, na.rm=TRUE) %>%
  pivot_longer('Rushing_ATT':'Passing_INT', names_to='Off_Skill', 
               values_to='Stat')
##  mutate(RushFact = (((Rushing_YDS/Rushing_ATT) + Rushing_TD)/TRush))
##  mutate(RecFact = ((((Receiving_YDS/Receiving_REC) + Receiving_TD)/TRec))) %>%
##  mutate(OffFact = ((RushFact + RecFact)/2)*100) %>%
##  arrange(desc(OffFact))

ggplot(offPos, aes(Position, Stat, fill=Off_Skill)) + 
  geom_bar(stat = "identity", position = 'dodge', width = .75) +  
  ggtitle("Offensive Skill by Position") + 
  theme(plot.title = element_text(hjust = 0.5))  + theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) + ylab("") + 
  xlab("Offensive Skill") + facet_wrap(~Off_Skill)

head(offPos[, c("Name", "OffFact")])

TotOffFact <- sum(offPos$RushFact, na.rm=TRUE)
TotOffFact

##Total Tackles
TT <- defClean %>%
  summarise(TT=sum(Tackles_Solo + Tackles_ASST + Tackles_TFL + Tackles_Sack))

##Total Turnovers
TTO <- defClean %>%
  summarise(TTO=sum(Turnover_FF + Turnover_FR + Turnover_INT))
TTO

##Total Pass
TP <- defClean %>%
  summarise(TP=sum(Pass_QBH + Pass_PB))
TP

defPos <- defClean %>%
  left_join(bioClean3) %>%
  select(-Opponent_Opponent, -Name) %>%
  group_by(Position) %>%
  filter(!is.na(Position)) %>%
  summarise_at(vars(Tackles_Solo:Pass_PB), sum, na.rm=TRUE) %>%
  pivot_longer('Tackles_Solo':'Pass_PB', names_to='Def_Skill', 
               values_to='Stat')
  
##  mutate(TackleFact = (Tackles_Solo + Tackles_ASST + Tackles_TFL + 
##                          Tackles_Sack)/TT) %>%
##  mutate(TurnFact = ((Turnover_FR + Turnover_FF + Turnover_INT)/TTO)) %>%
##  mutate(PassFact = (Pass_QBH + Pass_PB)/TP) %>%
##  mutate(DefFact = ((TackleFact + TurnFact + PassFact)/3)*100) %>%
##  arrange(desc(DefFact))

ggplot(defPos, aes(Position, Stat, fill=Def_Skill)) + 
  geom_bar(stat = "identity", position = 'dodge', width = .75) +  
  ggtitle("Offensive Skill by Position") + 
  theme(plot.title = element_text(hjust = 0.5))  + theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) + ylab("") + 
  xlab("Offensive Skill") + facet_wrap(~Def_Skill)

head(defPos[, c("Name", "DefFact")])
tail(defPos[, c("Name", "DefFact")])

TotDefFact <- sum(defPos$DefFact, na.rm=TRUE)
TotDefFact
  
defPos

Pos <- offPos %>% full_join(defPos, by=c("Name", "Position")) %>%
  mutate(Tackles = (Tackles_Solo + Tackles_ASST + 
                             Tackles_TFL + Tackles_Sack)/TT)
Pos

TFsum <- sum(Pos$Tackles, na.rm=TRUE)
TFsum

##How does the performance of quarterback Brock Purdy relate to the 
##performance of the ISU team?
offClean
str(offClean)

##https://www.dummies.com/sports/football/offense/how-to-read-a-quarterbacks-statistics/
?ifelse

offClean2 <- offClean %>%
  filter(Name == "Purdy, Brock") %>%
  select(-Receiving_REC, -Receiving_TD, -Receiving_YDS) %>%
  mutate(Rating = (((((Passing_CMP/Passing_ATT)-0.3)/0.2) + 
           (((Passing_YDS/Passing_ATT)-3)/4) + ((Passing_TD/Passing_ATT)/0.05) + 
  ((0.095-(Passing_INT/Passing_ATT))/0.04))*100)/6) %>%
  arrange(Rating) %>%
  mutate(W_L = ifelse(Opponent_Opponent %in% c('UNI', 'ULM', 'TCU', 'West Virginia', 
                                               'Texas Tech', 'Texas', 'Kansas'), 'W', 'L'))
 
offClean2


##Which ISU player(s) made big progress over the last year? Compare the 2019
##and the 2018 data here.

defense2 <- read_excel('C:/Users/lisa.mcdaniel/DS202/ds202lab4/cyclonesFootball2018.xlsx',
                      sheet="Defensive")
str(defense2)

offense2 <- read_excel('C:/Users/lisa.mcdaniel/DS202/ds202lab4/cyclonesFootball2018.xlsx',
                      sheet="Offensive")
?summarise_at
##Defense Comparison
defClean2N <- defense2 %>%
  select(Name) %>%
  distinct()

defClean3N <- defClean %>%
  select(Name) %>%
  distinct()

DefNames <- inner_join(defClean2N, defClean3N)

DefNames

defClean2 <- defense2 %>%
  select(-Opponent_Opponent) %>%
  mutate(Name = as.factor(Name)) %>%
  mutate(across(.cols = (Tackles_Solo:Pass_PB), .fns = (as.numeric))) %>%
  group_by(Name) %>%
  summarise_at(vars(Tackles_Solo:Pass_PB), sum, na.rm=TRUE) %>%
  mutate(Year = 2018) %>%
  right_join(DefNames) %>%
  pivot_longer(`Tackles_Solo`:`Pass_PB`, names_to='Def_Skill', 
               values_to='Stat')

defClean2

defClean3 <- defClean %>%
  select(-Opponent_Opponent) %>%
  group_by(Name) %>%
  summarise_at(vars(Tackles_Solo:Pass_PB), sum, na.rm=TRUE) %>%
  mutate(Year=2019) %>%
  right_join(DefNames) %>%
  pivot_longer(`Tackles_Solo`:`Pass_PB`, names_to='Def_Skill', 
               values_to='Stat')

defClean3

?inner_join

defComp <- full_join(defClean2, defClean3) %>%
  mutate(Year = as.character(Year))

str(defComp)

ggplot(defComp, aes(Def_Skill, Stat, fill=Year)) + 
  geom_bar(stat = "identity", position = 'dodge', width = 0.5) +  
  ggtitle("Progress Between 2018 & 2019") + 
  theme(plot.title = element_text(hjust = 0.5)) +  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) + ylab("") + 
  xlab("Defensive Skill") +  facet_wrap(~Name, 8)

unique(defComp$Name)

library(dplyr)

##Offense Comparison

offClean2N <- offense2 %>%
  select(Name) %>%
  distinct()

offClean3N <- offClean %>%
  select(Name) %>%
  distinct()

OffNames <- inner_join(offClean2N, offClean3N)

DefNames

offClean2 <- offense2 %>%
  select(-Opponent_Opponent) %>%
  mutate(Name = as.factor(Name)) %>%
  separate(`Passing_CMP-ATT`, c('Passing_CMP', 'Passing_ATT')) %>%
  mutate(across(.cols = (Receiving_REC:Passing_INT), .fns = (as.numeric))) %>%
  group_by(Name) %>%
  summarise_at(vars(Receiving_REC:Passing_INT), sum, na.rm=TRUE) %>%
  mutate(Year = "2018") %>% 
  right_join(OffNames) %>%
  pivot_longer('Receiving_REC':'Passing_INT', names_to='Off_Skill', 
               values_to='Stat')

offClean2

offClean3 <- offClean %>%
  select(-Opponent_Opponent) %>%
  group_by(Name) %>%
  summarise_at(vars(Receiving_REC:Passing_INT), sum, na.rm=TRUE) %>%
  mutate(Year='2019') %>%
  right_join(OffNames) %>%
  pivot_longer('Receiving_REC':'Passing_INT', names_to='Off_Skill', 
               values_to='Stat')

offComp <- full_join(offClean2, offClean3)

ggplot(offComp, aes(Off_Skill, Stat, fill=Year)) + 
  geom_bar(stat = "identity", position = 'dodge', width = 0.5) +  
  ggtitle("Progress Between 2018 & 2019") + 
  theme(plot.title = element_text(hjust = 0.5)) +  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) + ylab("") + 
  xlab("Defensive Skill") +  facet_wrap(~Name)
  
unique(offClean3$Name)

str(offClean2)


  

 
