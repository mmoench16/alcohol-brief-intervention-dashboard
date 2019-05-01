# TidyUpper is the script that turns ABI data tables into Tidy format.

# Load libraries
library(tidyverse)
library(readxl)
library(stringr)

# Load data

# - Table 1 - Total number of ABIs delivered from 08/09 to 17/18 for each board.

ABIsDelivered <- read_excel("Resources/Data/2018-06-26-AlcoholBriefInterventions-Tables.xlsx", range = "Table1!A5:I21")
ABIsTarget <- read_excel("Resources/Data/2018-06-26-AlcoholBriefInterventions-Tables.xlsx", range = "Table1!K5:R21")

# - Table 1 - Transform data into Tidy format, remove unnecessary parts and turn Strings into Factors.

ABIsDelivered <- ABIsDelivered[-c(1:2),]
ABIsDelivered <- ABIsDelivered %>%
  gather(FY, Delivered, `2008/09 - 2010/11`:`2017/18`) %>%
  rename(Board = X__1)

ABIsTarget <- ABIsTarget[-c(1:2),]
ABIsTarget <- ABIsTarget %>%
  gather(FY, Target, `2008/09 - 2010/11`:`2017/18`)

ABIs <- ABIsDelivered %>%
  add_column(Target = ABIsTarget$Target) %>%
  mutate(`Percent Of Target` = ceiling((Delivered / Target) * 100))

ABIs$Board <- str_replace(ABIs$Board, "NHS ", "")
ABIs$Board <- str_replace(ABIs$Board, "Tayside2", "Tayside")
ABIs$FY <- str_replace(ABIs$FY, "163", "16")

ABIs$Board <- factor(ABIs$Board)
ABIs$FY <- factor(ABIs$FY)

saveRDS(ABIs, "Resources/Data/TidyABIs.rds")

# - Table 2 - Alcohol Brief Interventions (ABIs) delivered 
#             against standard, by NHS board; financial year 2017/18
#             Number delivered, number required by LDP standard and 
#             performance against standard - overall and in priority settings

Data2 <- read_excel("Resources/Data/2018-06-26-AlcoholBriefInterventions-Tables.xlsx", range = "Table2!A5:E21")
Data2 <- Data2[-2,]
Data2 <- Data2 %>%
  mutate(Difference = `Total number delivered` - `LDP standard1`,
         `Total delivered as % of standard` = round(`Total number delivered`/`LDP standard1` * 100, digits = 0),
         `Delivered in priority settings as % of standard` = round(`Number delivered in priority settings`/`LDP standard1`*100, digits = 0)
         ) %>%
  rename(`LDP standard` = `LDP standard1`)

saveRDS(Data2, "Resources/Data/Data2.rds")

# - Table 3 - Number of Alcohol Brief Interventions (ABIs) delivered across 
#             Scotland split by priority1 and wider2 settings;
#             for financial years 2008/09 to 2017/18

Data3 <- read_excel("Resources/Data/2018-06-26-AlcoholBriefInterventions-Tables.xlsx", range = "Table3!A4:D14")
Data3 <- Data3 %>%
  rename(FY = "X__1")

saveRDS(Data3, "Resources/Data/Data3.rds")

# - Table 4 - Number and percentage of Alcohol Brief Interventiaons (ABIs) 
#             delivered by setting; within NHS Board Financial year 2017/18

Data4 <- read_excel("Resources/Data/2018-06-26-AlcoholBriefInterventions-Tables.xlsx", range = "Table4!A5:E21")
Data4 <- Data4 %>%
  na.omit() %>%
  rowwise() %>%
  mutate(`Primary Care [%]` = round(`Primary Care` / sum(`Primary Care`, `A&E`, Antenatal, `Wider Settings`) *100, digits = 1),
         `A&E [%]` = round(`A&E` / sum(`Primary Care`, `A&E`, Antenatal, `Wider Settings`) *100, digits = 1),
         `Antenatal [%]` = round(Antenatal / sum(`Primary Care`, `A&E`, Antenatal, `Wider Settings`) *100, digits = 1),
         `Wider Settings [%]` = round(`Wider Settings` / sum(`Primary Care`, `A&E`, Antenatal, `Wider Settings`) *100, digits = 1)
         ) %>%
  ungroup()

Data4$`NHS Board` <- str_replace(Data4$`NHS Board`, "NHS ", "")

saveRDS(Data4, "Resources/Data/Data4.rds")

# - Table 5 - (PLEASE NOTE: Table 5 & 6 merged into one!)
#             Number Alcohol Brief Interventions (ABIs) and percentage delivered in Wider Settings; 
#             Scotland, financial years 2015/16 to 2017/18

Data5 <- read_excel("Resources/Data/2018-06-26-AlcoholBriefInterventions-Tables.xlsx", range = "Table5&6!A5:D12")

Data5 <- Data5 %>%
  na.omit() %>%
  mutate(`2015/16 [%]` = round(`2015/16` / `2015/16`[`Wider setting` == "Total"] * 100, digits = 1),
         `2016/17 [%]` = round(`2016/17` / `2016/17`[`Wider setting` == "Total"] * 100,digits = 1),
         `2017/18 [%]` = round(`2017/18` / `2017/18`[`Wider setting` == "Total"] * 100, digits = 1))

saveRDS(Data5, "Resources/Data/Data5.rds")

# - Table 6 - (PLEASE NOTE: Table 7 & 8 merged into one!)
#             Number and percentage of Alcohol Brief Interventions (ABIs) delivered in 
#             Criminal Justice settings; Scotland, financial years 2015/16 to 2017/18

Data6 <- read_excel("Resources/Data/2018-06-26-AlcoholBriefInterventions-Tables.xlsx", range = "Table7&8!A5:D11")

Data6 <- Data6 %>%
  na.omit() %>%
  mutate(`2015/16 [%]` = round(`2015/16` / `2015/16`[`Criminal Justice Setting` == "Total"] * 100, digits = 1),
         `2016/17 [%]` = round(`2016/17` / `2016/17`[`Criminal Justice Setting` == "Total"] * 100, digits = 1),
         `2017/18 [%]` = round(`2017/18` / `2017/18`[`Criminal Justice Setting` == "Total"] * 100, digits = 1))

saveRDS(Data6, "Resources/Data/Data6.rds")

