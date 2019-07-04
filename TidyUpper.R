# TidyUpper is the script that turns ABI data tables into Tidy format.

# Load libraries
library(tidyverse)
library(readxl)
library(stringr)

# Load data

# - Table 1 - Total number of ABIs delivered from 08/09 to 18/19 for each board.

ABIsDelivered <- read_excel("Resources/Data/2019-06-25-AlcoholBriefInterventions-Tables-WIP.xlsx", range = "Table1!A5:J21")
ABIsTarget <- read_excel("Resources/Data/2019-06-25-AlcoholBriefInterventions-Tables-WIP.xlsx", range = "Table1!L5:T21")

# - Table 1 - Transform data into Tidy format, remove unnecessary parts and turn Strings into Factors.

ABIsDelivered <- ABIsDelivered[-c(1:2),]
ABIsDelivered <- ABIsDelivered %>%
  gather(FY, Delivered, `2008/09 - 2010/11`:`2018/19`) %>%
  rename(Board = X__1)

ABIsTarget <- ABIsTarget[-c(1:2),]
ABIsTarget <- ABIsTarget %>%
  gather(FY, Target, `2008/09 - 2010/11`:`2018/19`)

ABIs <- ABIsDelivered %>%
  add_column(Target = ABIsTarget$Target) %>%
  mutate(`Percent Of Target` = round((Delivered / Target) * 100, digits = 1))

ABIs$Board <- str_replace(ABIs$Board, "NHS ", "")
ABIs$Board <- str_replace(ABIs$Board, "Tayside2", "Tayside")
ABIs$FY <- str_replace(ABIs$FY, "163", "16")

ABIs$Board <- factor(ABIs$Board)
ABIs$FY <- factor(ABIs$FY)

saveRDS(ABIs, "Resources/Data/TidyABIs.rds")

# - Table 2 - Alcohol Brief Interventions (ABIs) delivered 
#             against standard, by NHS board; financial year 2018/19
#             Number delivered, number required by LDP standard and 
#             performance against standard - overall and in priority settings

Data2 <- read_excel("Resources/Data/2019-06-25-AlcoholBriefInterventions-Tables-WIP.xlsx", range = "Table2!A5:E21")
Data2 <- Data2[-2,]
Data2 <- Data2 %>%
  mutate(Difference = `Total number delivered` - `LDP standard1`,
         `Total delivered as % of standard` = round(`Total number delivered`/`LDP standard1` * 100, digits = 1),
         `Delivered in priority settings as % of standard` = round(`Number delivered in priority settings`/`LDP standard1`*100, digits = 1)
         ) %>%
  rename(`LDP standard` = `LDP standard1`)

saveRDS(Data2, "Resources/Data/Data2.rds")

# - Table 3 - Number of Alcohol Brief Interventions (ABIs) delivered across 
#             Scotland split by priority1 and wider2 settings;
#             for financial years 2008/09 to 2018/19

Data3 <- read_excel("Resources/Data/2019-06-25-AlcoholBriefInterventions-Tables-WIP.xlsx", range = "Table3!A4:D15")
Data3 <- Data3 %>%
  rename(FY = "X__1")

saveRDS(Data3, "Resources/Data/Data3.rds")

# - Table 4 - Number and percentage of Alcohol Brief Interventiaons (ABIs) 
#             delivered by setting; within NHS Board Financial year 2018/19

Data4 <- read_excel("Resources/Data/2019-06-25-AlcoholBriefInterventions-Tables-WIP.xlsx", range = "Table4!A5:E21")
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
#             Scotland, financial years 2016/17 to 2018/19

Data5 <- read_excel("Resources/Data/2019-06-25-AlcoholBriefInterventions-Tables-WIP.xlsx", range = "Table5&6!A5:D12")

Data5 <- Data5 %>%
  na.omit() %>%
  mutate(`2016/17 [%]` = round(`2016/17` / `2016/17`[`Wider setting` == "Total"] * 100, digits = 1),
         `2017/18 [%]` = round(`2017/18` / `2017/18`[`Wider setting` == "Total"] * 100, digits = 1),
         `2018/19 [%]` = round(`2018/19` / `2018/19`[`Wider setting` == "Total"] * 100, digits = 1))

saveRDS(Data5, "Resources/Data/Data5.rds")

# - Table 6 - (PLEASE NOTE: Table 7 & 8 merged into one!)
#             Number and percentage of Alcohol Brief Interventions (ABIs) delivered in 
#             Criminal Justice settings; Scotland, financial years 2016/17 to 2018/19

Data6 <- read_excel("Resources/Data/2019-06-25-AlcoholBriefInterventions-Tables-WIP.xlsx", range = "Table7,8&9!A5:D11")

Data6 <- Data6 %>%
  na.omit() %>%
  mutate(`2016/17 [%]` = round(`2016/17` / `2016/17`[`Criminal Justice Setting` == "Total"] * 100, digits = 1),
         `2017/18 [%]` = round(`2017/18` / `2017/18`[`Criminal Justice Setting` == "Total"] * 100, digits = 1),
         `2018/19 [%]` = round(`2018/19` / `2018/19`[`Criminal Justice Setting` == "Total"] * 100, digits = 1))

saveRDS(Data6, "Resources/Data/Data6.rds")

# - Table 7 - Number of ABIs delivered in Criminal Justice settings;
#             by NHS Board for 2018/19

Data7 <- read_excel("Resources/Data/2019-06-25-AlcoholBriefInterventions-Tables-WIP.xlsx", range = "Table7,8&9!A45:F61")

Data7 <- Data7 %>% 
  na.omit()

saveRDS(Data7, "Resources/Data/Data7.rds")

# Dummy Data for PHI Pubs Team

dd1 <- ABIs %>% 
  mutate(Delivered = sample(1250:15000, 126),
         Target = sample(2000:12000, 126),
         `Percent Of Target` = sample(50:150, 126, replace = T))
saveRDS(dd1, "Resources/Data/DummyData1.rds")

dd2 <- Data2 %>% 
  mutate(`Number delivered in priority settings` = sample(500:10000, 15),
         `Number delivered in wider settings` = sample(500:10000, 15),
         `Total number delivered` = sample(500:10000, 15),
         `LDP standard` = sample(500:10000, 15),
         Difference = sample(500:10000, 15),
         `Total delivered as % of standard` = sample(50:150, 15, replace = T),
         `Delivered in priority settings as % of standard` = sample(50:150, 15, replace = T))
saveRDS(dd1, "Resources/Data/DummyData2.rds")

dd3 <- Data3 %>% 
  mutate(`Priority Settings` = sample(30000:60000, 11),
         `Wider Settings` = sample(10000:30000, 11),
         Total = sample(60000:80000, 11))
saveRDS(dd3, "Resources/Data/DummyData3.rds")

dd4 <- Data4 %>% 
  mutate(`Primary Care` = sample(50:7000, 15),
         `A&E` = sample(10:2500, 15),
         Antenatal = sample(2:1000, 15),
         `Wider Settings` = sample(50:7000, 15),
         `Primary Care [%]` = sample(60:88, 15, replace = T),
         `A&E [%]` = sample(10:20, 15, replace = T),
         `Antenatal [%]` = sample(1:5, 15, replace = T),
         `Wider Settings [%]` = sample(20:45, 15, replace = T))
saveRDS(dd4, "Resources/Data/DummyData4.rds")

dd5 <- Data5 %>% 
  mutate(`2016/17` = sample(500:10000,6),
         `2017/18` = sample(500:10000,6),
         `2018/19` = sample(500:10000,6),
         `2016/17 [%]` = sample(5:40, 6, replace = T),
         `2017/18 [%]` = sample(5:40, 6, replace = T),
         `2018/19 [%]` = sample(5:40, 6, replace = T))
saveRDS(dd5, "Resources/Data/DummyData5.rds")

dd6 <- Data6 %>% 
  mutate(`2016/17` = sample(500:10000,5),
         `2017/18` = sample(500:10000,5),
         `2018/19` = sample(500:10000,5),
         `2016/17 [%]` = sample(5:40, 5, replace = T),
         `2017/18 [%]` = sample(5:40, 5, replace = T),
         `2018/19 [%]` = sample(5:40, 5, replace = T))
saveRDS(dd6, "Resources/Data/DummyData6.rds")

dd7 <- Data7 %>% 
  mutate(`Custody Suites` = sample(0:400, 14, replace = T),
         Prisons = sample(0:400, 14, replace = T),
         `Social Work` = sample(0:400, 14, replace = T),
         Police = sample(0:20, 14, replace = T),
         Total = sample(300:1000, 14, replace = T))
saveRDS(dd7, "Resources/Data/DummyData7.rds")
