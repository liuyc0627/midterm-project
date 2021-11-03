## ---------------------------
##
## Script name: wrangling_code_clean.R
##
##
##
## Last edited: 10-29-2021
##
## ---------------------------


##---load packages
options(scipen = 6, digits = 4)
memory.limit(30000000) 

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyr","tidyverse", "dplyr","data.table", "zoo","rstudioapi","gmodels","maps","mapproj")

#=================================== Load Data ====================================================================================
pesticides <- as_tibble(read.csv("Pesticides.csv", fileEncoding = 'UTF-8-BOM')) 
strawberries <- as_tibble(read.csv("Strawberries.csv"))

#=================================== Clean ========================================================================================
###### Clean strawberries data 
strawberries_df <- strawberries %>%
  subset(., select=-c(4,5,8,9,10,11,12,13,14,15,16))%>% # remove blank columns
  filter((Domain == "CHEMICAL, FUNGICIDE")|(Domain == "CHEMICAL, INSECTICIDE")|
           (Domain == "CHEMICAL, HERBICIDE")|(Domain == "FERTILIZER"))%>% #filter domain out, as FUNGICIDE, INSECTICIDE, HERBICIDE, FERTILIZER
  mutate(measure = gsub("STRAWBERRIES, BEARING - ", "", Data.Item)) %>% #remove strawberries, bearing - 
  mutate(chemical_type = gsub("CHEMICAL, ", "", Domain))%>% # get chemical type 
  mutate(chemical = gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.","", Domain.Category,perl=T)) %>%#get chemical type
  subset(., select=-c(Data.Item, Domain, Domain.Category, Program, Period))%>% # remove old columns
  mutate(chemical_code = gsub(".*=", "", chemical)) %>% # remove common attributions, get PC code for pesticides 
  mutate(chemical =gsub("\\=.*","", chemical)) # remove common atrributions
names(strawberries_df)[names(strawberries_df) == "CV...."] <- "cv_pct"

###### Clean pesticides data 
pesticides_df <- pesticides[-which(pesticides$Pesticide == ""),] #remove empty rows 
pesticides_df$Pesticide <- toupper(pesticides_df$Pesticide) #convert chemical names to all CAPITALS 
pesticides_df <- pesticides_df %>%
  mutate(toxicity_humanbee = ifelse(Carcinogen=="known"|Neurotoxins=="present"|
                                      Developmental.or.Reproductive.Toxins=="present"|Bee.Toxins=="high"|
                                      Bee.Toxins=="slight"|Bee.Toxins=="moderate","Yes",
                                    ifelse(Carcinogen=="possible"|Hormone.Disruptor=="suspected", "Possible", "No")))%>% ## adding tocixity_human/bee var
  # logic: if any harm to human, or bees, yes; possible, or suspected, possible; else, no 
  mutate(toxicity_humanbee_cat= ifelse(toxicity_humanbee=="No",1,
                                       ifelse(toxicity_humanbee=="Possible",2,3)))%>% 
  # adding numeric categorical var for toxicity human/bee
  mutate(toxicity_level = ifelse(Carcinogen == ""&Hormone.Disruptor==""&Neurotoxins==""&
                                   Developmental.or.Reproductive.Toxins==""&Bee.Toxins=="","No",
                                 ifelse(Carcinogen=="possible"|Hormone.Disruptor=="suspected"|Bee.Toxins=="slight", "Low",
                                        ifelse(Carcinogen=="known"&Neurotoxins=="present"
                                               &Developmental.or.Reproductive.Toxins=="present"&Bee.Toxins=="high","High","Moderate"))))%>%
  #adding toxicity level, categorical var 
  # logic: if no harm at all for all harms, no; if all are possible or suspected, or slight, then it is low; 
  ######### if all harm confirmed with high bee toxin, yes; else, no
  mutate(toxicity_level_cat = ifelse(toxicity_level=="No",1,
                                     ifelse(toxicity_level=="Low", 2,
                                            ifelse(toxicity_level=="Moderate",3,4))))%>% # adding toxicity level, factor 
  select(., select=-c(2:6))%>% # remove single harm columns 
  distinct(chemical_code, .keep_all= TRUE)%>% # remove duplicated chemical codes
  filter(chemical_code!="") #filter chemical code empty 


###### merge data 
strawberries_df$chemical_code = as.integer(strawberries_df$chemical_code)
agg_df <- merge(x=strawberries_df,y=pesticides_df,by = "chemical_code", all.x=TRUE)

col_order <- c("chemical","chemical_code","chemical_type","toxicity_humanbee",
               "toxicity_humanbee_cat","toxicity_level","toxicity_level_cat",
               "State","State.ANSI", "Year","measure","Value","cv_pct")
agg_df2 <- agg_df[, col_order]
agg_df2 <- agg_df2 %>%
  filter(Value!=" (D)") %>%
  filter(Value!=" (Z)") %>%
  filter(Value!=" (NA)")

agg_df2$Value<-as.numeric(agg_df2$Value)


work_data <- agg_df2 %>%
  filter(toxicity_level_cat!="") %>%
  filter(toxicity_humanbee_cat!="") %>%
  filter(measure == "APPLICATIONS, MEASURED IN LB")


us_states <- map_data("state")
us_states$region <- toupper(us_states$region)
names(us_states)[names(us_states) == "region"] <- "State"

work_data1 <- work_data %>%
  group_by(State) %>% 
  summarise(value = mean(Value))

toxicity_val <- left_join(us_states, work_data1)

