rm(list = ls(all=TRUE))  
library(dplyr)
library(lubridate)

# Set directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in the raw data, and do modifications
data <- read.csv("OpWall band data all.csv", header=T, na.strings=c("","NA", "-")) %>% 
  mutate(Date = dmy(Date), field.season = year(Date)) %>% 
  dplyr::rename(Sex = Sex.method)

# Read in file containing the mist netting locations
locations <- read.csv("Sites coordinates.csv") %>% 
  dplyr::rename(Loc = Abbreviation, Latitude = lat, Longitude = lon) %>% 
  select(-"Site")

# Subset raw data into individual years. Data had different format from year to year, so it needs to be edited separately
    # Standardize each subset
d2014 <- data %>% 
  filter(field.season == 2014) %>% 
  mutate(DateTime = ymd_hm(paste(Date, Time, sep= " "))) %>% 
  select(-one_of(c('Date', 'Time', 'Band.size'))) %>% 
  mutate(Recap. = ifelse(is.na(Recap.), "N", "Y"),  
         Sex = substr(Sex, start = 1, stop = 1),   
         Sexing.method = substr(Sexing.method, start = 1, stop = 1),   
         Age = substr(Age, start = 1, stop = 1))    

d2015 <- data %>% 
  filter(field.season == 2015) %>% 
  mutate(Time = sprintf("%04d", as.numeric(as.character(Time))),
                                         Time = gsub("(\\d{2})(?=\\d{2})", "\\1:", Time, perl = TRUE),
                                         DateTime = ymd_hm(paste(Date, Time, sep= " ")),
         Band.size = stringr::str_pad(Band.size, 2, side = "left", pad = "0"),
         Band.ID = stringr::str_pad(Band.ID, 4, side = "left", pad = "0"),
         Band.ID = paste(Band.size, Band.ID, sep = "-"),
         Sexing.method = sapply(strsplit(as.character(Sex), "/"), "[", 2),
         Sex = sapply(strsplit(as.character(Sex), "/"), "[", 1)) %>% 
  select(-one_of(c('Date', 'Time', 'Band.size')))

d2016 <- data %>% 
  filter(field.season == 2016) %>% 
  mutate(Time = sprintf("%04d", as.numeric(as.character(Time))),
         Time = gsub("(\\d{2})(?=\\d{2})", "\\1:", Time, perl = TRUE),
         DateTime = ymd_hm(paste(Date, Time, sep= " ")),
         Sexing.method = sapply(strsplit(as.character(Sex), "/"), "[", 2),
         Sex = sapply(strsplit(as.character(Sex), "/"), "[", 1)) %>% 
  select(-one_of(c('Date', 'Time', 'Band.size')))

d2017 <- data %>% 
  filter(field.season == 2017) %>% 
  mutate(DateTime = ymd_hm(paste(Date, Time, sep= " ")),
         Sexing.method = gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", Sex, perl=T),
         Method.Age. = gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", Sex, perl=T)) %>% 
  mutate(Sex = gsub("\\([^()]*\\)", "", Sex), Age = gsub("\\([^()]*\\)", "", Age)) %>% 
  select(-one_of(c('Date', 'Time', 'Band.size')))

d2018 <- data %>% 
  filter(field.season == 2018) %>% 
  mutate(Time = sprintf("%04d", as.numeric(as.character(Time))),
         Time = gsub("(\\d{2})(?=\\d{2})", "\\1:", Time, perl = TRUE),
         DateTime = ymd_hm(paste(Date, Time, sep= " ")),
         Sexing.method = sapply(strsplit(as.character(Sex), "/"), "[", 2),
         Sex = sapply(strsplit(as.character(Sex), "/"), "[", 1),
         Net = as.numeric(as.character(Net))) %>% 
  select(-one_of(c('Date', 'Time', 'Band.size')))

# Re-combine the subsets
data <- rbind(d2014, d2015, d2016, d2017, d2018) %>% 
  select(DateTime, field.season, everything()) 

# Add location information to the main data set 
data <- merge(data, locations, by = "Loc", all.x = TRUE) %>% 
  dplyr::rename(Site = Loc) %>% 
  mutate(Date = date(DateTime))

# Correct BAnd.ID and Species errors in th emain data set 
data$Band.ID <- as.character(data$Band.ID)
data$Species <- as.character(data$Species)
data$Band.ID[data$Band.ID == "01-440"] <- "0A-440"
data$Species[data$Species == "Cercomacra tyrannina"] <- "Cercomacra tyrannia"
data$Species[data$Species == "Formicarios analis"] <- "Formicarius analis"
data$Species[data$Species == "Isteria guttata"] <- "Isleria guttata"
data$Species[data$Species == "Mionectes macconelli"] <- "Mionectes macconnelli"
data$Species[data$Species == "Onycorhynchus coronatus"] <- "Onychorhynchus coronatus"
data$Species[data$Species == "Platyrincus platyrhynchos"] <- "Platyrinchus platyrhynchos"
data$Species[data$Species == "Platyrincus saturatus"] <- "Platyrinchus saturatus"

data$Species[data$Species == "Hylophilus ochraceiceps (?)"] <- "Tunchiornis ochraceiceps"
data$Species[data$Band.ID == "00-247" & data$Species == "Thamnomanes caesius"] <- "Thamnomanes ardesiacus"
data$Species[data$Band.ID == "1A-040" & data$Species == "Xiphorhynchus obsoletus"] <- "Xiphorhynchus pardalotus"
data$Band.ID[data$Band.ID == "00-013" & data$Species == "Glyphorhynchus spirurus"] <- "0A-013"
data$Band.ID[data$Band.ID == "00-0155" & data$Species == "Mionectes macconnelli"] <- "00-056"
data$Band.ID[data$Band.ID == "00-287" & data$Species == "Mionectes macconnelli"] <- "00-281"
data$Band.ID[data$Band.ID == "01-0051" & data$Species == "Tachyphonus cristatus"] <- "01-050"
data$Band.ID[data$Band.ID == "01-0081" & data$Species == "Rhynchocyclus olivaceus"] <- "01-087"
data$Band.ID[data$Band.ID == "01-023" & data$Species == "Thamnomanes caesius"] <- "01-033"
data$Band.ID[data$Band.ID == "01-024" & data$Species == "Thamnomanes caesius"] <- "01-034"
data$Band.ID[data$Band.ID == "01-029" & data$Species == "Willisornis poecilinotus"] <- "01-038"
data$Band.ID[data$Band.ID == "0A-033" & data$Species == "Mionectes macconnelli"] <- "0A-055"
data$Band.ID[data$Band.ID == "0A-051" & data$Species == "Ceratopipra erythrocephala"] <- "0A-050"
data$Band.ID[data$Band.ID == "1A-0043" & data$Species == "Gymnopithys rufigula"] <- "1A-045"
data$Species[data$Band.ID == "0A-277" & data$Species == "Thamnomanes ardesiacus"] <- "Thamnomanes caesius"
data$Species[data$Band.ID == "0A-206" & data$Species == "Platyrinchus mystaceus"] <- "Platyrinchus saturatus"
data$Species[data$Band.ID == "1B-027" & data$Species == "Xiphorhynchus obsoletus"] <- "Xiphorhynchus pardalotus"
data$Species[data$Band.ID == "1B-169" & data$Species == "Xiphorhynchus guttatus"] <- "Xiphorhynchus pardalotus"
data$Species[data$Band.ID == "1B-186" & data$Species == "Xiphorhynchus guttatus"] <- "Xiphorhynchus pardalotus"


data <- data %>% 
  dplyr::rename(Recap = Recap.)

# Read in effort sheet, standardise date and time values
effort_sheet <- read.csv("OpWall effort sheet all.csv") %>%
  dplyr::mutate(
    Date = dmy(Date),
    open = sprintf("%04d", as.numeric(as.character(open))),
    open = gsub("(\\d{2})(?=\\d{2})", "\\1:", open, perl = TRUE),
    close = sprintf("%04d", as.numeric(as.character(close))),
    close = gsub("(\\d{2})(?=\\d{2})", "\\1:", close, perl = TRUE),
    open = ymd_hm(paste(Date, open, sep = " ")),
    close = ymd_hm(paste(Date, close, sep = " ")),
    hours_netting_day = close - open,
    net_hours_day = as.numeric(as.character(nets * hours_netting_day))
  ) %>%
  dplyr::group_by(Date) %>%
  dplyr::mutate(total_net_hours_day = sum(net_hours_day))

# Create effort data set to be joined with main data set 
data$Site <- as.character(data$Site)
to_join <- data.frame(Site=effort_sheet$Site, Date=effort_sheet$Date,
                      total_net_hours_day=effort_sheet$total_net_hours_day, stringsAsFactors = FALSE)
to_join$Site <- as.character(to_join$Site)

# Merge effort sheet with main data set, standardise species and capture numbers by 100 mist netting hours
data.set <- merge(data, to_join, by = c("Site", "Date"), all.x=T) %>% 
  group_by(Date, Site) %>% 
  mutate(species_100hours = length(unique(Species))*100/total_net_hours_day,
         captures_100hours = n()*100/total_net_hours_day)

# Check which lines don't have information for total_net_hours_day 
View(data.set[is.na(data.set$total_net_hours_day),])

write.csv(data.set, "all_joined.csv")
                  