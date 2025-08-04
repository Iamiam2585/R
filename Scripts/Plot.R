library(tidyverse)
library(data.table)
library(viridis)
library(janitor)
library(readxl)
library(fs)
library(plotly)
library(reactable)

view(airquality)
airquality_clean <- airquality %>% 
  clean_names()

airquality_may <- airquality_clean %>% 
  filter(month == 5)

airquality_year <- airquality_clean %>% 
  mutate(year = "1973")

airquality_year_date <- airquality_year %>% 
  unite("date", month:year, remove = F) %>% 
  mutate(date = mdy(date))
airquality_year_date
str(airquality_year_date)

ggplot(airquality_year_date, aes(x=date, y=ozone)) + 
  geom_point() +
  labs( title = "Daily Ozone in New York, 1973", x = "Date", y = "Ozone (ppb)") + 
  theme_classic() +
  scale_x_date(date_labels = "%n /%d", date_breaks = "2 weeks")

historical_data_files <- list.files("./Data/historical-speciation-data", pattern = "*.csv", full.names = T) #contains all of the data for nine sites
print(historical_data_files)
all_data <- read_csv(historical_data_files, col_select = 1:12, col_types = list(Site = col_character(), Parameter = col_character(),
                                                                                Date = col_datetime("%m/%d/%Y %H:%M"), Duration = col_character(),
                                                                                POC = col_double(), Value = col_double(), Unit = col_character(),
                                                                                QualifierCodes = col_character(), MDL = col_double(), NullCode = col_character(),
                                                                                TransactionType = col_character(), SubTransactionType = col_character())) %>% 
  clean_names() %>% 
  separate(qualifier_codes, c("qualifier1", "qualifier2", "qualifier3", "qualifier4",
                              "qualifier5", "qualifier6", "qualifier7", "qualifier8",
                              "qualifier9", "qualifier10")) %>% 
  mutate(parameter = recode(parameter, "Sample Flow Rate- Cv - Method 810 (Teflon)" = "Sample Flow Rate CV - Teflon Filter",
                            "Sample Flow Rate- Cv - Method 812 (Nylon)" = "Sample Flow Rate CV - Nylon Filter",
                            "Sample Flow Rate- Cv - Method 838 (Quartz)" = "Sample Flow Rate CV - Quartz Filter",
                            "Sample Volume - Method 810 (Teflon)" = "Sample Volume - Teflon Filter",
                            "Sample Volume - Method 812 (Nylon)" = "Sample Volume - Nylon Filter",
                            "Sample Volume - Method 838 (Quartz)" = "Sample Volume - Quartz Filter",
                            "Ambient Temperature - Method 810 (Teflon)" = "Avg Ambient Temperature for MetOne SASS/SuperSASS",
                            "Ambient Temperature - Method 838 (Quartz)" = "Average Ambient Temperature for URG3000N",
                            "Sample Baro Pressure - Method 810 (Teflon)" = "Avg Ambient Pressure for MetOne SASS/SuperSASS",
                            "Sample Baro Pressure - Method 838 (Quartz)" = "Average Ambient Pressure for URG3000N"))
unique(all_data$transaction_type)
unique(all_data$parameter)
unique(all_data$site)


field_blank_data_vis_quartz <- all_data %>% 
  mutate(transaction_type = case_when (
    transaction_type == "RD" ~ "Sample",
    transaction_type == "RB" ~ "Field Blank"),
    parameter = recode(parameter, "OC PM2.5 LC Tor" = "Organic Carbon",
                       "EC PM2.5 LC Tor" = "Elemental Carbon",
                       "EC Csn_Rev Unadjusted PM2.5 LC Tor" = "Elemental Carbon",
                       "OC Csn_Rev Unadjusted PM2.5 LC Tor" = "Organic Carbon")) %>% 
  mutate(date = as_date(date)) %>% 
      filter(parameter == "Organic Carbon" | parameter == "Elemental Carbon") %>% 
  filter(value != "-999") %>% 
  filter(qualifier1 != "MD" | is.na(qualifier1)) %>% #keep all of the data except data qualified with MD in qualifier1 column
  filter(qualifier2 != "MD" | is.na(qualifier2)) %>% #same as above for qualifier2 column
  filter(qualifier3 != "MD" | is.na(qualifier3)) %>% #same as above for qualifier3 column
  filter(qualifier4 != "MD" | is.na(qualifier4)) %>% 
  filter(date >= mdy("01-01-2023") & date <= mdy("12-31-2023")) #same as above for qualifier4 columnilter(is.na(null_code)) 

field_blank_data_vis_quartz
view(field_blank_data_vis_quartz)
str(field_blank_data_vis_quartz)
unique(field_blank_data_vis_quartz$site)
ggplot(field_blank_data_vis_quartz, aes( x = date, y = value, color = transaction_type,
                                         shape = parameter)) + 
  geom_point() +
  theme_classic() + 
  facet_wrap(~site) + 
  scale_x_date(date_labels = "%m/%y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust =1 ),
        legend.title = element_blank(),
        legend.position = "bottom") + 
  labs( x = "Date (month/year)",
        y = expression(paste("PM"[2.5], " Concentration (", mu, "g/m"^3, ")")),,
        title = "Elemental and Organic Carbon Concentrations at PADEP Sites")

organic_carbon_sample_data <- field_blank_data_vis_quartz %>% 
  filter(parameter == "Organic Carbon") %>% 
  filter(transaction_type == "Sample")
ggplot(organic_carbon_sample_data, aes(x = site, y = value)) + 
  geom_boxplot() 
