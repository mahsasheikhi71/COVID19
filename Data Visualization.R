library("tidyverse")

library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")


#######################
cases <- read_csv("COVID-19_cases_plus_census.csv")

cases

cases <- cases %>% mutate_if(is.character, factor)
dim(cases)

cases_TX <- cases %>% filter(state == "TX")
dim(cases_TX)


summary(cases_TX[,1:10])

ggplot(cases_TX, mapping = aes(confirmed_cases)) + geom_histogram(bins = 20)

ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + geom_point()

ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX, deaths >= 1000)) 


cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income)
cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

head(cases_TX_select)


datatable(cases_TX_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)


ggplot(cases_TX_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))


ggplot(cases_TX_select, mapping = aes(x= total_pop, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))


cor_TX <- cor(cases_TX_select[,-1])
ggcorrplot(cor_TX, p.mat = cor_pmat(cases_TX_select[,-1]), insig = "blank", hc.order = TRUE)


counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))

cases_TX <- cases_TX_select %>% mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))

counties_TX <- counties_TX %>% left_join(cases_TX %>% 
                                           select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))

ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  # geom_text_repel(data = counties_TX %>% filter(complete.cases(.)) %>% group_by(county) %>% 
  #    summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", subtitle = "Only counties reporting 100+ cases")

##################### Texas Data Set:

cases_TX <- read_csv("COVID-19_cases_TX.csv")



cases_TX

cases_Dallas <- cases_TX %>% filter(county_name == "Dallas County" & state == "TX")
dim(cases_Dallas)


ggplot(cases_Dallas, aes(x = date, y = confirmed_cases)) + geom_line() + geom_smooth()

##################### Mobility Data Set:

mobility <- read_csv("Global_Mobility_Report.csv")


mobility <- read_csv("Global_Mobility_Report.csv", col_types =  cols(sub_region_2 = col_character()))


mobility <- mobility %>% mutate_if(is.character, factor)
dim(mobility)


head(mobility)

summary(mobility)

colSums(is.na(mobility))

mobility_Dallas <- mobility %>% filter(sub_region_1 == "Texas" & sub_region_2 == "Dallas County") 
mobility_Dallas%>% pull(retail_and_recreation_percent_change_from_baseline) %>% mean(na.rm = TRUE)

dim(mobility_Dallas)

mobility2<-mobility%>%  drop_na()
head(mobility_Dallas)
mobility_Dallas%>% pull(retail_and_recreation_percent_change_from_baseline) %>% mean(na.rm = TRUE)



mobility_Dallas
colSums(is.na(mobility_Dallas))
ggplot(mobility_Dallas, mapping = aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) + geom_line() + geom_smooth()


ggplot(mobility_Dallas, mapping = aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline)) + geom_line() + geom_smooth()


ggplot(mobility_Dallas, mapping = aes(x = date, y = transit_stations_percent_change_from_baseline)) + geom_line() + geom_smooth()

ggplot(mobility_Dallas, mapping = aes(x = date, y = workplaces_percent_change_from_baseline)) + geom_line() + geom_smooth()

ggplot(mobility_Dallas, mapping = aes(x = date, y = residential_percent_change_from_baseline)) + geom_line() + geom_smooth()



DataPreparation <- read_csv("D:/Spring 2021/Data Mining/Project folders/Project1/Final merged data V3.csv")
summary(DataPreparation)

DataPreparation <- as_tibble(DataPreparation)
 
DataPreparation_long <- DataPreparation %>% mutate(id = row_number(),na.rm(TRUE)) %>% pivot_longer(4:7)%>% 
head(DataPreparation_long) 
ggplot(DataPreparation_long, aes(name, value)) + geom_boxplot()

ggplot(DataPreparation, aes(spread_rate)) + geom_histogram()
       
head(DataPreparation_long)


ggplot(DataPreparation, aes(x = pop2021, y =  physicians_and_dentists),na.rm = TRUE) + geom_point()


DataPreparation2 <- read_csv("D:/Spring 2021/Data Mining/Project folders/Project1/registeredstaff.csv")
summary(DataPreparation2)

DataPreparation2 <- as_tibble(DataPreparation2)

DataPreparation_long <- DataPreparation %>% mutate(id = row_number()) %>% pivot_longer(3)
ggplot(DataPreparation_long, aes(name, value))  + geom_boxplot()


# Data Preparation:
DataPreparation <- read_csv("D:/Spring 2021/Data Mining/Project folders/Project1/Final merged data V3.csv")
summary(DataPreparation)
head(DataPreparation)
DataPreparation <- as_tibble(DataPreparation)

ggplot(DataPreparation, mapping = aes(spread_rate)) + geom_histogram(bins = 20)


ggplot(DataPreparation, mapping = aes(x = confirmed_cases, y = deaths, label = County_Name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = pop2021), color = "grey") + 
  geom_text_repel(data = subset(DataPreparation, deaths >= 1000)) 

ggplot(DataPreparation, mapping = aes(x = confirmed_cases, y = , label = County_Name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = pop2021), color = "grey") + 
  geom_text_repel(data = subset(DataPreparation, deaths >= 1000)) 





