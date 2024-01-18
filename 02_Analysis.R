# ---------------------------------------------------------------------------- #
#
#                        Offshore data leaks and tax enforcement 
#                                 in developing countries
#
#
#                                   CLEAN 
#
#       Author: Hao Lyu                           Update: 11/13/2023
#
# +++++++++++++++++++++++++++++++++ Steps ++++++++++++++++++++++++++++++++++++ #
#
#        1) Table 1: Descriptive Analysis Table  
#        
#        2) Figure 1: Distribution of Jusrisdiction where firms incorporated in leaked data
#
#        3) Figure 2: Individuals in Data Leaks by Country GDP Per Capita
#
#        4) Figure 3: Share of population named in Leak
#
#        5) Figure 4: Year of incorporation of Entities in Tax Havens

# Uploading the datasets ------------------------------------------------------

# Load data 
officer         <- fread(paste0(intermediate, "/Officer.csv"), encoding = 'UTF-8', stringsAsFactors = FALSE)

entity          <- fread(paste0(intermediate, "/Entity.csv"), encoding = 'UTF-8', stringsAsFactors = FALSE)

entity_detail   <- fread(paste0(intermediate, "/Entities_detail.csv"), encoding = 'UTF-8', stringsAsFactors = FALSE)

master          <- fread(paste0(cleaned, "/Master.csv"), encoding = 'UTF-8', stringsAsFactors = FALSE)

tax_havens      <- read.csv(paste0(raw, "/tax_havens.csv"), encoding = 'UTF-8', stringsAsFactors = FALSE)

worldmap        <- sf::st_read(paste0(intermediate, "/world_map.shp"))


###############################################################################################
#                                      Table 1

#    Function to create tables from dataset 
officer$Corp <- as.numeric(officer$Corp)

make_column = function(dt1, dt2) {
  
  shares1 = dt1 %>% 
    dplyr::summarise(across(panama_papers:source_other, 
                            ~mean(., na.rm = TRUE)))
  
  shares1 = shares1 %>% 
    dplyr::mutate(across(panama_papers:source_other,
                         ~.*100))
  
  shares2 = dt1 %>% 
    dplyr::summarise(across(owners:role_both, 
                            ~mean(., na.rm = TRUE)))
  
  shares2 = shares2 %>% 
    dplyr::mutate(across(owners:role_both,
                         ~.*100))
  
  numbers1 = dt1%>%
    dplyr::summarise(corp = mean(Corp, na.rm = TRUE))
  
  numbers1 = numbers1%>%
    dplyr::mutate(corp = corp*100)
  
  numbers2 = dt1 %>%
    dplyr::summarise(across(avg_entity:avg_rel, 
                            ~mean(., na.rm = TRUE)))
  
  numbers3 = c(length(unique(dt1$name_officer)))
  
  shares3 = dt2 %>% 
    dplyr::summarise(across(bvi:other, 
                            ~mean(., na.rm = TRUE))) 
  
  shares3 = shares3 %>% 
    dplyr::mutate(across(bvi:other,
                         ~.*100 ))
  
  numbers4 = c(length(unique(dt2$name_entity)),
               length(unique(dt2$jurisdiction_entity)))
  
  return(c(numbers3, shares2, shares1, numbers4, shares3 ))
  
}
#calculate  colums for different subsets

column_entire = make_column(officer, entity) 
column_honduras = make_column(officer %>% filter(honduras == "1"), entity %>% filter(honduras == "1")) 
column_senegal = make_column(officer %>% filter(senegal == "1"), entity %>% filter(senegal == "1")) 
column_ecuador = make_column(officer %>% filter(ecuador == "1"), entity %>% filter(ecuador == "1")) 

#round
round_with_commas <- function(x) {
  formatted <- format(round(x, 2), big.mark = ",")
  return(formatted)
}

column_entire <- lapply(column_entire, round_with_commas)
column_honduras <- lapply(column_honduras, round_with_commas)
column_senegal <- lapply(column_senegal, round_with_commas)
column_ecuador <- lapply(column_ecuador, round_with_commas)

# Draw table
table = data.table(c("N Individuals",
                     "Shareholder or Beneficial Owner (%)",
                     "Management (Director, Secretary, Judicial) (%)",
                     "Both Shareholder/BO and Management(%)",
                     "Panama Papers (%)",
                     "Pandora Papers (%)",
                     "Paradise Papers (%)",
                     "Other Sources",
                     "N entities",
                     "N jurisdictions",
                     "British Virgin Islands (%)",
                     "Malta (%)",
                     "Barbados (%)",
                     "Panama (%)",
                     "Bahamas (%)",
                     "Nevada (%)",
                     "Seychelles (%)",
                     "British Anguilla (%)",
                     "Bermuda (%)",
                     "Undetermined (%)",
                     "Other Jurisdictions"
                    ), 
                   column_entire, column_honduras, column_senegal, column_ecuador)%>%
  mutate(across(column_entire:column_ecuador, ~as.numeric(.)))

#ordering
custom_order <- c("N Individuals",
  "Shareholder or Beneficial Owner (%)",
  "Management (Director, Secretary, Judicial) (%)",
  "Both Shareholder/BO and Management(%)",
  "Paradise Papers (%)",
  "Panama Papers (%)",
  "Pandora Papers (%)",
  "Other Sources",
  "N entities",
  "N jurisdictions",
  "British Virgin Islands (%)",
  "Malta (%)",
  "Other Jurisdictions",
  "Barbados (%)",
  "Undetermined (%)",
  "Panama (%)",
  "Bahamas (%)",
  "Seychelles (%)",
  "Bermuda (%)",
  "British Anguilla (%)",
  "Nevada (%)")

#ordering by entire sample column values
table$V1 <- factor(table$V1, levels = custom_order, ordered = TRUE)

table <- table[order(table$V1), ]

# table summary

table_summary = kbl(table, format = 'latex', booktabs = TRUE,
                    col.names = c("","Entire Sample","Honduras", "Senegal", "Ecuador"),
                    format.args = list(big.mark = ",")) %>% 
  pack_rows("Panel A: Individuals", 1, 1) %>% 
  pack_rows("", 2, 4) %>% 
  pack_rows("", 5, 8) %>% 
  pack_rows("Panel B: Legal Entities", 9, 10)%>%
  pack_rows("", 11, 21)

write(table_summary, file = paste0(output, "/Table_1_Descriptive_Analysis.pdf"))

###############################################################################################
#                                      Figure 1

# Calculate the share of jurisdictions for Honduras 
honduras_jurisdiction <- entity_detail%>%
  filter(honduras == 1)%>%
  group_by(jurisdiction_description_entity)%>%
  dplyr::summarise(num_entities = n_distinct(name_entity))%>%
  ungroup()

honduras_jurisdiction$total <- sum(honduras_jurisdiction$num_entities)

honduras_jurisdiction$share <- (honduras_jurisdiction$num_entities/honduras_jurisdiction$total)*100

honduras_jurisdiction$share <- as.numeric(format(round(honduras_jurisdiction$share, 0), nsmall = 0))

# Calculate the share of jurisdictions for Senegal
senegal_jurisdiction <- entity_detail%>%
  filter(senegal == 1)%>%
  group_by(jurisdiction_description_entity)%>%
  dplyr::summarise(num_entities = n_distinct(name_entity))%>%
  ungroup()

senegal_jurisdiction$total <- sum(senegal_jurisdiction$num_entities)

senegal_jurisdiction$share <- (senegal_jurisdiction$num_entities/senegal_jurisdiction$total)*100

senegal_jurisdiction$share <- as.numeric(format(round(senegal_jurisdiction$share, 0), nsmall = 0))

# Calculate the share of jurisdictions for Ecuador
ecuador_jurisdiction <- entity_detail%>%
  filter(ecuador== 1)%>%
  group_by(jurisdiction_description_entity)%>%
  dplyr::summarise(num_entities = n_distinct(name_entity))%>%
  ungroup()%>%
  arrange(desc(num_entities))%>%
  mutate(jurisdiction_description_entity = str_replace(jurisdiction_description_entity, "Malta", "Other"))%>%
  mutate(jurisdiction_description_entity = str_replace(jurisdiction_description_entity, "South Dakota", "Other"))%>%
  mutate(jurisdiction_description_entity = str_replace(jurisdiction_description_entity, "Belize", "Other"))%>%
  mutate(jurisdiction_description_entity = str_replace(jurisdiction_description_entity, "Samoa", "Other"))%>%
  mutate(jurisdiction_description_entity = str_replace(jurisdiction_description_entity, "Cayman Islands", "Other"))%>%
  mutate(jurisdiction_description_entity = str_replace(jurisdiction_description_entity, "New Zealand", "Other"))%>%
  mutate(jurisdiction_description_entity = str_replace(jurisdiction_description_entity, "Undetermined", "Other"))%>%
  mutate(jurisdiction_description_entity = str_replace(jurisdiction_description_entity, "United Kingdom", "Other"))%>%
  mutate(jurisdiction_description_entity = str_replace(jurisdiction_description_entity, "Costa Rica", "Other"))%>%
  mutate(jurisdiction_description_entity = str_replace(jurisdiction_description_entity, "Delaware", "Other"))%>%
  mutate(jurisdiction_description_entity = str_replace(jurisdiction_description_entity, "Hong Kong", "Other"))%>%
  group_by(jurisdiction_description_entity)%>%
  dplyr::summarise(num_entities = sum(num_entities))%>%
  ungroup()%>%
  arrange(desc(num_entities))


ecuador_jurisdiction$total <- sum(ecuador_jurisdiction$num_entities)

ecuador_jurisdiction$share <- (ecuador_jurisdiction$num_entities/ecuador_jurisdiction$total)*100

ecuador_jurisdiction$share <- as.numeric(format(round(ecuador_jurisdiction$share, 0), nsmall = 0))

# create an aggregate dataset for the plot 
jurisdiction_share <- entity_detail%>%
  group_by(jurisdiction_description_entity)%>%
  dplyr::summarise(num_entities = n_distinct(name_entity))%>%
  ungroup()

jurisdiction_share$total <- sum(jurisdiction_share$num_entities)

jurisdiction_share$share <- (jurisdiction_share$num_entities/jurisdiction_share$total)*100

jurisdiction_share$share <- as.numeric(format(round(jurisdiction_share$share, 0), nsmall = 0))

jurisdiction_share <- jurisdiction_share%>%
  select(jurisdiction_description_entity, share)%>%
  dplyr::rename("World" = "share")%>%
  left_join(honduras_jurisdiction %>% select(jurisdiction_description_entity, share)%>% dplyr::rename("Honduras" = "share"), by = "jurisdiction_description_entity")%>%
  left_join(senegal_jurisdiction %>% select(jurisdiction_description_entity, share)%>% dplyr::rename("Senegal" = "share"), by = "jurisdiction_description_entity")%>%
  left_join(ecuador_jurisdiction %>% select(jurisdiction_description_entity, share)%>% dplyr::rename("Ecuador" = "share"), by = "jurisdiction_description_entity")

jurisdiction_share <- jurisdiction_share[jurisdiction_share$jurisdiction_description_entity == "British Virgin Islands" |
                                           jurisdiction_share$jurisdiction_description_entity == "Panama"              |
                                           jurisdiction_share$jurisdiction_description_entity == "Malta"    |
                                           jurisdiction_share$jurisdiction_description_entity == "Seychelles", ]

# calculate the top 4 shares and the other shares 
jurisdiction_share[nrow(jurisdiction_share) + 1, 1]  <-  "Other" 

jurisdiction_share[nrow(jurisdiction_share), 2]  <- 100 - sum(jurisdiction_share[, "World"], na.rm = TRUE)

jurisdiction_share[nrow(jurisdiction_share), 3]  <- 100- sum(jurisdiction_share[, "Honduras"], na.rm = TRUE)

jurisdiction_share[nrow(jurisdiction_share), 4]  <- 100 - sum(jurisdiction_share[, "Senegal"], na.rm = TRUE)

jurisdiction_share[nrow(jurisdiction_share), 5]  <- 100- sum(jurisdiction_share[, "Ecuador"], na.rm = TRUE)

# change from wide to long 
jurisdiction_share <- jurisdiction_share %>%
  pivot_longer(cols = World : Ecuador,
               names_to = "Sample",
               values_to = "Share")

jurisdiction_share$jurisdiction_description_entity <- factor(jurisdiction_share$jurisdiction_description_entity, 
                                                             levels = c("Other",
                                                                        "Seychelles",
                                                                        "Malta",
                                                                        "Panama",
                                                                        "British Virgin Islands"))

jurisdiction_share$Jurisdiction <- jurisdiction_share$jurisdiction_description_entity

desired_order <- c("World", 'Ecuador', 'Honduras', 'Senegal')
jurisdiction_share$Sample <- factor(jurisdiction_share$Sample, levels = desired_order)

# Calculate the offset for the second column (adjust this value as needed to increase/decrease the distance)
column_offset <- 0.5

# Adjust x-axis values for the second column
jurisdiction_share$Adjusted_X <- ifelse(jurisdiction_share$Sample == "World", 1 - column_offset, as.numeric(jurisdiction_share$Sample))


# plot a percent stack graph of the share for the three countries and the world 
ggplot(jurisdiction_share, aes(fill = Jurisdiction, y = Share, x = Adjusted_X))+
  geom_bar(position="fill", stat="identity", width= 0.75)+
  scale_x_continuous(
    breaks = jurisdiction_share$Adjusted_X,  # Use the adjusted X-axis values
    labels = jurisdiction_share$Sample  # Use the corresponding "Sample" variable as labels
  ) +
  theme_classic()+
  scale_fill_brewer(palette = "YlGnBu", limits = c("British Virgin Islands", "Panama", "Malta", "Seychelles", "Other"))+
  labs(x="", y = "Share of firms incorporated in each jurisdiction")+
  # labs(title="Distribution of Jurisdictions that Firms Incorporated", x="Sample", y = "Share of firms incorporated in each jurisdiction",
  #      caption = "Note: Data were retrieved from ICIJ Leaks Database. The graph compares the distribution of jurisdictions that are related to each country 
  #                 \nand the world. The authors chose four major jurisdictions in the dataset. The share of the other jurisdictions are summed up in 'Other'.")+ 
  theme(legend.position="bottom")+        
  theme(plot.caption = element_text(hjust = 0)) + 
  scale_y_continuous(labels = scales::percent_format())+
  my_theme

ggsave(filename = paste0(output, '/Figure_1_Share_of_Jurisdictions.png'), 
       units = "px",
       width=800, 
       height=514, 
       dpi=100)



###############################################################################################
#                                      Figure 2
###############################################################################################

# Map: the share of tax avoidance in population -------------------------------

# clean officer countries 
master <- as.data.frame(master)

master[c("country1", "country2", "country3", "country4", "country5", "country6", "country7", "country8", "country9")] <- str_split_fixed(master$countries_officer, ';', 9)

country2 <- master%>%
  filter(!is.na(country2) &
           country2 != "")%>%
  dplyr::select(-countries_officer)%>%
  dplyr::rename(countries_officer = country2)%>%
  relocate(countries_officer, .after = name_officer)%>%
  dplyr::select(-c(country1:country9))

country3 <- master%>%
  filter(!is.na(country3)&
           country3 != "")%>%
  dplyr::select(-countries_officer)%>%
  dplyr::rename(countries_officer = country3)%>%
  relocate(countries_officer, .after = name_officer)%>%
  dplyr::select(-c(country1:country9))

country4 <- master%>%
  filter(!is.na(country4)&
           country4 != "")%>%
  dplyr::select(-countries_officer)%>%
  dplyr::rename(countries_officer = country4)%>%
  relocate(countries_officer, .after = name_officer)%>%
  dplyr::select(-c(country1:country9))

country5 <- master%>%
  filter(!is.na(country5)&
           country5 != "")%>%
  dplyr::select(-countries_officer)%>%
  dplyr::rename(countries_officer = country5)%>%
  relocate(countries_officer, .after = name_officer)%>%
  dplyr::select(-c(country1:country9))

country6 <- master%>%
  filter(!is.na(country6)&
           country6 != "")%>%
  dplyr::select(-countries_officer)%>%
  dplyr::rename(countries_officer = country6)%>%
  relocate(countries_officer, .after = name_officer)%>%
  dplyr::select(-c(country1:country9))

country7 <- master%>%
  filter(!is.na(country7)&
           country7 != "")%>%
  dplyr::select(-countries_officer)%>%
  dplyr::rename(countries_officer = country7)%>%
  relocate(countries_officer, .after = name_officer)%>%
  dplyr::select(-c(country1:country9))

country8 <- master%>%
  filter(!is.na(country8)&
           country8 != "")%>%
  dplyr::select(-countries_officer)%>%
  dplyr::rename(countries_officer = country8)%>%
  relocate(countries_officer, .after = name_officer)%>%
  dplyr::select(-c(country1:country9))

country9 <- master%>%
  filter(!is.na(country9)&
           country9 != "")%>%
  dplyr::select(-countries_officer)%>%
  dplyr::rename(countries_officer = country9)%>%
  relocate(countries_officer, .after = name_officer)%>%
  dplyr::select(-c(country1:country8))

master <- master%>%
  dplyr::select(-c(country1:country9))

master <- rbind(master,
                country2, 
                country3, 
                country4, 
                country5, 
                country6,
                country7, 
                country8,
                country9)

# dplyr::select first 10 countries of each officers 
master <- master %>% 
  mutate(country_unique = word(countries_officer, 1, sep = ';'))

# clean country names 
master$country_unique <- trimws(master$country_unique, which = c("left"))

# rewrite these code by data table 
master <- as.data.table(master)

master = master%>%
  .[country_unique== 'Croatia (Hrvatska)',  country_unique := "Croatia"]%>%
  .[country_unique== 'Brunei',  country_unique := "Brunei Darussalam"]%>%
  .[country_unique== 'Nevis',  country_unique := "Saint Kitts and Nevis"]%>%
  .[country_unique== 'American Samoa',  country_unique := "United States"]%>%
  .[country_unique== 'Bolivarian Republic of Venezuela',  country_unique := "Venezuela"]%>%
  .[country_unique== 'British Indian Ocean Territory',  country_unique := "United Kingdom"]%>%
  .[country_unique== 'CHINA',  country_unique := "China"]%>%
  .[country_unique== 'Dominican Republic',  country_unique := "Dominica"]%>%
  .[country_unique== 'Islamic Republic of Iran',  country_unique := "Iran"]%>%
  .[country_unique== "Korea, Democratic People's Republic of",  country_unique := "North Korea"]%>%
  .[country_unique== 'Korea, Republic of',  country_unique := "South Korea"]%>%
  .[country_unique== "Lao People's Democratic Republic",  country_unique := "Laos"]%>%
  .[country_unique== 'Not identified',  country_unique := ""]%>%
  .[country_unique== 'ok',  country_unique := ""]%>%
  .[country_unique== 'null',  country_unique := ""]%>%
  .[country_unique== 'Plurinational State of Bolivia',  country_unique := "Bolivia"]%>%
  .[country_unique== 'Province of China Taiwan',  country_unique := "China"]%>%
  .[country_unique== 'Taiwan, Province of China',  country_unique := "China"]%>%
  .[country_unique== 'Taiwan',  country_unique := "China"]%>%
  .[country_unique== 'Republic of Korea',  country_unique := "South Korea"]%>%
  .[country_unique== 'Republic of Moldova',  country_unique := "Moldova"]%>%
  .[country_unique== 'Réunion',  country_unique := "France"]%>%
  .[country_unique== 'Russian Federation',  country_unique := "Russia"]%>%
  .[country_unique== 'SC',  country_unique := "Seychelles"]%>%
  .[country_unique== 'Swaziland',  country_unique := "Eswatini"]%>%
  .[country_unique== 'Syrian Arab Republic',  country_unique := "Syria"]%>%
  .[country_unique== 'SWITZERLAND',  country_unique := "Switzerland"]%>%
  .[country_unique== 'The Democratic Republic of the Congo',  country_unique := "DR Congo"]%>%
  .[country_unique== 'United Republic of Tanzania',  country_unique := "Tanzania"]%>%
  .[country_unique== 'United Stales',  country_unique := "United States"]%>%
  .[country_unique== 'United States Minor Outlying Islands',  country_unique := "United States"]%>%
  .[country_unique== 'Venezuela, Bolivarian Republic of',  country_unique := "Venezuela"]%>%
  .[country_unique== 'Virgin Islands, British',  country_unique := "British Virgin Islands"]%>%
  .[country_unique== 'Bahama',  country_unique := "Bahamas"]%>%
  .[country_unique== 'Bahamass',  country_unique := "Bahamas"]%>%
  .[country_unique== 'Co Islands',  country_unique := "Cook Islands"]%>%
  .[country_unique== 'Unied States',  country_unique := "United States"]%>%
  .[country_unique== 'Cape Verde',  country_unique := "Cabo Verde"]%>%
  .[country_unique== 'French Guiana',  country_unique := "France"]%>%
  .[country_unique== 'Martinique',  country_unique := "France"]%>%
  .[country_unique== 'Mayotte',  country_unique := "France"]%>%
  .[country_unique== 'Norfolk Island',  country_unique := "Australia"]%>%
  .[country_unique== 'Sint Eustatius and Saba Bonaire',  country_unique := "Netherlands"]%>%
  .[country_unique== 'State of Palestine',  country_unique := "Palestine"]%>%
  .[country_unique== 'Viet Nam',  country_unique := "Vietnam"]%>%
  .[country_unique== 'North Macedonia',  country_unique := "Macedonia"]%>%
  .[country_unique== 'U.S. Virgin Islands',  country_unique := "US Virgin Islands"]%>%
  .[country_unique== 'Curaçao',  country_unique := "Curacao"]

length(unique(master$country_unique)) # 216 countries 

# if an officer is associated with multiple countries, count he/she in all the countries
master <- as.data.frame(master)

officer <- master%>%
  group_by(country_unique)%>%
  dplyr::summarise(num_officer = n_distinct(name_officer))%>%
  ungroup()%>%
  filter(country_unique != "")

# flag tax havens 
tax_haven_pattern <- paste0(tax_havens$tax_havens, collapse = "|")

officer$tax_havens <- ifelse(grepl(tax_haven_pattern, 
                                   officer$country_unique), 1, 0)

# load WDI population and 
wdi_data <- as.data.frame(WDI(indicator = c('NY.GDP.PCAP.KD','SP.POP.TOTL', "SI.POV.GINI"), start = 2018, end=2018))

wdi_data <- wdi_data%>%
  mutate(country = str_replace(country, "Bahamas, The", "Bahamas"))%>%
  mutate(country = str_replace(country, "Congo, Dem. Rep.", "DR Congo"))%>%
  mutate(country = str_replace(country, "Congo, Rep.", "Congo"))%>%
  mutate(country = str_replace(country, "Egypt, Arab Rep.", "Egypt"))%>%
  mutate(country = str_replace(country, "Cote d'Ivoire", "Côte d'Ivoire"))%>%
  mutate(country = str_replace(country, "Czechia", "Czech Republic"))%>%
  mutate(country = str_replace(country, "Gambia, The", "Gambia"))%>%
  mutate(country = str_replace(country, "Hong Kong SAR, China", "Hong Kong"))%>%
  mutate(country = str_replace(country, "Iran, Islamic Rep.", "Iran"))%>%
  mutate(country = str_replace(country, "Kyrgyz Republic", "Kyrgyzstan"))%>%
  mutate(country = str_replace(country, "Lao PDR", "Laos"))%>%
  mutate(country = str_replace(country, "Macao SAR, China", "Macao"))%>%
  mutate(country = str_replace(country, "North Macedonia", "Macedonia"))%>%
  mutate(country = str_replace(country, "Korea, Dem. People's Rep.", "North Korea"))%>%
  mutate(country = str_replace(country, "Korea, Rep.", "South Korea"))%>%
  mutate(country = str_replace(country, "Russian Federation", "Russia"))%>%
  mutate(country = str_replace(country, "St.", "Saint"))%>%
  mutate(country = str_replace(country, "Slovak Republic", "Slovakia"))%>%
  mutate(country = str_replace(country, "Syrian Arab Republic", "Syria"))%>%
  mutate(country = str_replace(country, "Turkiye", "Turkey"))%>%
  mutate(country = str_replace(country, "Venezuela, RB", "Venezuela"))%>%
  mutate(country = str_replace(country, "Yemen, Rep.", "Yemen"))%>%
  mutate(country = str_replace(country, "United Sainttes", "United States"))%>%
  mutate(country = str_replace(country, "Virgin Islands (U.S.)", "US Virgin Islands"))

officer <- officer %>% 
  left_join(wdi_data, 
            by = c('country_unique' = 'country')) %>% 
  dplyr::rename(gdp_pc = `NY.GDP.PCAP.KD`,
                population = `SP.POP.TOTL`,
                gini = SI.POV.GINI) %>% 
  mutate(leaks_share = 1000000*(num_officer/population)) %>%
  # change the share of leaks to 0 for all tax havens 
  mutate(leaks_per_mil= ifelse(tax_havens == 1, 0, leaks_share))

officer_graph <- officer %>%
  filter(leaks_per_mil > 0 & leaks_per_mil < 2000 & ! (country_unique %in% c("Iceland", "Libya"))) %>% 
  mutate(log_gdp = log(gdp_pc))


# plot with GDP pc 
ggplot(officer_graph, aes(x = log_gdp, y = leaks_per_mil)) + 
  geom_point() + 
  theme_classic()+
  # add a best fit line 
  # geom_smooth(method = lm, se = FALSE, color = "darkgreen")+

  # for Honduras 
  annotate("point", x = 7.79, y = 17.5, colour = "darkgreen", size = 3, alpha = 0.5)+ 
  annotate(geom="text", x= 7.29, y= 67.5, label="Honduras", color = "darkgreen")+
  annotate("segment", x = 7.79, xend = 7.29, y = 17.5, yend = 57.5, colour = "darkgreen", size=1, alpha=0.5)+
  
  # for Senegal 
  annotate("point", x = 7.23, y = 2.38, colour = "darkgreen", size = 3, alpha = 0.5)+ 
  annotate(geom="text", x= 6.73, y= 62.38, label="Senegal", color = "darkgreen")+
  annotate("segment", x = 7.23, xend = 6.73, y = 2.38, yend = 52.38, colour = "darkgreen", size=1, alpha=0.5)+
  
  
  # for United Arab Emirates 
  annotate("point", x = 10.68, y = 476.79, colour = "blue", size = 3, alpha = 0.5)+ 
  annotate(geom="text", x= 10.6, y= 440, label="United Arab Emirates", color = "blue")+
  annotate("segment", x = 10.3, xend = 10.6, y = 450, yend = 470, colour = "blue", size=1, alpha=0.5)+
  
  # for Ecuador 
  annotate("point", x = 8.70, y = 54, colour = "darkgreen", size = 3, alpha = 0.5)+ 
  annotate(geom="text", x= 8.2, y= 114, label="Ecuador", color = "darkgreen")+
  annotate("segment", x = 8.70, xend = 8.20, y = 54, yend = 104, colour = "darkgreen", size=1, alpha=0.5)+
  
  labs(x = "log(GDP per capita)", y = "Individual in data leaks (per Mil. inhabitants")+
  theme(legend.position="bottom")+
  theme(plot.caption = element_text(hjust = 0))+
  my_theme +  theme(axis.text = element_text(size = 20))

# "Note: Number of officers was retrieved from ICIJ Leaks Database. GDP per capita and population were retrived from World Development Indicators (World Bank) Database. 
# We excluded Tax havens on appendix B from this graph.
# On the graph, we highlighted the country of interests and the two outliers, Libya and Iceland."

# dropped US Virgin Islands (outlier)
# does not include tax havens 

ggsave(filename = paste0(output, 'Figure_2_Leaks_GDPpc.png'), 
       units = "px",
       width=1000, 
       height=700, 
       dpi=100)

fwrite(officer, paste0(intermediate, "Officert.csv"), row.names = F)


###############################################################################################
#                                      Figure 3: Map


# calculate quartile 
quartiles <- worldmap %>%
  dplyr::filter(tx_hvns != 1)

quartiles <- quantile(quartiles$lks_pr_, probs = seq(0, 1, 1/4), na.rm = TRUE)

# 0%          25%          50%          75%         100% 
# 0.0000000    0.4592301    7.5331761   53.4711033 6084.0552892 

worldmap <- worldmap%>%
  mutate(quartile = case_when(
    lks_pr_ <= 3.398649 ~ 0.15,
    lks_pr_ > 3.398649 & lks_pr_ <= 10.931225 ~ 0.4, 
    lks_pr_ > 10.931225 & lks_pr_ <= 70.402136 ~ 0.6,
    lks_pr_ > 70.402136 ~ 0.9
  ))%>%
  mutate(quartile_cut= cut(quartile, breaks=c(0.0, 0.25, 0.5, 0.75, 1.0)))

# flag tax haven as white 
worldmap <- worldmap%>%
  mutate(quartile_cut = replace(quartile_cut, tx_hvns == 1, NA))

# plot quartiles map 
cols <- c("#2c7bb6", "#abd9e9", "#fee090", "#fc8d59","white")

ggplot(worldmap %>% filter(REGION != "Antarctica")) +
  geom_sf(aes(fill = quartile_cut)) +
  labs(fill = "Quartiles") +
  scale_fill_manual(values = cols, breaks = c("(0,0.25]", "(0.25,0.5]", "(0.5,0.75]", "(0.75,1]"),
                    labels = c("0-0.46", "0.46-7.53", "7.53-53.47", "53.47-6,084.1"))+
  theme_void() +
  theme(legend.position = "none")+
  geom_point(data = worldmap %>% filter(tx_hvns == 1),
             aes(x = lon, y = lat),
             size = 1.5, color = "#d73027", show.legend = FALSE)+
  theme(legend.position="bottom")+
  theme(plot.caption = element_text(hjust = 0))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(size=11), #change legend title font size
    legend.text = element_text(size=11))   

#save plot
ggsave(filename = paste0(output, 'Figure_3_Leaks_per_million_quartiles.png'), 
       units = "px",
       width=800, 
       height=514, 
       dpi=100)

###############################################################################################
#                         Figure 4: Year of incorporation of Entities in Tax Havens

master <- read_csv("C:/Users/roxan/Dropbox/HighNetWorth_Project/Leaks_Data/02_Intermediate/Master.csv")
#master          <- fread(paste0(intermediate, "/Master.csv"))

# Select relevant columns and remove duplicates
master <- master %>% select(name_entity, original_name_entity, former_name_entity, jurisdiction_entity,
                            jurisdiction_description_entity, address_entity, incorporation_date_entity,
                            inactivation_date_entity, struck_off_date_entity, dorm_date_entity, status_entity,
                            service_provider_entity, country_codes_entity, countries_entity, sourceID_entity,
                            valid_until, honduras, ecuador, senegal) %>% distinct()

# Convert the incorporation_date_entity to a Date object and extract the year
master$incorporation_date_entity <- as.Date(master$incorporation_date_entity, errors = "coerce")
master$year <- format(as.Date(master$incorporation_date_entity), "%Y")

# Ensure that the 'year' column exists and is not NA
master <- master %>% filter(!is.na(year) & !is.na(source))

# Categorize the sourceID_entity into named sources
master$source <- ifelse(grepl("panama papers", tolower(master$sourceID_entity)), "Panama Papers",
                        ifelse(grepl("pandora papers", tolower(master$sourceID_entity)), "Pandora Papers",
                               ifelse(grepl("paradise papers", tolower(master$sourceID_entity)), "Paradise Papers",
                                      ifelse(grepl("offshore leaks", tolower(master$sourceID_entity)), "Offshore Leaks", NA))))

# Create additional flags for 'other countries' and 'world'
master$others_countries <- as.integer(rowSums(master[c("honduras", "senegal", "ecuador")]) == 0)
master$world <- 1

# Ensure 'year' and 'source' are factors or characters to group properly
master$source <- as.character(master$source)

# Ensure 'year' is treated as a numeric variable
master$year <- as.numeric(master$year)

# Ensure 'source' is a factor as aggregate requires it
master$source <- as.factor(master$source)

# Use aggregate to sum the counts for each country and the world by year and source
entity_year_country <- aggregate(cbind(honduras, senegal, ecuador, world) ~ year + source, data = master, sum, na.rm = TRUE)

# Filter the data to include only years from 1990 to 2019
filtered_data <- entity_year_country %>%
  filter(year >= 1990 & year <= 2019)

# Convert the filtered data into long format for ggplot2
# Each row should have 'year', 'source', 'country', and 'count'
entity_year_country_long <- filtered_data %>%
  gather(key = "country", value = "count", -year, -source)

entity_year_country_long$country <- str_to_title(as.character(entity_year_country_long$country)) # Capitalize first letter

# Reorder the levels of 'country' so that 'world' comes first
entity_year_country_long$country <- factor(entity_year_country_long$country, 
                                           levels = c("World", "Ecuador", "Honduras", "Senegal"))


# Plotting with ggplot2
plot_entity_year <- ggplot(entity_year_country_long, aes(x = year, y = count, fill = source)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~country, scales = "free_y", nrow = 2, ncol = 2) + # Arrange in a 2x2 grid
  scale_fill_manual(values = c("Offshore Leaks" = "purple",
                               "Panama Papers" = "blue",
                               "Pandora Papers" = "green",
                               "Paradise Papers" = "yellow")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # Remove the grid
        axis.text.x = element_text(angle = 90, hjust = 1), # Rotate x labels
        strip.text = element_text(face = "bold"), # Bold facet labels
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_blank()) + # Remove y-axis label
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1990, 2019, by = 5)) # Set x-axis breaks


# Save the plot
width <- 10  # Adjust this as needed
height <- (width / 16) * 9
ggsave(filename = paste0(output, 'entity_year_country.png'), plot = plot_entity_year, width = width, height = height)

