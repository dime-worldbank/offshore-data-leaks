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
# ---------------------------------------------------------------------------- #

# +++++++++++++++++++++++++++++++++ Steps ++++++++++++++++++++++++++++++++++++ #
# 
#        1)  Merge officers, entities, addresses, and intermediaries to relationships 
#        2)  Assign officers to countries 
#        3)  Assign entities to countries 
#        4)  clean Master 
#                                                                                 
# ---------------------------------------------------------------------------- #


#   Loading datasets ----------------------------------------------------------
    nodes_officers        <- fread(paste0(raw, "/nodes-officers.csv"), encoding = 'UTF-8', stringsAsFactors = FALSE)
    nodes_addresses       <- fread(paste0(raw, "/nodes-addresses.csv"), encoding = 'UTF-8', stringsAsFactors = FALSE)
    nodes_entities        <- fread(paste0(raw, "/nodes-entities.csv"), encoding = 'UTF-8', stringsAsFactors = FALSE)
    nodes_intermediaries  <- fread(paste0(raw, "/nodes-intermediaries.csv"), encoding = 'UTF-8', stringsAsFactors = FALSE)
    relationships         <- fread(paste0(raw, "/relationships.csv"), encoding = 'UTF-8', stringsAsFactors = FALSE)

    
#   Cleaning and Set Up -------------------------------------------------------

    #   Initial cleaning 
  
      #   nodes_officers
      #   'officer' refers to BO, shareholders, or management, etc
      colnames(nodes_officers)<-paste(colnames(nodes_officers),"officer",sep="_")
      
      ###   Address data issue 1: shell company as officer name/BO 
      ###     solution: flag officers that have corporate suffix
      ###     ATTN    : need to trace ultimate BO 
      
      corp_suffix          <- read.csv(paste0(raw, "/corp_suffix.csv"), stringsAsFactors = FALSE)
    
      # to match words and not wrongly include officer names, we change both officer names and Corp suffix into lower case,
      # add a space in front of each crop suffix, and delete 'CU' 
      # add s.a., Trust, Manufacturing, N.V., Establishment, Trading Est
      
      nodes_officers$name_officer  <- tolower(nodes_officers$name_officer)
      corp_suffix$words    <- tolower(corp_suffix$words)
      
      corp_suffix$words   <- paste0(" ", corp_suffix$words, " ")

      nodes_officers <- as.data.frame(nodes_officers)
      
      name_of_characters_pattern <- paste0(corp_suffix$words, collapse = "|")
      
      nodes_officers$Corp <- ifelse(grepl(name_of_characters_pattern, 
                                          nodes_officers$name_officer), 1, 0)
      
      #   nodes_entities
      nodes_entities <- nodes_entities %>% 
        mutate_at(vars("name", "original_name", "address"), funs(tolower(.)))
      
      nodes_entities[, c("ibcRUC","internal_id", "company_type") := NULL]
      
      colnames(nodes_entities)<-paste(colnames(nodes_entities),"entity",sep="_")
    
      #   nodes_addresses 
      nodes_addresses$address <- tolower(nodes_addresses$address)
      
      colnames(nodes_addresses)<-paste(colnames(nodes_addresses),"address",sep="_")
      
      #   nodes_intermediaries
      nodes_intermediaries$name <- tolower(nodes_intermediaries$name)
      
      colnames(nodes_intermediaries)<-paste(colnames(nodes_intermediaries),"interm",sep="_")
      
      #   relationships
      setnames(relationships, 
               old = c("node_id_start", "node_id_end", "start_date", "end_date"),
               new = c("node_id_officer", "node_id_entity", "start_rel", "end_rel"))

  
    #   Merge to Master dataset
      
      #   Merge Officer to Relationships
      master <- left_join(relationships, nodes_officers,
                          by = "node_id_officer")
    
      #   Merge Entities to Relationships 
      master <- left_join(master, nodes_entities,
                        by = "node_id_entity")
    
      #   Merge Addresses to Relationships
      setnames(master, 
               old = "node_id_entity",
               new = "node_id_address")
    
      master <- left_join(master, nodes_addresses,
                          by = "node_id_address")
      
      #   Merge Intermediaries to Relationships
      setnames(master,
               old = "node_id_officer",
               new = "node_id_interm")
      
      master <- left_join(master, nodes_intermediaries,
                          by = "node_id_interm")
  
  
#   Assign officers to countries ---------------------------------------------
  
    #   Step 1 flag officers by entity 
    #   flag those who were not assigned to any countries but were related to an entity that was assigned to a country
    #   ATTN: "the bearer": the bearer 71862, el portador 9351, bearer 1 2668, bearer 1371, the bearer 114 
  
         #    First fix data issue 2: observations were not assigned to any countries, but had officers' names in entities' addresses column.
         #    This means that the entity is related to that country because of the officer. Thus, assign officers to the country their entities are related to 
         #    First split observations that combined 2 names in the name column// 184 observations
            master <- as.data.frame(master)
  
  
          #  master[c("name_officer1", "name_officer2")] <- 
          #    str_split_fixed(master$name_officer, " y , and , and/or ", 2)
            
          #  master[c("name_officer2", "name_officer3")] <- 
          #    str_split_fixed(master$name_officer2, " y, and , and/or ", 2)
            
            master[c("name_officer1", "name_officer2", 'name_officer3')] <- master %>% 
              separate(name_officer, into = paste0('name_officer', 1:3), sep = ' y ') %>%
              select(c("name_officer1", "name_officer2", 'name_officer3'))
             
         #    Flag the observations that were not assigned to any countries by entities' country
            master_fil <- 
              master%>%
              filter(countries_officer == "" 
                     & countries_entity != ""
                     & name_officer1 != "" 
                     & rel_type == "officer_of" 
                     & name_officer != "the bearer"
                     & name_officer != "bearer")%>%
              mutate(name_match = ifelse(str_detect(address_entity, fixed(as.character(name_officer1))), 1, 0))%>%
              filter(name_match == 1)%>%
              mutate(honduras_entity = case_when(str_detect(countries_entity, "Honduras") ~ 1,
                                          TRUE ~ 0),
                     senegal_entity = case_when(str_detect(countries_entity, "Senegal") ~ 1,
                                          TRUE ~ 0),
                     ecuador_entity = case_when(str_detect(countries_entity, "Ecuador") ~ 1,
                                          TRUE ~ 0))%>%
              select(starts_with("node_id"), 
                     honduras_entity:ecuador_entity)
            
            master <- left_join(master, master_fil, 
                                by = c("node_id_interm", "node_id_address"))
                       
          #     Second fix data issue 3: officer name column contains country information 
          #     Solution: identify and assign countries to those observations 
            master<- 
              master%>%
              mutate(honduras_off = case_when(str_detect(name_officer, "(honduras)") ~ 1,
                                             TRUE ~ 0),
                     senegal_off = case_when(str_detect(name_officer, "(senegal)") ~ 1,
                                            TRUE ~ 0),
                     ecuador_off = case_when(str_detect(name_officer, "(ecuador)") ~ 1,
                                            TRUE ~ 0))

      # Step 2 flag officers by officers' country and registered address  
      master <- 
        master%>%
        mutate(honduras_add = case_when( str_detect(countries_officer, "Honduras")|
                                       str_detect(countries_address, "Honduras")~ 1,
                                     TRUE ~ 0),
               senegal_add = case_when( str_detect(countries_officer, "Senegal")|
                                      str_detect(countries_address, "Senegal")~ 1, 
                                    TRUE ~ 0),
               ecuador_add = case_when( str_detect(countries_officer, "Ecuador")|
                                      str_detect(countries_address, "Ecuador")~ 1, 
                                    TRUE ~ 0))
      
      # Step 3 flag honduras, ecuador, and senegal officers 
      
      cols = c("honduras_entity", "ecuador_entity", "senegal_entity")
      master = setnafill(master, cols=cols, fill=0)
      master$honduras = apply(master[,c("honduras_entity", "honduras_off", "honduras_add")], 1, sum)
      master$ecuador = apply(master[,c("ecuador_entity", "ecuador_off", "ecuador_add")], 1, sum)
      master$senegal = apply(master[,c("senegal_entity", "senegal_off", "senegal_add")], 1, sum)
      
      
      
      # Generate an address dataset 
      registered_addresses <- master%>%
        filter(rel_type == "registered_address")%>%
        dplyr::rename(node_id_officer = node_id_interm)%>%
        select( c(node_id_officer:link),c(address_address, valid_until_address),c(honduras:senegal))
      
      registered_addresses <- left_join(registered_addresses, nodes_officers,
                                        by = "node_id_officer")
      
      # Generate a master dataset 
      master <- master%>%
        distinct(name_officer, link, name_entity, .keep_all = TRUE)%>%
        subset(rel_type == "officer_of")%>%
        select(-c(honduras_off:ecuador_add))

      

#   Construct officer-level dataset ---------------------------------------------

    # create dummy variables 
    officer_match <- master%>% 
      mutate(panama_papers = case_when(str_detect(sourceID, "Panama Papers") ~ 1, 
                                       TRUE ~ 0),
             pandora_papers = case_when(str_detect(sourceID, "Pandora Papers") ~ 1, 
                                        TRUE ~ 0),
             paradise_papers = case_when(str_detect(sourceID, "Paradise Papers") ~ 1, 
                                         TRUE ~ 0),
             shareholder = case_when(str_detect(link, "shareholder") ~ 1, 
                                     TRUE ~ 0),
             director = case_when(str_detect(link, "director") ~ 1, 
                                  TRUE ~ 0),
             secretary = case_when(str_detect(link, "secretary") ~ 1, 
                                   TRUE ~ 0),
             judicial = case_when(str_detect(link, "judicial") ~ 1, 
                                  TRUE ~ 0),
             bo = case_when(str_detect(link, c("eneficia")) ~ 1,       # beneficiary of, Ultimate Beneficial Owner, Beneficial Owner, beneficial owner of
                            TRUE ~ 0))%>%
      mutate(match_link = paste(countries_officer, 
                                name_officer, 
                                sourceID, 
                                Corp,
                                sep="___"))
    
    # calculate variables and 
    officer1 <- 
      officer_match %>% 
      group_by(match_link) %>% 
      dplyr::summarise(across(honduras:bo, ~max(.)))%>%
      ungroup()
    
    officer2 <- 
      officer_match %>%
      group_by(match_link)%>%
      dplyr::summarise(avg_entity = n_distinct(name_entity))%>%
      ungroup()
      
    officer3 <- 
      officer_match%>%
      group_by(match_link)%>%
      dplyr::summarise(avg_rel = n())%>%
      ungroup()
    
    officer <- purrr::reduce(
      list(officer1,
           officer2,
           officer3),
      dplyr::left_join,
      by = "match_link")
    
    officer[c("countries_officer", "name_officer", "sourceID", "Corp")] <- str_split_fixed(
      officer$match_link, "___", 4)
    
    officer <- officer%>%
      subset(avg_rel <1000)%>%
      select(-match_link)%>%
      relocate(name_officer)%>%
      mutate(owners       = ifelse(bo == 1 | shareholder == 1, 1, 0),
             management   = ifelse(director == 1 | secretary == 1 | judicial == 1, 1, 0))%>%
      relocate(c(owners, management), .after = paradise_papers)
    
    officer <- officer %>%
      mutate(source_other = ifelse(panama_papers != 1 & pandora_papers != 1 & paradise_papers != 1, 1, 0),
             role_both    = ifelse(owners == 1 & management == 1, 1, 0))%>%
      relocate(source_other, .after = paradise_papers)%>%
      relocate(role_both, .after = management)
    
    rm(officer_match)
    
    # create leak date (format: m/d/y) 
      leak_date        <- fread(paste0(raw, "/Leak_Date.csv"), stringsAsFactors = FALSE)
      
      officer <- left_join(officer, leak_date,
                          by = "sourceID")
      
      officer$leak_date <- mdy(officer$leak_date)
    
    fwrite(officer, paste0(intermediate, "/Officer.csv"), row.names = F)
    
  
#   Construct entity-level dataset --------------------------------------------
  
    # clean jurisdiction in the entire sample to make each jurisdiction name and its abbreviation unique 
    master <- as.data.table(master)
    
    master = master%>%
      .[jurisdiction_entity == '',                                     jurisdiction_entity := "XXX"]%>%
      .[jurisdiction_description_entity == 'Anguilla',                 jurisdiction_entity := 'AIA']%>%
      .[jurisdiction_entity == 'BHS',                                  jurisdiction_entity := 'BAH']%>%
      .[jurisdiction_entity == 'BS' ,                                  jurisdiction_entity := 'BAH']%>%
      .[jurisdiction_description_entity == 'Barbados',                 jurisdiction_entity := 'BRB']%>%
      .[jurisdiction_description_entity == 'Belize',                   jurisdiction_entity := 'BLZ']%>%
      .[jurisdiction_entity == 'BERMU',                                jurisdiction_entity := 'BM']%>%
      .[jurisdiction_description_entity == 'BRITISH VIRGIN ISLANDS',   jurisdiction_description_entity := 'British Virgin Islands']%>%
      .[jurisdiction_entity == 'VGB',                                  jurisdiction_entity := 'BVI']%>%
      .[jurisdiction_entity == 'VG',                                   jurisdiction_entity := 'BVI']%>%
      .[jurisdiction_description_entity == 'Cayman',                   jurisdiction_entity := 'KY']%>%
      .[jurisdiction_description_entity == 'Cayman Islands',           jurisdiction_entity := 'KY']%>%
      .[jurisdiction_description_entity == 'Cayman',                   jurisdiction_description_entity := 'Cayman Islands']%>%
      .[jurisdiction_description_entity == 'COOK ISLANDS',             jurisdiction_description_entity := 'Cook Islands']%>%
      .[jurisdiction_description_entity == 'COOKISLANDS',              jurisdiction_description_entity := 'Cook Islands']%>%
      .[jurisdiction_description_entity == 'Cook Islands',             jurisdiction_entity := 'COOK']%>%
      .[jurisdiction_description_entity == 'Cyprus',                   jurisdiction_entity := 'CYP']%>%
      .[jurisdiction_description_entity == 'Gibraltar',                jurisdiction_entity := 'GIB']%>%
      .[jurisdiction_description_entity == 'GUERNSEY',                 jurisdiction_description_entity := 'Guernsey']%>%
      .[jurisdiction_description_entity == 'Guernsey',                 jurisdiction_entity := 'GG']%>%
      .[jurisdiction_description_entity == 'Hong Kong',                jurisdiction_entity := 'HKG']%>%
      .[jurisdiction_description_entity == 'Isle of Man',              jurisdiction_entity := 'IM']%>%
      .[jurisdiction_description_entity == 'Jersey',                   jurisdiction_entity := 'JE']%>%
      .[jurisdiction_description_entity == 'Liberia',                  jurisdiction_entity := 'LR']%>%
      .[jurisdiction_description_entity == 'Malta',                    jurisdiction_entity := 'MLT']%>%
      .[jurisdiction_description_entity == 'Marshall Islands',         jurisdiction_entity := 'MARSH']%>%
      .[jurisdiction_description_entity == 'Mauritius',                jurisdiction_entity := 'MU']%>%
      .[jurisdiction_description_entity == 'Nevada',                   jurisdiction_entity := 'NEV']%>%
      .[jurisdiction_description_entity == 'New Zealand',              jurisdiction_entity := 'NZL']%>%
      .[jurisdiction_entity == 'PAN',                                  jurisdiction_entity := 'PMA']%>%
      .[jurisdiction_entity == 'PA',                                   jurisdiction_entity := 'PMA']%>%
      .[jurisdiction_description_entity == 'Saint Kitts and Nevis',    jurisdiction_entity := 'KN']%>%
      .[jurisdiction_description_entity == 'Samoa',                    jurisdiction_entity := 'SAM']%>%
      .[jurisdiction_entity == 'SC',                                   jurisdiction_entity := 'SEY']%>%
      .[jurisdiction_entity == 'SYC',                                  jurisdiction_entity := 'SEY']%>%
      .[jurisdiction_description_entity == 'Singapore',                jurisdiction_entity := 'SGP']%>%
      .[jurisdiction_description_entity == 'State of Delaware',        jurisdiction_description_entity := 'Delaware']%>%
      .[jurisdiction_entity == 'XX',                                   jurisdiction_entity := "XXX"]%>%
      .[jurisdiction_description_entity == 'United Arab Emirates',     jurisdiction_entity := 'ARE']%>%
      .[jurisdiction_description_entity == 'United Kingdom',           jurisdiction_entity := 'GBR']%>%
      .[jurisdiction_description_entity == 'United States of America', jurisdiction_entity := 'USA']%>%
      .[jurisdiction_description_entity == 'USA (Delaware)',           jurisdiction_description_entity := 'Delaware']%>%
      .[jurisdiction_description_entity == 'Delaware',                 jurisdiction_entity := 'USDE']%>%
      .[jurisdiction_description_entity == 'USA (South Dakota)',       jurisdiction_entity := 'USA']%>%
      .[jurisdiction_description_entity == 'USA (South Dakota)',       jurisdiction_description_entity := 'South Dakota']%>%
      .[jurisdiction_description_entity == 'Vanuatu',                  jurisdiction_entity := 'VANU']%>%
      .[jurisdiction_description_entity == 'Wyoming',                  jurisdiction_entity := 'WYO']%>%
      .[jurisdiction_description_entity == '',                         jurisdiction_entity := 'XXX']%>%
      .[jurisdiction_entity == 'XXX',                                  jurisdiction_description_entity := 'Undetermined']%>%
      .[jurisdiction_description_entity == 'United States',            jurisdiction_description_entity := 'United States of America']%>%
      .[jurisdiction_description_entity == 'United States Of America', jurisdiction_description_entity := 'United States of America']
      
      
  
    #   flag entities that are related to the BOs of the three countries
    entity <- master%>%
      mutate(match_link = paste(name_entity,
                                countries_entity,
                                jurisdiction_entity,
                                sep="___"))%>%
      group_by(match_link)%>%
      dplyr::summarise(across(honduras:senegal, ~max(.)))%>%
      ungroup()
    
    entity[c("name_entity", "countries_entity", "jurisdiction_entity")] <- str_split_fixed(
      entity$match_link, "___", 3
    )
    
    entity <- entity%>% 
      select(-match_link)%>%
      relocate(name_entity, countries_entity, jurisdiction_entity)
    
  #   jurisdiction
    entity <-
      entity %>%
      mutate(bvi        = ifelse(jurisdiction_entity == "BVI", 1, 0),
             malta      = ifelse(jurisdiction_entity == "MLT", 1, 0),
             barbados   = ifelse(jurisdiction_entity == "BRB", 1, 0),
             panama     = ifelse(jurisdiction_entity == "PMA", 1, 0),
             bahamas    = ifelse(jurisdiction_entity == "BAH", 1, 0),
             nev        = ifelse(jurisdiction_entity == "NEV", 1, 0),
             sey        = ifelse(jurisdiction_entity == "SEY", 1, 0),
             ang        = ifelse(jurisdiction_entity == "ANG", 1, 0),
             bm         = ifelse(jurisdiction_entity == "BM" , 1, 0),
             unidentified = ifelse(jurisdiction_entity == "XXX", 1, 0))
      
   entity <- entity %>%
     mutate(other = rowSums(.[7:16]))%>%
     mutate(other = 1 - other)

    fwrite(entity, paste0(intermediate, "/Entity.csv"), row.names = F)


#   Clean Master ----------------------------------------------------------------
      master$start_rel <- dmy(master$start_rel) # failed to parse
      
      master$end_rel <- dmy(master$end_rel) # failed to parse
      
      master$incorporation_date_entity <- dmy(master$incorporation_date_entity)
      
      master$inactivation_date_entity <- dmy(master$inactivation_date_entity)
      
      master$struck_off_date_entity <- dmy(master$struck_off_date_entity)
      
      # generate a Master.csv in intermediate 
      master <- 
        master%>%
        dplyr::rename(node_id_officer = node_id_interm,
               node_id_entity = node_id_address,
               valid_until = valid_until_officer)%>%
        select (-c("sourceID_officer",
                           "note_officer",
                           "valid_until_entity",
                           "note_entity",
                           "address_address":"note_interm"))
      
      fwrite(master, paste0(intermediate, "/Master.csv"), row.names = F)
      
      # generate a csv for entities details in intermediate
      entities <- master %>%
        filter(honduras == 1 | ecuador == 1 | senegal == 1) %>%
        select(name_entity:sourceID_entity, valid_until, honduras:senegal) %>%
        mutate(other_countries = ifelse(honduras == 0 & ecuador == 0 & senegal == 0, 1, 0))
      
      entities          <- unique(entities)
      
      fwrite(entities, paste0(intermediate, "/Entities_detail.csv"), row.names = F)
      
      
      # generate a csv file for registered address 
      fwrite(registered_addresses, paste0(intermediate, "Registered_address.csv"), row.names = F)
      

# Load Senegal data from Openlux 
     load(file= paste0(raw, '/Senegal.rda'))
     
     senegal_openlux <- Senegal %>%
       relocate(company_rcs:company_current_rbe_status, .after = leak_id)%>%
       dplyr::select(-final_weight)
    
     fwrite(senegal_openlux, paste0(cleaned, "List of Senegal Officer_openlux.csv"), row.names = F)
     
    
## Entities by year and countries
     master <- fread(paste0(intermediate, "/Master.csv"), encoding = 'UTF-8', stringsAsFactors = FALSE)
     
     
     master$incoporation_year <- year(master$incorporation_date_entity)
     
     entitiy_year_country <- master %>%
       dplyr::group_by(countries_entity, incoporation_year, sourceID) %>%
       dplyr::summarise(count = n())
      
      