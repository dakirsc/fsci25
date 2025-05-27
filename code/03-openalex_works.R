library(openalexR)
library(httr2)
library(tidyverse)

# pull in list of ORCID iDs
unique_orcids <- read_csv("data/processed/my_orcids_data.csv") %>% 
  pull(orcid_identifier_path)

# construct urls to send in API call
# base URL + ORCID iD from each institutional affiliate
openalex_api_url <- paste0("https://api.openalex.org/works?filter=authorships.author.orcid:",
                           unique_orcids)


### Create initial OpenAlex query & pull dataframe ---------------------------------------------------------
# create empty data frame
oa_df_openr <- data.frame()

######################################################### 
######### when running on your own,######################
######### you can run a larger set of ORCID iDs ######### 
######### but we will be using the first 40 here ######## 
######################################################### 

# loop through first 40 ORCID iDs
# create API query
# run the query for "works"
# obtain available metadata & add to oa_df_openr dataframe
# this will take a moment
for(orcid in 1:40){
  oa_test_request = oa_request(openalex_api_url[orcid]) # create the API request
  oa_test_df = oa2df(oa_test_request, # query the "works" endpoint
                     entity = "works") # and obtain and dataframe
  
  if(is_empty(lengths(oa_test_df))) { # if no data, then skip that ORCID iD
    next
  }
  
  oa_test_df_index = oa_test_df %>% 
    mutate(orcid_index = orcid, # makes ORCID iD traceable
           work_index = rep(1:nrow(oa_test_df))) %>% # identifies individual works per ORCID
    rename(work_id = id, # rename to avoid confusion/errors
           work_type = type, # rename to avoid confusion/errors
           work_display_name = display_name) %>% # rename to avoid confusion/errors
    relocate(orcid_index,.before = work_id)%>% 
    relocate(work_index,.after = orcid_index)
  
  if(is_empty(oa_df_openr)){ # for first entry - avoid NA row in dataframe
    oa_df_openr <- oa_test_df_index # replaces empty contents of oa_df_openr
  }
  if(!is_empty(oa_df_openr)){
    oa_df_openr <- oa_df_openr %>% 
      full_join(.,oa_test_df_index)
  }
}

# will get errors if ORCID search doesn't find any works
# will also get error if works have truncated lists of authors

View(oa_df_openr)
# nested columns include:
  # authorships
  # counts_by_year
  # apc
# plus some lists (related_works, ids)
nrow(oa_df_openr)

write_csv(oa_df_openr,"data/raw/openalex_df_works.csv")


### extract author info -----------------------------------------

# create empty dataframe
author_info_df <- data.frame()

# update "40" with larger number of ORCIDs when working on own
# for this class, we are limiting to 40
for(orcid in 1:40){
  if(!any(oa_df_openr$orcid_index == orcid)) { # skips ORCID iDs not in dataset
    next
  }
  orcid_author_info = oa_df_openr %>% 
    filter(orcid_index == orcid) 
  
  for(work in 1:nrow(orcid_author_info)){
    # author_flatten = orcid_author_info$authorships[[work]] %>% 
    author_flatten = orcid_author_info %>% 
      filter(work_index == work) %>% 
      unnest(cols = authorships) %>% 
      rename(author_id = id,
             author_display_name = display_name) 
    
    author_index = rep(1:nrow(author_flatten))
    # work_index = rep(work, times = nrow(author_flatten)) # check accuracy
    # orcid_index = rep(orcid,times = nrow(author_flatten))
    
    author_work_df = author_flatten %>% 
      cbind(.,author_index) %>% 
      mutate(has_affiliation = map_lgl(affiliations, ~ nrow(.x) > 0)) %>%
      relocate(c(orcid_index,work_index,author_index),.before = author_id) 
    
    for(author in 1:nrow(author_work_df)){
      if(author_work_df$has_affiliation[author] == FALSE){
        if(is_empty(author_info_df)){
          author_info_df <- author_work_df # replaces empty contents of author_info_df
        }
        if(!is_empty(author_info_df)){
          author_info_df <- author_info_df %>% 
            full_join(.,author_work_df)
        }
        next
      }
      # author_flatten = orcid_author_info$authorships[[work]] %>% 
      author_affil_df = author_work_df %>% 
        filter(author_index == author) %>% 
        unnest(cols = affiliations) %>% 
        rename(affil_id = id,
               affil_display_name = display_name)
      
      if(is_empty(author_info_df)){
        author_info_df <- author_affil_df # replaces empty contents of author_info_df
      }
      if(!is_empty(author_info_df)){
        author_info_df <- author_info_df %>% 
          full_join(.,author_affil_df)
      }
    }
  }
}

View(author_info_df)

write_csv(author_info_df,"data/raw/work_author_affil_data.csv")