# load required packages
library(openalexR)
library(httr2)
library(tidyverse)
library(purrr)

# this code functionally replaces the rorcid_works and rcrossref_metadata code
# and partially replaces the unpaywall code
# these scripts are still available on the GitHub repo if you are interested
# but we will not be covering rorcid_works or rcrossref_metadata in class

# A NOTE BEFORE WE START
# OpenAlex isn't always 100% accurate
# but it will pull all (or at least the majority) of works
# associated with a particular ORCID iD
# whereas the ORCID database only reports works that the person with the ORCID iD
# has approved and added to their profile
# and OpenAlex seems to be doing a fair bit of disambiguation

# remove all objects from the environment
# to start with a clean slate
rm(list = ls())

# Read in the orcid person data we collected in 02-rorcid_employments
orcid_ids <- read_csv("./data/results/orcid_employment_file.csv",
                      col_types = cols(.default = "c"))

# create a vector of unique, deduplicated ORCID IDs from that file
orcid_list <- orcid_ids %>%
  filter(!duplicated(orcid_identifier_path)) %>%
  pull(orcid_identifier_path) %>%
  na.omit() %>%
  as.character()

# construct urls to send in API call
# base URL + ORCID iD from each institutional affiliate
openalex_api_url <- paste0("https://api.openalex.org/works?filter=authorships.author.orcid:",
                           orcid_list)


### Create initial OpenAlex query & pull dataframe ---------------------------------------------------------

######################################################### 
######### when running on your own,######################
######### you can run a larger set of ORCID iDs ######### 
######### but we will be using the first 20 here ######## 
######################################################### 

# loop through first 20 ORCID iDs
# create API query
# run the query for "works"
# obtain available metadata & add to oa_df_openr dataframe
# this will take a moment

# create empty data frame
oax_works <- data.frame()

# "safely" remakes the function so that as it runs,
# any errors it encounters don't cause the loop to fail
# it simply moves on to the next item and reports its errors in the end
safe_oax_request <- safely(oa_request)

###################################################
## When you run this on your own after the class,##
#### replace the 1:20 with 1:length(orcid_list) ###
###################################################

for(orcid in 1:20){
  print(paste0("Loop ",orcid)) # keep track of which loop we are in
  
  oax_pull = safe_oax_request(query_url = openalex_api_url[orcid]) # create the API request
  oax_works_df = oa2df(oax_pull$result, # only need the "result" column from `safely` output
                     entity = "works") # query the "works" endpoint and obtain and dataframe
  
  if(is_empty(lengths(oax_works_df))) { # if no data, then skip that ORCID iD
    next
  }
  
  oax_works_df_index = oax_works_df %>% 
    mutate(orcid_index = orcid, # makes ORCID iD traceable
           work_index = rep(1:nrow(oax_works_df))) %>% # identifies individual works per ORCID
    rename(work_id = id, # rename to avoid confusion/errors
           work_type = type, # rename to avoid confusion/errors
           work_display_name = display_name) %>% # rename to avoid confusion/errors
    relocate(orcid_index,.before = work_id)%>% 
    relocate(work_index,.after = orcid_index)
  
  if(is_empty(oax_works)){ # for first entry - avoid NA row in dataframe
    oax_works <- oax_works_df_index # replaces empty contents of oa_works
  }
  if(!is_empty(oax_works)){
    oax_works <- oax_works %>% 
      full_join(.,oax_works_df_index)
  }
}

# will get warnings if ORCID search doesn't find any works
# will also get warnings if works have truncated lists of authors

# reorder columns
oax_works <- oax_works %>% 
  select(doi, orcid_index, work_index,title, work_type, 
         publication_year, source_display_name,everything())

# and add ORCID iDs in
# create a list of numbers as long as the orcid_list
index <- seq(1,length(orcid_list),by = 1)
# combine that list with the orcid_list item to create a dataframe
orcid_df <- as.data.frame(cbind(orcid_list,index)) %>% 
  mutate(index = as.integer(index)) %>% 
  rename(orcid_index = "index",
         orcid_search = "orcid_list")
# add the ORCID iDs to oa_works
oax_works_orcid <- oax_works %>% 
  left_join(.,orcid_df, by = "orcid_index") %>%
  relocate(orcid_search, .after = doi)

View(oax_works_orcid)
# nested columns include:
# authorships
# counts_by_year
# apc
# plus some lists (related_works, ids)

# save this file
write_csv(oax_works_orcid,"data/results/openalex_orcid_works.csv")


### extract author info -----------------------------------------

# create empty dataframe
oax_authors <- data.frame()

###################################################
## When you run this on your own after the class,##
#### replace the 1:20 with 1:length(orcid_list) ###
###################################################

for(orcid in 1:20){
  print(paste0("Loop ",orcid)) # keep track of which loop we are in
  
  if(!any(oax_works_orcid$orcid_index == orcid)) { # skips ORCID iDs not in dataset
    next
  }
  author_info = oax_works_orcid %>% 
    filter(orcid_index == orcid) 
  
  for(work in 1:nrow(author_info)){
    author_flatten = author_info %>% 
      filter(work_index == work) %>% # filter for this loop's work_index
      unnest(cols = authorships) %>% # unnests contents of authorships column
      rename(author_id = id, # rename to avoid confusion/errors
             author_display_name = display_name) # rename to avoid confusion/errors
    
    author_index = rep(1:nrow(author_flatten)) # create index list for authors in a particular work
    
    author_df = author_flatten %>% 
      cbind(.,author_index) %>% # add author_index column
      mutate(has_affiliation = map_lgl(affiliations, ~ nrow(.x) > 0)) %>% # test whether author has affiliation info available
      relocate(c(orcid_index,work_index,author_index),.before = author_id) # shift column locations
    
    for(author in 1:nrow(author_df)){
      if(author_df$has_affiliation[author] == FALSE){ # test whether affiliation info is available
        if(is_empty(oax_authors)){
          oax_authors <- author_df # replaces empty contents of author_info_df
        }
        if(!is_empty(oax_authors)){
          oax_authors <- oax_authors %>% 
            full_join(.,author_df)
        }
        next
      }
      author_affil = author_df %>% 
        filter(author_index == author) %>% # filter for this author_index for this work_index for this orcid_index
        unnest(cols = affiliations) %>% # unnests contents of affiliations column
        rename(affil_id = id, # rename to avoid confusion/errors
               affil_display_name = display_name,
               affil_type = type) # rename to avoid confusion/errors
      
      if(is_empty(oax_authors)){
        oax_authors <- author_affil # replaces empty contents of author_info_df
      }
      if(!is_empty(oax_authors)){
        oax_authors <- oax_authors %>% 
          full_join(.,author_affil)
      }
    }
  }
}

# relocate order of columns
oax_authors <- oax_authors %>% 
  select(doi, orcid_search, orcid_index, work_index, author_index,
         title, work_type, publication_year, source_display_name,
         author_display_name, orcid, is_corresponding,
         affil_display_name, ror, affil_type,everything())

View(oax_authors)

write_csv(oax_authors,"data/results/openalex_author_works.csv")


# get a full list of authors without all the extra info
author_collated <- oax_authors %>% 
  select(orcid_index,work_index,author_display_name) %>% 
  group_by(orcid_index,work_index) %>% 
  mutate(author_list = paste(author_display_name, collapse = "|")) %>% 
  select(orcid_index,work_index,author_list) %>% 
  distinct()

# combine the author list with the original oa_works dataset we collected
oax_works_author_collated <- oax_works_orcid %>% 
  full_join(.,author_collated, by = c("orcid_index","work_index")) %>% 
  relocate(author_list, .after = work_display_name) %>% 
  select(doi, orcid_search, orcid_index, work_index,title, work_type, 
         publication_year, source_display_name,author_list,
         everything())

# save this file too
write_csv(oax_works_author_collated,"data/results/openalex_works_author_collated.csv")

# now let's combine this with the orcid_employment_file.csv
# so we have a more complete dataset

# looking at the data -----------------------------------------------------

# number of unique ORCID iDs found in OpenAlex
length(unique(oax_works_orcid$orcid_index)) 

# max number of works from single author 
max(oax_works_orcid$work_index) 

# histogram of number of works from each author
# you can adjust the number of bins (breaks) if you want
hist(oax_works_orcid$orcid_index)

# count up the number of each work type
# and order them from most to least prevalent
oax_works_orcid %>% 
  group_by(work_type) %>% 
  count() %>% 
  arrange(desc(n))

# histogram of reported citation counts
hist(oax_works_orcid$cited_by_count,
     breaks = 20)

# sort by top cited
top_cited <- oax_works_orcid %>%
  relocate(cited_by_count, .after = doi) %>% 
  arrange(desc(cited_by_count))

# source (journal) name
# create a table looking at the number of articles per journal
# then sort so that more highly used journals are at the top 
top_journals <- oax_works_orcid %>%
  filter(!is.na(source_display_name)) %>%
  group_by(source_display_name) %>%
  tally() %>%
  arrange(desc(n))

# host organization (publisher)
# count up the number of works published by a particular publisher
# then sort so more highly used publishers are at the top
top_publisher <- oax_works_orcid %>% 
  filter(!is.na(host_organization_name)) %>% 
  group_by(host_organization_name) %>% 
  tally() %>% 
  arrange(desc(n))


# line plot of publication_date (publication_year may be pretty similar)
pub_date_plot <- oax_works_orcid %>%
  count(publication_date) %>%
  ggplot(aes(x = publication_date, 
             y = n)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 year",
               date_minor_breaks = "6 months") +
  theme(axis.text.x = element_text(angle = 90))

print(pub_date_plot)

# publication_year instead
pub_year_plot <- oax_works_orcid %>%
  ggplot(aes(x = publication_year)) + 
  geom_histogram(binwidth = 1,
                 color = "black",
                 fill = "gold") 

print(pub_year_plot)

# visualizations and tabulations for authors

# see list of authors affiliated with your institution
# first get a list of potential name matches
# replace INSTITUTION BASE NAME with your institution's name
# for example: Oklahoma State University
my_institution_list <- oax_authors %>% 
  select(affil_display_name) %>% 
  filter(str_detect(affil_display_name,
                    "INSTITUTION BASE NAME")) %>% 
  distinct() %>% 
  pull()

# if your institution has name variations with more dissimilarity, try this:
# get a list of all institutions matching a more general name
# for example: Oklahoma
name_list <- oax_authors %>% 
  select(affil_display_name) %>% 
  filter(str_detect(affil_display_name,
                    "GENERAL NAME")) %>% 
  distinct()

# then select only the institution names that fit what you need
# NOTE - you will need to update c(1,5) with the list items you want to keep
my_institution_list_special <- name_list %>% 
  filter(row_number() %in% c(1,5)) %>% 
  pull()

# then filter the dataset for those affilations
# and select for the author name, OpenAlex ID, ORCID, and affiliation
# then remove duplicates

# NOTE - you will need to replace my_institution_list with my_institution_list_special
# if you used that approach
my_author_affil <- oax_authors %>% 
  filter(affil_display_name %in% my_institution_list) %>% 
  select(author_display_name,author_id,orcid,affil_display_name) %>% 
  distinct() %>% 
  arrange(author_display_name)

# since the list is sorted alphabetically
# you can observe authors with multiple OpenAlex IDs or affiliations
# for instance, OSU has a lot of OSU-OKC affiliations that don't all seem to be accurate
# and in some instances, affiliation metadata is missing
# such as in Crossref (api.crossref.org/works/doi:)
# but sometimes match OpenAlex DOI metadata (api.openalex.org/works?filter=doi:)

# let's look at overall affiliation names and their prevalence
top_affiliations <- oax_authors %>% 
  filter(!is.na(affil_display_name)) %>% 
  group_by(affil_display_name) %>% 
  tally() %>% 
  arrange(desc(n))

# and let's look at top authors
# NOTE - this does not include any name standardization
# so authors with name inconsistencies will be counted separately
top_authors <- oax_authors %>% 
  group_by(author_display_name) %>% 
  tally() %>% 
  arrange(desc(n))

