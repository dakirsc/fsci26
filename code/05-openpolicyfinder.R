# load the required packages
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(httr)
library(forcats)
library(usethis)
library(anytime)
library(janitor)
library(glue)
library(rorcid)
library(rcrossref)
library(roadoi)
library(inops)
library(openalexR)
library(httr2)
library(listviewer) # required for jsonedit
library(patchwork)

# remove all objects from the environment
# to start with a clean slate
rm(list = ls())

# read in the openalex/orcid merge data
oax_works_author <- read_csv("./data/results/openalex_works_author_collated.csv",
                     col_types = cols(.default = "c"))

# paste in your Open Policy Finder API key
# you can obtain this....
opf_key <- "ENTER YOUR KEY HERE"

# create safe, slow version of request and response 
safely_perform <- safely(req_perform)
safely_res <- safely(resp_body_json)

# create a  list with unique issns
oax_issn_lookup <- oax_works_author %>%
  filter(!is.na(issn_l),
         !duplicated(issn_l)) 


# construct urls to send in API call
  # each ISSN is queried separately 
opf_api_url <- paste0("https://api.openpolicyfinder.jisc.ac.uk/retrieve_by_id?item-type=publication&format=Json&identifier=",
                  oax_issn_lookup$issn_l)

###################################################
## When you run this on your own after the class,##
############### REMOVE THE [1:10] #################
###################################################
# We need to limit this because it takes a long time to get this data
# send the request
opf_request <- map(opf_api_url[1:10], ~request(.x))

# parse the results
opf_send <- map(opf_request, function(z) {
  print(z)
  getdata <- req_method(z, "GET") %>%
    req_headers("x-api-key" = opf_key) %>%
    safely_perform(.)
  return(getdata)
})

opf_response <- map(opf_send, function(z) {
  print("request")
  i <- pluck(z, "result")
  safely_res(i, check_type = FALSE)
})

opf_extract <- map(opf_response, pluck, "result", "items", 1)

# view JSON file
# NOTE: even though number_unnamed() ensures the top level of the list
# starts at 1 (instead of 0), 
# subsequent levels of the list start with 0 in this viewing mode
jsonedit(number_unnamed(opf_extract), mode = "view")

# if we instead view the file through a tab in RStudio, 
# we can see that subsequent list levels actually start at 1
View(opf_extract)

# create a data frame with some pertinent datapoints
###################################################
## When you run this on your own after the class,##
############### REMOVE THE [1:10] #################
####### after call = opf_api_url[1:10] ################
###################################################
opf_df <- opf_extract %>% {
  tibble(issn = map_chr(., pluck, "issns", 1, "issn", .default = NA_character_),
         title = map_chr(., pluck, "title", 1, "title", .default = NA_character_),
         sherpa_id = map_dbl(., pluck, "system_metadata", "id", .default = NA_integer_),
         publisher = map_chr(., pluck, "publishers", 1, "publisher", "name", 1, "name", .default = NA_character_),
         publisher_policy = map(., pluck, "publisher_policy"),
         call = opf_api_url[1:10]
  )
} %>%
  filter(!is.na(sherpa_id))

# view publisher policy column using jsonedit
jsonedit(number_unnamed(opf_df$publisher_policy),
         mode = "view")

# create a list with the policies for each call.
# one ISSN might have multiple policies.
# discard the empty items, and retrieve the ID
publisher_policyid <- opf_df$publisher_policy %>%
  discard(is_empty) %>%
  map_depth(., 2, pluck, "id", .default = NA_integer_)

# create a file that links each issn to its policy IDs
# this is done by creating a vector of policy ID numbers from above
# and the ISSNs we used in the call that actually 
# retrieved data (as deemed by the NA in the sherpa_id variable)
policyid_vec <- publisher_policyid %>%
  purrr::flatten() %>%
  as_vector()

issn_vec <- rep(opf_df$issn[!is.na(opf_df$sherpa_id)], lengths(publisher_policyid))

issn_policy <- tibble(policyid_vec,
                      issn_vec) 

# create a list including just the publisher policies
pubpolicy <- opf_df$publisher_policy %>% 
  map_depth(., 2, pluck, "permitted_oa") %>%
  discard(is_empty) 

# each policy may include one or more different pathways
# thus we take the lengths of each policy to determine
# how many pathways are in each, and repeat (rep) the 
# policy IDs in order to create a vector of policy IDs
# that is precisely as long as the number of pathways
policyid_names <- rep(policyid_vec, lengths(purrr::flatten(pubpolicy)))

# now we start parsing the data. 
# Each of the following retrieves (plucks) the desired
# data, collapsing it into a vector if necessary,
# and flattening it as many times as is necessary in order to create a single vector

# view pubpolicy using jsonedit
jsonedit(number_unnamed(pubpolicy), mode = "view")

conditions <- map_depth(pubpolicy, 3, pluck, "conditions", .ragged = TRUE, .default = NA) %>%
  modify_depth(., 3, paste, collapse = "|") %>%
  purrr::flatten() %>%
  purrr::flatten() %>%
  as_vector()

oa_fee <- map_depth(pubpolicy, 3, pluck, "additional_oa_fee", .ragged = TRUE, .default = NA_character_) %>%
  purrr::flatten() %>%
  purrr::flatten() %>%
  as_vector()

location <- map_depth(pubpolicy, 3, pluck, "location", "location", .ragged = TRUE, .default = NA_character_) %>%
  modify_depth(., 3, paste, collapse = "|") %>%
  purrr:::flatten() %>%
  purrr::flatten() %>%
  as_vector()


article_version <- map_depth(pubpolicy, 3, pluck, "article_version", .ragged = TRUE, .default = NA_character_) %>%
  modify_depth(., 3, paste, collapse = "|") %>%
  purrr:::flatten() %>%
  purrr::flatten() %>%
  as_vector()

prerequisites <- map_depth(pubpolicy, 3, pluck, "prerequisites", "prerequisite_funders", .ragged = TRUE, .default = NA_character_) %>%
  map_depth(., 4, pluck, "funder_metadata", .ragged = TRUE, .default = NA_character_) %>%
  map_depth(., 4, pluck, "name", 1, "name", .ragged = TRUE, .default = NA_character_) %>%
  modify_depth(., 3, paste, collapse = ", ") %>%
  purrr::flatten() %>%
  purrr::flatten() %>%
  as_vector()

embargo_units <- map_depth(pubpolicy, 3, "embargo", .ragged = TRUE, .default = NA_character_) %>%
  map_depth(., 3, pluck, "units", .ragged = TRUE, .default = NA_character_) %>%
  purrr::flatten() %>%
  purrr::flatten() %>%
  as_vector()

embargo_amount <- map_depth(pubpolicy, 3, "embargo", .ragged = TRUE, .default = NA_integer_) %>%
  map_depth(., 3, pluck, "amount", .ragged = TRUE, .default = NA_integer_) %>%
  purrr::flatten() %>%
  purrr::flatten() %>%
  as_vector()

embargo <- paste(embargo_amount, embargo_units) %>%
  replace(.,
          which(. == "NA NA"),
          "No embargo")

license <- map_depth(pubpolicy, 3, "license", .ragged = TRUE, .default = NA_character_) %>%
  map_depth(., 4, pluck, "license", .ragged = TRUE, .default = NA_character_) %>%
  modify_depth(., 3, paste, collapse = ", ") %>%
  purrr::flatten() %>%
  purrr::flatten() %>%
  purrr::flatten() %>%
  as_vector()

copyright_owner <- map_depth(pubpolicy, 3, pluck, "copyright_owner", .ragged = TRUE, .default = NA_character_) %>%
  purrr::flatten() %>%
  purrr::flatten() %>%
  as_vector()

publisher_deposit <- map_depth(pubpolicy, 3, pluck, "publisher_deposit", .ragged = TRUE, .default = NA_character_) %>%
  map_depth(., 4, pluck, "repository_metadata", .ragged = TRUE, .default = NA_character_) %>%
  map_depth(., 4, pluck, "name", 1, "name", .ragged = TRUE, .default = NA_character_) %>%
  modify_depth(., 3, paste, collapse = "|") %>%
  purrr::flatten() %>%
  purrr::flatten() %>%
  as_vector()

# pull all of these together into a tibble and join it to the linking table so we can then join it to the original file
opf_results <- tibble(policyid_names,
                      article_version,
                      conditions,
                      oa_fee,
                      location,
                      prerequisites,
                      embargo_units,
                      embargo_amount,
                      embargo,
                      license,
                      copyright_owner,
                      publisher_deposit) %>%
  left_join(issn_policy, by = c("policyid_names" = "policyid_vec"), relationship = "many-to-many")

# join the sherpa data to our crossref/orcid file
# there are some duplicates within these because there
# are a few distinctions in the policies that we did not
# pull here, so we filter to keep only those distinct
# observations between the listed variable names
orcid_oa_opf <- oa_issn_lookup %>%
  mutate(work_license = license) %>% 
  select(-license) %>% 
  left_join(.,opf_results, by = c("issn_l" = "issn_vec")) %>%
  distinct(doi, policyid_names, article_version, conditions, oa_fee, 
           location, prerequisites, embargo, license, .keep_all = TRUE)

write_csv(orcid_oa_opf, "./data/results/orcid_oa_opf.csv")
