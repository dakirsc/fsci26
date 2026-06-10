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


orcid_oa_opf <- read_csv("./data/results/orcid_oa_opf.csv")


# look at the accepted versions a single person can deposit without paying a fee
# you may need to choose a different individual depending on your data
  # change [10] for a different number, such as [2] or [5]

orcid_ids <- orcid_oa_opf$orcid_search[10]

no_fee_accepted_single <- orcid_oa_opf %>%
  filter(orcid_search %in% orcid_ids,
         oa_fee == "no",
         str_detect(article_version, "accepted")) %>% 
  select(doi:author_list,issn_l,policyid_names:license)

# or find all instances of this
no_fee_accepted_all <- orcid_oa_opf %>% 
  filter(oa_fee == "no",
         str_detect(article_version, "accepted")) %>% 
  select(doi:author_list,issn_l,policyid_names:license)

# which published versions can be deposited in the IR? (vor = version of record)
# can choose whether to filter for no OA fee
vor_ir <- orcid_oa_opf %>%
  filter(oa_fee == "no",
         str_detect(article_version, "published"),
         is.na(prerequisites),
         str_detect(location, "institutional")) %>% 
  select(doi:author_list,issn_l,policyid_names:license)

# can also check for other article versions--e.g., submitted--to go in IR
sub_ir <- orcid_oa_opf %>%
  filter(oa_fee == "no",
         str_detect(article_version, "submitted"),
         is.na(prerequisites),
         str_detect(location, "institutional")) %>% 
  select(doi:author_list,issn_l,policyid_names:license)

# get all works and publisher policy info in a single table
# use work_index to distinguish between DOIs since there may be multiple pathways to OA
single_user_works_policies <- orcid_oa_opf %>% 
  filter(orcid_search %in% orcid_ids) %>% 
  select(doi:author_list,issn_l,policyid_names:license) 
