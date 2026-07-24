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
# and trim DOI so that it only contains the DOI, not https://doi.org/
oax_works_orcid <- read_csv("data/results/openalex_orcid_works.csv") %>% 
  mutate(doi = str_replace(doi,
                           "https://doi.org/",
                           ""))

# most of the open access data we are interested in is already captured by OpenAlex
# but we can still obtain a little extra info using the Unpaywall API


# input your email address to send with your unpaywall api requests
my_email <- "TYPE YOUR EMAIL ADDRESS HERE"

# create a slow oadoi_fetch call to be used during this class
# after class you can replace the 2 with a 0.
# also wrap the function in safely() so if there is a problem, it won't break the entire loop.
slow_oadoi_fetch <- safely(slowly(oadoi_fetch, rate_delay(2)))

###################################################
## When you run this on your own after the class,##
############### REMOVE THE [1:20] #################
###################################################

# loop through the dois, calling oadoi_fetch and returning the results
dois_unpay <- map(oax_works_orcid$doi[1:20], function(z) {
  print(z)
  o <- slow_oadoi_fetch(dois = z, email = my_email)
  return(o)
})

# write the json, if necessary or desired
# write_json(dois_oa, "./data/processed/dois_oa.json")

# read in the json if necessary
# dois_oa2 <- read_json("./data/processed/dois_oa.json", simplifyVector = TRUE)


# view warnings to see information on any items that weren't found in the unpaywall database
warnings()

# loop (map) through the results
# is_empty will return a TRUE or FALSE if there were no results, and the _lgl
# part of map will return that TRUE or FALSE into a single vector, which can be used 
# to subset the crossref/orcid merge

#################################################################
### DELETE THE [1:20, ] WHEN YOU ARE RUNNING THIS AFTER CLASS ###
#################################################################

# view the JSON file
jsonedit(number_unnamed(dois_unpay), mode = "view")


# loop through (map) the returned results, remove empty items, extract (flatten) the 
# data frame, and bind (_dfr) the rows together
dois_unpay_df <- dois_unpay %>%
  map(pluck, "result") %>%
  discard(is_empty) %>%
  map_dfr(., flatten) %>%
  clean_names() 

# get a list of DOIs that failed to find an OA version
  # note: some works in OpenAlex may not have a DOI to begin with
  # and those works will have an NA populate in dois_not_found

###########################################################
##   When you run this on your own after the class,   #####
## remove the [1:20,] and keep the whole oa_works_orcid ###
###########################################################
dois_not_found <- oax_works_orcid[1:20,] %>% 
  filter(!(doi %in% dois_unpay_df$doi)) %>% 
  pull(doi)

# View the data
View(dois_unpay_df)

# have a look at the column names
View(as.data.frame(names(dois_unpay_df)))


# filter to create a new data frame of the open access objects
open_access_only <- dois_unpay_df %>%
  filter(is_oa == TRUE)

# of these, have a look at the best location
best_oa <- open_access_only %>%
  tidyr::unnest(best_oa_location)

# because you can't write nested lists to CSV, you must either unnest them or remove them. In this case, we remove them.
# But if you really want to explore this data, you'll want to unnest them
# here in RStudio
best_oa_merge <- best_oa %>%
  filter(!duplicated(doi)) %>%
  select_if(purrr::negate(is.list))

# now that we have the best OA location, we can merge this back to our ORCID/Crossref file
oax_works_orcid_unpay <- oax_works_orcid %>%
  left_join(best_oa_merge, by = "doi", suffix = c("_oax", "_up"))


# write the csv
write_csv(oax_works_orcid_unpay, "./data/results/oa_works_orcid_unpay.csv")  


# exploring the data ------------------------------------------------------

# let's see what info is potentially redundant between Unpaywall and OpenAlex
# sort the names alphabetically and look for _oa (OpenAlex) and _up (Unpaywall)
View(as.data.frame(names(oax_works_orcid_unpay)))

# create a subset of only the DOIs tested [1:20]
# NOTE - you will not need this when you run this yourself with all the DOIs
subset_unpaywall_df <- oax_works_orcid_unpay[1:20,]

# is_oa
# have a look at how many of the results have an open access version available
# using Unpaywall data
tabyl(subset_unpaywall_df$is_oa_up)
# vs OpenAlex data
tabyl(subset_unpaywall_df$is_oa_oax)

#plot number of OA vs non-OA
# NOTE - you may need to change the limits of your y-axis
# depending on how many OA articles you do and don't have
oa_unpaywall <- subset_unpaywall_df %>%
  ggplot(., aes(x = is_oa_up)) +
  geom_bar(stat = "count",
           color = "black",
           fill = "#1FE167") +
  labs(title = "Unpaywall Data") +
  ylim(0,15)

# vs OpenAlex data
# (need to reorder TRUE/FALSE so that TRUE is first)
oa_openalex <- subset_unpaywall_df %>%
  ggplot(., aes(x = is_oa_oax)) +
  geom_bar(stat = "count",
           color = "black",
           fill = "#CCCACD") +
  labs(title = "OpenAlex Data") +
  scale_x_discrete(limits = c("TRUE","FALSE")) +
  ylim(0,15)

# put the 2 plots side-by-side
oa_unpaywall + oa_openalex


# version
# using Unpaywall data
tabyl(subset_unpaywall_df$version_up)
# vs OpenAlex data, which also includes submittedVersion
tabyl(subset_unpaywall_df$version_oax)

# license
# using Unpaywall data
tabyl(subset_unpaywall_df$license_up)
# vs OpenAlex data
tabyl(subset_unpaywall_df$license_oax)

# plot licenses for Unpaywall and OpenAlex
# get a list of licenses
# using Unpaywall data
up_license_list <- subset_unpaywall_df %>% 
  select(license_up) %>% 
  distinct()

# vs OpenAlex data
oax_license_list <- subset_unpaywall_df %>% 
  select(license_oax) %>% 
  distinct()


# extract a comprehensive list of all licenses
license_list <- oax_license_list %>% 
  full_join(.,up_license_list, join_by("license_oax" == "license_up")) %>% 
  distinct() %>% 
  arrange(license_oax) %>% 
  pull()

# NOTE - you may need to change the limits of your y-axis
# depending on how many of each license type you do and don't have
lic_unpaywall <- subset_unpaywall_df %>%
  mutate(license_up = factor(license_up,
                             levels = license_list)) %>% 
  ggplot(., aes(x = license_up)) +
  geom_bar(stat = "count",
           color = "black",
           fill = "#1FE167") +
  labs(title = "Unpaywall Data") +
  scale_x_discrete(limits = license_list,
                   breaks = license_list) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0,15)

# vs OpenAlex data
# (need to reorder TRUE/FALSE so that TRUE is first)
lic_openalex <- subset_unpaywall_df %>%
  mutate(license_up = factor(license_up,
                             levels = license_list)) %>% 
  ggplot(., aes(x = license_oax)) +
  geom_bar(stat = "count",
           color = "black",
           fill = "#CCCACD") +
  labs(title = "OpenAlex Data") +
  scale_x_discrete(limits = license_list,
                   breaks = license_list) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0,15)

# put the 2 plots side-by-side
lic_unpaywall + lic_openalex


# oa status
# using Unpaywall data
tabyl(subset_unpaywall_df$oa_status_up)
# vs OpenAlex data
tabyl(subset_unpaywall_df$oa_status_oax)

# get a list of oa statuses
# using Unpaywall data
up_status_list <- subset_unpaywall_df %>% 
  select(oa_status_up) %>% 
  distinct()

# vs OpenAlex data
oax_status_list <- subset_unpaywall_df %>% 
  select(oa_status_oax) %>% 
  distinct()

# extract a comprehensive list of all oa statuses
status_list <- oax_status_list %>% 
  full_join(.,up_status_list, join_by("oa_status_oax" == "oa_status_up")) %>% 
  distinct() %>% 
  arrange(oa_status_oax) %>% 
  pull()

# create color palette
oa_colors <- c("bronze" = "#D55E00",
               "closed" = "#000000",
               "gold" = "#F0E442",
               "green" = "#009E73", 
               "hybrid" = "#E69F00")

# my_colors <- oa_colors[names(oa_colors) %in% subset_unpaywall_df$oa_status]

# plot OA status between Unpaywall
# NOTE - you may need to change the y-axis limits
oa_status_plot_up <- subset_unpaywall_df %>%
  mutate(oa_status_up = factor(oa_status_up,
                               levels = status_list)) %>% 
  ggplot(., aes(x = oa_status_up,
                fill = oa_status_up)) +
  geom_bar(stat = "count", 
           color = "black",
           show.legend = FALSE) +
  scale_fill_manual(values = oa_colors,
                    na.value = "gray70") +
  scale_x_discrete(limits = status_list,
                   breaks = status_list) +
  labs(title = "Unpaywall Data") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
  ylim(0,12)

# and OpenAlex
oa_status_plot_oax <- subset_unpaywall_df %>%
  mutate(oa_status_oax = factor(oa_status_oax,
                               levels = status_list)) %>% 
  ggplot(., aes(x = oa_status_oax,
                fill = oa_status_oax)) +
  geom_bar(stat = "count", 
           color = "black",
           show.legend = FALSE) +
  scale_fill_manual(values = oa_colors,
                    na.value = "gray70") +
  scale_x_discrete(limits = status_list,
                   breaks = status_list) +
  labs(title = "OpenAlex Data") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
  ylim(0,12)

oa_status_plot_up + oa_status_plot_oax

# has_repository_copy (unpaywall-only data)
tabyl(subset_unpaywall_df$has_repository_copy)

# graph showing repository copies over different years 
subset_unpaywall_df %>% 
  ggplot(aes(x = year, 
             fill = has_repository_copy)) +
  geom_bar()

# journal_is_oa (unpaywall-only data)
tabyl(subset_unpaywall_df$journal_is_oa)

subset_unpaywall_df %>% 
  ggplot(aes(x = year, 
             fill = journal_is_oa)) +
  geom_bar()

# year
# Unpaywall Data
pub_year_up <- subset_unpaywall_df %>% 
  ggplot(aes(x = publication_year, 
             fill = is_oa_up)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("TRUE" = "#00BFC4",
                               "FALSE" = "#F8766D"),
                    na.value = "gray70",
                    breaks = c("TRUE","FALSE")) +
  theme(legend.position = "bottom") +
  labs(title = "Unpaywall Data")

# OpenAlex data
pub_year_oax <- subset_unpaywall_df %>% 
  ggplot(aes(x = publication_year, 
             fill = is_oa_oax)) +
  geom_bar(color = "black",
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("TRUE" = "#00BFC4",
                               "FALSE" = "#F8766D"),
                    na.value = "gray70",
                    breaks = c("TRUE","FALSE")) +
  theme(legend.position = "bottom") +
  labs(title = "OpenAlex Data")

pub_year_up + pub_year_oax
