# this script enables you to search for ORCID iDs affiliated with your 
# institution and extract a subset of individuals who are currently 
# employed at your institution

# NOTE: the rorcid package is deprecated, so this script will likely
# eventually fail to work properly
# code works as of the latest update of this script

# load the packages
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
library(listviewer) # required for jsonedit

# build the query  --------------------------------------------------------

email_domain <- "enter your institution's email domain" 
organization_name <- "enter your organization's name"
# grid_id <- "enter your institution's grid ID" (now retired)
ror_id <- "enter your institution's ROR ID"


# example
# email_domain <- "@okstate.edu"
# organization_name <- "Oklahoma State University"
# grid_id <- "grid.65519.3e"
# ror_id <- "https://ror.org/01g9vbr38"


# create the query
orcid_query <- glue('ror-org-id:"', 
                 ror_id, 
                 '" OR email:*', 
                 email_domain, 
                 ' OR affiliation-org-name:"', 
                 organization_name, '"')

# here is the query you would use if you wanted to use your
# institution's Ringgold and GRID IDs too.
# my_query <- glue('ringgold-org-id:', ringgold_id, ' OR grid-org-id:', grid_id, ' OR ror-org-id:"', ror_id, '" OR email:*', email_domain, ' OR affiliation-org-name:"', organization_name, '"')

# examine my_query
orcid_query

# get the counts
orcid_count <- base::attr(rorcid::orcid(query = orcid_query),
                          "found")

# create the page vector
page_vector <- seq(from = 0, to = orcid_count, by = 200)

# get the ORCID iDs
orcid_pull <- purrr::map(
  page_vector,
  function(page) {
    print(page)
    my_orcids <- rorcid::orcid(query = orcid_query,
                               rows = 200,
                               start = page)
    return(my_orcids)
  })

# put the ORCID iDs into a single tibble
orcid_data <- orcid_pull %>%
  map_dfr(., as_tibble) %>%
  janitor::clean_names()

write_csv(orcid_data, "./data/processed/orcid_data.csv")


# get employment data -----------------------------------------------------

# get the employments from the orcid_identifier_path column

# for the purposes of this class, we will only be retrieving data for the first 50 people.

###################################################
## When you run this on your own after the class,##
############### REMOVE THE [1:50] #################
###################################################

# be patient, this may take a while
orcid_employ <- rorcid::orcid_employments(orcid_data$orcid_identifier_path[1:50])

# View it
View(orcid_employ)

# you can write the file to json if you want to work with it outside of R
# write_json(my_employment, "./data/processed/employment.json")

# here is how you would read it back in, if necessary
# my_employment <- read_json("./data/processed/employment.json", simplifyVector = TRUE)

# preview the structure of the JSON file before we extract things
# NOTE - it is possible to edit files in this viewing mode, so be careful!
jsonedit(number_unnamed(orcid_employ), mode = "view", elementId = NULL)

# extract the employment data and flatten it into a dataframe
employment_data <- orcid_employ %>%
  purrr::map(., purrr::pluck, "affiliation-group", "summaries") %>% 
  purrr::flatten_dfr() %>%
  janitor::clean_names() 

# View it
View(employment_data)


# clean up the column names
# effectively removes the following quoted phrases from the column names
names(employment_data) <- names(employment_data) %>%
  stringr::str_replace(., "employment_summary_", "") %>%
  stringr::str_replace(., "source_source_", "") %>%
  stringr::str_replace(., "organization_disambiguated_", "")

# view the unique institutions in the organization names columns
# keep in mind this will include all institutions a person has in their employments section
org_list <- employment_data %>%
  group_by(organization_name) %>%
  count() %>%
  arrange(desc(n))

# you can also filter it with a keyword or set of keywords.
# Keep it short, like the state name (e.g. Oklahoma).
# If you are adding more than one keyword, separate them by a pipe (|)
# for example, "Oklahoma|Okla"
org_list_filtered <- org_list %>%
  filter(str_detect(organization_name, "KEYWORD")) 

################################################################
# WHEN YOU RUN THIS ON YOUR OWN, REPLACE THE NUMBERS in c()    #
# WITH NUMBERS CORRESPONDING TO INSTITUTIONS IN FILTERED ABOVE #
################################################################
# filter the dataset to include only the institutions you want. 
# There may be different variants depending on if the person
# hand-entered the data. Referring to the my_organizations_filtered
# file, add in all numbers that match 
employment_data_filtered <- employment_data %>%
  dplyr::filter(organization_name %in% org_list_filtered$organization_name[c(1,2)])


# finally, filter to include only people who have NA as the end date
# if your subset has no end dates, that column will not exist
# so this code accounts for that possibility
employment_data_filtered_current <- employment_data_filtered %>%
  dplyr::filter(if_any(any_of("end_date_year_value"), ~ is.na(.)))

# note that this will give you employment records ONLY. 
# In other words, each row represents a single employment record for an individual.
# the name_value variable refers specifically to the name of the person or system
# that wrote the record, NOT the name of the individual. 

# To get that, you must first get all the unique ORCID iDs from the dataset:

# There is actually no distinct value identifying the orcid ID of the person.
# The orcid_path value corresponds to the path of the person who added the employment record (which is usually, but not always the same)
# Therefore you have to strip out the ORCID iD from the 'path' variable first and put it in it's own value and use it
# We do this using str_sub from the stringr package
# While we are at it, we can select and reorder the columns we want to keep
current_employment_all <- employment_data_filtered_current %>%
  mutate(orcid_identifier = str_sub(path, 2, 20)) %>%
  select(any_of(c("orcid_identifier",
                  "organization_name",
                  "organization_address_city",
                  "organization_address_region",
                  "organization_address_country",
                  "organization_identifier",
                  "organization_disambiguated_organization_identifier",
                  "organization_disambiguation_source",
                  "department_name",
                  "role_title",
                  "url_value",
                  "display_index",
                  "visibility",
                  "created_date_value",
                  "start_date_year_value",
                  "start_date_month_value",
                  "start_date_day_value",
                  "end_date_year_value",
                  "end_date_month_value",
                  "end_date_day_value")))
         

# next, create a new vector unique_orcids that includes only unique ORCID iDs from our filtered dataset.     
unique_orcids <- unique(current_employment_all$orcid_identifier) %>%
  na.omit(.) %>%
  as.character()

# then run the following expression to get all biographical information for those iDs.
# This will take a few seconds to process
orcid_person <- rorcid::orcid_person(unique_orcids)

# view this JSON file as well
jsonedit(number_unnamed(orcid_person), mode = "view", elementId = NULL)

# then we construct a data frame from the response. 
# See more at https://ciakovx.github.io/rorcid.html#Getting_the_data_into_a_data_frame for this.

orcid_person_data <- orcid_person %>% {
  dplyr::tibble(
    given_name = purrr::map_chr(., purrr::pluck, "name", "given-names", "value", .default=NA_character_),
    created_date = purrr::map_dbl(., purrr::pluck, "name", "created-date", "value", .default=NA_integer_),
    last_modified_date = purrr::map_dbl(., purrr::pluck, "name", "last-modified-date", "value", .default=NA_integer_),
    family_name = purrr::map_chr(., purrr::pluck, "name", "family-name", "value", .default=NA_character_),
    credit_name = purrr::map_chr(., purrr::pluck, "name", "credit-name", "value", .default=NA_character_),
    other_names = purrr::map(., purrr::pluck, "other-names", "other-name", "content", .default=NA_character_),
    orcid_identifier_path = purrr::map_chr(., purrr::pluck, "name", "path", .default = NA_character_),
    biography = purrr::map_chr(., purrr::pluck, "biography", "content", .default=NA_character_),
    researcher_urls = purrr::map(., purrr::pluck, "researcher-urls", "researcher-url", .default=NA_character_),
    emails = purrr::map(., purrr::pluck, "emails", "email", "email", .default=NA_character_),
    keywords = purrr::map(., purrr::pluck, "keywords", "keyword", "content", .default=NA_character_),
    external_ids = purrr::map(., purrr::pluck, "external-identifiers", "external-identifier", .default=NA_character_))
  } %>%
  dplyr::mutate(created_date = anytime::anydate(as.double(created_date)/1000),
                last_modified_date = anytime::anydate(as.double(last_modified_date)/1000))

# Join it back with the employment records
orcid_person_employment_join <- orcid_person_data %>%
  left_join(current_employment_all, by = c("orcid_identifier_path" = "orcid_identifier"))

# now you can write this file to a CSV
write_csv(orcid_person_employment_join, "./data/results/orcid_employment_file.csv")


#
# if you are a part of an ORCID member institution, 
# you can get this data more easily from the member portal
# at https://member-portal.orcid.org/ and it's quite comparable
#

# exploring departments ---------------------------------------------------

depts <- orcid_person_employment_join %>%
  mutate(department_name = str_remove(department_name, "[Ss]chool [Oo]f |[Dd]epartment [Oo]f"),
         department_name = tolower(department_name),
         department_name = str_replace_all(department_name, "&", "and"),
         department_name = str_remove_all(department_name, "[[:punct:]]"),
         department_name = str_trim(department_name))

dept_tally <- depts %>%
  group_by(department_name) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(!is.na(department_name))

# you might need to adjust the n > depending on what your data looks like
dept_plot <- dept_tally %>%
  filter(n >= 2) %>%
  ggplot(aes(x = fct_reorder(department_name, n), y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip()

print(dept_plot)

# exploring roles

# NOTE: roles are free text, so your data may not be at all similar
# it may also be beneficial to maintain the language users use
# to describe themselves
# depending on what your interest is

# make a list of titles for graduate students
grad_student_titles <- c("Graduate Research Assistant",
                         "GRA","Graduate Student ",
                         "Graduate Assistant",
                         "Graduate Teaching Assistant",
                         "Graduate Research Associate",
                         "Graduate Research and Teaching Assistant",
                         "Graduate Student and Research Assistant",
                         "Master's Student",
                         "PhD Student",
                         "PhD Student ", # some entries have trailing white space
                         "Graduate Researcher/Teaching Assistant")

# rename values in role_title if they exist in the grad_student_titles
# this helps standardize related terms
roles <- orcid_person_employment_join %>%
  mutate(role_title = ifelse(role_title %in% grad_student_titles,
                             "Graduate Student",role_title),
         role_title = tolower(role_title),
         role_title = str_remove_all(role_title, "[[:punct:]]"))

role_tally <- roles %>% 
  group_by(role_title) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(!is.na(role_title))

role_plot <- role_tally %>%
  filter(n >= 2) %>%
  ggplot(aes(x = fct_reorder(role_title, n), y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip()

print(role_plot)
