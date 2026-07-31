# this script attempts to illustrate some customizations to the ORCID API query
# for example, wanting to search multiple email domains or ROR IDs
# we also include an example to account for distinct prefixes in an email domain

# each example provided here replaces lines 35-55 in 02-rorcid_employments.R


# Multiple Email Domains --------------------------------------------------

# NOTE: if you need to add more email domains, we can help you
# add those additional components to your query
email_domain_01 <- "enter your institution's email domain" 
email_domain_02 <- "enter your institution's other email domain"
organization_name <- "enter your organization's name"
ror_id <- "enter your institution's ROR ID"

# create the query
orcid_query <- glue('ror-org-id:"', 
                    ror_id, 
                    '" OR email:*', 
                    email_domain_01, 
                    ' OR email:*',
                    email_domain_02,
                    ' OR affiliation-org-name:"', 
                    organization_name, '"')

# examine my_query
orcid_query


# Email Domain Prefixes ---------------------------------------------------

# for this example, we will use a wildcard (*) as a stand-in for the prefix
  # @phy.iith.ac.in and @bio.iith.ac.in would become @*iith.ac.in
  # with the * searching for any possible domain prefix 
  # but also permitting no prefix at all (e.g., @iith.ac.in)

# replace "DOMAIN" in the example below with your institution's base email domain
  # for example, okstate.edu or iith.ac.in
  # leave the @* where it is, as those are needed to capture domain prefixes
email_domain_prefix <- "@*DOMAIN" 
organization_name <- "enter your organization's name"
ror_id <- "enter your institution's ROR ID"

# create the query
orcid_query <- glue('ror-org-id:"', 
                    ror_id, 
                    '" OR email:*', 
                    email_domain_prefix, 
                    ' OR affiliation-org-name:"', 
                    organization_name, '"')

# examine my_query
orcid_query


# Multiple ROR IDs --------------------------------------------------------

# NOTE: if you need to add more ROR IDs or organization names, we can help you
# add those additional components to your query
email_domain <- "enter your institution's email domain" 
organization_name <- "enter your organization's name"
ror_id_01 <- "enter your institution's ROR ID"
ror_id_02 <- "enter your institution's other ROR ID"

# create the query
orcid_query <- glue('ror-org-id:"', 
                    ror_id_01, 
                    '" OR ror-org-id:"',
                    ror_id_02,
                    '" OR email:*', 
                    email_domain, 
                    ' OR affiliation-org-name:"', 
                    organization_name, '"')

# examine my_query
orcid_query


####################
# iith.ac.in = 2540 full query; 104 email only
# *iith.ac.in = 2543; 154 email only

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