# Install and load packages -----------------------------------------------

# you will need to install these packages first, using the following
# if you've already installed them, skip this step
install.packages('dplyr')
install.packages('tibble')
install.packages('tidyr')
install.packages('purrr')
install.packages('readr')
install.packages('stringr')
install.packages('jsonlite')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('httr')
install.packages('forcats')
install.packages('rorcid')
install.packages('usethis')
install.packages('anytime')
install.packages('janitor')
install.packages('glue')
install.packages('remotes')
remotes::install_github("ropensci/rcrossref")
install.packages('roadoi')
install.packages('inops')
install.packages("png")
install.packages("repurrrsive")
install.packages("listviewer")
install.packages("httr2")
install.packages("openalexR")
install.packages("patchwork")

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
library(png)
library(repurrrsive)
library(purrr)
library(listviewer)
library(httr2)
library(openalexR)
library(patchwork)

# Set up orcid ------------------------------------------------------------

# 1. If you haven’t done so already, create an ORCID account at https://orcid.org/signin. 
# 2. In the upper right corner, click your name, then in the drop-down menu, click Developer Tools. Note: In order to access Developer Tools, you must verify your email address. 
# 3. If you have not already verified your email address, you will be prompted to do so at this point.
# 4. Click the “Register for the free ORCID public API” button
# 5. Review and agree to the terms of service when prompted.
# 6. Add your name in the Name field, https://www.orcid.org in the Your Website URL field, “Getting public API key” in Description field, and https://www.orcid.org in the redirect URI field. Click the diskette button to save.
# 7. A gray box will appear including your Client ID and Client Secret. In the below code chunk, copy and paste the client ID and the client secret respectively. 
# 8. Make sure to leave the quotation marks (e.g. orcid_client_id <- "APP-FDFJKDSLF320SDFF" and orcid_client_secret <- "c8e987sa-0b9c-82ed-91as-1112b24234e"). 

# copy/paste your client ID from https://orcid.org/developer-tools
orcid_client_id <- "PASTE MY CLIENT ID HERE"

# copy/paste your client secret from https://orcid.org/developer-tools
orcid_client_secret <- "PASTE MY CLIENT SECRET HERE"

# Alternatively, to ensure private information remains private
# (especially when sharing code with others)
# install.packages("rstudioapi")
# library(rstudioapi)
# rstudioapi::askForPassword('ORCID Client Secret:')

orcid_request <- POST(url  = "https://orcid.org/oauth/token",
                      config = add_headers(`Accept` = "application/json",
                                           `Content-Type` = "application/x-www-form-urlencoded"),
                      body = list(grant_type = "client_credentials",
                                  scope = "/read-public",
                                  client_id = orcid_client_id,
                                  client_secret = orcid_client_secret),
                      encode = "form")

# parse the API request with content
orcid_response <- content(orcid_request)


# run the following code
print(orcid_response$access_token)


#You will see a string of text print out in your R console.
# Copy that string to the keyboard for use below

# now we are going to save the token to our R environment
# Run this code:
usethis::edit_r_environ()



# A new window will open in RStudio.
# In this separate R environment page, type the following (except the pound sign):
# ORCID_TOKEN="my-token"
# replace 'my-token' with the access_token you just copied. 
# Then press enter to create a new line.


# while we are here, we'll add in our rcrossref credentials
# type crossref_email="name@example.com", using your own email address.
# press enter to create a new line, and leave it blank. 
# Press Ctrl + S (Mac: Cmd + S) to save this information to your R environment and close the window. You won't see anything happen here because it is just saving the page.
# Click Session > Restart R. Your token should now be saved to your R environment. 

# You will need to reload all packages after restarting R


#You can confirm this by calling orcid_auth(), and it will print the token
rorcid::orcid_auth()


# configure OpenAlex ------------------------------------------------------

# 1. Create an account with OpenAlex at https://openalex.org/
# 2. Verify your email to gain access to your account.
# 3. Got to Account Settings >> API Key and copy your API key
# 4. Run the code below, replacing YOUR_API_KEY with the key you copied
options(openalexR.apikey = "YOUR_API_KEY")

# configure Open Policy Finder --------------------------------------------------

# 1. Create an account with Open Policy Finder at [insert URL when available]
# 2. Click the Admin button. Your API key will be displayed. We will use this in 06
# 3. For now, paste it here: 

# to avoid sharing private information in shared code:
# rstudioapi::askForPassword('Open Policy Finder API Key:')


# find your institution's identifiers -------------------------------------

# Sign in to your ORCID account and scroll down to Employment. Click the Add button.
# Start typing your institution. When it appears, click it. Click Save changes.
# On your orcid profile, click Show more detail. Your institution’s ROR ID should be displayed. 
# You can also look it up in the ROR database (https://ror.org)
# add in as many identifiers as you can find:

# ROR:





# practice working with data ----------------------------------------------






# loading in a dataset from the ggplot2 package
# lists prices of 50,000+ round cut diamonds
d <- diamonds

# use unique() and $ to view the unique values in the cut column
unique(d$cut)

# use mean() to get the mean of carat.
mean(d$carat)

# use select() to keep only the carat and cut
d %>%
  select()

# use filter() to keep only carats above 0.3
d %>%
  filter()

# use mutate() to create a new column pricepercarat that divides price / carat
d %>%
  mutate()


# practice working with lists ---------------------------------------------

# working with data in lists
# https://mybinder.org/v2/gh/ciakovx/ciakovx.github.io/master?filepath=jennybc_lists_lesson.ipynb
# based on https://jennybc.github.io/purrr-tutorial/ls00_inspect-explore.html 



# wes anderson color palettes ---------------------------------------------

wesanderson <- wesanderson

str(wesanderson)

jsonedit(wesanderson, mode = "view", elementId = "wesanderson")


# game of thrones POV characters ------------------------------------------

got_chars <- got_chars

jsonedit(got_chars, mode = "view", elementId = "got_chars")

# to start list at 1 instead of 0
jsonedit(number_unnamed(got_chars), mode = "view", elementId = "got_chars")



# vectorized and "list-ized" operations -----------------------------------

# square of each element in the vector
(3:5) ^ 2

# square root of each element in the vector
sqrt(c(9, 16, 25))

# for each element in the vector, take the square root
# result will be returned as a list
map(c(9, 16, 25), sqrt)


## map(YOUR_LIST, YOUR_FUNCTION)

# for elements 1-4 in the list, extract the name value
map(got_chars[1:4], "name")

# for elements 5-8 in the list, extract the third value
map(got_chars[5:8], 3)

# piping
got_chars %>%
  map("name")

got_chars %>%
  map(3)

# for elements in the list, extract the name value
# map_chr will return results as a character vector
map_chr(got_chars[9:12], "name")

# compare to just map(got_chars[9:12], "name")

# rectangling: manually create data frame from list
got_chars_df <- got_chars %>% {
  tibble(
    name = map_chr(., pluck, "name", .default = NA_character_),
    culture = map_chr(., pluck, "culture", .default = NA_character_),
    gender = map_chr(., pluck, "gender", .default = NA_character_),       
    id = map_int(., pluck, "id", .default = NA_integer_),
    born = map_chr(., pluck, "born", .default = NA_character_),
    alive = map_lgl(., pluck, "alive", .default = NA)
  )
}


