
# load libraries ----------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# API request - list of instititutions in OpenAlex ------------------------

editors <- read_csv("./data_clean/InstitutionData_clean.csv")
root = "https://api.openalex.org/institutions?filter=display_name.search:"
inst_all<-editors %>% 
  select(inst) %>% 
  distinct() %>% 
  na.omit() %>% 
  # mutate(url = str_replace_all(inst,"[[:punct:]]", "")) %>% 
  mutate(url = str_replace(inst," and ", "and")) %>% 
  mutate(url = str_replace(inst," and ", " ")) %>% 
  mutate(url = str_replace_all(url," ", "%20")) %>% 
  mutate(url = paste(root, url, sep="")) 
inst_all$counter<-seq_along(inst_all$inst)

# inst<-inst_all %>% slice(301:700)
# inst <- inst_all

inst<-inst_all %>% slice(1:1182)
uni_results_match<- data.frame(id=NA, 
                               display_name=NA, 
                               ror=NA, 
                               country_code=NA, 
                               type=NA, 
                               # homepage_url=NA,
                               display_name_acronyms=NA,
                               geo=NA,
                               inst_eb=NA,
                               counter=NA)

uni_results_no_match<- data.frame(inst_eb=NA,
                                  counter=NA)

uni_results_no_match
uni_results_match

vec<-seq_along(inst$inst)
vec

for(i in vec) {

  api_return<-httr::GET(inst$url[i],add_headers(mailto="embruna@ufl.edu"))
  api_return<-fromJSON(rawToChar(api_return$content))
  
  
  if (api_return$met$count>0) 
  {
    api_return<-api_return$results
    api_return<-api_return %>% select(id,
                                      display_name,
                                      ror,
                                      country_code,
                                      type,
                                      display_name_acronyms,
                                      geo)
    api_return$inst_eb<-inst$inst[i]
    api_return$counter<-inst$counter[i]
    # names(uni_results_match)==names(api_return)
    uni_results_match<-bind_rows(uni_results_match,api_return)
  } else {
    uni_results_no_match<-bind_rows(uni_results_no_match,data.frame(inst_eb=inst$inst[i],counter=inst$counter[i]))
    
  }
}

uni_results_no_match
uni_results_match

max(uni_results_no_match$counter, na.rm=TRUE)
max(uni_results_match$counter, na.rm=TRUE)

# 
# 1-502
# 503-699
# 700-800
# 801-932
# 932-1182

saveRDS(uni_results_match, "./output_review/uni_results_match_932-1182.rds")
write_csv(uni_results_no_match, "./output_review/uni_results_no_match_932-1182.csv")

#TODO: https://twitter.com/researchremix/status/1501682528184197121 wikipdata for carnegie
# Notes -------------------------------------------------------------------


# note: can only use lapply lapply function instead of a for loop if you want to 
# return a list of the same length as the vector or list you want to iterate with.
# see https://r-coder.com/lapply-r/
# inst_query_results<-lapply(inst,fun)



# https://docs.openalex.org/api/get-single-entities
# https://api.openalex.org/institutions
# https://api.openalex.org/institutions?filter=display_name.search:university%20of%20north%20carolina%20at%20chapel%20hill
# see also https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html especially for multipage responses

# mailto=you@example.com


# res = GET("https://api.openalex.org/institutions?mailto=embruna@ufl.edu")
# res[12]

# In its current state, the data in the res variable is not usable. 
# The actual data is contained as raw Unicode in the res list, 
# which ultimately needs to be converted into JSON format.
# To do this, we first need to convert the raw Unicode into a character 
# vector that resembles the JSON format shown above. 
# The rawToChar() function performs just this task, as shown below:
# 
# rawToChar(res$content)
# data = fromJSON(rawToChar(res$content))
# data[1]
# names(data)
# example$meta
# data$meta$page
# data$results$id
# data$results$display_name
# https://openalex.org/I143302722
# GET("https://api.openalex.org/works/I143302722")
# res2 = GET("https://api.openalex.org/institutions?filter=display_name.search:university%20of%20north%20carolina%20at%20chapel%20hill")
# str(res2)
# res2 = GET("https://api.openalex.org/institutions?filter=display_name.search:texas%20a%20m%20university")
# res2 = GET("https://api.openalex.org/institutions?filter=display_name.search:university%20of%20texas",add_headers(mailto="embruna@ufl.edu"))
# res2 = GET('https://api.openalex.org/institutions?filter=display_name.search:university%20of%20maryland?mailto=name@isp.edu')

#> Response [http://www.google.com/search?q=ham]