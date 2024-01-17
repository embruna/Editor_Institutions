dryad_data
alldata

names(dryad_data)
names(alldata)

dd<-dryad_data %>% select(journal, year, category,last_name, first_name, editor_id)
dd<-dd %>%
  mutate(across(everything(), as.character))
dd<-dd %>%
  mutate(across(everything(), tolower))


ad<-alldata %>% select(journal, year, category,last_name, first_name, editor_id)
ad<-ad %>%
  mutate(across(everything(), as.character))

intersect(dd, ad)%>% arrange(journal, last_name,first_name) %>% filter(last_name=='chapin')
union(dd, ad)%>% arrange(journal, last_name,first_name) %>% filter(last_name=='chapin')
missing<-setdiff(dd, ad) %>% arrange(journal, last_name,first_name)

ad_tbd<-ad %>% filter(is.na(category))
levels(as.factor(alldata$category))
levels(as.factor(ad$category))
foo<-full_join(ad,dd) %>% filter(last_name=='chapin') %>% arrange(journal, year)

foo<-full_join(ad,dd) 

need to do an if_else replacement based on if NA, year, journal, last name, first name