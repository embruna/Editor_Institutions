CHANGE LOG - CHO et al dryad v2

# minor corrections to Cho_V2 Caught by Bruna's 2017 SciWri Class
Cho$COUNTRY[Cho$LAST_NAME=="Pedreira" & Cho$FIRST_NAME=="Carlos"] <- "Brazil"
Cho$COUNTRY[Cho$LAST_NAME=="Benbi" & Cho$FIRST_NAME=="Dinesh"] <- "India"
Cho$COUNTRY[Cho$LAST_NAME=="Borras" & Cho$FIRST_NAME=="Lucas"] <- "Argentina"
Cho$COUNTRY[Cho$LAST_NAME=="Esker" & Cho$FIRST_NAME=="Paul"] <- "Costa Rica"
Cho$COUNTRY[Cho$LAST_NAME=="Buresh" & Cho$FIRST_NAME=="Roland"] <- "USA"

Cho$COUNTRY<-as.character(Cho$COUNTRY)
Cho$COUNTRY[Cho$COUNTRY=="UK"] <- "United Kingdom"
Cho$COUNTRY[Cho$COUNTRY=="USSR"] <- "Russia"
Cho$COUNTRY[Cho$COUNTRY=="ITALY"] <- "Italy"
Cho$COUNTRY[Cho$COUNTRY=="United States"] <- "USA"
Cho$COUNTRY[Cho$COUNTRY=="Usa"] <- "USA"
Cho$COUNTRY[Cho$COUNTRY=="UsA"] <- "USA"

