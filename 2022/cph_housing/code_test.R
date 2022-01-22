rm(list=ls())

u1 <- "https://api.dataforsyningen.dk/adgangsadresser?q=Rentemestervej 8, 2400"


d1 <- GET(u1)
http_status(d1)


d1_1 <- content(d1, "text", encoding = "UTF-8")


u2 <- "https://api.dataforsyningen.dk/bbrlight/opgange?adgangsadresseid=0a3f507a-e179-32b8-e044-0003ba298018"

d2 <- GET(u2)
http_status(d2)

d2 <- content(d2)

d2 <- d2[[1]]

u3 <- d2$Bygning_id

# u3 <- "https://api.dataforsyningen.dk/bbrlight/bygninger?id=9a837c75-a4d4-4a0a-8379-0b76ab654265"


