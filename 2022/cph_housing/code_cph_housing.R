# https://dawadocs.dataforsyningen.dk/dok/api/generelt#flervaerdisoegning
# https://dawadocs.dataforsyningen.dk/dok/bbr#find-en-enhed-ud-fra-dens-adresse


library(tidyverse)
library(httr)

rm(list=ls())

#url <- "https://services.datafordeler.dk/BBR/BBRPublic/1/REST/"


url <- "https://api.dataforsyningen.dk/adresser?kommunekode=0101|0147&husnr=187"
url <- "https://api.dataforsyningen.dk/adresser?kommunekode=0101|0147"

#url <- "https://api.dataforsyningen.dk/kommuner?format=csv&download"


#url <- "https://api.dataforsyningen.dk/adresser?kommunekode=0101|0147&husnr=177?format=json"
#url <- "https://api.dataforsyningen.dk/adresser?kommunekode=0101|0147"

raw <- GET(url)
http_status(raw)

text <- content(raw)

test <- text[[1]]


df <- test[[10]] %>% unlist() %>% enframe()



test$adgangsadresse$adgangspunkt$id



u2 <- "https://api.dataforsyningen.dk/bbrlight/opgange?adgangsadresseid=0a3f507a-e179-32b8-e044-0003ba298018"
u2 <- "https://api.dataforsyningen.dk/bbrlight/opgange?adgangsadresseid=24ce39ab-b0d5-4e34-97f3-a1efe3d6bb2a"

d2 <- GET(u2)
http_status(d2)
d2 <- content(d2)






href <- test$href

bbr <- GET(href)
http_status(bbr)
bbr <- content(bbr)


bbr <- GET("https://api.dataforsyningen.dk/bbrlight/bygninger?id=9a837c75-a4d4-4a0a-8379-0b76ab654265")
http_status(bbr)
bbr <- content(bbr)





test[[1]]

tt <- unique(rapply(test, function(x) head(x, 1))) %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble()

test <- test[[10]]



#test <- bind_rows(test)

tt <- unique(rapply(test, function(x) head(x, 1)))


extract_fun <- function(input, output) {
  
  output <- unique(rapply(input, function(x) head(x, 1))) %>% 
    t() %>% 
    as.data.frame() %>% 
    tibble() %>% 
    select(c(V28, V29))
  
}

df <- map(.x = text,
          .f = extract_fun) %>% 
  bind_rows(.id = "id")


