library(tidyverse)
library(httr)

#url <- "https://services.datafordeler.dk/BBR/BBRPublic/1/REST/"


url <- "https://api.dataforsyningen.dk/adresser?kommunekode=0101|0147&husnr=177"
#url <- "https://api.dataforsyningen.dk/adresser?kommunekode=0101|0147&husnr=177?format=json"
#url <- "https://api.dataforsyningen.dk/adresser?kommunekode=0101|0147"

raw <- GET(url)
http_status(raw)

text <- content(raw)

test <- text[[1]]
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


