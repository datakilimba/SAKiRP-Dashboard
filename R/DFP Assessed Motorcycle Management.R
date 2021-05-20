library(tidyverse)
library(xlsx)
library(magrittr)
library(httr)
library(RPostgres)

con = DBI::dbConnect(odbc::odbc(), "PostgreSAKiRP")  

wards = dbReadTable(con,"wards")
districts = dbReadTable(con,"districts")
waeos = dbReadTable(con,"waeos")


kobo_server_url = "https://kc.humanitarianresponse.info/"
form_id = "786483" 
url = paste0(kobo_server_url,"api/v1/data/",form_id,".csv")
rawdata = GET(url,authenticate(Sys.getenv("sakirp_user"),Sys.getenv("sakirp_pw")))
content = content(rawdata,"raw",encoding="UTF-8")
mc_data = read_csv(content) 

mc_long = mc_data %>% 
  pivot_longer(cols = ends_with("attitude"):ends_with())
