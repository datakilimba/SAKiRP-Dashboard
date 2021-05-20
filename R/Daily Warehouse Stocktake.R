library(tidyverse)
library(httr)
library(RPostgres)
library(plotly)

con = dbConnect(odbc::odbc(),"PostgreSAKIRP")
ward = dbReadTable(con,"wards")
district = dbReadTable(con,"districts")
amcos = dbReadTable(con,"amcos")

kobo_server_url = "https://kc.humanitarianresponse.info/"
form_id = "769790" 
url = paste0(kobo_server_url,"api/v1/data/",form_id,".csv")
rawdata = GET(url,authenticate(Sys.getenv("sakirp_user"),Sys.getenv("sakirp_pw")))
content = content(rawdata,"raw",encoding="UTF-8")
warehouse_data = read_csv(content)

# warehouse_data = xlsx::read.xlsx(
#   paste0("C:/Users/tnkil/OneDrive/Desktop/ENABEL Miscellaneous/AMCOS Groups/",
#          "SPM Manifest/Daily_SPM_Warehouse_Stocktake_-_all_versions_-_English_-_2021-04-06-13-42-50.xlsx"),1)

close_data = warehouse_data %>% 
  select(
    endtime,
    dist = `survey_info/district`,
    wrd = `survey_info/ward`,
    calima = as.double(ends_with("closing_stock-calima-y")),
    red = as.double(ends_with("closing_stock-red-y")),
    yellow = as.double(ends_with("closing_stock-njano-y")),
    amcos = ends_with("amcos")
    
    ) %>% 
  left_join(ward, by = c("wrd" = "id")) %>% 
  left_join(district, by = c("dist" = "id")) %>% 
  select(
    endtime,district,ward,amcos,calima,red,yellow
  )
  

close_data_long = close_data %>% 
  pivot_longer(cols = c('calima':'yellow'),
               names_to = "type",
               values_to = "closing_qty") %>% 
  left_join(amcos, by = c("amcos" = "id")) %>% 
  select(-ward.y) %>% 
  rename(ward = ward.x)

warehouse_closing = function(){
  ggplot(close_data_long, aes(x = endtime,y = closing_qty, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_bw() +
    facet_wrap(~amcos) +
    ylab("")+
    xlab("") + 
    scale_y_continuous(labels=scales::comma)
}

getCloseValue = function(variety){
    
  value = close_data_long %>% 
    filter(type == variety) %>% 
    group_by(amcos, m = lubridate::month(endtime)) %>% 
    filter(endtime == max(endtime)) 
  
   value = as.double(sum(value$closing_qty))
}


