# COMPROBAR ULTIMA FECHA:
readRDS("data/data_searchesGoogle_ts.rds") %>% arrange(desc(date))
readRDS("data/data_GDELT_ts.rds") %>% arrange(desc(date))                 
