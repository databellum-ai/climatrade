
# CAMBIAR HISTORICO DE RECOMENDACIONES
transf <- readRDS("project2_main/recommendationsNN_all.RDS")
transf <- transf %>% mutate(
  regressors = ifelse(
    regressors == "VIX_n, VVIX_n, VIX3M_n, VIXNsdq_n, GoldVlty_n, + DAI3_n, CCI_n", 
    "VX+V1", 
    ifelse(regressors == "VIX_n, VVIX_n, VIX3M_n, VIXNsdq_n, GoldVlty_n, + DAI3_n, CCI_n + WkDay, YrWeek", 
           "VX+V1+C1", 
           "VX+C1"))
)

transf %>% mutate(transformations = if_else(transformations != "NuevoExtractMultihorizonte + >=2015", ">=2017", ">=2015"))

head(transf)
# saveRDS(transf, "project2_main/recommendationsNN_all.RDS")