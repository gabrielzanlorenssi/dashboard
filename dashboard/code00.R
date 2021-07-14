credentials <- read_excel('credentials.xlsx')


credentials %>% 
  left_join(nexo.utils::popMunic %>%  filter(year==2020), by=c('ibge'='ibge7'))


credentials %>% 
  left_join(nexo.utils::popMunic %>%  filter(year==2020) %>% 
              mutate(ibge2 = as.numeric(str_sub(ibge7,1,2))) %>% 
              group_by(ibge2) %>% 
              summarise(pop = sum(pop, na.rm=T)), by=c('ibge'='ibge2'))


escolas00 <- read_delim("C:/freelances/escolas.CSV", 
                      "|", escape_double = FALSE, trim_ws = TRUE)

escolas00 %>% 
  filter(TP_SITUACAO_FUNCIONAMENTO %in% c(1,2)) %>% 
  select(CO_ENTIDADE, NO_ENTIDADE, CO_UF, CO_MUNICIPIO, TP_DEPENDENCIA, TP_LOCALIZACAO) %>%
  filter((CO_UF %in% credentials$ibge & TP_DEPENDENCIA==2) |
          (CO_MUNICIPIO %in% credentials$ibge & TP_DEPENDENCIA==3)) %>% 
  select(-one_of('TP_DEPENDENCIA')) %>% 
  mutate(TP_LOCALIZACAO = ifelse(TP_LOCALIZACAO == 1, "Urbana", "Rural")) -> escolas01
  
write_rds(escolas01, './data/escolas01.rds')
  
  