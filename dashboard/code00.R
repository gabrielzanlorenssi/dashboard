
library(tidyverse)
library(readxl)


credentials <- read_excel('credentials.xlsx')



credentials %>% 
  left_join(nexo.utils::popMunic %>%  filter(year==2020), by=c('ibge'='ibge7'))


credentials %>% 
  left_join(nexo.utils::popMunic %>%  filter(year==2020) %>% 
              mutate(ibge2 = as.numeric(str_sub(ibge7,1,2))) %>% 
              group_by(ibge2) %>% 
              summarise(pop = sum(pop, na.rm=T)), by=c('ibge'='ibge2'))


escolas00 <- read_delim("../escolas.CSV", 
                      "|", escape_double = FALSE, trim_ws = TRUE)

escolas00 %>% 
  mutate(across(starts_with("QT_PROF_"), ~ ifelse(.x>999, 0, .x))) %>% 
  mutate(QT_PROF = rowSums(across(starts_with("QT_PROF_")))) %>% 
  filter(TP_SITUACAO_FUNCIONAMENTO %in% c(1,2)) %>% 
  select(CO_ENTIDADE, NO_ENTIDADE, CO_UF, CO_MUNICIPIO, TP_DEPENDENCIA, TP_LOCALIZACAO, QT_PROF) %>%
  filter((CO_UF %in% credentials$ibge & TP_DEPENDENCIA==2) |
          (CO_MUNICIPIO %in% credentials$ibge & TP_DEPENDENCIA==3)) %>% 
  mutate(tipo = ifelse(CO_UF %in% credentials$ibge & TP_DEPENDENCIA==2, "uf", "muni"),
         ibge = ifelse(tipo=="uf", CO_UF, CO_MUNICIPIO)) %>% 
  select(-one_of('TP_DEPENDENCIA')) %>% 
  mutate(TP_LOCALIZACAO = ifelse(TP_LOCALIZACAO == 1, "Urbana", "Rural"),
         ID = paste0(NO_ENTIDADE, " (", CO_ENTIDADE, ")")) %>% 
  left_join(credentials[,c(2,5)], by="ibge") %>% 
  filter(cidade != "Admin") %>% 
  filter(cidade != "g") %>% 
  select(cidade, ID) -> escolas01
  
write_rds(escolas01, './data/escolas01.rds')

write.csv2(escolas01, "escolas.csv", row.names=F, na="")

  