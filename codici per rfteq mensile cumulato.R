library(tidyverse)
library(here)

ore <- readRDS(here("data", "processed", "ore.RDS"))
ricavi <- readRDS(here("data", "processed", "CC.rds"))
ftep <- readRDS(here("data", "processed", "ftepDIP.RDS"))


#fteq mensili
ore %>% 
  filter(ANNO == 2021) %>% 
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza"), 
         Ore = ifelse(Ore == SmartWorking, Ore, Ore+SmartWorking)) %>%  
  filter(Dipartimento != "Non applicabile") %>% 
  # group_by(Mese, Dipartimento, Reparto, Laboratorio, Dirigente) %>%  
  group_by(Mese, Dipartimento, Dirigente) %>%   
  filter(!is.na(Dirigente) & !is.na(Ore)) %>% 
  summarise(hworked = sum(Ore, na.rm = T)) %>%  
  filter(!Dipartimento %in% c("Costi Comuni e Centri contabili", 
                              "Dipartimento amministrativo",
                              "Direzione Amministrativa",
                              "Direzione Generale")) %>% 
  mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(36*4.34), hworked/(38*4.34))) %>% 
  pivot_wider(names_from = "Dirigente", values_from = c("hworked", "FTE"))  %>% View()

#pivot_wider(names_from = "Dirigente", values_from = "hworked")  %>%  
mutate(TotHW = Comparto + Dirigenza) %>%  ungroup() %>% 
  group_by(Dipartimento) %>% 
  mutate(CumSHW = cumsum(TotHW)) %>% View()


#mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(36*4.34), hworked/(38*4.34))) %>% 
#pivot_wider(names_from = "Dirigente", values_from = c("hworked", "FTE"))  %>% 
#select(-hworked_, -FTE_)  %>% 
# mutate(FTET = FTE_Comparto+FTE_Dirigenza) %>%  
group_by(Mese, Dipartimento) %>% 
  summarise(FTET = sum(FTET, na.rm=TRUE)) %>%  
  filter(!Dipartimento %in% c("Costi Comuni e Centri contabili", 
                              "Dipartimento amministrativo",
                              "Direzione Amministrativa",
                              "Direzione Generale")) %>% 
  
  left_join(  
    
    
    
    
    (ricavi %>% 
       filter(ANNO == 2021 & Costi== "Ricavo") %>% 
       # filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
       rowwise() %>% 
       mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>%   
       filter(TotRic >0) %>% 
       group_by(MESE,Dipartimento) %>%  
       summarise(TRic= sum(TotRic, na.rm = TRUE)) %>%  ungroup() %>% 
       group_by(Dipartimento) %>% 
       mutate(CumS=cumsum(TRic))), by = c("Mese"="MESE","Dipartimento" )  
    
    
    
  ) %>%  View()
mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>% 
  
  left_join(
    ftep , by= "Dipartimento") %>%  
  
  mutate(RFTE = round((TRic/((FTET)*FTp/100)), 2)) %>% 
  
  
  ggplot()+
  aes(x = Mese, 
      y = RFTE)+
  geom_point()+
  geom_line()+
  facet_wrap(~Dipartimento, ncol = 1, scales = "free_y")


#BISOGNA AGGIUNGERE IL DATO DEI FTE PROGRAMMATO....

