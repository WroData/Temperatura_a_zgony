#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#### Usuniêcie wszystkich bibliotek, zmiennych oraz ustalenie seed, by stan by³ odtwarzalny za ka¿dym razem ####
#restartowanie sesji
#.rs.restartR()

#od³¹czeni wszytkich pakietów
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo³anie funkcji 

#usuniêcie zmiennych
gc(reset = TRUE)
rm(list = ls())

#ustalenie seed
set.seed(1)

Sys.setenv(LANGUAGE='pl')

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

library(dplyr)
library(tidyr)
library(ggplot2)
# install.packages("ggridges")
library(ggridges)


MAIN_PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
OUT_PATH <- paste0(MAIN_PATH, "\\CHARTS\\")


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### THEME ####


C1 <- "#993404"
C2 <- "#d95f0e"
C3 <- "#fec44f"

FILL_COL <- "#dedfe0"
TEXT_COL <- "#4e4d47"
TEXT_BASE_SIZE <- 12

THEME <-
  theme(
    axis.line = element_blank(),
    axis.text.x = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.5,  color = TEXT_COL),
    axis.text.y = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.0,  color = TEXT_COL),
    axis.ticks = element_blank(),
    axis.title.x = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0.5,  color = TEXT_COL),
    axis.title.y = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0.5,  color = TEXT_COL),
    
    
    #panel.border = element_blank(),
    # panel.grid.major=element_blank(),
    #panel.grid.minor = element_blank(),
    
    #tlo
    plot.background  = element_rect(fill = FILL_COL,  color = NA), 
    panel.background = element_rect(fill = FILL_COL,  color = NA),
    text = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE - 2, color = TEXT_COL),
    
    # legenda
    legend.position = "bottom",# "none",
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title.align = 0.5,
    legend.title = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0, color = TEXT_COL),#element_blank(),
    legend.background = element_rect(fill = FILL_COL, color = NA),
    legend.key = element_rect(fill = FILL_COL),
    legend.text       = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0, color = TEXT_COL),
    legend.direction = "horizontal",
    
    # tytuy
    plot.title    = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE + 2, hjust = 0.0,  color = TEXT_COL, face="bold"),
    plot.subtitle = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.01, face = "italic", color = TEXT_COL),
    plot.caption  = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE - 2,  hjust = 0.99, color = TEXT_COL),
  )  


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#### Wczytanie danych ####
PATH <- MAIN_PATH
setwd(PATH)

zgony_pogoda <- read.csv(paste0(PATH, "\\DANE\\", 'Przerobione', ".csv"))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#### Krzywe wpływu ####

zgony_pogoda_long <- zgony_pogoda %>%
  filter(!Rok %in% c(2020, 2021)) %>%
  #rename(c(X00...4 = "X00.04", X05...09 = "X05.09")) %>%
  pivot_longer(
    cols = "Ogolem": "X90iwiecej", 
    names_to = "wiek",
    values_to = "zgony"
  ) %>%
  mutate(
    treshold_15 = ifelse(tavg > 15, 'above', "below"),
    treshold_20 = ifelse(tavg > 20, 'above', "below"),
    treshold_25 = ifelse(tavg > 25, 'above', "below"),
  )

# wyliczenie dla równych tresholwóP
zgony_pogoda_tres_15 <- zgony_pogoda_long %>%
  group_by(wiek, treshold_15) %>%
  summarise(z = mean(zgony)) %>%
  pivot_wider(names_from = treshold_15, values_from = z) %>%
  mutate(wplyw_15 = above / below ) %>%
  select(wiek, wplyw_15)


zgony_pogoda_tres_20 <- zgony_pogoda_long %>%
  group_by(wiek, treshold_20) %>%
  summarise(z = mean(zgony)) %>%
  pivot_wider(names_from = treshold_20, values_from = z) %>%
  mutate(wplyw_20 = above / below ) %>%
  select(wiek, wplyw_20)

zgony_pogoda_tres_25 <- zgony_pogoda_long %>%
  group_by(wiek, treshold_25) %>%
  summarise(z = mean(zgony)) %>%
  pivot_wider(names_from = treshold_25, values_from = z) %>%
  mutate(wplyw_25 = above / below ) %>%
  select(wiek, wplyw_25)

pogoda_a_wiek <- zgony_pogoda_tres_15 %>%
  merge(
    y = zgony_pogoda_tres_20,
    by = "wiek"
  ) %>%
  merge(
    y = zgony_pogoda_tres_25,
    by = "wiek"
  ) %>%
  pivot_longer(
    cols = "wplyw_15": "wplyw_25", 
    names_to = "treshold_temperatury",
    values_to = "wplyw"
  ) 
  



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#### wykres ####

ggplot(pogoda_a_wiek %>% filter(wiek != "Ogolem") %>% filter(treshold_temperatury != "wplyw_25"),
       aes(
         x = wiek, y = wplyw, color = treshold_temperatury, group = treshold_temperatury
         )) +
  geom_line()

