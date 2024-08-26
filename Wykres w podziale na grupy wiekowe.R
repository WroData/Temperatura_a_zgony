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
names(zgony_pogoda)[1] <- "tyg"
zgony_pogoda_obrobione <- zgony_pogoda  %>% 
  filter(!Rok %in% c(2020, 2021))%>%
  mutate(
    infant = X00.04 + X05.09,
    senior = 
      X50.54 + X55.59 + X60.64 + X65.69 + 
      X70.74 + X75.79 + X80.84 + X85.89 + 
      X90iwiecej,
    mid = Ogolem - infant - senior
  ) %>%
  select(-c(Ogolem:X90iwiecej)) %>%
  select(-c(tmin, tmax, tyg, Kod.tyg.pure)) %>%
  pivot_longer(
    
    cols = c(infant, senior, mid), 
    names_to = "wiek",
    values_to = "zgony"
  ) %>%
  group_by(wiek) %>%
  mutate(sr = mean(zgony)) %>%
  ungroup() %>%
  mutate(
         zgony_index = zgony / sr
         )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#### Wykres ####

w <- ggplot(
  zgony_pogoda_obrobione ,
  aes(x = tavg, y = zgony_index, color = wiek)
) +
  #scale_color_viridis_c(option = "plasma") + 
  geom_vline(xintercept  = 0, color = "black", ) +
  geom_point(alpha = 0.5) + 
  geom_smooth() +
  coord_cartesian(y = c(0.5, 1.5), x = c(-10, 25)) + 
  labs(title = "W czasie upałów umiera 15% więcej osób",
       subtitle = "Lata 2000-24",
       y = "Liczba zgonów",
       x = paste0("Średnia temperatura w tygodniu"),
       #color = "Skrzyzowanie",
       caption = "Autor: WroData (Krzysztof Karabon) | Dane: liczba zgonów - GUS, temperatura - meteostat" ) + 
  THEME




# save
png(filename = paste0(OUT_PATH, "", "\\Z pandemii - by wiek index - ", 
                      Sys.Date(), " .png", sep=""),
    bg="#dedfe0", width = 8, height = 5, units = 'in', res = 500)
plot(w)
dev.off()


#############

w <- ggplot(
  zgony_pogoda_obrobione %>% filter(wiek == 'senior'),
  aes(x = tavg, y = zgony, color = tavg)
) +
  scale_color_viridis_c(option = "plasma") + 
  geom_vline(xintercept  = 0, color = "black", ) +
  geom_point(alpha = 0.5) + 
  geom_smooth() +
  coord_cartesian(y = c(50, 150), x = c(-10, 25)) + 
  labs(title = "W czasie upałów umiera 15% więcej osób",
       subtitle = "Lata 2000-24",
       y = "Liczba zgonów",
       x = paste0("Średnia temperatura w tygodniu"),
       #color = "Skrzyzowanie",
       caption = "Autor: WroData (Krzysztof Karabon) | Dane: liczba zgonów - GUS, temperatura - meteostat" ) + 
  THEME



20 / 120

# save
png(filename = paste0(OUT_PATH, "", "\\Bes pandemii - seniorzy - ", 
                      Sys.Date(), " .png", sep=""),
    bg="#dedfe0", width = 8, height = 5, units = 'in', res = 500)
plot(w)
dev.off()
