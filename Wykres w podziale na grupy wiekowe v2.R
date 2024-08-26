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


zgony_pogoda_long <- zgony_pogoda %>%
  filter(!Rok %in% c(2020, 2021)) %>%
  pivot_longer(
    
    cols = 'X00.04':'X90iwiecej', 
    names_to = "wiek",
    values_to = "zgony"
  ) %>%
  group_by(wiek) %>%
  mutate(z_avg = mean(zgony),
         zgony_index = zgony / z_avg) %>%
  ungroup()
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#### Krzywe wpływu ####
w <- ggplot(
  zgony_pogoda_long ,
  aes(x = tavg, y = zgony_index, color = wiek)
) +
  scale_color_viridis_d(option = "plasma") + 
  geom_vline(xintercept  = 0, color = "black", ) +
  #geom_point(alpha = 0.5) + 
  geom_smooth(se = F) +
  coord_cartesian(y = c(0.750, 1.25), x = c(-10, 25)) + 
  labs(title = "Na upały narażone są najbardziej osoby starsze",
       subtitle = "Lata 2000-19 i 2022-24",
       y = "wielokrotność zgodnów w stosunku do średniej",
       x = paste0("Średnia temperatura w tygodniu"),
       color = "grupa wiekowa",
       caption = "Autor: WroData (Krzysztof Karabon) | Dane: liczba zgonów - GUS, temperatura - meteostat" ) + 
  THEME

# save
png(filename = paste0(OUT_PATH, "", "\\Bez pandemii - grupy wiekowe slope - ", 
                      Sys.Date(), " .png", sep=""),
    bg="#dedfe0", width = 8, height = 5, units = 'in', res = 500)
plot(w)
dev.off()



#### MODEL ####
lm <- lm(Ogolem ~ tavg, data = zgony_pogoda_long)
summary(lm)
0.63589 * 15 / 118.26974 *100


lm <- lm(Ogolem ~ tavg, data = zgony_pogoda_long)

zgony_pogoda_long$wiek %>% unique()
coefs <- data.frame()
for (col in c(
  "X00.04", "X05.09", "X10.14", "X15.19", "X20.24", "X25.29",  "X30.34", 
  "X35.39", "X40.44", "X45.49", "X50.54", "X55.59", "X60.64" , "X65.69",
  "X70.74" ,"X75.79" ,"X80.84" ,"X85.89", 
  "X90iwiecej"
  )) {
  print(col)
  tmp_dat <- zgony_pogoda_long %>% filter(wiek == toString(col)) 
  lm <- lm(zgony ~ tavg, data = tmp_dat)
  tmp <- data.frame(coefficients(lm))
  tmp <- data.frame(t(tmp))
  tmp['wiek'] <- col
  
  coefs <- rbind(coefs, tmp)
  
}

coefs['prc_wplyw'] <- coefs['tavg'] / coefs['X.Intercept.']

w <- ggplot(
  coefs ,
  aes(x = wiek, y =prc_wplyw)
) +
  scale_color_viridis_c(option = "plasma") + 
  geom_hline(yintercept  = 0, color = "black", ) +
  geom_point(alpha = 0.5) + 
  #coord_cartesian(y = c(0.75, 1.25))+ 
  geom_smooth() +
  labs(title = "Procentowy wzrost śmiertelności z każdym wzrotem temperatury o 1 stopień",
       subtitle = "Lata 2000-19 i 2022-24",
       y = "Wpływ [slope / intercept]",
       x = paste0("Grupa wiekowa "),
       #color = "Skrzyzowanie",
       caption = "Autor: WroData (Krzysztof Karabon) | Dane: liczba zgonów - GUS, temperatura - meteostat" ) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1))+
  scale_x_discrete(labels = c(
    "00\n04", "05\n09", "10\n14", "15\n19", "20\n24", "25\n29",  "30\n34", 
    "35\n39", "40\n44", "45\n49", "50\n54", "55\n59", "60\n64" , "65\n69",
    "70\n74" ,"75\n79" ,"80\n84" ,"85\n89", 
    "90+"
  ))+
  
  THEME

plot(w)



# save
png(filename = paste0(OUT_PATH, "", "\\Bez pandemii - slope a grupa wiekowa", 
                      Sys.Date(), " .png", sep=""),
    bg="#dedfe0", width = 8, height = 5, units = 'in', res = 500)
plot(w)
dev.off()

