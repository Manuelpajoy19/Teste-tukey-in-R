library(multcomp)
library(tidyverse)
library(readxl)
library(gvlma)
library(broom)
library(nlme)
library(car)
library(emmeans)
library(DHARMa)
library(ggpubr)
library(fpp)
library(ScottKnott)
library(agricolae)

Base <- read_excel("ChesseModificada.xlsx")
Base$day <- as.character(Base$day)

#Acidez

ggplot(Base)+
  aes(x = day, 
      y = acidity, 
      fill = municipality)+
  geom_boxplot()

Maci<- lm(acidity ~ day * municipality, data = Base)
anova(Maci)
#lot(Maci)
gvlma(Maci)
shapiro.test(residuals(Maci))
qqPlot(Maci)
SimRes <- simulateResiduals(Maci)
plot(SimRes)

res1 <- emmeans(Maci, ~ day|municipality, type = "response")%>%
  cld(method = "tukey", Letters = letters, reversed = TRUE)%>%
  as.data.frame()%>%
  mutate(.group = str_trim(.group))


res2 <- emmeans(Maci, ~ municipality|day, type = "response")%>%
  cld(method = "tukey", Letters = LETTERS, reversed = TRUE)%>%
  as.data.frame()%>%
  mutate(.group = str_trim(.group))


res2

resultados <-  res1 %>%
  left_join(res2, by = c("day", "municipality")) %>% 
  select(dia = day, municipio = municipality, meia = emmean.x, grupo1 = .group.x, grupo2 = .group.y) %>%
  mutate(letra = paste0(grupo2, grupo1))
resultados <- as.data.frame(resultados)

Des <- Base %>%
  group_by(day, municipality) %>%
  summarise(SD = sd(acidity))

Des <- Des %>%
  rename(
    dia = day,
    municipio = municipality,
    SD = SD)
Des <- as.data.frame(Des)

resultados <- Des %>%
left_join(resultados, by = c("dia","municipio"))


resultados <- resultados %>%
  mutate(limite_superior = meia  + SD, 
         limite_inferior = meia - SD)
resultados <- as.data.frame(resultados)

# tiff("Acidity.tiff", width = 3000, height = 1600, units = "px", res = 300)
ggplot(resultados)+
  aes(x = reorder(dia, as.numeric(as.character(dia))),
      y = meia ) +
  geom_col( aes (fill = municipio), position = position_dodge(0.9), 
            color = "white")+
  geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior),
                position = position_dodge2(width = 0.6, padding = 0.5))+
  geom_text(aes(label = letra, y= limite_superior, group = municipio), 
            position = position_dodge(1.0), vjust = -0.5, size = 3)+
  labs(x = "Ripening days", 
       y = "Acidity", 
       fill= "Region")+
  scale_y_continuous(limits = c(0, 0.65), expand = c(0, 0))+
  theme(axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))
# dev.off()