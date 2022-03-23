#-------------------------- Loading Packs ----------------------------------

library(tidyverse)
library(ggplot2)
library(readxl)

#------------------------- Loading Files ----------------------------

# Carregando o arquivo

best_west <- read_xlsx(
  "Best_in_show_westminster.xlsx"
)

# Viasualizando o arquivo

View(best_west)

#------------------------ Operations ------------------------------

# Agrupando o data frame por espécie e realizando a soma
# da raça que mais venceu

best_breed <- best_west %>% 
  group_by(Breed) %>% 
  summarize(count = n()) %>% 
  filter(count > 2)

# Agrupando o data frama por grupo e realizando a soma

best_group <- best_west %>% 
  group_by(Group) %>% 
  summarize(count = n()) %>% 
  filter(count > 3)

#------------------------ Ploting ---------------------------------

ggplot(data = best_breed, mapping = 
         aes(x = Breed, y = count,fill = Breed)) +
  geom_col() +
  geom_text(aes(label = count), vjust = 1.5, color = "white") +
  labs(title = "Westminster Show 1907 - 2017", fill = "Raça") +
  theme(axis.text.x = element_blank()) +
  xlab(NULL) +
  ylab("Quantidade de Vitórias")

ggplot(data = best_group) +
  geom_col(mapping = aes(x = Group, y = count, fill = Group))
