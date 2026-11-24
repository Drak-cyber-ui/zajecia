#29.5
library(ggplot2)
library(dplyr)
library(MASS)
library(tidyr)

data(crabs)
data(birthwt)
#1
birthwt$low <- factor(birthwt$low)
birthwt$race <- factor(birthwt$race)

ggplot(birthwt, aes(x = low, fill = race)) +
  geom_bar(position = 'dodge') +
  labs(
    title = "Liczba dzieci według masy urodzeniowej i rasy",
    x = "Mała masa urodzeniowa (1 = tak, 0 = nie)",
    y = "Liczba dzieci",
    fill = "Rasy",
    caption = "Źródło: pakiet MASS"
  )

#2
ggplot(BodyTemperature, aes(x = Temperature, y = HeartRate, color = Gender)) +
  geom_point() +
  labs(
    title = "Zależność pulsu od temperatury ciała",
    x = "Temperatura ciała [°F]",
    y = "Puls",
    color = "Płeć",
    caption = "Źródło: http://extras.springer.com/2012/978-1-4614-1302-8/BodyTemperature.txt"
  )

#30.6
#1
kobiety <- BodyTemperature %>%
  filter(Gender == "F", Age > 45)

srednie <- BodyTemperature %>%
  group_by(Gender) %>%
  summarise(puls = mean(HeartRate)) %>%
  mutate(
    Age = max(BodyTemperature$Age),
    HeartRate = min(BodyTemperature$HeartRate) + c(5, 0),
    label = paste(Gender, round(puls, 1))
  )

ggplot(BodyTemperature, aes(x = Age, y = HeartRate)) +
  geom_point(aes(color = Gender)) +
  geom_text(data = kobiety, aes(label = Age), nudge_y = 2) +
  geom_label(data = srednie, aes(label = label), hjust = 1) +
  labs(
    title = "Puls w zależności od wieku",
    x = "Wiek",
    y = "Puls",
    color = "Płeć"
  )

#2
ggplot(birthwt, aes(x = lwt)) +
  geom_histogram(color = "black", fill = "white") +
  geom_vline(
    xintercept = mean(birthwt$lwt),
    color = "red"
  ) +
  geom_vline(
    xintercept = median(birthwt$lwt),
    color = "blue",
    linetype = "dashed"
  )

#31.10
#1
ggplot(BodyTemperature, aes(x = Age, y = HeartRate, color = Gender)) +
  geom_point() +
  scale_x_continuous(breaks = seq(20, 50, by = 4)) +
  scale_color_discrete(
    name = "Płeć",
    labels = c("Kobiety", "Mężczyźni")
  ) +
  theme(legend.position = "bottom")

#2
birthwt.agr <- birthwt %>%
  group_by(race, ht) %>%
  summarise(srednia = mean(bwt), .groups = "drop")

ggplot(birthwt.agr, aes(x = race, y = srednia,
                        color = factor(ht), group = ht)) +
  geom_line() +
  geom_point() +
  scale_color_manual(
    name = "Nadciśnienie",
    values = c("green", "red"),
    labels = c("Nie", "Tak")
  ) +
  theme(legend.position = "bottom") +
  labs(
    x = "Rasa",
    y = "Średnia masa dziecka (g)"
  )

#3
ggplot(birthwt, aes(x = log(lwt))) +
  geom_histogram(aes(y = after_stat(density)),
                 color = "black", fill = "white")


#32.3
#1
p <- ggplot(crabs, aes(x = CL, y = FL, color = sp)) +
  geom_point()
p
p + theme_bw()
p + theme_minimal()
p + theme_classic()
p + theme_void()
#najlepiej minimal a nie nada sie void

#2
p2 <- p +
  labs(
    title = "Długość szczypiec rośnie wraz z długością pancerza",
    subtitle = "Porównanie dwóch gatunków krabów"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text = element_text(size = 12)
  )

p2

#3
library(gapminder)
data(gapminder)

zycie <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(srednia = mean(lifeExp))

ggplot(zycie, aes(x = continent, y = srednia)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Średnia długość życia w 2007 roku",
    x = "Kontynent",
    y = "Średnia długość życia (lata)"
  )

#4
ggsave("wykres.png", plot = p2,
       width = 8, height = 5, dpi = 300)

ggsave("wykres.pdf", plot = p2,
       width = 20, height = 12, units = "cm")

ggsave("wykres_maly.png", plot = p2,
       width = 4, height = 2.5)

#32.4

#2
library(palmerpenguins)
data(penguins)

pingwiny <- penguins %>%
  filter(!is.na(sex))

p_pingwiny <- ggplot(pingwiny,
                     aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("female" = "orange", "male" = "blue"),
    labels = c("Samica", "Samiec")
  ) +
  labs(
    title = "Samce pingwinów są zazwyczaj cięższe od samic",
    x = "Gatunek pingwina",
    y = "Masa ciała (g)",
    fill = "Płeć",
    caption = "Źródło: pakiet palmerpenguins"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_pingwiny
ggsave("pingwiny.png", plot = p_pingwiny,
       width = 8, height = 5, dpi = 300)

#Uzasadnienie: Wykres pudełkowy ponieważ pozwala porównać medianę masy ciała

#33.1
library(tidyverse)

set.seed(123)

servers <- c("Compute", "Storage", "Database", "Gateway", "Analytics")
months <- month.name[1:12]

data <- expand_grid(
  Server = servers,
  Month = months
) %>%
  mutate(Failures = rpois(n(), lambda = sample(1:6, 1)))

#1
data$Month <- factor(data$Month, levels = months)

ggplot(data, aes(x=Month, y= Failures, group=Server))+
  geom_line()

#2
ggplot(data, aes(x=Month, y= Failures, group=Server, color=Server))+
  geom_line()

#3
ggplot(data, aes(x=Month, y= Failures, group=Server, color=Server))+
  geom_line() +
  geom_point()+
  labs(
    title = "Liczba awarii serwerów w ciągu roku",
    x = "Miesiąc",
    y = "Liczba awarii",
    color = "Typ serwera"
  )+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

#4


