
#26.3
#1
library(ggplot2)
ggplot(BodyTemperature, aes(x = Temperature , y = HeartRate)) +
  geom_point() +
  geom_smooth(method = "loess")

#2
library(MASS)
data("birthwt")
birthwt$low <- factor(birthwt$low)

ggplot(birthwt, aes(x = lwt, y = bwt, color = low)) +
  geom_point() +
  geom_smooth(method = lm)

#27.9
#1
birthwt$race <- factor(birthwt$race, labels = c("white", "black", "other"))
birthwt$smoke <- factor(birthwt$smoke)

ggplot(birthwt, aes(x = race, fill = race)) +
  geom_bar(color="blue")

ggplot(birthwt, aes(x = race, fill = smoke)) +
  geom_bar()

ggplot(birthwt, aes(x=race, fill=smoke))+
  geom_bar(position = "fill")

ggplot(birthwt, aes(x=race, fill=smoke))+
  geom_bar(position = "dodge")
#dla rasy other

birth.agr <- birthwt %>% 
  group_by(race,smoke) %>% 
  summarise(liczba=n(), .groups = "drop")

ggplot(birth.agr, aes(x=race, y=liczba, group = smoke, shape=smoke))+
  geom_line()+
  geom_point()

#2
bt <- BodyTemperature %>% 
  mutate(grupa = cut(Age, breaks=c(20,30,40,50)))

bt.agr<- bt %>% 
  group_by(Gender, grupa) %>% 
  summarise(puls=mean(HeartRate), .groups = "drop")

ggplot(bt.agr, aes(x = Gender, y = puls, group = grupa, color = grupa, shape = grupa)) +
  geom_line(linetype = 'dashed', linewidth = .7)+
  geom_point(size=2)


#28.5
#1
data("birthwt")
ggplot(birthwt, aes(x= lwt))+
  geom_histogram()

#2
ggplot(Protein, aes(x=WhiteMeat))+
  geom_histogram(aes(y=after_stat(density)),
                 color="black", fill="red")+
  geom_density(fill="blue", alpha=.4)


#3
ggplot(BodyTemperature,aes(x=Gender, y=Temperature, fill=Gender))+
  geom_boxplot()
# Która grupa ma wyższe wartości?
#F
#   Która grupa ma większy rozrzut?
#M

#4
data("penguins")
ggplot(penguins, aes(x=flipper_length_mm, fill=species))+
  geom_density(alpha=.4)
# czesciowo sie pokrywaja

#5
ggplot(penguins, aes(x = bill_length_mm)) +
  geom_histogram() +
  facet_wrap(~ species, ncol = 1)

ggplot(penguins, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_boxplot()

ggplot(penguins, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_violin()

ggplot(penguins, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  coord_flip()

#6
library(gapminder)
data("gapminder")

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x = reorder(continent, lifeExp, median), y = lifeExp)) +
  geom_boxplot() +
  coord_flip()

#7
data(mtcars)

mtcars$cyl <- factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am)

ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~ am)

#8
gapminder.arg<- gapminder %>% 
  group_by(year, continent) %>% 
  summarise(srednia = mean(lifeExp), .groups = "drop")

ggplot(gapminder.arg, aes(x=year, y=srednia, color=continent))+
  geom_line()+
  geom_point()


gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x=continent, fill = continent))+
  geom_bar()





