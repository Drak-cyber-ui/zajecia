
library(dplyr)
library(tidyr)
library(MASS)
data(crabs)
data(birthwt)

#21.7
#1
ggplot(crabs, aes(x=CL,y=FL)) +
  geom_point()

#2
ggplot(crabs, aes(x=CL,y=FL, colour = sp)) +
  geom_point()

#3
ggplot(crabs, aes(x=CL,fill=sp)) +
  geom_histogram(binwidth = 0.5, color="black")

#4
ggplot(crabs, aes(x=sp,y=CL,fill = sp)) +
  geom_boxplot()

#5
crabs.long <- crabs %>%
  pivot_longer(
    cols = FL:BD,
    names_to = "cecha",
    values_to = "wartosc"
  )

ggplot(crabs.long, aes(x = cecha, y = wartosc,fill=sp)) +
  geom_boxplot()

#7
?birthwt

ggplot(birthwt, aes(x=lwt, y=bwt))+
  geom_point()


#8
library(gap)
data(hla)

hla.long <- hla %>% 
  pivot_longer(
    cols = -c(id,y),
    names_to = "marker",
    values_to = "ekspresja"
  ) %>% 
  mutate(y=as.factor(y)) %>% 
  rename(grupa=y)

hla.long %>% 
  group_by(marker,grupa) %>% 
  summarise(
    n(),
    mean(ekspresja),
    sd(ekspresja)
  )

hla.long %>% 
  group_by(marker,grupa) %>% 
  summarise(srednia = mean(ekspresja)) %>% 
  pivot_wider(names_from = grupa, values_from = srednia)


#9
protein.long <- Protein %>%
  pivot_longer(
    cols = -1,
    names_to = "zmienna",
    values_to = "wartosc"
  )

#22.6
#1
ggplot(crabs, aes(x=CL, y=FL, color=sp,shape = sex))+
  geom_point(alpha=0.5)


#2
ggplot(hla.long, aes(x=marker,y=ekspresja,size=id))+
  geom_point()

ggplot(hla.long, aes(x=marker,y=ekspresja,color=id))+
  geom_point(alpha=0.5)


#3
birthwt$race <- factor(birthwt$race, labels = c("white", "black", "other"))
birthwt$ht <- factor(birthwt$ht)

ggplot(birthwt, aes(x=lwt,y=bwt,color=race,shape=ht))+
  geom_point()

#4
ggplot(BodyTemperature,aes(x=HeartRate,y=Temperature,colour = Gender))+
  geom_point()


#23.4
#1
ggplot(crabs, aes(x = CL, y = FL, color = sex)) +
  geom_point() +
  facet_wrap(~ sp)


#2
birthwt$smoke <- factor(birthwt$smoke)

ggplot(birthwt, aes(x = lwt, y = bwt, color = smoke)) +
  geom_point() +
  facet_grid(race ~ ht)

#3
ggplot(hla.long, aes(x = marker, y = ekspresja)) +
  geom_point() +
  facet_wrap(~ grupa)

#4
ggplot(BodyTemperature, aes(x = HeartRate, y = Temperature)) +
  geom_point() +
  facet_wrap(~ Gender)


#24
#1
library(palmerpenguins)
data(penguins)

dim(penguins)
colnames(penguins)
summary(penguins)

?penguins

#Ile pingwinów każdego gatunku zmierzono na poszczególnych wyspach?

penguins %>% 
  count(species,island)



#2
ggplot(penguins, aes(x=body_mass_g, y=flipper_length_mm))+
  geom_point()

#Jaki widzisz związek między tymi cechami?
#Większe pingwiny mają dłuższe płetwy

#3
ggplot(penguins, aes(x=body_mass_g, y=flipper_length_mm))+
  geom_line()

#4
sum(is.na(penguins$body_mass_g))
sum(is.na(penguins$sex))
#wyskakuje ostrzeżenie że usunięto x wierszów bo były brakujące wartości

#5
ggplot(penguins, aes(x=body_mass_g))+
  geom_histogram(binwidth = 50,fill="blue", color="black")


ggplot(penguins, aes(x=body_mass_g))+
  geom_histogram(binwidth = 200,fill="blue", color="black")

ggplot(penguins, aes(x=body_mass_g))+
  geom_histogram(binwidth = 1000,fill="blue", color="black")

#najlepiej binwidth 200

#6
ggplot(penguins,aes(x=species))+
  geom_bar()

#7
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm, color = species)) +
  geom_point()
#widać podział na gatunek

#8
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm,
                     color = species, size = species, shape = species)) +
  geom_point()
#wnosi chaos

#9
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point(color = "blue", alpha = 0.5)

#ponieważ ustawiamy je jednakowo dla wszystkich punktów

#10
ggplot(penguins, aes(x=body_mass_g, y=flipper_length_mm, color= species))+
  geom_point()+
  facet_wrap(~island)
#nie

#11
ggplot(penguins,aes(x=bill_length_mm,y=bill_depth_mm, color= species))+
  geom_point()+
  facet_wrap(~sex)
#nie ma tam gatunku chinstrap

#12
penguins.long<-penguins %>% 
  pivot_longer(
    cols= c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
    names_to = "cecha",
    values_to = "wartosc"
  )

ggplot(penguins.long, aes(x=species, y=wartosc, fill=species))+
  geom_boxplot()+
  facet_wrap(~cecha, scales="free_y")
