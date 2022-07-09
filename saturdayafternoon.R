library(tidyverse) # contains ggplot2 and other packages: readr, stringr, dplyr, forcats ...

penguins <- read.csv("data/penguins.csv")

plot1 <- ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, y = flipper_length_mm)) + 
  geom_point()

plot2 <- ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, y = flipper_length_mm,
                     colour = species, shape = sex)) + 
  geom_point() + theme_minimal() + 
  labs(x = "bill length", y = "flipper length",
       title = "Relationship between flipper length and bill length")

plot3 <- ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, y = flipper_length_mm,
                     colour = species, shape = sex)) + 
  geom_point() + theme_minimal() + 
  scale_color_manual(values = c("Adelie" = "#F96F5D",
                                "Chinstrap" = "#43281C",
                                "Gentoo" = "#7B9BBA"))
  labs(x = "bill length", y = "flipper length",
       title = "Relationship between flipper length and bill length")

plot2

plot1  

### Histograms 

# using base R 

hist1 <- hist(penguins$bill_length_mm, col = "purple", 
     main = "New title") # $ means select that column

hist2 <- ggplot(data = penguins,
       mapping = aes(x = bill_length_mm)) + 
  geom_histogram(fill = "royalblue", color = "black")  

hist3 <- ggplot(data = penguins,
       mapping = aes(x = bill_length_mm)) + 
  geom_histogram(fill = "royalblue", color = "black", 
                 bins = 14)  

hist3

ggplot(data = penguins,
                 mapping = aes(x = bill_length_mm)) + 
  geom_histogram(fill = "royalblue", color = "black") + 
  facet_wrap(~species, ncol = 1, scales = "free_y")


## Make a histogram of body masses faceted by species

ggplot(penguins,
       aes(x = body_mass_g)) +
  geom_histogram(fill = "#4345A6", color = "black") + 
  facet_wrap(~species, ncol = 1)

ggplot(penguins,
       aes(x = body_mass_g, fill = species)) + 
  geom_histogram(color = "black") + 
  facet_wrap(~species, ncol = 1)

plot(hist1)

## Box plots showing body mass across different species

ggplot(data = penguins, 
       aes(x = species, y = body_mass_g)) + 
  geom_boxplot()

ggplot(data = penguins, 
       aes(x = species, y = body_mass_g,
           color = sex)) + 
  geom_boxplot()

## removing entries with NA values for the sex variable
penguins2 <- filter(penguins, !is.na(sex))

ggplot(penguins2,
       aes(x = species, y = body_mass_g, 
           color = sex)) + 
  geom_boxplot()

## Using facet_grid

ggplot(data = penguins2,
       aes(x = body_mass_g, y = bill_length_mm,
           color = species)) + 
  geom_point() + 
  facet_grid(island~sex) + 
  theme_minimal()

ggplot(data = penguins2,
       aes(x = body_mass_g, y = bill_length_mm,
           color = sex)) + 
  geom_point() + 
  facet_grid(island~species) + 
  theme_minimal()

## using a violin plot

ggplot(penguins2,
       aes(x = species, y = body_mass_g, 
           color = sex)) + 
  geom_violin()

library(ggbeeswarm)

swarm_plt <- ggplot(penguins2,
       aes(x = species, y = body_mass_g, color = sex)) + 
  geom_beeswarm()

ggsave("images/myswarmplot.png", swarm_plt)

# bar chart
ggplot(data = penguins2,
       aes(x = species, fill = sex)) + 
  geom_bar(position = "dodge")












