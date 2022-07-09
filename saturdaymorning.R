library(dplyr)

cars2020 <- read.csv("data/cars2020.csv") 

manual <- filter(cars2020, transmission == "Manual")

# How many cars have 6 or more cylinders?

cyl6 <- filter(cars2020, cyl >= 6)

# How many cars are manual transmission and have 6 or more cylinders?

manual6 <- filter(cars2020, transmission == "Manual",
                  cyl >= 6)

auto6 <- filter(cars2020, transmission == "Automatic",
                gears < 6)

automat6 <- filter(cars2020, transmission == "Automatic",
                     gears < 6)

copy_auto6 <- auto6


auto6inc <- filter(cars2020, transmission == "Automatic", 
                   gears <= 6)

#### Keep only the model, mpg, transmission and cyl columns (of original data)

cars_narrow <- select(cars2020, model, mpg, transmission, cyl)


#### Keep all columns of data except startStop and aspiration

cars_alt <- select(cars2020, -startStop, -aspiration)

hist(cars2020$mpg, main = "Histogram of fuel efficiency",
     col = "purple")

### Sort cars in descending order of mpg

cars_desc <- arrange(cars2020, desc(mpg))

### sort by transmission type (alphabetically) then by mpg (descending)

cars_transmpg <- arrange(cars2020, transmission, desc(mpg))

### Example of writing to a .csv file
write.csv(cars_transmpg, "my_newfile.csv")

## Example of reading from an excel spreadsheet
library(readxl)

example <- read_xlsx("data/Example_File.xlsx")

### add a column to indicate if the mpg is greater than 30

cars2020 <- mutate(cars2020, 
                   above30 = if_else(mpg > 30, TRUE, FALSE))

## moving the above30 column to be after mpg
cars2 <- relocate(cars2020, above30, .after = mpg)

## create a report with the total number of cars and the average mpg

report <- summarise(cars2020,
                    total = n(), 
                    avg_mpg = mean(mpg),
                    med_mpg = median(mpg))
### create a grouped data frame

cars_grouped <- group_by(cars2020, make)

### summarising a grouped data frame

report2 <- summarise(cars_grouped, 
          total = n(),
          avg_mpg = mean(mpg),
          med_mpg = median(mpg))

cars_grouped2 <- group_by(cars2020, make, transmission)

report3 <- summarise(cars_grouped2, total = n())



































