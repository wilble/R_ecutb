install.packages("dplyr")
install.packages("ggplot2")
install.packages("fastDummies")
install.packages("car")
install.packages('ISLR2')
install.packages('leaps')

library(dplyr)
library(ggplot2)
library(fastDummies)
library(car)
library(ISLR2)
library(leaps)

file_csv <- "C:\\Users\\Willi\\Documents\\ec_utb\\R_kurs\\data_blocket_cleaned.csv"

# undersöker datan

data <- read.csv(file_csv, sep = ";")
dim(data)
class(data)
head(data)
summary(data)
unique(data$Fuel)

# Skapar en ny data frame baserat på el och hybridbilar

elhybrid_data <- data %>%
  filter(!is.na(Horsepower)) %>%
  mutate(Fuel = ifelse(Fuel == "Milj\xf6br\xe4nsle/Hybrid", "Hybrid", Fuel)) %>%
  filter(Fuel %in% c("El", "Hybrid")) %>%
  select(-Location, -Column1)

summary(elhybrid_data)
unique(elhybrid_data$Fuel)

# undersöker outliers

z_scores <- scale(elhybrid_data$Price)
outliers <- which(abs(z_scores) > 3)
outlier_data <- elhybrid_data[outliers, ]

# visualiserar datan
boxplot(elhybrid_data$Price, main = "Priser")
points(outlier_data$Price, col = "red", pch = 16)

# Tar ut gränsvärdena för outliers
outlier_values <- elhybrid_data$Price[outliers]

print(outlier_values)
min(outlier_values)

#Jag vill ta bort nya bilar, outliers och eventuella leasingbilar som ändå finns kvar i urvalet

good_data <- subset(elhybrid_data, 
                    Price >= 10000 & 
                      Price < 939900 & 
                      Miles >= 10 & 
                      !(Brand %in% names(table(data$Brand)[table(data$Brand) < 10]))
                      & Brand != "Unkown" )
summary(good_data)
unique(good_data$Brand)

boxplot(good_data$Price, main = "Priser")


# Dummies av vårt testset för att genomföra linjär regression

dummy_data <- dummy_cols(good_data, select_columns = c("Fuel", "Gear", "Brand"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)
summary(dummy_data)



#vi har skapat dummies och nu vill jag se på modellen

test_data <- subset(dummy_data %>% 
                      rename(Brand_citroen = `Brand_Citro\xebn`))
                

model <- lm(Price ~ ., data = test_data)

summary(model)

par(mar = c(5, 5, 2, 2)) 
plot(model)


#undersöker normalfördelningen för vår beroende variabel
shapiro_result <- shapiro.test(test_data$Prices)

shapiro_result

# noterar att pris inte är normalfördelat så transformerar det med den naturliga logaritmen

test_data_transformed <- test_data %>%
  mutate(Price = log(Price))
      
model_transformed <- lm(Price ~ ., data = test_data_transformed)

summary(model_transformed)
plot(model_transformed)

# Undersöker vidare efter outliers i vår modell och tar bort dem för att testa modellen utan dem
cooksD <- cooks.distance(model_transformed)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

names_of_influential <- names(influential)
outliers2 <- test_data_transformed[names_of_influential,]
data_without_outliers <- test_data_transformed %>% anti_join(outliers2)
colSums(data_without_outliers)
data_without_outliers <- select(data_without_outliers, -Brand_Fiat, -Brand_Saab)
model2 <- lm(Price ~ ., data = data_without_outliers)

summary(model2)
plot(model2)

# Undersöker multicolinearity

vif_values <- vif(model2)
vif_values

# Undersöker subset selection

subset_bilar <- regsubsets(Price~., data = data_without_outliers, nvmax = 39, method = "forward")
summary_subset <- summary(subset_bilar)

options(max.print=2000)

print(summary_subset[-1])

par(mfrow = c(2, 2))
plot(subset_bilar, scale = "r2")
plot(subset_bilar, scale = "adjr2")
plot(subset_bilar, scale = "Cp")
plot(subset_bilar, scale = "bic")



