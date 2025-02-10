#Saad Ahmed Jamal
# 60608
#2025-01-10


library(readxl)
batdata <- read.csv("C:/Users/saada/Desktop/Drive/Evora/Courses/R_Programming/Rdata/Assignment/exercise/databases/batdata.csv", sep = ';')


str(batdata)
class(batdata$especie)

str(batdata)

batdata

for (y in colnames(batdata)){
  for (x in batdata[[y]]){
  if (is.na(x)){
    print(x)
    print(batdata)
    }
  }
}  

library(dplyr)
library(tidyverse)
batdata_clean <- batdata %>% drop_na()
batdata<-batdata_clean


barplot(table(batdata$especie), xlab="Species", ylab="Frequency")  
barplot(table(batdata$sexo), xlab="Gender", ylab="Frequency")  
barplot(table(batdata$sexo), xlab="Gender", ylab="Frequency")   
barplot(table(batdata$idade), xlab="Gender", ylab="Frequency")   

hist(table(batdata$peso) , breaks =3)
hist(table(batdata$ab) , breaks =2)




boxplot(as.numeric((batdata$ab)),        main = "Boxplot",   col = "green")
batdata
plot(batdata$ab, batdata$peso) 


library(ggplot2)


ggplot(data = batdata, aes(x = especie, y = peso)) + geom_point(color = "blue") +
  labs(title = "Scatter Plot Showing weight of species", x = "Bat Species", y = "Weight") +
  theme_minimal()



ggplot(data = batdata, aes(x = factor(idade), y = peso)) +
  geom_boxplot(fill = "cyan", color = "black") +
  labs(title = "Boxplot ", x = "idade", y = "peso") +
  theme_bw()




library(reshape2)
df <- batdata

df$ab <- gsub(",", ".", df$ab)
df$peso <- gsub(",", ".", df$peso)


df$ab <- as.numeric(df$ab)
df$peso <- as.numeric(df$peso)

df


mean_value <- mean(df$ab)
print(mean_value)

median_value <- median(df$ab)
print(median_value)

sd_value <- sd(df$ab)
print(sd_value)


mean_value2 <- mean(df$peso)
print(mean_value2)

median_value2 <- median(df$peso)
print(median_value2)

sd_value2 <- sd(df$peso)
print(sd_value2)



correlation_matrix <- cor(df$ab, df$peso)
print(correlation_matrix)
batdata


library(Hmisc)
rcorr(as.matrix(mtcars))


one.way <- aov(df$ab ~ df$peso, data = df)

one.way

summary(one.way)


df$peso2 <- as.factor(df$peso)
one.way2 <- aov(df$ab ~ peso2, data = df)


TukeyHSD(one.way2)


# Example data
data <- matrix(c(12, 5, 7, 10), nrow = 2, byrow = TRUE)
colnames(data) <- c("Category 1", "Category 2")
rownames(data) <- c("Group A", "Group B")
data <- as.table(data)

# View the table
print(data)




# Example data
data <- matrix(c(df$ab[0:100]), nrow = 2, byrow = TRUE)
colnames(data) <- c("Category 1", "Category 2")
rownames(data) <- c(df$peso)
data <- as.table(data)

# View the table
print(data)

result <- fisher.test(data)

# View the result
print(result)



# Correct Testing 
str(batdata)



groupA <- batdata[batdata$sexo == "m", ] 
groupB <- batdata[batdata$sexo == "f", ] 

groupA
groupB




library(ggplot2)     # For data visualization

ggplot(batdata, aes( x = batdata$ab, y = batdata$peso)) +
  geom_point(color = "blue") +
  labs(title = "Scatterplot Linearity Check", x = "ab", y = "peso") +
  theme_minimal()



correlation_matrix <- cor(df$ab, df$peso)
print(correlation_matrix)

batdata$peso

group1 <- df[df$peso >25,]  
group2 <- df[df$peso > 15 & df$peso <= 25,] 
group3 <- df[df$peso <15, ] 



group1  
group2
group3



# For noramlity check:

# Shapiro Wilk Test: Normality Check
shapiro.test(as.numeric(group1$ab))
shapiro.test(as.numeric(group2$ab))
shapiro.test(as.numeric(group3$ab))

$ks.test(tourism$Total_expenditure, "pnorm", mean=mean(tourism$Total_expenditure), sd=sd(tourism$Total_expenditure))
#leveneTest(tourism$Total_expenditure ~ tourism$Purpose_of_trip, data = tourism)


# Man Witney wilcox test
wilcox.test(df$ab ~ df$sexo, data = df)
wilcox.test(df$ab ~ df$idade, data = df)


