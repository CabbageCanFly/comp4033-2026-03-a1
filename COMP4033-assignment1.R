library(this.path)
library(tidyverse)
setwd(dirname(this.path()))

oral_data = read.csv("oral_cancer_prediction_dataset.csv")

str(oral_data)
names(oral_data)
head(oral_data, 15)

risk_score <- function(tobacco, alcohol, hpv, family_history, immune_system) {
  (tobacco == "Yes") * 2 +
    (alcohol == "Yes") +
    (hpv == "Yes") * 2 + 
    (family_history == "Yes") + 
    (immune_system == "Yes")
}

risk_score(oral_data$Tobacco.Use[1],
           oral_data$Alcohol.Consumption[1],
           oral_data$HPV.Infection[1],
           oral_data$Family.History.of.Cancer[1],
           oral_data$Compromised.Immune.System[1])

output = oral_data %>% filter(
       oral_data$Cancer.Stage == 1,
       oral_data$Tumor.Size..cm. < 5,
       oral_data$Country == "Sri Lanka",
       oral_data$Age > 60,
       oral_data$Gender == "Female",
       oral_data$Tobacco.Use == "No",
       oral_data$Betel.Quid.Use == "No")
output

# Independent var: Tumor Size -- Dependent var: Cost of Treatment
head(data.frame(oral_data$Tumor.Size..cm., oral_data$Cost.of.Treatment..USD.), 15)

# Independent var: Diet (fruits, veg intake) -- Dependent var: Cancer Stage
head(data.frame(oral_data$Diet..Fruits...Vegetables.Intake., oral_data$Cancer.Stage), 15)

clean_data = oral_data %>% filter(!is.na(Cost.of.Treatment..USD.))
clean_data = clean_data %>% distinct()

clean_data = clean_data %>% arrange(desc(Cost.of.Treatment..USD.))

clean_data = clean_data %>% rename(
  Drinker = Alcohol.Consumption,
  Treatment.Cost.USD = Cost.of.Treatment..USD.
)

clean_data = clean_data %>% mutate(cost.per.cm = Treatment.Cost.USD / Tumor.Size..cm.)

set.seed(1234)
training_data = clean_data %>% sample_frac(0.05, replace = FALSE)

summary(clean_data)

mean(clean_data$Treatment.Cost.USD, na.rm = TRUE)
median(clean_data$Treatment.Cost.USD, na.rm = TRUE)
range(clean_data$Treatment.Cost.USD, na.rm = TRUE)

cancer_stage_count = table(oral_data$Cancer.Stage)
cancer_stage_count = sort(cancer_stage_count, decreasing = TRUE)
names(cancer_stage_count)[1]

cor(clean_data$Treatment.Cost.USD, clean_data$Survival.Rate..5.Year...., method="pearson")

ggplot(data = training_data, aes(x = Survival.Rate..5.Year...., y = Treatment.Cost.USD)) + 
  geom_point(color = "steelblue", size = 1.2)

ggplot(data = training_data, aes(x = Cancer.Stage, fill = Gender)) + geom_bar() + 
  ylab("Number of Individuals")
