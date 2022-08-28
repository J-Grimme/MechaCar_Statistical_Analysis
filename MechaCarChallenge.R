# Deliverable 1
library(dplyr)
MechCar_file <- read.csv(file='MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)
lm(MechCar_file)
summary(lm(MechCar_file))

# Deliverable 2
library(dplyr)
Suspension_Coil_file <- read.csv(file='Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
total_summary <- summarise(Suspension_Coil_file, Mean_PSI = mean(PSI), 
                           Median_PSI = median(PSI),
                           Variance_PSI = var(PSI),
                           Standard_Deviation_PSI = sd(PSI))
lot_summary <- Suspension_Coil_file %>% group_by(Manufacturing_Lot) %>% 
                          summarise(Suspension_Coil_file, Mean_PSI = mean(PSI), 
                          Median_PSI = median(PSI),
                          Variance_PSI = var(PSI),
                          Standard_Deviation_PSI = sd(PSI))

# Deliverable 3                           
t.test((Suspension_Coil_file$PSI), mu=1500)

t.test((subset(Suspension_Coil_file,Manufacturing_Lot=="Lot1")$PSI), mu=1500)
t.test((subset(Suspension_Coil_file,Manufacturing_Lot=="Lot2")$PSI), mu=1500)
t.test((subset(Suspension_Coil_file,Manufacturing_Lot=="Lot3")$PSI), mu=1500)