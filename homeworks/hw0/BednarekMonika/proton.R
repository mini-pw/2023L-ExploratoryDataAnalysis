install.packages("proton")
library(proton)
proton()
employees
#1
employees[employees$name == 'John',]
proton(action = "login", login = "johnins")

top1000passwords

for (i in 1 : 1000) {
  
  proton(action = "login", login="johnins", password=top1000passwords[i])
  
}
employees[employees$surname == "Pietraszko",]