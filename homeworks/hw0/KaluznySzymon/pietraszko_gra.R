install.packages("proton")
library(proton)
proton()
employees
head(employees[employees$name == 'John',])
proton(action = "login", login="johnins")
for(i in 1:1000){
  proton(action = "login", login = "johnins", password = top1000passwords[i])
  
}
logs
for(i in )