install.packages("proton")
library(proton)
proton()
employees

employees[employees$name == "John",]
proton(action = "login", login="johnins")
top1000passwords
for (i in 1:1000){
  proton(action = "login", login = "johnins", password= top1000passwords[i])
         
}
head(logs)
employees[employees$login = "johnins",]


