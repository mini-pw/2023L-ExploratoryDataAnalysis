install.packages("proton")
library(proton)
proton()

employees[employees$name == "John" & employees$surname == "Insecure",]
proton(action = "login", login="johnins")


for (i in 1:length(top1000passwords)) {
  print(top1000passwords[i])
  proton(action = "login", login="johnins", password=top1000passwords[i])
}