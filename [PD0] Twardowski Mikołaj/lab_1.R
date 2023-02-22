install.packages("proton")
library(proton)
proton()
head(employees)
employees[employees$surname == "Pietraszko",]
proton(action = "login", login = "johnins")
head(top1000passwords,30)
for (i in top1000passwords){
  proton(action = "login", login = "johnins", password =i)
}
head(logs)
table(logs[login == "slap"]$host)
logs[login == "slap"]
