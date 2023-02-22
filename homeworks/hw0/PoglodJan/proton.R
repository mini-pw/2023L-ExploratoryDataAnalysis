install.packages("proton")
library(proton)
proton()

head(employees)

employees[employees$name == "John", "login"]

proton(action = "login", login="johnins")

top1000passwords[2]

proton(action = "login", login="XYZ", password="johnny")


for (i in 1:1000){
  pass = top1000passwords[i]
  proton(action = "login", login="johnins", password=pass)
  print(i)
}

top1000passwords[120]

head(logs)
proton(action = "login", login="johnins", password="q1w2e3r4t5")
head(logs)

logs[logs$login == "johnins", "host"]

proton(action = "server", host="194.29.178.13")

table(logs[logs$login == "johnins", "host"])
