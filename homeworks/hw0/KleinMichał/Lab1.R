install.packages("proton")
library(proton)
proton()

employees[employees$name == "John",]

proton(action = "login", login="johnins")

top1000passwords

for (i in 1:1000) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

logs

employees[employees$surname == "Pietraszko",]

logs[logs$login == "slap",]

proton(action="server", host="194.29.178.16")

bash_history
