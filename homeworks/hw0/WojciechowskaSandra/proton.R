install.packages("proton")
library(proton)
proton()
employees
employees$login[employees$name=="John" & employees$surname=="Insecure"]
proton(action = "login", login="johnins")
top1000passwords
for (i in 1:1000) {
  proton(action = "login", login="johnins", password=top1000passwords[[i]])
}
logs
logs[logs$login=="Pietraszko",]
