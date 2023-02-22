install.packages("proton")
library(proton)
proton()
employees[employees$name == "John", ]
proton(action = "login", login="johnins")
for (i in 1:length(top1000passwords)){
  proton(action = "login", login="johnins", password=top1000passwords[[i]])
}
employees[employees$surname == "Pietraszko", ]
tmp<-table(logs[logs$login == "slap", c("host")])
data.frame(tmp)