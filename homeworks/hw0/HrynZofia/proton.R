install.packages("proton")
library(proton)
proton()
employees
employees$login[employees$name=="John" & employees$surname=="Insecure"]
proton(action = "login", login="johnins")
top1000passwords
for (i in 1:length(top1000passwords)){
  proton(action = "login", login="johnins", password=top1000passwords[[i]])
}  
logs
