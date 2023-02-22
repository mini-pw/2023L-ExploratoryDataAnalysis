install.packages("proton")
library(proton)
proton()

df <- data.frame(employees)
df[df$surname == "Insecure", "login"]
proton(action = "login", login="johnins")
wektor <- c(top1000passwords)
wektor
for (i in 1:length(wektor)){
  proton(action = "login", login="johnins", password=wektor[[i]])
  
}

logs2 <- data.frame(logs)

logs2[logs2$login == "johnins", "host"]

proton(action = "server", host="194.29.178.13")
proton(action = "server", host="194.29.178.81")
proton(action = "server", host="194.29.178.56")

