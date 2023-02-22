install.packages("proton")
library(proton)
proton()
df <- employees
df[df$surname == "Insecure", ]
proton(action = "login", login="johnins")
hasla <- top1000passwords

for (i in 1:length(hasla)){
  proton(action = "login", login="johnins", password=hasla[[i]])
}

msc <- logs
df <- employees
df[df$surname == "Pietraszko", ]
pietraszko <- msc[msc$login == "slap", ]
sapply(pietraszko, )