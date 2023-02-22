install.packages("proton")
library(proton)
proton()
df <- data.frame(employees)
df[df$name=="John" & df$surname=="Insecure", "login"]
proton(action = "login", login="johnins")