install.packages("proton")
library(dplyr)
library(proton)
proton()

emp <- employees
login <- emp[emp$surname == "Insecure" & emp$name == "John","login"]
passwords <- top1000passwords
proton(action = "login", login = login)
for (i in 1:1000) {
  ans <- proton(action = "login", login = login, password = passwords[i])
  if (ans != "Password or login is incorrect") {
    password <- passwords[i]
    break
  }
}
logs <- logs
pietraszko_logs <- logs[logs$login == "slap",]
f = factor(pietraszko_logs$host)
group_by()
group_by(pietraszko_logs, host)
}
