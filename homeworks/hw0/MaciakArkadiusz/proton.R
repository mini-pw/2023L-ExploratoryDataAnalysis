install.packages("proton")
library(proton)
proton()

#pierwszy
loginJ <- employees$login[employees$name == "John" & employees$surname == "Insecure"]
proton(action = "login", login=loginJ)

#drugi
#proton(action = "login", login=loginJ, password="top1000passwords")

for( pass in top1000passwords) {
  proton(action = "login", login=loginJ, password=pass)
  
}

#trzeci
proton(action = "server", host="XYZ")

login_P <- employees$login[employees$surname == "Pietraszko"]

host_P <- logs$host[logs$login==login_P]
#dalej trzeba zliczyc i wziac head( , 1) :D