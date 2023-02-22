install.packages("proton")
library(proton)
proton()

employees[employees$name  == 'John',]
proton(action="login","login"="johnins")

for (pass in top1000passwords){
 
  proton(action="login",login="johnins",password = pass)
}

logs[logs$login  == 'pietraszko',]


for (host in logs$host){
  proton(action = "server", host="194.29.178.81")
}

