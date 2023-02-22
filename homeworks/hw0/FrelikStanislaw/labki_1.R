install.packages("proton")

library("proton")
proton()


employees[employees$name == "John",]

proton(action = "login", login = 'johnins')
top <- top1000passwords
for (i in top1000passwords){
  proton(action = "login", login = 'johnins', password=i)
}

logs

logs[logs$login=="Pietraszko",]
