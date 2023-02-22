install.packages("proton")
library(proton)
proton()


employees[employees$surname == 'Insecure', ]
proton(action = "login", login="johnins")

top1000passwords
for (i in 1:1000) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}


employees[employees$surname == 'Pietraszko', ]
X <- logs[logs$login == 'slap', ]

