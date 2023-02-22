install.packages("proton")
library(proton)
proton()

head(employees)
head(employees[employees$name == 'John', ])

for (i in 1:1000) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

head(logs)
