install.packages("proton")
library(proton)
proton()

#1
head(employees)
employees[employees$name == 'John', ]
proton(action = "login", login = "johnins")

#2
for (pass in top1000passwords) {
  response <- proton(action = "login", login="johnins", password=pass)
  if (response == 'Success! User is logged in!') {
    cat(pass)
  }
}
proton(action = 'server', host = 'q1w2e3r4t5')
