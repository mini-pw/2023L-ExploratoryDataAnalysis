install.packages("proton")
library(proton)
proton()

head(employees)
employees[employees$name == 'John', ]
proton(action = "login", login="johnins")
top1000passwords
length(top1000passwords)

for (pass in top1000passwords){
  x <- proton(action = "login", login="johnins", password= pass)
  if (x == 'Success! User is logged in!'){
    cat(pass)
  }
}

#3

employees[employees$surname == 'Pietraszko',]

table(logs[logs$login == 'slap', c("host")]) -> tmp
data.frame(tmp)

proton(action = "server", host = '194.29.178.16')

#4

head(bash_history) #lista


