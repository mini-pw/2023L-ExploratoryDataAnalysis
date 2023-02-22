install.packages("proton")
library(proton)
proton()
x <- data.frame(employees)
x[x$name == 'John'& x$surname == 'Insecure', 'login']
proton(action = "login", login="johnins")
