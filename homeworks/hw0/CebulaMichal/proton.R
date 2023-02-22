# install.packages("proton")
# library(proton)
# proton()

# employees[employees$name == "John",]
# proton(action="login", login="johnins")
 
# for (pass in top1000passwords) {
#   response <- proton(action="login", login="johnins", password=pass)
#   if(response == "Success! User is logged in!") {
#     cat(pass)
#   }
# }


employees[employees$surname == "Pietraszko",]
table(logs[logs$login == "slap", c("host")])->tmp
data.frame(tmp)