
all_con <- dbListConnections(MySQL())
for(con in all_cons)
  +  dbDisconnect(con)
dbListConnections(MySQL())
list()