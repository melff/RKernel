con2 <- socketConnection(port = 26011,server=TRUE)
res <- readLines(con2)
print(res)
while(isIncomplete(con2)) {
   Sys.sleep(.1)
   z <- readLines(con2)
   if(length(z)) print(z)
}

close(con2)
message("Done!")
