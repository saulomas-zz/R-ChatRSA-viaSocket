require(svSocket)
startSocketServer()

room <- function(){
    while(TRUE) {
        writeLines("Listening...")
        con <- socketConnection(host="localhost", port = 6011, blocking=FALSE, server=TRUE, open="r+")

        writeLines("Teste...")
        sClientsName <- getSocketClientsNames
        
        writeLines(as.character(length(sClientsName)))
    }
}
room()

stopSocketServer(port = 6011)


