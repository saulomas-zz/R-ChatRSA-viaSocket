require(svSocket)

newchat <- function() {
    while(TRUE) {
        con <- socketConnection(host="localhost", port = 6011, blocking=FALSE, server=FALSE, open="r+")

        close(con)
    }
}

newchat()