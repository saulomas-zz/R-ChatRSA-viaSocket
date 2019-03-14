require(svSocket)
require(primes)
require(FRACTION)

client <- function(){
    #Geração das Chaves Públicas e Privadas
    isPrime = FALSE
    while(isPrime == FALSE) {
        primNumber = sample(1:99,1)
        if (primes::is_prime(primNumber)) {
            isPrime = TRUE
        }
    }
    
    p = primNumber

    isPrime = FALSE
    while(isPrime == FALSE) {
        primNumber = sample(1:99,1)
        if (primNumber != p && primes::is_prime(primNumber)) {
            isPrime = TRUE
        }
    }

    q = primNumber
    cli_N = p*q;

    fiN = (p-1)*(q-1)
    
    for (i in 2:fiN) {
        if (gcd(fiN, i) == 1) {
            cli_E = i
            break
        }
    }
    
    while(TRUE){
        con <- socketConnection(host="localhost", port = 6011, blocking=TRUE, server=FALSE, open="r+")
        f <- file("stdin")
        open(f)
        print("Enter text to be upper-cased, q to quit")
        sendme <- readLines(f, n=1)
        if(tolower(sendme)=="q"){
        break
        }
        write_resp <- writeLines(sendme, con)
        server_resp <- readLines(con, 1)
        print(paste("Your upper cased text:  ", server_resp))
        close(con)
    }
}
client()