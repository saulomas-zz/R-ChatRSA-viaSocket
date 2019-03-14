require(svSocket)
require(primes)
require(FRACTION)

server <- function(){
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
    
    serv_N = p*q

    fiN = (p-1)*(q-1)
    
    for (i in 2:fiN) {
        if (gcd(fiN, i) == 1) {
            serv_E = i
            break
        }
    }

    rm(primNumber)
    rm(isPrime)
    rm(i)

    while(TRUE){
        writeLines("Listening...")
        con <- socketConnection(host="localhost", port = 6011, blocking=TRUE, server=TRUE, open="r+")

        cli_n <- readLines(con, 1)
        cli_e <- readLines(con, 1)
        print(cli_n)
        print(cli_e)

        close(con)
    }
}
server()