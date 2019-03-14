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
    
    cli_N = p*q

    fiN = (p-1)*(q-1)
    
    cli_E = 0
    for (i in 2:fiN) {
        if (gcd(fiN, i) == 1) {
            cli_E = i
            break
        }
    }

    rm(primNumber)
    rm(isPrime)
    rm(i)

    while(TRUE){
        con = socketConnection(host="localhost", port = 6011, blocking=TRUE, server=FALSE, open="r+")

        cli_N_resp = writeLines(cli_N, con)
        cli_E_respo = writeLines(cli_E, con)

        close(con)    
    }
}
client()