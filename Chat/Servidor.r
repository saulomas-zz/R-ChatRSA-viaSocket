require (primes)
require (FRACTION)

server <- function() {
    #Geração das Chaves Públicas e Privadas
    print("Geracao da Chave Publica")
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
    
    n_Serv = p*q

    fiN = (p-1)*(q-1)
    
    e_Serv = 0
    for (i in 2:fiN) {
        if (gcd(fiN, i) == 1) {
            e_Serv = i
            break
        }
    }

    rm(primNumber)
    rm(isPrime)
    rm(i)
    #--------------------------------------------------------------------

    print("Troca de chaves")
    #kpus = c(n_Serv,e_Serv)
    kpus = c(toString(n_Serv), toString(e_Serv))
    con <- socketConnection(host="localhost", port=666, blocking=TRUE, server=TRUE, open="r+")

    # receber a chave publica do cliente
    kpuc = readLines(con,2)
    print(kpuc)
    # enviar a chave publica do servidor
    write_resp = writeLines(kpus, con, useBytes = TRUE)

    close(con)

    while(TRUE){
        writeLines("Listening...")
        con <- socketConnection(host="localhost", port = 666, blocking=TRUE, server=TRUE, open="r+")

        f <- file("stdin")
        open(f)
        writeLines("msg", sep=": ")
        msg <- readLines(f, n=1)
        if(tolower(msg)=="q"){
            break
        }

        close(con)
    }
}
server()