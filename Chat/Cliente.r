require(primes)
require(FRACTION)

client <- function(){
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
    
    n_Cli = p*q

    fiN = (p-1)*(q-1)
    
    e_Cli = 0
    for (i in 2:fiN) {
        if (gcd(fiN, i) == 1) {
            e_Cli = i
            break
        }
    }

    rm(primNumber)
    rm(isPrime)
    rm(i)
    #--------------------------------------------------------------------

    print("Troca de chaves")
    #kpuc = c(n_Cli,e_Cli)
    kpuc = c(toString(n_Cli), toString(e_Cli))
    con <- socketConnection(host="localhost", port=666, blocking=TRUE, server=FALSE, open="r+")

    # enviar a chave publica do cliente
    write_resp = writeLines(kpuc, con, useBytes = TRUE)
    # receber a chave publica do servidor
    kpus = readLines(con,2)
    print(kpus)

    close(con)

    while(TRUE){
        con = socketConnection(host="localhost", port = 666, blocking=TRUE, server=FALSE, open="r+")

        # cliente captura mensagem da entrada padrao (teclado)
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
client()