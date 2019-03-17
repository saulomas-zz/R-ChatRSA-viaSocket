require(primes)
require(FRACTION)
require(stringi)

client <- function(){
    #--------------------------------------------------------------------
    #Geração das Chaves Públicas e Privadas
    maxPrimeNumber = 99
    print("Geracao da Chave Publica")
    isPrime = FALSE
    while(isPrime == FALSE) {
        primNumber = sample(1:maxPrimeNumber,1)
        if (primes::is_prime(primNumber)) {
            isPrime = TRUE
        }
    }
    
    cli_p = primNumber

    isPrime = FALSE
    while(isPrime == FALSE) {
        primNumber = sample(1:maxPrimeNumber,1)
        if (primNumber != cli_p && primes::is_prime(primNumber)) {
            isPrime = TRUE
        }
    }

    cli_q = primNumber
    
    cli_n = cli_p*cli_q

    cli_n_fi = (cli_p-1)*(cli_q-1)
    
    cli_e = 0
    for (i in 2:cli_n_fi) {
        if (gcd(cli_n_fi, i) == 1) {
            cli_e = i
            break
        }
    }

    cli_d = 1
    repeat {
        if (cli_d < cli_n_fi && (cli_d*cli_e) %% cli_n_fi == 1) {
            break
        }
        cli_d = cli_d + 1
    }
    #--------------------------------------------------------------------
    print("Troca de chaves")
    #kpuc = c(cli_n,cli_e)
    kpuc = paste(c(cli_n, cli_e), collapse = ",")
    con <- socketConnection(host="localhost", port=666, blocking=TRUE, server=FALSE, open="r+")

    # enviar a chave publica do cliente
    write_resp = writeLines(kpuc, con)

    # receber a chave publica do servidor
    kpus = readLines(con,1)
    print(kpus)    

    serv_n = as.integer(unlist(strsplit(kpus, split=",")))[1]
    serv_e = as.integer(unlist(strsplit(kpus, split=",")))[2]

    close(con)
    #--------------------------------------------------------------------
    rm(maxPrimeNumber)
    rm(primNumber)
    rm(isPrime)
    rm(i)
    rm(kpuc)
    rm(kpus)
    #--------------------------------------------------------------------

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
        
        # cliente criptografa a mensagem e a envia para o servidor
        # fazer aqui a criptografia
        msgUTF8 = utf8ToInt(stringi::stri_enc_toascii(msg))
        msgCrypt = (msgUTF8 ^ serv_e) %% serv_n
        write_resp = writeLines(paste(as.character(msgCrypt), collapse = ","), con)

        # cliente recebe mensagem enviada pelo servidor
        data = readLines(con, 1)
        
        # cliente decriptografa a mensagem e a mostra na tela
        # fazer aqui a decriptografia
        msgDecrypt = (msgCrypt ^ cli_d) %% cli_n
        msg = intToUtf8(msgDecrypt)
        print(msg)

        close(con)    
    }
}
client()