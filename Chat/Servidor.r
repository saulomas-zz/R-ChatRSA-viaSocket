require(gmp)
require(FRACTION)
require(stringi)

server <- function() {
    #--------------------------------------------------------------------
    #Geração das Chaves Públicas e Privadas
    maxPrimeNumber = 999
    print("Geracao da Chave Publica")
    isPrime = FALSE
    while(isPrime == FALSE) {
        primNumber = sample(128:maxPrimeNumber,1)
        if (isprime(primNumber) == 2) {
            isPrime = TRUE
        }
    }
    
    serv_p = primNumber

    isPrime = FALSE
    while(isPrime == FALSE) {
        primNumber = sample(128:maxPrimeNumber,1)
        if (primNumber != serv_p && isprime(primNumber) == 2) {
            isPrime = TRUE
        }
    }

    serv_q = primNumber

    serv_n = serv_p*serv_q

    serv_n_fi = (serv_p-1)*(serv_q-1)
    
    serv_e = 0
    for (i in 2:serv_n_fi) {
        if (gcd(serv_n_fi, i) == 1) {
            serv_e = i
            break
        }
    }

    serv_d = 1
    repeat {
        if (serv_d < serv_n_fi && (serv_d*serv_e) %% serv_n_fi == 1) {
            break
        }
        serv_d = serv_d + 1
    }
    #--------------------------------------------------------------------
    print("Troca de chaves")
    #kpus = c(serv_n,serv_e)
    kpus = paste(c(serv_n, serv_e), collapse = ",")
    con <- socketConnection(host="localhost", port=666, blocking=TRUE, server=TRUE, open="r+")

    # receber a chave publica do cliente
    kpuc = readLines(con,1)
    print(kpuc)

    # enviar a chave publica do servidor
    write_resp = writeLines(kpus, con)

    cli_n = as.integer(unlist(strsplit(kpuc, split=",")))[1]
    cli_e = as.integer(unlist(strsplit(kpuc, split=",")))[2]

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
        writeLines("Listening...")
        con = socketConnection(host="localhost", port = 666, blocking=TRUE, server=TRUE, open="r+")

        # servidor recebe mensagem enviada pelo cliente
        msgCrypt = readLines(con, 1)
        msgCrypt = as.bigz(unlist(strsplit(msgCrypt, split=",")))

        # servidor decriptografa a mensagem e a mostra na tela
        # fazer aqui a decriptografia
        resPotencia = msgCrypt ^ as.bigz(serv_d)
        msgDecrypt = resPotencia %% serv_n
        
        msgDecrypt = as.integer(msgDecrypt)
        msg = intToUtf8(msgDecrypt)
        print(msg)
        
        # servidor captura mensagem da entrada padrao (teclado)
        f = file("stdin")
        open(f)
        writeLines("msg", sep=": ")
        msg <- readLines(f, n=1)
        if(tolower(msg)=="q"){
            break
        }
        
        # servidor criptografa a mensagem e a envia para o cliente
        # fazer aqui a criptografia
        msgUTF8 = utf8ToInt(msg)
        msgCrypt = (msgUTF8 ^ cli_e) %% cli_n
        write_resp <- writeLines(paste(as.character(msgCrypt), collapse = ","), con)

        close(con)
    }
}
server()