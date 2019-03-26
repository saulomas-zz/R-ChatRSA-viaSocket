#Pacotes usados e comandos para instalação dos mesmos:
#install.packages('gmp', repos='http://cran.us.r-project.org')
#install.packages('stringi', repos='http://cran.us.r-project.org')

require(gmp)
require(stringi)

minPrimeNumber = 12  #O limite minimo de 12 é para "n" poder considerar pelo menos os caracteres alfabeticos que vão ate 122 em ASCII
maxPrimeNumber = 999 #A partir do 4º digito o processamento começa ficar mais lento para textos maiores

getChaveP = function() {
    isPrime = FALSE
    while(isPrime == FALSE) {
        primNumber = sample(minPrimeNumber:maxPrimeNumber,1) 

        isPrime = isprime(primNumber) == 2
    }
    
    return(primNumber)
}

getChaveQ = function(p) {
    isPrime = FALSE
    while(isPrime == FALSE) {
        primNumber = sample(minPrimeNumber:maxPrimeNumber,1)

        isPrime = primNumber != p && isprime(primNumber) == 2
    }

    return(primNumber)
}

getChaveN = function(p, q) {
    return(p * q)
}

getChaveNFi = function(p, q) {
    return((p - 1) * (q - 1))
}

getChaveE = function(nFi) {
    serv_e = 0
    for (i in 2:nFi) {
        if (gcd(nFi, i) == 1) {
            serv_e = i
            break
        }
    }

    return(serv_e)
}

getChaveD = function(nFi, e) {
    serv_d = 1
    repeat {
        if (serv_d < nFi && (serv_d * e) %% nFi == 1) {
            break
        }
        serv_d = serv_d + 1
    }
    return(serv_d)
}

toMsgCrypt = function(msg, e, n) {
    #stri_enc_toascii => converte o texto em ASCII (que consistem em bytes não superiores a 127)
    #utf8ToInt => converte o texto ASCII acima para um vetor de inteiros do Unicode
    msgUTF8 = utf8ToInt(stringi::stri_enc_toascii(msg))

    #Onde realmente é feita a CRIPTOGRAFIA da mensagem
    resPotencia = msgUTF8 ^ as.bigz(e)
    msgCrypt = resPotencia %% n
    
    #paste => transforma o array de inteiro, em um array unico de caracteres separados por vírgula, para facilitar na passagem de informação no Socket e o retorna
    return(paste(msgCrypt, collapse = ","))
}

toMsgDecrypt = function(msgCrypt, d, n) {
    #strsplit, unlist => transforma a informação separada por virgula recebida em um array
    #as.bigz => converte os valores em bigz
    msgCrypt = as.bigz(unlist(strsplit(msgCrypt, split=",")))

    #Onde realmente é feita a DESCRIPTOGRAFIA da mensagem
    resPotencia = msgCrypt ^ as.bigz(d)
    msgDecrypt = resPotencia %% n

    #as.integer => transforma o vetor do tipo bigz para o tipo inteiro que posteriormente será transformado em Unicode
    msgDecrypt = as.integer(msgDecrypt)

    #intToUtf8 => converte o vetor de inteiro para um texto Unicode e o retorna
    return(intToUtf8(msgDecrypt))
}