client <- function(){
    print("Geracao da Chave Publica")
    # Gerar as chaves privada e publica
    # kpuc = (nc, ec); kprc(nc,dc)

    print("Troca de chaves")

    con <- socketConnection(host="localhost", port=6666, blocking=TRUE, server=FALSE, open="r+")
    # enviar a chave publica do cliente
    write_resp = writeLines("Chave publica do cliente", con)
    # receber a chave publica do servidor
    kpus = readLines(con,1)
    print(kpus)
    close(con)

    print("Troca de msg entre cliente e servidor, use q para sair")

    while(TRUE){
        con <- socketConnection(host="localhost", port=6666, blocking=TRUE, server=FALSE, open="r+")
        
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
        write_resp <- writeLines(msg, con)

        # cliente recebe mensagem enviada pelo servidor
        data <- readLines(con, 1)
        
        # cliente decriptografa a mensagem e a mostra na tela
        # fazer aqui a decriptografia
        print(data)

        close(con)
    }

}
client()