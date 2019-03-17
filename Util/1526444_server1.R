server <- function(){
    print("Geracao da Chave Publica")
    # Gerar as chaves privada e publica
    # kpus = (ns,es); kprs(ns,ds)

    print("Troca de chaves")

    con <- socketConnection(host="localhost", port=6666, blocking=TRUE, server=TRUE, open="r+")
    # receber a chave publica do cliente
    kpuc = readLines(con,1)
    print(kpuc)
    # enviar a chave publica do servidor
    write_resp = writeLines("Chave publica do servidor", con)
    close(con)

    print("Troca de msg entre cliente e servidor, use q para sair")

    while(TRUE){
        con <- socketConnection(host="localhost", port=6666, blocking=TRUE, server=TRUE, open="r+")
        
        # servidor recebe mensagem enviada pelo cliente
        data <- readLines(con, 1)
        
        # servidor decriptografa a mensagem e a mostra na tela
        # fazer aqui a decriptografia
        print(data)
        
        # servidor captura mensagem da entrada padrao (teclado)
        f <- file("stdin")
        open(f)
        writeLines("msg", sep=": ")
        msg <- readLines(f, n=1)
        if(tolower(msg)=="q"){
        break
        }
        
        # servidor criptografa a mensagem e a envia para o cliente
        # fazer aqui a criptografia
        write_resp <- writeLines(msg, con)

        close(con)
    }
}
server()