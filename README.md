# R-ChatRSA-viaSocket

Esse trabalho consistiu na criação de aplicações cliente/servidor que troquem mensagens utilizando o algoritmo de criptografia **<a href="https://pt.wikipedia.org/wiki/RSA_(sistema_criptogr%C3%A1fico)">RSA</a>** em **<a href="https://www.r-project.org/">R</a>** via **<a href="https://pt.wikipedia.org/wiki/Soquete_de_rede">Socket</a>**

## Bibliotecas necessárias

Para o funcionamento correto desse projeto é necessário instalar a biblioteca **gmp** e **stringi**, executando no console do **<a href="https://www.r-project.org/">R</a>** ou **<a href="https://www.rstudio.com/">RStudio</a>** os comandos abaixo:
```
* install.packages('gmp', repos='http://cran.us.r-project.org')
* install.packages('stringi', repos='http://cran.us.r-project.org')
```
## Funcionamento (Windows)

1. Com o **<a href="https://www.r-project.org/">R</a>** instalado em sua máquina, adicione a pasta **bin** do local onde o Software foi instalado nas **Variáveis de Ambiente** do **Sistema** para facilitar a execução via **Prompt de Comando**.
2. Abra duas janelas do **Prompt de Comando**
3. Nas duas janelas, mude o diretorio padrão para o local onde os arquivos desse projeto estão salvos:
    ```
    cd ..\..\..\R-ChatRSA-viaSocket\Chat 
    ```
4. Na janela 1, inicie o Servidor com o comando abaixo:
    ```
    Rscript.exe .\Servidor.r 
    ```
5. Na janela 2, inicie o Cliente com o comando abaixo:
    ```
    Rscript.exe .\Cliente.r 
    ```
6. Siga as instruções escritas na tela e **divirta-se**!!!
