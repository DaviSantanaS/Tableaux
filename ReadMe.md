

# Sistema de Tableaux para Lógica Clássica Proposicional

## Objetivo

Implementar um sistema de Tableaux para a Lógica Clássica Proposicional utilizando a linguagem funcional Haskell.

## Descrição

Um tableau é uma árvore com raiz rotulada por uma fórmula na Lógica Clássica Proposicional cujos nós dessa árvore são subfórmulas da raiz e são rotulados como "verdadeiro" ou "falso". A partir da raiz, novas fórmulas são criadas de acordo com a aplicação de regras especificadas que geram uma ou duas novas folhas.

Trata-se de uma prova por refutação, então supõe-se que a fórmula de entrada é falsa. Caso haja contradições (i.e., uma mesma fórmula rotulada como verdadeira e como falsa em nós diferentes de um mesmo ramo) em todos os ramos da árvore, então a fórmula da raiz é uma tautologia (i.e., a partir de toda tentativa possível de torná-la falsa obteve-se alguma contradição). Caso todas as regras possíveis tenham sido aplicadas e haja um ou mais ramos sem contradições, então a fórmula da raiz é falsificável (um ramo é um caminho da folha até a raiz).

### Exemplo

Entrada: Uma fórmula da Lógica Clássica Proposicional.

Saída: Uma árvore de prova/refutação indicando se a fórmula é ou não válida.

## Como Rodar o Projeto

Para rodar o projeto, é necessário ter o GHC (Glasgow Haskell Compiler) instalado.

### Executando o Projeto

```bash
cd src
./tableau
```

### Compilando o Projeto

Caso queira compilar o projeto, siga os passos abaixo:

```bash
cd src
ghc -o tableau Main.hs
./tableau
```

## Estrutura do Projeto

```
.
├── src
│   ├── Main.hs
│   └── [outros arquivos de código fonte]
├── Examples
│   ├── ex1.formula
│   └── [outros arquivos de exemplo]
└── README.md
```

## Dependências

Certifique-se de ter o GHC instalado. Você pode instalá-lo através do [Haskell Platform](https://www.haskell.org/) ou utilizando um gerenciador de pacotes de sua preferência.

## Contribuições

Contribuições são bem-vindas! Sinta-se à vontade para abrir issues ou pull requests.


## Contato

Para mais informações, entre em contato:

- Nome: Davi Santos Santana
- Email: [davisantana@id.uff.br](mailto:davisantana@id.uff.br)



