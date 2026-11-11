# Projeto da 1ª VA - Interpretador em Haskell

Este repositório contém a implementação estendida de um interpretador em Haskell, referente à avaliação da 1ª VA da disciplina de Paradigmas de Programação (2026.2). O projeto foi realizado utilizando **TDD (Test-Driven Development)**, dividindo as implementações entre os membros da equipe.

## 1. Justificativas para Mudanças nos Tipos das Funções Existentes
Para acomodar os novos requisitos estipulados no projeto, realizamos pequenas modificações nos tipos das funções originais:
* **`evalBinOp`**: Embora a assinatura `BinOp -> Value -> Value -> Value` tenha se mantido, expandimos o tipo abstrato `Value` com `VFloat` para suportar as operações da Pessoa 1, promovendo o tipo dos retornos para tratar a coerção de Float e Int.
* **`evalExpr` e Escopo**: O ambiente global/local exigiu que `evalExpr` gerenciasse um estado sequencial na chamada de funções (Item 2), onde a lista de parâmetros passou a ser mapeada acumulando as atualizações (da esquerda para a direita).
* **Estruturas AST (`Expr`, `Stmt`, `Type`, `Value`)**: Foram estendidas para englobar os novos nós sintáticos de Arrays (`TArray`, `ArrInit`, `ArrGet`, `ArrSet`) e o laço de repetição condicional (`For`).

## 2. Comandos para Executar o Projeto
Recomenda-se rodar o interpretador utilizando o GHCi. Certifique-se de que possui as bibliotecas necessárias instaladas.
Para carregar o projeto em seu terminal, execute:

```bash
ghci --package silently --package containers --package HUnit interpretador-25-04-26.hs
