      *****************************************************************
      *                                                               *
      *       SISTEMA DE FOLHA DE PAGAMENTO - COBOL                  *
      *                                                               *
      *       PROJETO : FOLHAPAG                                     *
      *       VERSAO  : 1.0                                          *
      *                                                               *
      *       AUTOR   : PAULO HENRIQUE SANTANA MOTTA                 *
      *                                                               *
      *       OBJETIVO: Simular um sistema corporativo de             *
      *       processamento de folha salarial utilizando COBOL.       *
      *                                                               *
      *       TECNOLOGIAS: COBOL / MAINFRAME CONCEPTS                 *
      *                                                               *
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOLHAPAG.

      *****************************************************************
       ENVIRONMENT DIVISION.

      *****************************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

      *===============================================================*
      * CONTROLE DO SISTEMA                                           *
      *===============================================================*

       01 WS-CONTROLE.

          05 WS-OPCAO              PIC 9 VALUE 0.

          05 WS-ENCERRA            PIC X VALUE 'N'.


      *===============================================================*
      * DADOS DO FUNCIONARIO                                          *
      *===============================================================*

       01 WS-FUNCIONARIO.

          05 WS-DADOS-PESSOAIS.

             10 WS-NOME.

                15 WS-PRIMEIRO-NOME PIC X(20).

                15 WS-SOBRENOME     PIC X(30).


             10 WS-CPF              PIC 9(11).


             10 WS-DATA-NASC.

                15 WS-DIA           PIC 99.

                15 WS-MES           PIC 99.

                15 WS-ANO           PIC 9(04).


      *===============================================================*
      * DADOS FINANCEIROS                                             *
      *===============================================================*

          05 WS-DADOS-SALARIAIS.

             10 WS-SALARIO-BRUTO

                PIC 9(08)V99 VALUE 0.


             10 WS-DESCONTO-INSS

                PIC 9(06)V99 VALUE 0.


             10 WS-SALARIO-LIQUIDO

                PIC 9(08)V99 VALUE 0.



      *****************************************************************
       PROCEDURE DIVISION.


      *===============================================================*
      * ROTINA PRINCIPAL                                              *
      *===============================================================*

       0000-PRINCIPAL.

           PERFORM UNTIL WS-ENCERRA = 'S'


              DISPLAY '================================='
              DISPLAY '   SISTEMA FOLHA PAGAMENTO COBOL'
              DISPLAY '================================='
              DISPLAY '1 - CADASTRAR FUNCIONARIO'
              DISPLAY '2 - CALCULAR SALARIO'
              DISPLAY '3 - CONSULTAR DADOS'
              DISPLAY '0 - ENCERRAR'
              DISPLAY '================================='
              DISPLAY 'OPCAO: '

              ACCEPT WS-OPCAO



              EVALUATE WS-OPCAO


                 WHEN 1

                    DISPLAY 'PRIMEIRO NOME: '
                    ACCEPT WS-PRIMEIRO-NOME


                    DISPLAY 'SOBRENOME: '
                    ACCEPT WS-SOBRENOME


                    DISPLAY 'CPF: '
                    ACCEPT WS-CPF


                    DISPLAY 'DIA NASCIMENTO: '
                    ACCEPT WS-DIA


                    DISPLAY 'MES NASCIMENTO: '
                    ACCEPT WS-MES


                    DISPLAY 'ANO NASCIMENTO: '
                    ACCEPT WS-ANO


                    DISPLAY 'SALARIO BRUTO: '
                    ACCEPT WS-SALARIO-BRUTO


                    PERFORM 1000-CALCULA-SALARIO


                    DISPLAY 'FUNCIONARIO CADASTRADO!'


                 WHEN 2

                    PERFORM 1000-CALCULA-SALARIO


                    DISPLAY 'CALCULO REALIZADO!'


                 WHEN 3

                    DISPLAY '================================='
                    DISPLAY ' DADOS DO FUNCIONARIO'
                    DISPLAY '================================='


                    DISPLAY 'NOME: '
                    DISPLAY WS-PRIMEIRO-NOME
                    DISPLAY WS-SOBRENOME


                    DISPLAY 'CPF: '
                    DISPLAY WS-CPF


                    DISPLAY 'DATA NASCIMENTO: '
                    DISPLAY WS-DIA '/'
                            WS-MES '/'
                            WS-ANO


                    DISPLAY 'SALARIO BRUTO: '
                    DISPLAY WS-SALARIO-BRUTO


                    DISPLAY 'DESCONTO INSS: '
                    DISPLAY WS-DESCONTO-INSS


                    DISPLAY 'SALARIO LIQUIDO: '
                    DISPLAY WS-SALARIO-LIQUIDO



                 WHEN 0

                    MOVE 'S' TO WS-ENCERRA


                 WHEN OTHER

                    DISPLAY 'OPCAO INVALIDA'


              END-EVALUATE


           END-PERFORM.


           STOP RUN.



      *===============================================================*
      * CALCULO DA FOLHA                                              *
      *===============================================================*

       1000-CALCULA-SALARIO.


           COMPUTE WS-DESCONTO-INSS =
                   WS-SALARIO-BRUTO * 0.10


           COMPUTE WS-SALARIO-LIQUIDO =
                   WS-SALARIO-BRUTO -
                   WS-DESCONTO-INSS.


       EXIT.
