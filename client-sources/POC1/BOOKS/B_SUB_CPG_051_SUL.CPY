      *****************************************************************
      *                                                               *
      *    LIBRARY DO ARQUIVO QUE CONTEM QTDE/VALOR POR OP            *
      *                                                               *
      *    TAM. REGISTRO:120    FATOR DE BLOCO: 150  BLOCKSIZE: 36000 *
      *                                                               *
      *****************************************************************
      *
       01  DSUBCPG051-REG.
           03 CP51-N-ID-ORGAO-PAGADOR         PIC 9(06).
           03 CP51-N-CS-MEIO-PAGTO            PIC 9(02).
           03 CP51-N-CS-ORIGEM-CREDITO        PIC 9(01).
           03 CP51-N-DT-INI-VALIDADE          PIC 9(08).
           03 CP51-N-DT-PROVISAO              PIC 9(08).
           03 FILLER                          PIC X(01).
           03 CP51-N-CS-AGENCIA-PIONEIRA      PIC 9(01).
           03 CP51-N-ID-BANCO                 PIC 9(03).
           03 CP51-N-ID-MICRO-REG-PREV        PIC 9(06).
           03 CP51-N-CS-MUNIC-SINPAS          PIC 9(05).
           03 CP51-N-CS-MUNIC-SINPAS-R REDEFINES CP51-N-CS-MUNIC-SINPAS.
              05 CP51-N-CS-UF                 PIC 9(02).
              05 FILLER                       PIC 9(03).
           03 CP51-N-ID-PREGAO                PIC 9(02).
           03 CP51-N-ID-LOTE                  PIC 9(02).
           03 CP51-N-CS-ORIG-ORC              PIC 9(01).
           03 CP51-N-CS-REDE-ECT              PIC 9(01).
           03 CP51-N-CS-URBANO-RURAL          PIC 9(01).
           03 CP51-N-GR-ESPECIES              PIC 9(02).
           03 CP51-N-ID-SAL-FAMILIA           PIC 9(01).
           03 CP51-N-ID-PAIS                  PIC 9(03).
           03 CP51-N-QTDE-CREDITOS            PIC 9(08).
           03 CP51-N-VALOR-LIQ-CREDITOS       PIC 9(15)V99.
           03 CP51-N-DT-PROC-CREDITO          PIC 9(08).
           03 CP51-N-IN-CREDITO-SEAS          PIC 9(01).
           03 FILLER                          PIC 9(02).
           03 CP51-N-QTDE-IR                  PIC 9(08).
           03 CP51-N-VL-IR                    PIC 9(15)V99.
           03 FILLER                          PIC X(05).
      *
      *    DEFINICAO DA DT-PROVISAO:
      *
      *    CONCESSAO
      *    ---------
      *        OBTER A MENOR DT-INI-VALIDADE DO PROCESSAMENTO DAQUELE
      *                      BANCO = DT-X
      *        SE ECV
      *        ENTAO DESPREZAR DA PROVISAO
      *        SENAO
      *          SE ECT
      *          ENTAO DT-PROVISAO  = DT-X - 4 DIAS UTEIS
      *          SENAO
      *             SE AGENCIA-PIONEIRA
      *             ENTAO DT-PROVISAO = DT-X - 3 DIAS UTEIS
      *             SENAO
      *                CARTAO => DT-PROVISAO = DT-INI-VALID - 1 DIA UTIL
      *                C/C    => DT-PROVISAO = DT-INI-VALID
      *
      *    MACICA E PAB
      *    ------------
      *        SE ECV
      *        ENTAO DESPREZAR DA PROVISAO
      *        SENAO
      *          SE PAB (MEIO-PAGTO)
      *          ENTAO
      *             DT-PROVISAO = DT-INI-VALIDADE
      *          SENAO
      *             SE ECT
      *             ENTAO DT-PROVISAO = 4O.DIA UTIL ANTES DO 1O.DIA UTIL
      *                                 DO MES DE PAGAMENTO
      *             SENAO
      *                 SE AGENCIA-PIONEIRA
      *                 ENTAO DT-PROVISAO = 3O.DIA UTIL ANTES DO 1O.DIA
      *                                     UTIL DO MES DE PAGAMENTO
      *                 SENAO
      *                    CARTAO => DT-PROVISAO = DT-INI-VALIDADE -
      *                                            1 DIA UTIL
      *                    C/C    => DT-PROVISAO = DT-INI-VALIDADE
