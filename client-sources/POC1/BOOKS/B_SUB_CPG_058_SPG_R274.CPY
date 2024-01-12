      *****************************************************************
      *    LIBRARY LAYOUT DO ARQUIVO D/SUB/CPG/058/SPG PARA SUBSTITUIR*
      *    O SCF COM  O NOVO SISPAGBEN.                               *
      *    TAMANHO DO REGISTRO 12360 CARACTERES - BLOCO 15 REGISTROS   *
      *OBS: EM TER 07/05/2013 INCLUIDO SPGENT-VALOR-RUB301 NO DETALHE *
      *****************************************************************

      *HEDAER SPGENT-ID-REG-H = 1
       01  DSUBCPG058-REG-SPGENT-HEADER.
           03  SPGENT-ID-REG-H           PIC 9(01).
           03  SPGENT-ORIG-CRED-H        PIC 9(01).
           03  SPGENT-DT-PROC-H          PIC 9(08).
           03  SPGENT-ID-CALAMIDADE-H    PIC 9(03).
           03  SPGENT-NUM-PORTARIA-H     PIC X(15).
           03  SPGENT-DT-PORTARIA-H      PIC 9(08).
           03  SPGENT-DT-INI-VALIDADE-H  PIC 9(08).
           03  SPGENT-DT-FIM-VALIDADE-H  PIC 9(08).
           03  SPGENT-NU-SEQ-ARQ-H       PIC 9(01).
           03  SPGENT-ID-ULT-ARQ-H       PIC 9(01).
           03  SPGENT-LISTA-RUBR-H       PIC X(285).
           03  SPGENT-LISTA-RUBR-H-R REDEFINES
               SPGENT-LISTA-RUBR-H.
               05 SPGENT-LISTA-RUBRICA-H OCCURS 95 TIMES.
                  07 SPGENT-CS-RUBRICA-H PIC 9(03).
           03  FILLER                   PIC X(1461).

      *DETALHE SPGENT-ID-REG = 2
       01  DSUBCPG058-REG-SPGENT.
           03  SPGENT-ID-REG             PIC 9(01).
           03  SPGENT-ID-BANCO           PIC 9(03).
           03  SPGENT-ID-PAIS            PIC 9(03).
           03  SPGENT-IN-CRED-PIONEIRA   PIC 9(01).
           03  SPGENT-CS-MEIO-PAGTO      PIC 9(02).
           03  SPGENT-DT-INI-VALIDADE    PIC 9(08).
           03  SPGENT-CS-ORIG-ORC        PIC 9(01).
           03  SPGENT-CS-URBANO-RURAL    PIC 9(01).
           03  SPGENT-IN-CRED-EPU        PIC 9(01).
           03  SPGENT-COD-CALAMIDADE     PIC 9(03).
           03  SPGENT-IN-ADIANTE-13-SAL  PIC 9(01).
           03  SPGENT-CS-ESPECIE         PIC 9(02).
           03  SPGENT-ENT-CLASS          PIC 9(04).
           03  SPGENT-QTD-ENT-CLASS      PIC 9(08).
           03  SPGENT-QUANT-CREDITOS     PIC 9(10).
           03  SPGENT-VALOR-BRUTO        PIC 9(15)V99.
           03  SPGENT-VALOR-LIQUIDO      PIC 9(15)V99.
      *Ate aqui, 71.79.80.83
           03  SPGENT-VALOR-GRUPO-RUBRICAS.
               05  SPGENT-VALOR-RUB104   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB105   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB107   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB108   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB112   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB136   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB154   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB160   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB161   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB162   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB201   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB202   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB203   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB204   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB205   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB206   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB207   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB208   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB209   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB210   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB214   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB215   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB216   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB217   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB218   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB219   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB220   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB221   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB222   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB223   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB224   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB225   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB226   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB227   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB228   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB229   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB230   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB231   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB232   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB233   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB234   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB236   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB237   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB238   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB239   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB240   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB241   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB242   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB243   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB244   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB245   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB246   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB247   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB248   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB249   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB250   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB251   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB252   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB253   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB254   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB255   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB256   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB257   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB258   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB259   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB260   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB261   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB262   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB263   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB264   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB265   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB266   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB267   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB268   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB269   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB270   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB271   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB272   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB273   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB274   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB301   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB905   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB909   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB910   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB911   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB912   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB916   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB917   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB921   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB925   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB926   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB927   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB928   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB929   PIC 9(12)V99.
               05  SPGENT-VALOR-RUB933   PIC 9(12)V99.
           03  SPGENT-VL REDEFINES SPGENT-VALOR-GRUPO-RUBRICAS.
               05  SPGENT-VL-RUBRICA OCCURS 95    PIC 9(012)V99.
      *Total das rubricas = 1330.
      *ABAIXO  05  FILLER  EQUIVALE     PIC X(185).
           03  OUTROS-CAMPOS-SPGENT.
               05  SPGENT-OCOR-RUB            PIC 9(02).
               05  SPGENT-CS-RUB  PIC 9(03) OCCURS 30 TIMES.
               05  SPGENT-BRANCO1             PIC X(04).
               05  SPGENT-NB                  PIC 9(10).
            03  FILLER                        PIC X(281).
      *1323 + 37     03  FILLER                   PIC X(110).

      *TRAILER SPGENT-ID-REG-T = 3
       01  DSUBCPG058-REG-SPGENT-T.
           03  SPGENT-ID-REG-T           PIC 9(01).
           03  SPGENT-QTD-REG-T          PIC 9(08).
           03  SPGENT-TOT-VL-LIQ-T       PIC 9(15)V99.
           03  FILLER                   PIC X(1774).
      *FIM DA BOOK BSUBCPG058SPG.
