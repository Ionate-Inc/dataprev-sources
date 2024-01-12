      *****************************************************************
      *    LIBRARY LAYOUT DO ARQUIVO D/SUB/CPG/058/SPG PARA SUBSTITUIR*
      *    O SCF COM  O NOVO SISPAGBEN.                               *
      *    TAMANHO DO REGISTRO 1500 CARACTERES - BLOCO 14 REGISTROS   *
      *OBS: EM TER 07/05/2013 INCLUIDO SISPG-VALOR-RUB301 NO DETALHE *
      *****************************************************************

      *HEDAER SISPG-ID-REG-H = 1
       01  DSUBCPG058-REG-SISPG-HEADER.
           03  SISPG-ID-REG-H           PIC 9(01).
           03  SISPG-ORIG-CRED-H        PIC 9(01).
           03  SISPG-DT-PROC-H          PIC 9(08).
           03  SISPG-ID-CALAMIDADE-H    PIC 9(03).
           03  SISPG-NUM-PORTARIA-H     PIC X(15).
           03  SISPG-DT-PORTARIA-H      PIC 9(08).
           03  SISPG-DT-INI-VALIDADE-H  PIC 9(08).
           03  SISPG-DT-FIM-VALIDADE-H  PIC 9(08).
           03  SISPG-NU-SEQ-ARQ-H       PIC 9(01).
           03  SISPG-ID-ULT-ARQ-H       PIC 9(01).
           03  SISPG-LISTA-RUBR-H       PIC X(288).
           03  SISPG-LISTA-RUBR-H-R REDEFINES
               SISPG-LISTA-RUBR-H.
               05 SISPG-LISTA-RUBRICA-H OCCURS 96 TIMES.
                  07 SISPG-CS-RUBRICA-H PIC 9(03).
           03  FILLER                   PIC X(1458).

      *DETALHE SISPG-ID-REG = 2
       01  DSUBCPG058-REG-SISPG.
           03  SISPG-ID-REG             PIC 9(01).
           03  SISPG-ID-BANCO           PIC 9(03).
           03  SISPG-ID-PAIS            PIC 9(03).
           03  SISPG-IN-CRED-PIONEIRA   PIC 9(01).
           03  SISPG-CS-MEIO-PAGTO      PIC 9(02).
           03  SISPG-DT-INI-VALIDADE    PIC 9(08).
           03  SISPG-CS-ORIG-ORC        PIC 9(01).
           03  SISPG-CS-URBANO-RURAL    PIC 9(01).
           03  SISPG-IN-CRED-EPU        PIC 9(01).
           03  SISPG-COD-CALAMIDADE     PIC 9(03).
           03  SISPG-IN-ADIANTE-13-SAL  PIC 9(01).
           03  SISPG-CS-ESPECIE         PIC 9(02).
           03  SISPG-ENT-CLASS          PIC 9(04).
           03  SISPG-QTD-ENT-CLASS      PIC 9(08).
           03  SISPG-QUANT-CREDITOS     PIC 9(10).
           03  SISPG-VALOR-BRUTO        PIC 9(15)V99.
           03  SISPG-VALOR-LIQUIDO      PIC 9(15)V99.
      *Ate aqui, 71.79.80.83
           03  SISPG-VALOR-GRUPO-RUBRICAS.
               05  SISPG-VALOR-RUB104   PIC 9(12)V99.
               05  SISPG-VALOR-RUB105   PIC 9(12)V99.
               05  SISPG-VALOR-RUB107   PIC 9(12)V99.
               05  SISPG-VALOR-RUB108   PIC 9(12)V99.
               05  SISPG-VALOR-RUB112   PIC 9(12)V99.
               05  SISPG-VALOR-RUB136   PIC 9(12)V99.
               05  SISPG-VALOR-RUB154   PIC 9(12)V99.
               05  SISPG-VALOR-RUB160   PIC 9(12)V99.
               05  SISPG-VALOR-RUB161   PIC 9(12)V99.
               05  SISPG-VALOR-RUB162   PIC 9(12)V99.
               05  SISPG-VALOR-RUB201   PIC 9(12)V99.
               05  SISPG-VALOR-RUB202   PIC 9(12)V99.
               05  SISPG-VALOR-RUB203   PIC 9(12)V99.
               05  SISPG-VALOR-RUB204   PIC 9(12)V99.
               05  SISPG-VALOR-RUB205   PIC 9(12)V99.
               05  SISPG-VALOR-RUB206   PIC 9(12)V99.
               05  SISPG-VALOR-RUB207   PIC 9(12)V99.
               05  SISPG-VALOR-RUB208   PIC 9(12)V99.
               05  SISPG-VALOR-RUB209   PIC 9(12)V99.
               05  SISPG-VALOR-RUB210   PIC 9(12)V99.
               05  SISPG-VALOR-RUB214   PIC 9(12)V99.
               05  SISPG-VALOR-RUB215   PIC 9(12)V99.
               05  SISPG-VALOR-RUB216   PIC 9(12)V99.
               05  SISPG-VALOR-RUB217   PIC 9(12)V99.
               05  SISPG-VALOR-RUB218   PIC 9(12)V99.
               05  SISPG-VALOR-RUB219   PIC 9(12)V99.
               05  SISPG-VALOR-RUB220   PIC 9(12)V99.
               05  SISPG-VALOR-RUB221   PIC 9(12)V99.
               05  SISPG-VALOR-RUB222   PIC 9(12)V99.
               05  SISPG-VALOR-RUB223   PIC 9(12)V99.
               05  SISPG-VALOR-RUB224   PIC 9(12)V99.
               05  SISPG-VALOR-RUB225   PIC 9(12)V99.
               05  SISPG-VALOR-RUB226   PIC 9(12)V99.
               05  SISPG-VALOR-RUB227   PIC 9(12)V99.
               05  SISPG-VALOR-RUB228   PIC 9(12)V99.
               05  SISPG-VALOR-RUB229   PIC 9(12)V99.
               05  SISPG-VALOR-RUB230   PIC 9(12)V99.
               05  SISPG-VALOR-RUB231   PIC 9(12)V99.
               05  SISPG-VALOR-RUB232   PIC 9(12)V99.
               05  SISPG-VALOR-RUB233   PIC 9(12)V99.
               05  SISPG-VALOR-RUB234   PIC 9(12)V99.
               05  SISPG-VALOR-RUB236   PIC 9(12)V99.
               05  SISPG-VALOR-RUB237   PIC 9(12)V99.
               05  SISPG-VALOR-RUB238   PIC 9(12)V99.
               05  SISPG-VALOR-RUB239   PIC 9(12)V99.
               05  SISPG-VALOR-RUB240   PIC 9(12)V99.
               05  SISPG-VALOR-RUB241   PIC 9(12)V99.
               05  SISPG-VALOR-RUB242   PIC 9(12)V99.
               05  SISPG-VALOR-RUB243   PIC 9(12)V99.
               05  SISPG-VALOR-RUB244   PIC 9(12)V99.
               05  SISPG-VALOR-RUB245   PIC 9(12)V99.
               05  SISPG-VALOR-RUB246   PIC 9(12)V99.
               05  SISPG-VALOR-RUB247   PIC 9(12)V99.
               05  SISPG-VALOR-RUB248   PIC 9(12)V99.
               05  SISPG-VALOR-RUB249   PIC 9(12)V99.
               05  SISPG-VALOR-RUB250   PIC 9(12)V99.
               05  SISPG-VALOR-RUB251   PIC 9(12)V99.
               05  SISPG-VALOR-RUB252   PIC 9(12)V99.
               05  SISPG-VALOR-RUB253   PIC 9(12)V99.
               05  SISPG-VALOR-RUB254   PIC 9(12)V99.
               05  SISPG-VALOR-RUB255   PIC 9(12)V99.
               05  SISPG-VALOR-RUB256   PIC 9(12)V99.
               05  SISPG-VALOR-RUB257   PIC 9(12)V99.
               05  SISPG-VALOR-RUB258   PIC 9(12)V99.
               05  SISPG-VALOR-RUB259   PIC 9(12)V99.
               05  SISPG-VALOR-RUB260   PIC 9(12)V99.
               05  SISPG-VALOR-RUB261   PIC 9(12)V99.
               05  SISPG-VALOR-RUB262   PIC 9(12)V99.
               05  SISPG-VALOR-RUB263   PIC 9(12)V99.
               05  SISPG-VALOR-RUB264   PIC 9(12)V99.
               05  SISPG-VALOR-RUB265   PIC 9(12)V99.
               05  SISPG-VALOR-RUB266   PIC 9(12)V99.
               05  SISPG-VALOR-RUB267   PIC 9(12)V99.
               05  SISPG-VALOR-RUB268   PIC 9(12)V99.
               05  SISPG-VALOR-RUB269   PIC 9(12)V99.
               05  SISPG-VALOR-RUB270   PIC 9(12)V99.
               05  SISPG-VALOR-RUB271   PIC 9(12)V99.
               05  SISPG-VALOR-RUB272   PIC 9(12)V99.
               05  SISPG-VALOR-RUB273   PIC 9(12)V99.
               05  SISPG-VALOR-RUB274   PIC 9(12)V99.
               05  SISPG-VALOR-RUB275   PIC 9(12)V99.
               05  SISPG-VALOR-RUB301   PIC 9(12)V99.
               05  SISPG-VALOR-RUB905   PIC 9(12)V99.
               05  SISPG-VALOR-RUB909   PIC 9(12)V99.
               05  SISPG-VALOR-RUB910   PIC 9(12)V99.
               05  SISPG-VALOR-RUB911   PIC 9(12)V99.
               05  SISPG-VALOR-RUB912   PIC 9(12)V99.
               05  SISPG-VALOR-RUB916   PIC 9(12)V99.
               05  SISPG-VALOR-RUB917   PIC 9(12)V99.
               05  SISPG-VALOR-RUB921   PIC 9(12)V99.
               05  SISPG-VALOR-RUB925   PIC 9(12)V99.
               05  SISPG-VALOR-RUB926   PIC 9(12)V99.
               05  SISPG-VALOR-RUB927   PIC 9(12)V99.
               05  SISPG-VALOR-RUB928   PIC 9(12)V99.
               05  SISPG-VALOR-RUB929   PIC 9(12)V99.
               05  SISPG-VALOR-RUB933   PIC 9(12)V99.
           03  SISPG-VL REDEFINES SISPG-VALOR-GRUPO-RUBRICAS.
               05  SISPG-VL-RUBRICA OCCURS 96    PIC 9(012)V99.
      *Total das rubricas = 1260.
      *ABAIXO  05  FILLER  EQUIVALE     PIC X(185).
           03  OUTROS-CAMPOS-SISPG.
               05  SISPG-OCOR-RUB            PIC 9(02).
               05  SISPG-CS-RUB  PIC 9(03) OCCURS 30 TIMES.
               05  SISPG-BRANCO1             PIC X(04).
               05  SISPG-NB                  PIC 9(10).
            03  FILLER                        PIC X(267).
      *1323 + 37     03  FILLER                   PIC X(110).

      *TRAILER SISPG-ID-REG-T = 3
       01  DSUBCPG058-REG-SISPG-T.
           03  SISPG-ID-REG-T           PIC 9(01).
           03  SISPG-QTD-REG-T          PIC 9(08).
           03  SISPG-TOT-VL-LIQ-T       PIC 9(15)V99.
           03  FILLER                   PIC X(1774).
      *FIM DA BOOK BSUBCPG058SPG.
