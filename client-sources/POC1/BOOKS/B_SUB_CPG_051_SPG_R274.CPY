      *NOVO LAYOUT REGISTRO DO DSUBCPG058SISPG.
      *      COPY "*B/SUB/CPG/051/SPG/R267   ON PKBDSGF".
      *       COPY "B/SUB/CPG/051/SPG/247  ON PKPRG".
      * 01  REG-051-SPG  PIC X(1800).
      *
      *****************************************************************
      *    LIBRARY LAYOUT DO ARQUIVO D/SUB/CPG/051/SPG   PARA SUBSTITU-
      *    IR O SCF COM  O NOVO SISPAGBEN.                            *
      *    TAMANHO DO REGISTRO 1800CARACTERES - BLOCO 10 REGISTROS   *
      *****************************************************************
      *HEDAER SPG51-ID-REG-H = 1
       01  DSUBCPG051-REG-SPG51-HEADER.
           03  SPG51-ID-REG-H           PIC 9(01).
           03  SPG51-ORIGEM-CREDITO     PIC 9(01).
           03  SPG51-DT-PROC-H          PIC 9(08).
           03  SPG51-ID-CALAMIDADE-H    PIC 9(03).
           03  SPG51-NUM-PORTARIA-H     PIC X(15).
           03  SPG51-DT-PORTARIA-H      PIC 9(08).
           03  SPG51-DT-INI-VALIDADE-H  PIC 9(08).
           03  SPG51-DT-FIM-VALIDADE-H  PIC 9(08).
           03  SPG51-NU-SEQ-ARQ-H       PIC 9(01).
           03  SPG51-ID-ULT-ARQ-H       PIC 9(01).
           03  SPG51-LISTA-RUBR-H       PIC X(285).
           03  SPG51-LISTA-RUBR-H-R REDEFINES
               SPG51-LISTA-RUBR-H.
               05 SPG51-LISTA-RUBRICA-H OCCURS 95 TIMES.
                  07 SPG51-CS-RUBRICA-H PIC 9(03).
           03  FILLER                   PIC X(1461).

      *DETALHE SPG51-ID-REG = 2
       01  DSUBCPG051-REG-SPG51.
           03  SPG51-ID-REG             PIC 9(01).
           03  SPG51-ID-BANCO           PIC 9(03).
           03  SPG51-ID-PAIS            PIC 9(03).
           03  SPG51-IN-CRED-PIONEIRA   PIC 9(01).
           03  SPG51-CS-MEIO-PAGTO      PIC 9(02).
           03  SPG51-DT-INI-VALIDADE    PIC 9(08).
           03  SPG51-CS-ORIG-ORC        PIC 9(01).
           03  SPG51-CS-URBANO-RURAL    PIC 9(01).
           03  SPG51-IN-CRED-EPU        PIC 9(01).
           03  SPG51-COD-CALAMIDADE     PIC 9(03).
           03  SPG51-IN-ADIANTE-13-SAL  PIC 9(01).
           03  SPG51-CS-ESPECIE         PIC 9(02).
           03  SPG51-ENT-CLASS          PIC 9(04).
           03  SPG51-QTD-ENT-CLASS      PIC 9(08).
           03  SPG51-QUANT-CREDITOS     PIC 9(10).
           03  SPG51-VALOR-BRUTO        PIC 9(15)V99.
           03  SPG51-VALOR-LIQUIDO      PIC 9(15)V99.
      *ATE AQUI = 71 - 77 - 83
           03  SPG51-VALOR-GRUPO-RUBRICAS OCCURS 95 TIMES.
               05  SPG51-VL-RUBRICA     PIC 9(015)V99.
           03  FILLER                   PIC X(185).

      *TRAILER SPG51-ID-REG-T = 3
       01  DSUBCPG051-REG-SPG51-TRAILER.
           03  SPG51-ID-REG-T           PIC 9(01).
           03  SPG51-QTD-REG-T          PIC 9(08).
           03  SPG51-TOT-VL-LIQ-T       PIC 9(15)V99.
           03  FILLER                   PIC X(1774).
      *FIM DA BOOK BSUBCPG058SPG51
