      *
      * UNISYS Y2K - TEAM 2000
      *
      *****************************************************************
      *                                                               *
      *    LIBRARY DO ARQUIVO TEMPORARIO DE MOVIMENTO DE CREDITO      *
      *                                                               *
      *****************************************************************
      *
       01  CP01-REG1.
      *
         03  CP01-REGISTRO-COMP COMP.
      *
           05  CP01-CHAVE.
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 15:01:34
      *      07  CP01-DT-MOV-CREDITO        PIC 9(06).
             07  CP01-DT-MOV-CREDITO        PIC 9(08).
      *          FORMATO: AAMMDD
      *
             07  CP01-DT-MOV-CREDITO-R REDEFINES CP01-DT-MOV-CREDITO.
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 15:01:34
      *          10  CP01-DT-MOV-CREDITO-AAMM  PIC 9(04).
                 10  CP01-DT-MOV-CREDITO-AAMM  PIC 9(06).
                 10  CP01-DT-MOV-CREDITO-DD    PIC 9(02).
      *
             07  CP01-NU-NB                 PIC 9(10).
             07  CP01-CS-NATUR-CREDITO      PIC 9(02).
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 15:01:34
      *      07  CP01-DT-FIM-PERIODO        PIC 9(06).
             07  CP01-DT-FIM-PERIODO        PIC 9(08).
      *          FORMATO: AAMMDD
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 15:01:34
      *      07  CP01-DT-INI-PERIODO        PIC 9(06).
             07  CP01-DT-INI-PERIODO        PIC 9(08).
      *          FORMATO: AAMMDD
             07  CP01-ID-NIT                PIC 9(10).
             07  CP01-SEQ-REG               PIC 9(09).
             07  FILLER                     PIC 9(01).
           05  CP01-ID-BANCO              PIC 9(03).
           05  CP01-ID-ORGAO-PAGADOR      PIC 9(06).
           05  CP01-CS-UF-OP              PIC 9(02).
           05  CP01-CS-MEIO-PAGTO         PIC 9(02).
           05  CP01-CS-ESPECIE            PIC 9(03).
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 15:01:34
      *    05  CP01-DT-COMPET-CREDITO     PIC 9(04).
           05  CP01-DT-COMPET-CREDITO     PIC 9(06).
      *        FORMATO: AAMM
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 15:01:34
      *    05  CP01-DT-FIM-VALIDADE       PIC 9(06).
           05  CP01-DT-FIM-VALIDADE       PIC 9(08).
      *        FORMATO: AAMMDD
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 15:01:34
      *    05  CP01-DT-INI-VALIDADE       PIC 9(06).
           05  CP01-DT-INI-VALIDADE       PIC 9(08).
      *        FORMATO: AAMMDD
           05  CP01-CS-ORIGEM-CREDITO     PIC 9(01).
           05  CP01-VL-LIQUIDO-CRED       PIC 9(10)V99.
           05  CP01-QT-RUBRICA            PIC 9(02).
           05  CP01-CS-RUBRICA1           PIC 9(03).
           05  CP01-VL-RUBRICA1           PIC 9(10)V99.
           05  CP01-CS-RUBRICA2           PIC 9(03).
           05  CP01-VL-RUBRICA2           PIC 9(10)V99.
           05  CP01-IN-RUBRICA-CREDITO    PIC 9(01).
           05  CP01-IN-CR-BLOQUEADO       PIC 9(01).
      *        (1) - NAO BLOQUEADO
      *        (2) - BLOQUEADO
           05  CP01-IN-CRED-ISENTO-IR     PIC 9(01).
           05  CP01-IN-CRED-CANCELADO     PIC 9(01).
           05  CP01-CS-ESP-LOAS           PIC 9(01).
      *        (1) - NAO EH ESPECIE LOAS
      *        (3) - EH ESPECIE LOAS (ESPECIES 87 E 88)
           05  CP01-IN-CREDITO-EPU        PIC 9(01).
           05  CP01-IN-CRED-CART-SOC      PIC 9(01).
           05  CP01-IN-CRED-PIONEIRA      PIC 9(01).
           05  CP01-IN-CRED-PROG-SEAS     PIC 9(01).
           05  CP01-IN-DESC-EMPREST       PIC 9(01).
           05  CP01-IN-CRED-ESPECIAL      PIC 9(01).
           05  CP01-NU-NIT-DV1            PIC 9(01).
           05  CP01-CS-ROTINA-PAGTO       PIC 9(01).
      *        (1) - CONCESSAO
      *        (2) - MACICA
      *        (4) - PAB
           05  CP01-IN-BLOQUEIO           PIC 9(02).
           05  CP01-IN-ADIANT-13-SAL      PIC 9(01).
           05  CP01-IN-BANCO-POSTAL       PIC 9(01).
           05  CP01-IN-IR-ADIAN-13SAL     PIC 9(01).
           05  CP01-ID-PREGAO             PIC 9(02).
           05  CP01-ID-LOTE-PREGAO        PIC 9(02).
           05  CP01-ID-CALAMIDADE         PIC 9(05).
      *     05  FILLER                     PIC 9(01).
           05  CP01-CS-ORIGEM-ORCAMENTO   PIC 9(02).
      *
           05  CP01-NU-CPF-TITULAR        PIC 9(11).
           05  CP01-NU-NIT-TITULAR        PIC 9(11).
      *
           05  CP01-DT-COMP-ATU-VL-CR     PIC 9(06).
           05  FILLER                     PIC 9(20).
      *
      *
         03  CP01-REGISTRO-DISPLAY.
      *
           05  CP01-NU-CONTA-CORRENTE     PIC X(10).
           05  CP01-IN-RRA-EXERC-ANT      PIC 9(01).
           05  CP01-IN-CREDITO-RRA        PIC 9(01).
           05  CP01-IN-RRA-PARCELADO      PIC 9(01).
           05  CP01-IN-CRED-SUBSTIT       PIC 9(01).
           05  CP01-IN-RRA-ATRAS-CONC     PIC 9(01).
           05  CP01-IN-CRED-ATRASADO      PIC 9(01).
      *     05  FILLER                     PIC X(04).
           05  CP01-IN-CRE-CALAMIDADE     PIC 9(01).
      *     05  CP01-ID-CALAMIDADE         PIC 9(05) COMP.
      *     05  FILLER                     PIC 9(01) COMP.
