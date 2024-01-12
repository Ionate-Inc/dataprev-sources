      *
      * UNISYS Y2K - TEAM 2000
      *
      *****************************************************************
      *                                                               *
      *    LIBRARY DO ARQUIVO DE CREDITO REJEITADOS                   *
      *                                                               *
      *****************************************************************
      *
       01  CP10-REG1 COMP.
      *
           05  CP10-NU-NB                 PIC 9(10).
           05  CP10-CS-NATUR-CREDITO      PIC 9(02).
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 13:59:13
      *    05  CP10-DT-FIM-PERIODO        PIC 9(06).
           05  CP10-DT-FIM-PERIODO        PIC 9(08).
      *        FORMATO: AAMMDD
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 13:59:13
      *    05  CP10-DT-INI-PERIODO        PIC 9(06).
           05  CP10-DT-INI-PERIODO        PIC 9(08).
      *        FORMATO: AAMMDD
           05  CP10-CS-MEIO-PAGTO         PIC 9(02).
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 13:59:13
      *    05  CP10-DT-FIM-VALIDADE       PIC 9(06).
           05  CP10-DT-FIM-VALIDADE       PIC 9(08).
      *        FORMATO: AAMMDD
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 13:59:13
      *    05  CP10-DT-INI-VALIDADE       PIC 9(06).
           05  CP10-DT-INI-VALIDADE       PIC 9(08).
      *        FORMATO: AAMMDD
           05  CP10-CS-ORIGEM-CREDITO     PIC 9(01).
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 13:59:13
      *    05  CP10-DT-MOV-CREDITO        PIC 9(06).
           05  CP10-DT-MOV-CREDITO        PIC 9(08).
           05  CP10-VL-LIQUIDO-CRED       PIC 9(10)V99.
           05  CP10-CS-ESPECIE            PIC 9(03).
           05  CP10-ID-BANCO              PIC 9(03).
           05  CP10-CS-UF                 PIC 9(02).
           05  CP10-NU-SEQ-PROC           PIC 9(03).
           05  CP10-ID-NIT                PIC 9(10).
           05  CP10-NU-NIT-DV1            PIC 9(01).
           05  FILLER                     PIC 9(27).
      *
           05  CP10-IN-NB-OK              PIC 9(01).
           05  CP10-IN-NATUR-CREDITO-OK   PIC 9(01).
           05  CP10-IN-PERIODO-OK         PIC 9(01).
           05  CP10-IN-VALIDADE-OK        PIC 9(01).
           05  CP10-IN-MEIO-PAGTO-OK      PIC 9(01).
           05  CP10-IN-BANCO-OK           PIC 9(01).
           05  CP10-IN-UF-OK              PIC 9(01).
           05  CP10-IN-VALOR-OK           PIC 9(01).
           05  CP10-IN-NIT-OK             PIC 9(01).
           05  FILLER                     PIC 9(06).
      *
      *
