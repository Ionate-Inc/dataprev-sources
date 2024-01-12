      *
      * UNISYS Y2K - TEAM 2000
      *
      ***************************************************************
      *                                                             *
      *  ARQUIVO DADOS COMPLEMENTARES DE CREDITO COM AUTORIZACAO DE *
      *  PAGAMENTO                                                  *
      *                                                             *
      ***************************************************************
      *
       01  CP77-REG.
      *    --------
           03  CP77-CHAVE.
      *        ----------
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 13:32:15
      *      05  CP77-DT-MOV-CREDITO        PIC 9(06) COMP.
             05  CP77-DT-MOV-CREDITO        PIC 9(08) COMP.
      *          FORMATO: AAMMDD
      *
             05  CP77-DT-MOV-CREDITO-R REDEFINES CP77-DT-MOV-CREDITO.
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 13:32:15
      *          10  CP77-DT-MOV-CREDITO-AAMM  PIC 9(04) COMP.
                 10  CP77-DT-MOV-CREDITO-AAMM  PIC 9(06) COMP.
                 10  CP77-DT-MOV-CREDITO-DD    PIC 9(02) COMP.
      *
             05  CP77-NU-NB                 PIC 9(10) COMP.
             05  CP77-CS-NATUR-CREDITO      PIC 9(02) COMP.
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 13:32:15
      *      05  CP77-DT-FIM-PERIODO        PIC 9(06) COMP.
             05  CP77-DT-FIM-PERIODO        PIC 9(08) COMP.
      *          FORMATO: AAMMDD
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 13:32:15
      *      05  CP77-DT-INI-PERIODO        PIC 9(06) COMP.
             05  CP77-DT-INI-PERIODO        PIC 9(08) COMP.
      *          FORMATO: AAMMDD
             05  CP77-SEQ-REG               PIC 9(09) COMP.
             05  FILLER                     PIC 9(01) COMP.
           03  CP77-CS-MUNICIPIO            PIC 9(06) COMP.
           03  CP77-NU-VARA-ACAO-JUD        PIC 9(02) COMP.
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 13:32:15
      *    03  CP77-DT-PAGTO-ACAO-JUD       PIC 9(06) COMP.
           03  CP77-DT-PAGTO-ACAO-JUD       PIC 9(08) COMP.
      *        FORMATO: AAMMDD
           03  CP77-NU-AP-ACAO-JUD          PIC 9(11) COMP.
           03  CP77-NU-AP-IR-ACAO-JUD       PIC 9(11) COMP.
           03  CP77-NU-MATR-SOL-CALC        PIC 9(08) COMP.
           03  CP77-CS-PRAZO-PAGTO          PIC 9(01) COMP.
           03  FILLER                       PIC 9(01) COMP.
           03  CP77-CS-UF-OL-ACAO-JUD       PIC X(02).
           03  CP77-NU-PROC-ACAO-JUD        PIC X(20).
           03  FILLER                       PIC X(07).
