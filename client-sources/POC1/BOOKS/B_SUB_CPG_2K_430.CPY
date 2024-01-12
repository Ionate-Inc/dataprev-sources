      *
      * UNISYS Y2K - TEAM 2000
      *
      ******************************************************************
      *                                                                *
      *    LIBRARY DO ARQUIVO DE RUBRICAS EXCEDENTES                   *
      *                                                                *
      *                                                                *
      ******************************************************************
      *
       01  CP430-RUBRICA-REG.
           03 CP430-CHAVE.
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 14:09:49
      *       05 CP430-DT-MOV-CREDITO      PIC 9(06)       COMP.
              05 CP430-DT-MOV-CREDITO      PIC 9(08)       COMP.
      ***        FORMATO : AAMMDD
              05 CP430-NU-NB               PIC 9(10)       COMP.
              05 CP430-CS-NATUR-CREDITO    PIC 9(02)       COMP.
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 14:09:49
      *       05 CP430-DT-FIM-PERIODO      PIC 9(06)       COMP.
              05 CP430-DT-FIM-PERIODO      PIC 9(08)       COMP.
      ***        FORMATO : AAMMDD
      ********** UNISYS Y2K - Linha alterada em 24/04/1999 14:09:49
      *       05 CP430-DT-INI-PERIODO      PIC 9(06)       COMP.
              05 CP430-DT-INI-PERIODO      PIC 9(08)       COMP.
      ***        FORMATO : AAMMDD
              05 CP430-ID-NIT              PIC 9(10)       COMP.
              05 CP430-SEQ-REG             PIC 9(09)       COMP.
              05 FILLER                    PIC 9(01)       COMP.
           03 CP430-RUBRICA OCCURS 13 TIMES.
              05 CP430-CS-RUBRICA       PIC 9(03)       COMP.
              05 CP430-VL-RUBRICA       PIC 9(10)V99    COMP.
              05 FILLER                 PIC 9(01)       COMP.
           03 FILLER                    PIC 9(20)       COMP.
           03 FILLER                    PIC X(02).
      *
