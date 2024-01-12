      *****************************************************************
      *                                                               *
      *    LIBRARY DO ARQUIVO QUE CONTEM QTDE/VALOR POR OP            *
      *    CONTAINS 105 CHARACTERS - BLOCK 180 RECORDS
      *****************************************************************
      *
       01  DSUBCPG050-REG.
      *
           03  DSUBCPG050-NOVO-CHAVE.
               05 CP50-N-ID-ORGAO-PAGADOR     PIC 9(06).
               05 CP50-N-CS-MEIO-PAGTO        PIC 9(02).
               05 CP50-N-CS-ORIGEM-CREDITO    PIC 9(01).
               05 CP50-N-DT-INI-VALIDADE      PIC 9(08).
      *        05 CP50-N-CS-ESP-LOAS          PIC 9(01).
           03 CP50-FILLER                     PIC 9(01).
           03 CP50-N-ID-BANCO                 PIC 9(03).
           03 CP50-N-ID-MICRO-REG-PREV        PIC 9(06).
      *--- 08/FEV/2001 -------------------------------
           03 CP50-N-IN-CREDITO-SEAS          PIC 9(01).
           03 CP50-N-IN-CRED-PIONEIRA         PIC 9(01).
      *     03 FILLER                          PIC 9(03).
      *    03 CP50-CS-MUNIC-SINPAS          PIC 9(05).
      *    03 CP50-CS-MUNIC-SINPAS-R REDEFINES CP50-CS-MUNIC-SINPAS.
      *       05 CP50-CS-UF                 PIC 9(02).
      *       05 FILLER                     PIC 9(03).
      *--- 08/FEV/2001 -------------------------------
           03 CP50-N-ID-PREGAO                     PIC 9(02).
           03 CP50-N-ID-LOTE                       PIC 9(02).
           03 CP50-N-CS-ORIG-ORC                   PIC 9(01).
           03 CP50-N-CS-REDE-ECT                   PIC 9(01).
           03 CP50-N-CS-URBANO-RURAL               PIC 9(01).
           03 CP50-N-GR-ESPECIES                   PIC 9(02).
           03 CP50-N-ID-SAL-FAMILIA                PIC 9(01).
           03 CP50-N-ID-PAIS                       PIC 9(03).
           03 CP50-N-QTDE-CREDITOS                 PIC 9(08).
           03 CP50-N-VL-LIQ-CREDITOS               PIC 9(15)V99.
           03 CP50-N-QTDE-IR                       PIC 9(08).
           03 CP50-N-VL-IR                         PIC 9(15)V99.
           03 CP50-N-NU-NB                         PIC 9(10).
           03 CP50-N-CS-ESPECIE                    PIC 9(03).


