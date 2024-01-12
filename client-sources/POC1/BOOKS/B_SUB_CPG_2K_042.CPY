      *
      * UNISYS Y2K - TEAM 2000
      *
      *
      ****************************************
      *                                      *
      *  LIBRARY DE TABELA DE FERIADOS       *
      *                                      *
      *  (A CADA ANO DEVERA SER ATUALIZADA)  *
      *                                      *
      ****************************************
      *
       01  WS-ANO-TAB-FERIADO              PIC 9(04) VALUE 2023.
      *
       01  TABELA-FERIADOS.
           05  FILLER  PIC X(08)  VALUE  "01000000".
           05  FILLER  PIC X(08)  VALUE  "20210000".
           05  FILLER  PIC X(08)  VALUE  "00000000".
           05  FILLER  PIC X(08)  VALUE  "07210000".
           05  FILLER  PIC X(08)  VALUE  "01000000".
           05  FILLER  PIC X(08)  VALUE  "08000000".
           05  FILLER  PIC X(08)  VALUE  "00000000".
           05  FILLER  PIC X(08)  VALUE  "00000000".
           05  FILLER  PIC X(08)  VALUE  "07000000".
           05  FILLER  PIC X(08)  VALUE  "12000000".
           05  FILLER  PIC X(08)  VALUE  "02150000".
           05  FILLER  PIC X(08)  VALUE  "24253100".
      *
       01  TABELA-FERIADOS-R    REDEFINES
           TABELA-FERIADOS.
               05  TAB-MES-FER          OCCURS  12   TIMES.
                   10  TAB-DIA-FERIADO  OCCURS   4    TIMES.
                        15  TAB-FERIADO    PIC 9(02).
