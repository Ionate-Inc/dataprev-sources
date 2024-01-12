      ******************************************************************
      * COPY-BOOK:    B/SDC/LBD/SETTB00494     ON PKBDSGF              *
      * ENTRY-POINT:  SETTB00494                                       *
      ******************************************************************
       77 RETORNO-TB00494        PIC 9(01) BINARY.
       01 FONTE-LAYOUT-TB00494.
          03 ITEM-TB00494-1      PIC X(17) VALUE "TB0049           ".
          03 ITEM-TB00494-2      PIC X(17) VALUE "NU-INTERNO-TB0049".
          03 TAM-TB00494-2       PIC 9(06) VALUE       5.
          03 ITEM-TB00494-3      PIC X(17) VALUE "ID-MUNI-PREV     ".
          03 TAM-TB00494-3       PIC 9(06) VALUE       5.
          03 ITEM-TB00494-4      PIC X(17) VALUE "ID-OP-MENOS-BENEF".
          03 TAM-TB00494-4       PIC 9(06) VALUE       6.
          03 ITEM-TB00494-5      PIC X(17) VALUE "ID-MICRO-REG-PREV".
          03 TAM-TB00494-5       PIC 9(06) VALUE       6.
          03 ITEM-TB00494-6      PIC X(17) VALUE "CS-TIPO-MICRO-REG".
          03 TAM-TB00494-6       PIC 9(06) VALUE       1.
          03 ITEM-TB00494-7      PIC X(17) VALUE "ID-APS           ".
          03 TAM-TB00494-7       PIC 9(06) VALUE       8.
          03 ITEM-TB00494-8      PIC X(17) VALUE "ID-APS-EXTRA     ".
          03 TAM-TB00494-8       PIC 9(06) VALUE       8.
          03 ITEM-TB00494-9      PIC X(17) VALUE "IN-ACESS-RESTRITO".
          03 TAM-TB00494-9       PIC 9(06) VALUE       1.
          03 ITEM-TB00494-10     PIC X(17) VALUE "ID-OP-PREFERENCIA".
          03 TAM-TB00494-10      PIC 9(06) VALUE       6.
          03 ITEM-TB00494-11     PIC X(17) VALUE "ID-PREGAO        ".
          03 TAM-TB00494-11      PIC 9(06) VALUE       2.
          03 ITEM-TB00494-12     PIC X(17) VALUE "ID-LOTE-PREGAO   ".
          03 TAM-TB00494-12      PIC 9(06) VALUE       2.
          03 ITEM-TB00494-13     PIC X(17) VALUE "ID-OP-PREF-2     ".
          03 TAM-TB00494-13      PIC 9(06) VALUE       6.
          03 ITEM-TB00494-14     PIC X(17) VALUE "ID-PREGAO-2      ".
          03 TAM-TB00494-14      PIC 9(06) VALUE       2.
          03 ITEM-TB00494-15     PIC X(17) VALUE "ID-LOTE-PREGAO-2 ".
          03 TAM-TB00494-15      PIC 9(06) VALUE       2.
          03 ITEM-TB00494-16     PIC X(17) VALUE "ID-OP-PREF-3     ".
          03 TAM-TB00494-16      PIC 9(06) VALUE       6.
          03 ITEM-TB00494-17     PIC X(17) VALUE "ID-PREGAO-3      ".
          03 TAM-TB00494-17      PIC 9(06) VALUE       2.
          03 ITEM-TB00494-18     PIC X(17) VALUE "ID-LOTE-PREGAO-3 ".
          03 TAM-TB00494-18      PIC 9(06) VALUE       2.
          03 ITEM-TB00494-19     PIC X(17) VALUE "ID-GESTOR-ATU    ".
          03 TAM-TB00494-19      PIC 9(06) VALUE      17.
          03 ITEM-TB00494-20     PIC X(17) VALUE "ID-GESTOR-EXC    ".
          03 TAM-TB00494-20      PIC 9(06) VALUE      17.
          03 ITEM-TB00494-21     PIC X(17) VALUE "DT-INIC-VALI     ".
          03 TAM-TB00494-21      PIC 9(06) VALUE       8.
          03 ITEM-TB00494-22     PIC X(17) VALUE "DT-FIM-VALI      ".
          03 TAM-TB00494-22      PIC 9(06) VALUE       8.
          03 ITEM-TB00494-23     PIC X(17) VALUE "DT-EFET-COTE     ".
          03 TAM-TB00494-23      PIC 9(06) VALUE       8.
          03 DELIMITADOR-TB00494    PIC X(01) VALUE "#".
      *
       01 CODE-LAYOUT-TB00494       PIC X(01).
      *
       01 REGISTRO-TB00494.
          03 TB00494-CHAVE-GRUPO.
             05 CHAVE-ID-MICRO-REG-PREV PIC 9(006).
          03 TB00494-CHAVE              REDEFINES
             TB00494-CHAVE-GRUPO        PIC X(006).
          03 FILLER                     PIC X(122).
      *
       01 REGISTRO-TB00494-R  REDEFINES  REGISTRO-TB00494.
          03 NU-INTERNO-TB0049          PIC 9(005).
          03 ID-MUNI-PREV               PIC 9(005).
          03 ID-OP-MENOS-BENEF          PIC 9(006).
          03 ID-MICRO-REG-PREV          PIC 9(006).
          03 CS-TIPO-MICRO-REG          PIC 9(001).
          03 ID-APS                     PIC 9(008).
          03 ID-APS-EXTRA               PIC 9(008).
          03 IN-ACESS-RESTRITO          PIC 9(001).
      *   FALSO = 0  E  VERDADEIRO = 1
          03 ID-OP-PREFERENCIA          PIC 9(006).
          03 ID-PREGAO                  PIC 9(002).
          03 ID-LOTE-PREGAO             PIC 9(002).
          03 ID-OP-PREF-2               PIC 9(006).
          03 ID-PREGAO-2                PIC 9(002).
          03 ID-LOTE-PREGAO-2           PIC 9(002).
          03 ID-OP-PREF-3               PIC 9(006).
          03 ID-PREGAO-3                PIC 9(002).
          03 ID-LOTE-PREGAO-3           PIC 9(002).
          03 ID-GESTOR-ATU              PIC X(017).
          03 ID-GESTOR-EXC              PIC X(017).
          03 DT-INIC-VALI               PIC 9(008).
          03 DT-FIM-VALI                PIC 9(008).
          03 DT-EFET-COTE               PIC 9(008).
