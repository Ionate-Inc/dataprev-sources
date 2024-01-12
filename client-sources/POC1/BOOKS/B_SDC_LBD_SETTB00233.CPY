      ******************************************************************
      * COPY-BOOK:    B/SDC/LBD/SETTB00233     ON PKBDSGF              *
      * ENTRY-POINT:  SETTB00233                                       *
      ******************************************************************
       77 RETORNO-TB00233        PIC 9(01) BINARY.
       01 FONTE-LAYOUT-TB00233.
          03 ITEM-TB00233-1      PIC X(17) VALUE "TB0023           ".
          03 ITEM-TB00233-2      PIC X(17) VALUE "NU-AAP           ".
          03 TAM-TB00233-2       PIC 9(06) VALUE       6.
          03 ITEM-TB00233-3      PIC X(17) VALUE "ID-AAP           ".
          03 TAM-TB00233-3       PIC 9(06) VALUE       6.
          03 ITEM-TB00233-4      PIC X(17) VALUE "CS-CATE-AAP      ".
          03 TAM-TB00233-4       PIC 9(06) VALUE       2.
          03 ITEM-TB00233-5      PIC X(17) VALUE "ID-APS           ".
          03 TAM-TB00233-5       PIC 9(06) VALUE       8.
          03 ITEM-TB00233-6      PIC X(17) VALUE "ID-MICRO-REG-PREV".
          03 TAM-TB00233-6       PIC 9(06) VALUE       6.
          03 ITEM-TB00233-7      PIC X(17) VALUE "ID-INSTIT-AAP    ".
          03 TAM-TB00233-7       PIC 9(06) VALUE       4.
          03 ITEM-TB00233-8      PIC X(17) VALUE "ID-GRSS          ".
          03 TAM-TB00233-8       PIC 9(06) VALUE       5.
          03 ITEM-TB00233-9      PIC X(17) VALUE "ID-PSS           ".
          03 TAM-TB00233-9       PIC 9(06) VALUE       8.
          03 ITEM-TB00233-10     PIC X(17) VALUE "NU-CAR           ".
          03 TAM-TB00233-10      PIC 9(06) VALUE       9.
          03 ITEM-TB00233-11     PIC X(17) VALUE "ID-PAF           ".
          03 TAM-TB00233-11      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-12     PIC X(17) VALUE "CS-SITUACAO-AAP  ".
          03 TAM-TB00233-12      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-13     PIC X(17) VALUE "CS-PORTE-LOTERICO".
          03 TAM-TB00233-13      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-14     PIC X(17) VALUE "DT-TRANSF-AAP    ".
          03 TAM-TB00233-14      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-15     PIC X(17) VALUE "CS-MUNI-SINPAS   ".
          03 TAM-TB00233-15      PIC 9(06) VALUE       5.
          03 ITEM-TB00233-16     PIC X(17) VALUE "NM-UF-SIGL       ".
          03 TAM-TB00233-16      PIC 9(06) VALUE       2.
          03 ITEM-TB00233-17     PIC X(17) VALUE "QT-BENEFICIO-AAP ".
          03 TAM-TB00233-17      PIC 9(06) VALUE       6.
          03 ITEM-TB00233-18     PIC X(17) VALUE "NM-AAP           ".
          03 TAM-TB00233-18      PIC 9(06) VALUE      40.
          03 ITEM-TB00233-19     PIC X(17) VALUE "TE-ENDERECO      ".
          03 TAM-TB00233-19      PIC 9(06) VALUE      55.
          03 ITEM-TB00233-20     PIC X(17) VALUE "NM-BAIRRO        ".
          03 TAM-TB00233-20      PIC 9(06) VALUE      27.
          03 ITEM-TB00233-21     PIC X(17) VALUE "NU-CEP           ".
          03 TAM-TB00233-21      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-22     PIC X(17) VALUE "IN-ATIVO-PREGAO  ".
          03 TAM-TB00233-22      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-23     PIC X(17) VALUE "IN-ATIVO-PREGAO-2".
          03 TAM-TB00233-23      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-24     PIC X(17) VALUE "IN-ATIVO-PREGAO-3".
          03 TAM-TB00233-24      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-25     PIC X(17) VALUE "ID-GRAF          ".
          03 TAM-TB00233-25      PIC 9(06) VALUE       5.
          03 ITEM-TB00233-26     PIC X(17) VALUE "IN-PAGA          ".
          03 TAM-TB00233-26      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-27     PIC X(17) VALUE "IN-ARRC          ".
          03 TAM-TB00233-27      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-28     PIC X(17) VALUE "DT-COMANDO-EXCL  ".
          03 TAM-TB00233-28      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-29     PIC X(17) VALUE "DT-ABERTURA-AAP  ".
          03 TAM-TB00233-29      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-30     PIC X(17) VALUE "DT-FECHAMENTO-AAP".
          03 TAM-TB00233-30      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-31     PIC X(17) VALUE "ID-AAP-DESTINO   ".
          03 TAM-TB00233-31      PIC 9(06) VALUE       6.
          03 ITEM-TB00233-32     PIC X(17) VALUE "ID-MICRO-DESTINO ".
          03 TAM-TB00233-32      PIC 9(06) VALUE       6.
          03 ITEM-TB00233-33     PIC X(17) VALUE "ID-GESTOR-ATU    ".
          03 TAM-TB00233-33      PIC 9(06) VALUE      17.
          03 ITEM-TB00233-34     PIC X(17) VALUE "ID-GESTOR-EXC    ".
          03 TAM-TB00233-34      PIC 9(06) VALUE      17.
          03 ITEM-TB00233-35     PIC X(17) VALUE "DT-INIC-VALI     ".
          03 TAM-TB00233-35      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-36     PIC X(17) VALUE "DT-FIM-VALI      ".
          03 TAM-TB00233-36      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-37     PIC X(17) VALUE "DT-EFET-COTE     ".
          03 TAM-TB00233-37      PIC 9(06) VALUE       8.
          03 DELIMITADOR-TB00233    PIC X(01) VALUE "#".
      *
       01 CODE-LAYOUT-TB00233       PIC X(01).
      *
       01 REGISTRO-TB00233.
          03 TB00233-CHAVE-GRUPO.
             05 CHAVE-ID-AAP            PIC 9(006).
          03 TB00233-CHAVE              REDEFINES
             TB00233-CHAVE-GRUPO        PIC X(006).
          03 FILLER                     PIC X(313).
      *
       01 REGISTRO-TB00233-R  REDEFINES  REGISTRO-TB00233.
          03 NU-AAP                     PIC 9(006).
          03 ID-AAP                     PIC 9(006).
          03 CS-CATE-AAP                PIC 9(002).
          03 ID-APS                     PIC 9(008).
          03 ID-MICRO-REG-PREV          PIC 9(006).
          03 ID-INSTIT-AAP              PIC 9(004).
          03 ID-GRSS                    PIC 9(005).
          03 ID-PSS                     PIC 9(008).
          03 NU-CAR                     PIC 9(009).
          03 ID-PAF                     PIC 9(008).
          03 CS-SITUACAO-AAP            PIC 9(001).
          03 CS-PORTE-LOTERICO          PIC X(001).
          03 DT-TRANSF-AAP              PIC 9(008).
          03 CS-MUNI-SINPAS             PIC 9(005).
          03 NM-UF-SIGL                 PIC X(002).
          03 QT-BENEFICIO-AAP           PIC 9(006).
          03 NM-AAP                     PIC X(040).
          03 TE-ENDERECO                PIC X(055).
          03 NM-BAIRRO                  PIC X(027).
          03 NU-CEP                     PIC 9(008).
          03 IN-ATIVO-PREGAO            PIC 9(001).
      *   FALSO = 0  E  VERDADEIRO = 1
          03 IN-ATIVO-PREGAO-2          PIC 9(001).
      *   FALSO = 0  E  VERDADEIRO = 1
          03 IN-ATIVO-PREGAO-3          PIC 9(001).
      *   FALSO = 0  E  VERDADEIRO = 1
          03 ID-GRAF                    PIC 9(005).
          03 IN-PAGA                    PIC 9(001).
      *   FALSO = 0  E  VERDADEIRO = 1
          03 IN-ARRC                    PIC 9(001).
      *   FALSO = 0  E  VERDADEIRO = 1
          03 DT-COMANDO-EXCL            PIC 9(008).
          03 DT-ABERTURA-AAP            PIC 9(008).
          03 DT-FECHAMENTO-AAP          PIC 9(008).
          03 ID-AAP-DESTINO             PIC 9(006).
          03 ID-MICRO-DESTINO           PIC 9(006).
          03 ID-GESTOR-ATU              PIC X(017).
          03 ID-GESTOR-EXC              PIC X(017).
          03 DT-INIC-VALI               PIC 9(008).
          03 DT-FIM-VALI                PIC 9(008).
          03 DT-EFET-COTE               PIC 9(008).
