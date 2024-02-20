      $SET LIST
      $SET ERRORLIMIT = 5
      $SET FEDLEVEL   = 5
      *****$SET TADS
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PSUBCPG140UNI.
       AUTHOR. EVELI.
       DATE-WRITTEN. 30/04/99.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT DSUBCPG999  ASSIGN TO DISK.
          SELECT DSUBCPG999A ASSIGN TO DISK.
          SELECT DSUBCPG058SPGENT      ASSIGN TO DISK.
          SELECT DSUBCPG058SPGENTA      ASSIGN TO DISK.
          SELECT DSUBCPG058SPGTAL    ASSIGN TO DISK.
      *    SELECT DSUBCPG058SPGENT301   ASSIGN TO DISK.
          SELECT LSUBCPG058SPGENT      ASSIGN TO PRINTER.
          SELECT  LSUBCPG140MAIL       ASSIGN  TO DISK.

       DATA DIVISION.
       FILE SECTION.

       FD DSUBCPG999
          RECORD CONTAINS 105 CHARACTERS
          BLOCK  CONTAINS 180 RECORDS
          VA OF AREAS IS 1000
          VA OF AREASIZE IS 26000
          VALUE OF TITLE IS "D/SUB/CPG/058 ON PKDADOS."
          SECURITYTYPE PUBLIC.

       01  REG-DSUBCPG999          PIC X(105).
      *
       FD  DSUBCPG999A
           RECORD CONTAINS 105 CHARACTERS
           BLOCK  CONTAINS 180 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 26000
           VALUE OF TITLE IS "D/SUB/CPG/058/CRED/ESP ON PKDADOS."
           SECURITYTYPE IS PUBLIC.

       01  REG-DSUBCPG999A         PIC X(105).

        FD DSUBCPG058SPGENT
           RECORD CONTAINS  1800 CHARACTERS
           BLOCK  CONTAINS    10 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 40000
           SECURITYTYPE PUBLIC
           VALUE OF TITLE IS "D/SUB/CPG/058/ALT/SPG/R274 ON PKDADOS.".

       01  REG-058-SPGENT PIC X(1800).

        FD DSUBCPG058SPGENTA
           RECORD CONTAINS  1800 CHARACTERS
           BLOCK  CONTAINS    10 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 40000
           SECURITYTYPE PUBLIC
           VALUE OF TITLE IS "D/SUB/CPG/058/CRED/ESP/R274 ON PKDADOS.".

       01  REG-058-SPGENTA PIC X(1800).

        FD DSUBCPG058SPGTAL
           RECORD CONTAINS  1800 CHARACTERS
           BLOCK  CONTAINS    10 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 40000
           SECURITYTYPE PUBLIC
           VALUE OF TITLE IS "D/SUB/CPG/058/CRED/ESP/TAL ON PKDADOS.".

       01  REG-058-SPGTAL PIC X(1800).

       FD  LSUBCPG058SPGENT.
       01  LSUBSPGENT-REG               PIC X(132).

       FD  LSUBCPG140MAIL
           VALUE  OF TITLE IS WS-LSUBCPG140MAIL-TITLE
           SECURITYTYPE IS PUBLIC
           PROTECTION IS SAVE.

       01 REG-LSUBCPG140MAIL            PIC X(080).

       DATA-BASE SECTION.
      * DB  BDBENCRED = BDBENCREDTR.
       DB  BDBENCRED.
       01  DS-MOV-CRED-ALTER.
       01  DS-BENEF-CALAM.
       01  DS-MOV-CR-MAN-ESP.
       01  DS-RESTART.

      * DB  BDSISBEN  = BDSISBENTR.
       DB  BDSISBEN.

       01  DS-BENEFICIO.

       WORKING-STORAGE SECTION.
      *
      *NOVO LAYOUT REGISTRO DO DSUBCPG058SPGENT.
           COPY "*B/SUB/CPG/058/SPG/R274 ON PKBDSGF".
      *      COPY "*B/SUB/CPG/058/SPG/CNF ON PKBDSGF".
      *     COPY "B/SUB/CPG/058/SPG/JCM/RUB7 ON PKPRG".

      * Pagamentos para entidades de classe. JCM em 22/02/2016.
       01 TAB-ENTID.
         03 FILLER PIC X(22) VA "0126219COBAP         ".
         03 FILLER PIC X(22) VA "0227220CONTAG        ".
         03 FILLER PIC X(22) VA "0328221STFERJ        ".
         03 FILLER PIC X(22) VA "0429222ASTRE         ".
         03 FILLER PIC X(22) VA "0530223FORCA SINDICAL".
         03 FILLER PIC X(22) VA "0631224CUT           ".
         03 FILLER PIC X(22) VA "0732225UNIDOS        ".
         03 FILLER PIC X(22) VA "0833226CGT           ".
         03 FILLER PIC X(22) VA "0935228SINDAPB       ".
         03 FILLER PIC X(22) VA "1036229ASBAPI        ".
         03 FILLER PIC X(22) VA "1137230FNTF          ".
         03 FILLER PIC X(22) VA "1241234FETRAF        ".
         03 FILLER PIC X(22) VA "1345239ANAPPS        ".
         03 FILLER PIC X(22) VA "1446240ANAPPS(13)    ".
         03 FILLER PIC X(22) VA "1547241ASBAPI(13)    ".
         03 FILLER PIC X(22) VA "1648242SINDIAPI      ".
         03 FILLER PIC X(22) VA "1749243SINTRAAPI/CUT ".
         03 FILLER PIC X(22) VA "1850244ABAMSP        ".
         03 FILLER PIC X(22) VA "1951245FITF/CNTT/CUT ".
         03 FILLER PIC X(22) VA "2052246RIAAM-BRASIL  ".
         03 FILLER PIC X(22) VA "2153247SINAP         ".
         03 FILLER PIC X(22) VA "2254248ABSP          ".
         03 FILLER PIC X(22) VA "2355249CONAFER       ".
         03 FILLER PIC X(22) VA "2456250UNIBRASIL     ".
         03 FILLER PIC X(22) VA "2560254UNIBA         ".
         03 FILLER PIC X(22) VA "2662256AAPB          ".
         03 FILLER PIC X(22) VA "2763257AMBEC         ".
         03 FILLER PIC X(22) VA "2864258ABRAPPS       ".
         03 FILLER PIC X(22) VA "2965259UNASPUB       ".
         03 FILLER PIC X(22) VA "3070264AAPPS UNIVERSO".
         03 FILLER PIC X(22) VA "2872266CINAAP        ".
         03 FILLER PIC X(22) VA "3273267CAAP          ".
         03 FILLER PIC X(22) VA "3375269AP BRASIL     ".
         03 FILLER PIC X(22) VA "3476270CBPA          ".
         03 FILLER PIC X(22) VA "3577271AMAR BRASIL   ".
         03 FILLER PIC X(22) VA "3678272ACOLHER       ".
         03 FILLER PIC X(22) VA "3779273CEBAP         ".
         03 FILLER PIC X(22) VA "3880274ABENPREV      ".

       01 WS-TAB-ENT-R REDEFINES TAB-ENTID.
          03 WS-TAB-ENT OCCURS 38 TIMES.
             07 TAB-NUM-ENTIDADE        PIC 99.
             07 TAB-IND-RUBR            PIC 99.
             07 TAB-CS-RUBR-ENT         PIC 9(003).
             07 TAB-NM-ENTIDADE         PIC X(015).

        01 WS-TAB-RUBR-ENT.
           03 WS-TB-RUBRICAS-ENT OC 38.
              05 TB-VL-RUB-ENT  PIC 9(12)V99.

        01 WS-TAB-QTD-VL-ENT.
           03 WS-TB-QT-VL-ENT OC 38.
              05 TB-QT-TOTAL-ENT      PIC 9(08).
              05 TB-VL-TOTAL-RUB-ENT  PIC 9(12)V99.

       77  W77-DDATA                       PIC 9(08) BINARY.
       77  W77-DATA-INICIO                 PIC 9(08).
       77  WS-DATA-ATUAL-DMA               PIC 9(08) BINARY.
       77  WS-DIAS-A-SUBTRAIR              PIC S9(03) BINARY.
       77  W77-DATA-RESPOSTA               PIC 9(08) BINARY.

       01 WS-DT-ANTES-11-MESES-AMD PIC 9(08).
       01 WS-DT-ANTES-11-MESES-AMD-R REDEFINES WS-DT-ANTES-11-MESES-AMD.
          03 WS-ANO-ANTES-11-MESES  PIC 9(04).
          03 WS-MES-ANTES-11-MESES  PIC 9(02).
          03 WS-DIA-ANTES-11-MESES  PIC 9(02).

       01 WS-DT-ANTES-11-MESES-DMA PIC 9(08).
       01 WS-DT-ANTES-11-MESES-DMA-R REDEFINES WS-DT-ANTES-11-MESES-DMA.
          03 WS-DIA-ANTES-11-MESES  PIC 9(02).
          03 WS-MES-ANTES-11-MESES  PIC 9(02).
          03 WS-ANO-ANTES-11-MESES  PIC 9(04).

       77 WS-IN-CRED-DELETADO          PIC X(05).
       77 WS-IN-VAGO-01                PIC X(05).
       77 WS-ID-BANCO                  PIC 9(03).

       77  WS-IND-ENT                  PIC 9(02).
       77  WS-GRAV-ENTIDADES           PIC 9(06) VALUE ZEROS.
       77  W77-NU-NB                   PIC 9(10).
       77  W77-D2-FIM-PER              PIC 9(08).
       77  WS-QT-AUX                   PIC 9(08).
       77  WS-VL-AUX                   PIC 9(15)V99.
      *
      *    VARIAVEIS USADAS PELA BOOK DE PROCEDURE B/SUB/CPG/050/PROC
       77  CP01-N-VL-LIQ-CREDITOS      PIC 9(12)V99 VALUE ZEROS.
       77  CP01-N-VL-BRUTO-CRED        PIC 9(12)V99 VALUE ZEROS.
       77  WS-GERA-CREDITOS            PIC 9(01) VALUE 1.
       77  WS-IN-EMP-PREGAO            PIC 9(01) VALUE 1.
       77  AUX1-ID-PREGAO              PIC 9(02) VALUE ZEROS.
       77  AUX1-ID-LOTE-PREGAO         PIC 9(02) VALUE ZEROS.
       77  AUX1-IN-CREDITO-EPU         PIC 9(01) VALUE ZEROS.
       77  AUX1-NU-NB                  PIC 9(10) VALUE ZEROS.
       77  WS-REGS-058-LOAS-DEF        PIC 9(08) VALUE ZEROS.
       77  WS-REGS-058-LOAS-IDOSO      PIC 9(08) VALUE ZEROS.
       77  WS-REGS-058-RMV-DEF         PIC 9(08) VALUE ZEROS.
       77  WS-REGS-058-RMV-ID          PIC 9(08) VALUE ZEROS.
       77  WS-REGS-058-SO-PREV         PIC 9(08) VALUE ZEROS.
       77  WS-REGS-058-SO-EPU          PIC 9(08) VALUE ZEROS.
       77  WS-REGS-058-MISTO           PIC 9(08) VALUE ZEROS.
       77  WS-REGS-058-MISTO-SO-EPU    PIC 9(08) VALUE ZEROS.
       77  WS-REGS-O58-NOVO            PIC 9(08) VALUE ZEROS.
       77  WS-REGS-058-SAL-FAMILIA     PIC 9(08) VALUE ZEROS.
       77  WS-BRUTO-RUB-105            PIC 9(12)V99 VALUE ZEROS.
       77  WS-VL-RUB-301               PIC 9(12)V99 VALUE ZEROS.
       77  WS-IR-UNIAO                 PIC 9(12)V99 VALUE ZEROS.
       77  WS-IR-UNIAO-2               PIC 9(12)V99 VALUE ZEROS.
       77  WS-RUB-108-UNIAO            PIC 9(12)V99 VALUE ZEROS.
       77  WS-RUB-108-PREV             PIC 9(12)V99 VALUE ZEROS.
      *77  WS-TOT-DESCIR-2             PIC 9(12)V99 VALUE ZEROS.
       77  WS-VL-RUB-108               PIC 9(12)V99 VALUE ZEROS.
       77  WS-REGS-058-NOVO            PIC 9(08) VALUE ZEROS.
       77  WS-REGS-058-NOVO-ESP        PIC 9(08) VALUE ZEROS.

       01 WS-ESPECIES-LOAS          PIC 9(003).
          88 ESPECIES-LOAS    VALUE 16 18 87 88 98.
          88 ESPECIES-LOAS-87 VALUE 87.
          88 ESPECIES-LOAS-88 VALUE 88.
          88 ESPECIES-RMV     VALUE 11 12 30 40.
          88 ESPECIES-RMV-ID  VALUE    12    40.
          88 ESPECIES-RMV-DEF VALUE 11 30.
          88 ESPECIES-EPU     VALUE 22 37 38 54 56 58 59 60 85 86 89 96.

       01 AUX1-CS-TRATAMENTO        PIC 9(002).
          88 AUX1-CS-TRAT-ECT           VALUE 64 65.
          88 AUX1-CS-TRAT-RFFSA         VALUE 54 59 60.
          88 AUX1-CS-TRAT-MISTO         VALUE 03.
          88 AUX1-CS-TRAT-REDE          VALUE 54 59 60 04 05 08 15.
          88 AUX1-CS-TRAT-ESTATUTARIO   VALUE 07 09 16 26 70 71 72 73 74
                                              75 03.
          88 AUX1-CS-TRAT-ZIKA          VALUE 82.

       01 AUX1-CS-RAMO-ATIVIDADE    PIC 9(002).
          88 AUX1-RURAL                 VALUE 08.
       01 WS-URBANO-RURAL           PIC 9(001).
      *Sipagben-----------------------------------------
       01 AUX1-URBANO-RURAL          PIC 9(001) VA 0.
       01 AUX1-CS-ORIG-ORC          PIC 9(001).
       01 AUX1-CS-CRED-EPU          PIC 9(001).
       01 AUX1-DT-FIM-VALIDADE      PIC 9(008).
       01 AUX1-CRED-ESPECIAL        PIC 9(001) VA 0.
       01 WS-TOT-BRUTO              PIC 9(015)V99 VA 0.
       01 WS-BRUTO-PREV             PIC 9(015)V99 VA 0.
       01 WS-TOT-DESC               PIC 9(015)V99 VA 0.
       01 WS-VL-TOT-LIQ             PIC 9(015)V99 VA 0.
       01 AUX1-VL-RUBRICA           PIC 9(015)V99.
       01 WS-PERC-PREV              PIC 9(003)V9(5).
       01 WS-PERC-UNIAO             PIC 9(003)V9(5).
       77 WS-FLAG-DS-BENEFICIO      PIC 9(001) VALUE ZEROS.
       77 W77-MOTIVO-SOLIC          PIC 9(002) VA 0.
       77 W77-IND                   PIC 9(005) VA 0.
       77 W77-IND-H                 PIC 9(002) VA 0.
       77 W77-IND-K                 PIC 9(002) VA 0.
       77 W77-IND-RUB               PIC 9(005) VA 0.
       77 W77-IND-TAB               PIC 9(002) VA 0.
       77 W77-IND-301               PIC 9(002) VA 0.
       77 W77-ERRO                  PIC 9(001) VA 0.
       77 GRAVADOS-301              PIC 9(07) VALUE ZEROS.
       77 GRAVADOS-ZIKA             PIC 9(07) VALUE ZEROS.
       77 WS-TOT-R274               PIC 9(07) VALUE ZEROS.
       77 WS-TOT-R273              PIC 9(07) VALUE ZEROS.
       77 WS-CS-RUB-A              PIC 9(03) VALUE 258.
       77 WS-VL-RUB-A              PIC 9(12)V99 VALUE 2,58.
       77 WS-SPGENT-TOT-VL-LIQ-T     PIC 9(15)V99  VALUE ZEROS.
       77 WS-SPGENT-TOT-VL-LIQ-TA    PIC 9(15)V99  VALUE ZEROS.
       77 WS-SPG-TOT-VL-LIQ-TAL     PIC 9(15)V99  VALUE ZEROS.
       77 WS-CONTADOR-X             PIC X(12).
       77 WS-CONTA-BRUTO-ZERADO     PIC 9(11) VALUE ZEROS.
       77 WS-FLAG-BRUTO-ZERO-ABORTA PIC 9(01) VALUE ZEROS.
       77 SPGENT-MARCA-CALAM        PIC 9(01) VALUE ZEROS.
       77 SPGENT-MARCA-MP87519      PIC 9(01) VALUE ZEROS.
       77  WS-SPGENT-COD-CALAMIDADE  PIC 9(03) VALUE ZEROS.
       77 SPGENT-MARCA-R329         PIC 9(01) VALUE ZEROS.
       77 SPGENT-MARCA-R230         PIC 9(01) VALUE ZEROS.

       77 WS-ACHOU-CALAM                PIC 9.
       88 ACHOU-CALAM                   VALUE 1.

       77 WS-IND-CALAM                  PIC 99.
       77 WS-TOT-CALAM                  PIC 9(006) VALUE ZEROS.
       77 WS-IND-OP                     PIC 9(002).
       77 WS-ID-ORGAO-PAGADOR           PIC 9(006).

       01 TAB-OP-CALAMIDADE.
      *-------CALAMIDADE 001  1 a 6

          03 FILLER                   PIC X(120) VALUE
           "088795623314666784677727745702499554550327499561537800623403
      -  "678011522163677950286784554709678326499674678395749122759871".

          03 FILLER                   PIC X(120) VALUE
           "623681677525217632250177623698677683677690744521781760499989
      -  "770107236479623612667174678895748167475452623643677330236520".

          03 FILLER                   PIC X(120) VALUE
           "540990190561623701677741499718537824677816755728000391180418
      -  "250204427910623770677885743364000411250211623787677892742570".
          03 FILLER                   PIC X(120) VALUE
           "238999620322705728528089706238076968249165252793512109620384
      - "654173706269239149707306751137770467096335620593707344753099".
          03 FILLER                   PIC X(120) VALUE
           "525117707351525408699758525319700045034399034402252844512349
      - "620740658497700247763988769311528205700449525357701144034882".
          03 FILLER                   PIC X(120) VALUE
           "034899061721187786512623599860603447664815671685704523760369
      -   "7603837603907627070000000000000000000000000000000000000000".

      *-------CALAMIDADE 002 7 a 13
          03 FILLER                PIC X(120) VALUE
           "239409254422469750513688538252712365743080779166027252036477
      - "036511036542130418137731173422254693313799402606408337417110".
          03 FILLER                PIC X(120) VALUE
           "472706478406485043485276502121514434514441514458514465514472
      - "514489514496514509514523514530514547552114555308568289569528".
          03 FILLER                PIC X(120) VALUE
           "577668613021634888634984635329643065656737656908672157673732
      - "713940713957749064752712017685033769038435067630130425141228".
          03 FILLER                PIC X(120) VALUE
           "173460255569402113650215664017664048669505713135713166761211
      - "406608479252479276486015515386549860550834555339613090020984".
          03 FILLER                PIC X(120) VALUE
           "036597036624036655036679036720080448126781128092131846134869
      - "173446184845185040233305249747254737291503312967313819401723".
          03 FILLER                PIC X(120) VALUE
           "422073469452469469469476469483472693486899487532487563514605
      - "514612532329590469612939612946648762648793648813650777659192".
          03 FILLER                PIC X(120) VALUE
           "677443714114714138714152714176714183714210741508746856781366
      -  "782198456677486748609130717295743326483845486697538290717435".

      *----------CALAMIDADE 003  14
          03 FILLER                PIC X(120) VALUE
           "233970253981440913676180736614041772041789041796245252258157
      -  "403921517197548867609825717322372034000000000000000000000000".
      *----------CALAMIDADE 004  15 a 19
          03 FILLER                   PIC X(120) VALUE
           "523868720878773497464488566947737511773480000497000500067010
      - "202060208400225727255874371613408902408926430473430613455516".
          03 FILLER                   PIC X(120) VALUE
           "474808477198477201477225477294496391515742532987580501641634
      - "642942642959643027643034643089660471664884669985720892720905".
          03 FILLER                   PIC X(120) VALUE
           "720912720929720936720943720950720981740834756118761571762670
      - "763060768654772367523899721011461852643010721042773473098259".
          03 FILLER                   PIC X(120) VALUE
           "190181642980667465721066748496568878721097464495567046737542
      - "721117431243642966721155748732523902494600642973643041719455".
          03 FILLER                   PIC X(120) VALUE
           "426448642997737600747948755982302390643003719462523988719479
      - "758998464508719486778005000000000000000000000000000000000000".

       01 TAB-OP-CALAM-R REDEFINES TAB-OP-CALAMIDADE.
            03  TAB-OP                  OCCURS 19.
               05  TAB-OP-CALAM            OCCURS 20 PIC 9(06).

       01 TAB-PROC-CALAMIDADE.
          03 FILLER                         PIC 9(03) VALUE 999.
          03 FILLER                         PIC 9(03) VALUE 006.
          03 FILLER                         PIC 9(03) VALUE 005.
          03 FILLER                         PIC 9(03) VALUE 007.
          03 FILLER                         PIC 9(03) VALUE 008.
          03 FILLER                         PIC 9(03) VALUE 010.
          03 FILLER                         PIC 9(03) VALUE 011.
          03 FILLER                         PIC 9(03) VALUE 012.
          03 FILLER                         PIC 9(03) VALUE 013.
          03 FILLER                         PIC 9(03) VALUE 014.
          03 FILLER                         PIC 9(03) VALUE 015.
          03 FILLER                         PIC 9(03) VALUE 016.
          03 FILLER                         PIC 9(03) VALUE 017.
          03 FILLER                         PIC 9(03) VALUE 018.
          03 FILLER                         PIC 9(03) VALUE 019.

       01 TAB-PROC-R REDEFINES TAB-PROC-CALAMIDADE.
            03 TAB-PROC-CALAM               OCCURS 15  PIC 9(03).

      $SET MAP
       77 WS-GRAV-SPGENT             PIC 9(011) BINARY VA 0.
       77 WS-GRAV-SPGENTA            PIC 9(011) BINARY VA 0.
       77 WS-GRAV-SPGENTAL           PIC 9(011) BINARY VA 0.
       77 WS-LIDOS-DISPLAY    PIC 9(011) BINARY VALUE ZEROS.
       77 W77-LIDOS-ALTER            PIC 9(011) BINARY VALUE ZEROS.
      $RESET MAP

       01 WS-DATA-SIS-INV               PIC 9(08).
       01 WS-DATA-SIS-INVR REDEFINES WS-DATA-SIS-INV.
          05 WS-DD-SIS-INV              PIC 99.
          05 WS-MM-SIS-INV              PIC 99.
          05 WS-AA-SIS-INV              PIC 9999.

      *Tabela para gerar os grupos de especies para o novo SCF.

         01 TAB-GRUPO-ESPECIE.
            03 FILLER                    PIC X(02) VA "07".
            03 FILLER                    PIC X(02) VA "08".
            03 FILLER                    PIC X(02) VA "07".
            03 FILLER                    PIC X(02) VA "01".
            03 FILLER                    PIC X(02) VA "05".
            03 FILLER                    PIC X(02) VA "01".
            03 FILLER                    PIC X(02) VA "02".
            03 FILLER                    PIC X(02) VA "02".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "13".
            03 FILLER                    PIC X(02) VA "30".
            03 FILLER                    PIC X(02) VA "29".
            03 FILLER                    PIC X(02) VA "09".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "32".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "07".
            03 FILLER                    PIC X(02) VA "07".
            03 FILLER                    PIC X(02) VA "07".
            03 FILLER                    PIC X(02) VA "07".
            03 FILLER                    PIC X(02) VA "10".
            03 FILLER                    PIC X(02) VA "07".
            03 FILLER                    PIC X(02) VA "26".
            03 FILLER                    PIC X(02) VA "07".
            03 FILLER                    PIC X(02) VA "07".
            03 FILLER                    PIC X(02) VA "30".
            03 FILLER                    PIC X(02) VA "09".
            03 FILLER                    PIC X(02) VA "01".
            03 FILLER                    PIC X(02) VA "06".
            03 FILLER                    PIC X(02) VA "01".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "11".
            03 FILLER                    PIC X(02) VA "25".
            03 FILLER                    PIC X(02) VA "25".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "29".
            03 FILLER                    PIC X(02) VA "02".
            03 FILLER                    PIC X(02) VA "03".
            03 FILLER                    PIC X(02) VA "06".
            03 FILLER                    PIC X(02) VA "06".
            03 FILLER                    PIC X(02) VA "03".
            03 FILLER                    PIC X(02) VA "06".
            03 FILLER                    PIC X(02) VA "17".
            03 FILLER                    PIC X(02) VA "17".
            03 FILLER                    PIC X(02) VA "03".
            03 FILLER                    PIC X(02) VA "09".
            03 FILLER                    PIC X(02) VA "01".
            03 FILLER                    PIC X(02) VA "02".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "26".
            03 FILLER                    PIC X(02) VA "07".
            03 FILLER                    PIC X(02) VA "21".
            03 FILLER                    PIC X(02) VA "03".
            03 FILLER                    PIC X(02) VA "23".
            03 FILLER                    PIC X(02) VA "24".
            03 FILLER                    PIC X(02) VA "22".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "12".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "03".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "02".
            03 FILLER                    PIC X(02) VA "03".
            03 FILLER                    PIC X(02) VA "16".
            03 FILLER                    PIC X(02) VA "04".
            03 FILLER                    PIC X(02) VA "04".
            03 FILLER                    PIC X(02) VA "04".
            03 FILLER                    PIC X(02) VA "07".
            03 FILLER                    PIC X(02) VA "18".
            03 FILLER                    PIC X(02) VA "18".
            03 FILLER                    PIC X(02) VA "28".
            03 FILLER                    PIC X(02) VA "27".
            03 FILLER                    PIC X(02) VA "20".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "13".
            03 FILLER                    PIC X(02) VA "05".
            03 FILLER                    PIC X(02) VA "08".
            03 FILLER                    PIC X(02) VA "14".
            03 FILLER                    PIC X(02) VA "15".
            03 FILLER                    PIC X(02) VA "19".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".
            03 FILLER                    PIC X(02) VA "31".

         01 TAB-2-R REDEFINES TAB-GRUPO-ESPECIE.
            03 TAB-GR-ESP            OC 99  PIC 9(02).

      *CONTADORES DE REFERNCIA DOS ARQUIVOS.
       77  WS-CP50-N-QTDE-CREDITOS            PIC 9(08) VALUE ZEROS.
       77  WS-CP50-N-ID-SAL-FAMILIA           PIC 9(08) VALUE ZEROS.
       77  WS-CP50-N-VALOR-LIQ-CREDITOS       PIC 9(15)V99  VALUE ZEROS.
       77  WS-CP50-N-VL-LIQ-CREDITOS          PIC 9(15)V99  VALUE ZEROS.

      *FIM VARIAVEIS USADAS PELA BOOK DE PROCEDURE B/SUB/CPG/050/PROC

       77  WS-NU-ULT-PROC-ALTER-SENDO-PR     PIC 9(03).
       77  WS-TOT-BRUTO-2              PIC 9(12)V99.
       77  WS-TOT-DESC-2               PIC 9(12)V99.
       77  WS-TOT-DESCIR-2             PIC 9(12)V99.
       77  WS-IR-PREV-2                PIC 9(12)V99.
       77  WS-TOT-PARCIAL              PIC 9(15)V99.
       77  WS-REGS-058A-LOAS-DEF       PIC 9(10).
       77  WS-REGS-058A-LOAS-IDOSO     PIC 9(10).
       77  WS-REGS-058A-MISTO-SO-EPU   PIC 9(10).
       77  WS-REGS-058A-MISTO          PIC 9(10).
       77  WS-REGS-058A-SO-PREV        PIC 9(10).
       77  WS-PERC-PREV-2              PIC 9(03)V9(05).
       77  WS-HEADER                   PIC 9 VA 1.
       77  WS-FIM                      PIC 9(01) VALUE ZEROS.
           88  FIM                                VALUE 1.
       77  WS-OL-305                   PIC  9(08)  VALUE 0.
       77  WS-OP-305                   PIC  9(06)  VALUE 0.
       77  WS-RETORNO-305              PIC  9(01)  VALUE 0.
       77  WS-LD-CRED-PEND              PIC 9(08) VALUE ZEROS.
       77  WS-S-CRED                    PIC 9(08) VALUE ZEROS.
       77  WS-PAB-EXIST                 PIC 9(08) VALUE ZEROS.
       77  WS-NB                        PIC 9(10)  VALUE 0.
       77  WS-GR-CONTG                  PIC 9(08) VALUE ZEROS.
       77  WS-DATA-SIS                  PIC 9(06).
       01  WS-HORA-SIS                  PIC 9(06).
       01  WS-HH-INTR REDEFINES WS-HORA-SIS.
          05 WS-HORA                    PIC 9(02).
          05 WS-MINUTO                  PIC 9(02).
          05 FILLER                     PIC 9(02).

       77  WS-ACHOU-OL                  PIC  9(01)  VALUE 0.
           88  ACHOU-OL                             VALUE 1.
       77  WS-ACHOU-CREDITO             PIC  9(01)  VALUE 0.
           88  ACHOU-CREDITO                        VALUE 1.
           88  SEM-CREDITO                          VALUE 2.
       77  WS-MSG                       PIC  X(132) VALUE SPACES.
       77  WS-IND                       PIC  9(04)  VALUE 0.
       77  WS-IND1                      PIC  9(04)  VALUE 0.
       77  WS-STATUS-BACKUP             PIC  9(11)  BINARY VALUE 0.
       01  WS-CONT-MOT-91        PIC 9(05).
       01  WS-OL                 PIC  9(08).
       01  WS-OP                 PIC  9(06).
       01  WS-RETORNO            PIC  9(01).
       01  WS-AAAAMMDD           PIC 9(8).
       01 WS-DATA-SISR REDEFINES WS-AAAAMMDD.
          05 WS-AA-SIS                  PIC 9999.
          05 WS-MM-SIS                  PIC 99.
          05 WS-DD-SIS                  PIC 99.


      ******************************************************************
      * Variaveis necessarias para encontrar a AGENCIA PIONEIRA
      * Inicio para criar um "OBJETO.AGENCIA.PIONEIRA" (CALL SHARING)
      * Ate o momento o processo "GERADOR DO CREDITO PAB" (DS-...-ALTER)
      * nao esta posicionando IN-CRED-PIONEIRA ( e IN-CRED-CART-SOC ).
      * Desta forma foi recriada a rotina neste programa (encontra-se
      * desativada no P/SUB/CPG/050).
      *
      * www.tutorialdocumenta.com.br 20/Abr/2001
      *
      ******************************************************************
       77 WS-ID-MICRO-REG-TAB            PIC 9(06).
       77 WS-CS-MUNIC-SINPAS-TAB         PIC 9(05).
       77 WS-ID-OP-ANT                   PIC 9(06).
       77 WS-MENSAGEM-ZDISPLAY           PIC X(80).
       77 WS-PARAM1-ZDISPLAY             PIC X(120).
       77 WS-PARAM2-ZDISPLAY             PIC X(15).
       77 WS-INTEIRO-ZDISPLAY            PIC 9(01) BINARY VALUE 0.
       77 WS-TIPO-MICRO-REG              PIC 9(01).
          88 AGENCIA-PIONEIRA VALUE 6.
       77 WS-OPS-ERRO-TB0023             PIC 9(08).

       77 WS-ID-OL-SOLIC-CRED            PIC 9(09).
       77 WS-NU-SEQ-SOLIC-CRED           PIC 9(04).
       77 WS-D2-SOLIC-CREDITO            PIC 9(08).

       77 WS-MRS-ERRO-TB0049             PIC 9(08).
          COPY "*B/SDC/LBD/SETTB00233 ON PKBDSGF".
          COPY "*B/SDC/LBD/SETTB00494 ON PKBDSGF".
      *** 20/Abr/2001 **************************************************
      *
       01 WS-DT-ANTES-6-MESES           PIC 9(08).
       01 WS-DT-ANTES-6-MESES-R REDEFINES WS-DT-ANTES-6-MESES.
          03 WS-ANO-ANTES-6-MESES       PIC 9(04).
          03 WS-MES-ANTES-6-MESES       PIC 9(02).
          03 WS-DIA-ANTES-6-MESES       PIC 9(02).
      *
      *    COPY "B/SUB/CPG/050      ON PKPRG".
            COPY "*B/SUB/CPG/050/SUL      ON PKBDSGF".

      *SISPAGBEN-------------------------------------

       77 WS-QUAIS-RUBR             PIC 9(002) VA 1.
       77 WS-CONT-OUTRAS            PIC 9(002) VA 1.
       77 W77-TOT-RUBRICAS          PIC 9(002) VA 0.
       77 W77-TOT-RUBR-CRED         PIC 9(002) VA 0.
       77 WS-ACHOU-RUBRICA          PIC 9(001) VA 0.
       01 WS-DESCR-RUBR             PIC 9(003).
          88 WS-CS-RUBR             VALUE  104 105 107 108 112 136 154
                                           160 161 162 201 202 203 204
                                           205 206 207 208 209 210 214
                                           215 216 217 218 219 220 221
                                           222 223 224 225 226 227 228
                                           229 230 231 232 233 234 236
                                           237 238 239 240 241 242 243
                                           244 245 246 247 248 249 250
                                           251 252 253 254 255 256 257
                                           258 259 260 261 262 263 264
                                           265 266 267 268 269 270 271
                                           272 273 274 301 905 909 910
                                           911 916 917 921 925 926 927
                                           928 929 933.

      * COPY "*B/SUB/CPG/SPG/RUB ON PKBDSGF".
      *  COPY  "*B/SUB/CPG/SPG/RUB/ENT ON PKBDSGF".
       01 TAB-CS-RUBRICA.
         03 FILLER                    PIC X(03) VA "104".
         03 FILLER                    PIC X(03) VA "105".
         03 FILLER                    PIC X(03) VA "107".
         03 FILLER                    PIC X(03) VA "108".
         03 FILLER                    PIC X(03) VA "112".
         03 FILLER                    PIC X(03) VA "136".
         03 FILLER                    PIC X(03) VA "154".
         03 FILLER                    PIC X(03) VA "160".
         03 FILLER                    PIC X(03) VA "161".
         03 FILLER                    PIC X(03) VA "162".
         03 FILLER                    PIC X(03) VA "201".
         03 FILLER                    PIC X(03) VA "202".
         03 FILLER                    PIC X(03) VA "203".
         03 FILLER                    PIC X(03) VA "204".
         03 FILLER                    PIC X(03) VA "205".
         03 FILLER                    PIC X(03) VA "206".
         03 FILLER                    PIC X(03) VA "207".
         03 FILLER                    PIC X(03) VA "208".
         03 FILLER                    PIC X(03) VA "209".
         03 FILLER                    PIC X(03) VA "210".
         03 FILLER                    PIC X(03) VA "214".
         03 FILLER                    PIC X(03) VA "215".
         03 FILLER                    PIC X(03) VA "216".
         03 FILLER                    PIC X(03) VA "217".
         03 FILLER                    PIC X(03) VA "218".
         03 FILLER                    PIC X(03) VA "219".
         03 FILLER                    PIC X(03) VA "220".
         03 FILLER                    PIC X(03) VA "221".
         03 FILLER                    PIC X(03) VA "222".
         03 FILLER                    PIC X(03) VA "223".
         03 FILLER                    PIC X(03) VA "224".
         03 FILLER                    PIC X(03) VA "225".
         03 FILLER                    PIC X(03) VA "226".
         03 FILLER                    PIC X(03) VA "227".
         03 FILLER                    PIC X(03) VA "228".
         03 FILLER                    PIC X(03) VA "229".
         03 FILLER                    PIC X(03) VA "230".
         03 FILLER                    PIC X(03) VA "231".
         03 FILLER                    PIC X(03) VA "232".
         03 FILLER                    PIC X(03) VA "233".
         03 FILLER                    PIC X(03) VA "234".
         03 FILLER                    PIC X(03) VA "236".
         03 FILLER                    PIC X(03) VA "237".
         03 FILLER                    PIC X(03) VA "238".
         03 FILLER                    PIC X(03) VA "239".
         03 FILLER                    PIC X(03) VA "240".
         03 FILLER                    PIC X(03) VA "241".
         03 FILLER                    PIC X(03) VA "242".
         03 FILLER                    PIC X(03) VA "243".
         03 FILLER                    PIC X(03) VA "244".
         03 FILLER                    PIC X(03) VA "245".
         03 FILLER                    PIC X(03) VA "246".
         03 FILLER                    PIC X(03) VA "247".
         03 FILLER                    PIC X(03) VA "248".
         03 FILLER                    PIC X(03) VA "249".
         03 FILLER                    PIC X(03) VA "250".
         03 FILLER                    PIC X(03) VA "251".
         03 FILLER                    PIC X(03) VA "252".
         03 FILLER                    PIC X(03) VA "253".
         03 FILLER                    PIC X(03) VA "254".
         03 FILLER                    PIC X(03) VA "255".
         03 FILLER                    PIC X(03) VA "256".
         03 FILLER                    PIC X(03) VA "257".
         03 FILLER                    PIC X(03) VA "258".
         03 FILLER                    PIC X(03) VA "259".
         03 FILLER                    PIC X(03) VA "260".
         03 FILLER                    PIC X(03) VA "261".
         03 FILLER                    PIC X(03) VA "262".
         03 FILLER                    PIC X(03) VA "263".
         03 FILLER                    PIC X(03) VA "264".
         03 FILLER                    PIC X(03) VA "265".
         03 FILLER                    PIC X(03) VA "266".
         03 FILLER                    PIC X(03) VA "267".
         03 FILLER                    PIC X(03) VA "268".
         03 FILLER                    PIC X(03) VA "269".
         03 FILLER                    PIC X(03) VA "270".
         03 FILLER                    PIC X(03) VA "271".
         03 FILLER                    PIC X(03) VA "272".
         03 FILLER                    PIC X(03) VA "273".
         03 FILLER                    PIC X(03) VA "274".
         03 FILLER                    PIC X(03) VA "301".
         03 FILLER                    PIC X(03) VA "905".
         03 FILLER                    PIC X(03) VA "909".
         03 FILLER                    PIC X(03) VA "910".
         03 FILLER                    PIC X(03) VA "911".
         03 FILLER                    PIC X(03) VA "912".
         03 FILLER                    PIC X(03) VA "916".
         03 FILLER                    PIC X(03) VA "917".
         03 FILLER                    PIC X(03) VA "921".
         03 FILLER                    PIC X(03) VA "925".
         03 FILLER                    PIC X(03) VA "926".
         03 FILLER                    PIC X(03) VA "927".
         03 FILLER                    PIC X(03) VA "928".
         03 FILLER                    PIC X(03) VA "929".
         03 FILLER                    PIC X(03) VA "933".

         01 TAB-1-R REDEFINES TAB-CS-RUBRICA.
            03 TAB-CS-RUBR            OC 95  PIC 9(03).


       01  WS-LISTA-RUBR           PIC X(285).
       01  WS-LISTA-RUBR-R REDEFINES WS-LISTA-RUBR.
          03 WS-LISTA-RUBRICA OCCURS 95 TIMES.
               05 WS-TODAS-RUBR PIC 9(03).

       01  WS-LISTA-OUTRAS-RUBR           PIC X(285).
       01  WS-LISTA-OUTRAS-RUBR-R REDEFINES WS-LISTA-OUTRAS-RUBR.
          03 WS-LIST-RUBR OCCURS 95 TIMES.
               05 WS-OUTRAS-RUBR  PIC 9(03).

       01  TB-VALOR-RUBR           PIC X(1330).
       01  TB-VALOR-RUBR-R REDEFINES TB-VALOR-RUBR.
           03 TB-VALORES   OCCURS 95 TIMES.
              05 TB-VL-RUBRICA      PIC 9(012)V99.

       77 WS-CS-RUBR-ANT            PIC 9(003).
       77 WS-ID-CREDITO             PIC 9(009).
       77 WS-IND-TBH                PIC 9(02) VALUE ZEROS.

       01 LIN-ENT.
          03 LSPG1A-SPACES.
             05 FILLER                        PIC X(080) VALUE SPACES.

          03 LSPG1A-SUB.
             05 FILLER                        PIC X(080) VALUE ALL "-".

          03 LSPG1A-CAB1.
             05 FILLER                        PIC X(016) VALUE
                "MPAS/DATAPREV".
             05 FILLER                        PIC X(048) VALUE
                "INSS - INSTITUTO NACIONAL DO SEGURO SOCIAL".
             05 FILLER                        PIC X(006) VALUE
                "DATA: ".
             05 LSPG1A-CAB1-DATA               PIC 99/99/9999.

          03 LSPG1A-CAB2.
             05 FILLER                        PIC X(026) VALUE
                "P/SUB/CPG/140/R274 ".
             05 FILLER                        PIC X(038) VALUE
                "SISTEMA UNICO DE BENEFICIOS".
             05 FILLER                        PIC X(009) VALUE
                "HORA:".
             05 LSPG1A-CAB2-HORA.
                 07 WS-HORA-R             PIC 99.
                 07 FILLER              PIC X VALUE ":".
                 07 WS-MINUTO-R           PIC 99.

          03 LSPG1A-CAB3.
             05 FILLER                        PIC X(013) VALUE
                "LSUBCPGSPG1A-".
             05 FILLER                        PIC X(043) VALUE
                "   INFORMACOES PARA ENTIDADES DE CLASSE - ".
             05 LSPG1A-CAB3-MOV               PIC X(08).

             05 FILLER                        PIC X(007) VALUE
                "PAG.:".
             05 LSPG1A-CAB3-PAG               PIC ZZZ.ZZ9.

          03 LSPG-CAB6.
             05 FILLER                        PIC X(006) VALUE SPACES.
             05 FILLER                        PIC X(026) VALUE
                "ENTIDADE DE CLASSE".
             05 FILLER                        PIC X(029) VALUE
                "QUANTIDADE".
             05 FILLER                        PIC X(020) VALUE
                "VALOR R$".
      *
          03 LSPG-CAB7.
             05 FILLER                        PIC X(006) VALUE SPACES.
             05 FILLER                        PIC X(020) VALUE
                "------------------- ".
             05 FILLER                        PIC X(054) VALUE ALL "-".
      *
          03 LSPG-DET.
             05 FILLER                       PIC X(006) VALUE SPACES.
             05 DESC-DET-SPG                 PIC X(015).
             05 FILLER                       PIC X(001) VALUE "(".
             05 CS-RUBR-ENT                  PIC X(003).
             05 FILLER                       PIC X(001) VALUE ")".
             05 FILLER                       PIC X(006) VALUE ":     ".
             05 QT-DET-SPG                   PIC ZZ.ZZZ.ZZ9.
             05 FILLER                       PIC X(006) VALUE SPACES.
             05 VL-DET-SPG                   PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
             05 FILLER                       PIC X(011) VALUE SPACES.

      ************************************
      * LINHA DE IMPRESSAO DO LSUBCPG140A*
      ************************************
       01  LINHAS.
           03  TRACO-01.
               05 FILLER                PIC X(080) VALUE ALL "-".
      *
           03  CAB01.
               05 FILLER                PIC X(18) VALUE "MPAS/DATAPREV".

               05 FILLER                PIC X(46) VALUE
                  "INSS - INSTITUTO NACIONAL DO SEGURO SOCIAL".
               05 FILLER                PIC X(06) VALUE "DATA:".
               05 C01-DATA              PIC 99/99/9999.
      *
           03  CAB02.
               05 FILLER                PIC X(26) VALUE
                                                 "P/SUB/CPG/140/UNI".
               05 FILLER                PIC X(40) VALUE
                  "SISTEMA UNICO DE BENEFICIOS".
               05 FILLER                PIC X(09) VALUE "HORA:".
               05 C02-HORA              PIC 99.
               05 FILLER                PIC X     VALUE ":".
               05 C02-MINUTO            PIC 99.
      *
           03  CAB03.
               05 C03-NOME-RELATORIO    PIC X(13) VALUE
                                                    "LSUBCPG140A".
               05 FILLER                PIC X(55) VALUE
              "*** ROTINA DE GERACAO DE PROVISAO PAB/CED SISPAGBEN ***".
               05 FILLER                PIC X(06) VALUE "PAG.:".
               05 C03-PAGINA            PIC ZZ.ZZ9.
      *
           03  TRACO-02.
               05  FILLER               PIC X(80) VALUE ALL "-".
      *
           03  CAB04.
               05 FILLER                PIC X(41) VALUE
                  "TEMPO DE PROCESSAMENTO (HH:MM:SS):".
               05 FILLER                PIC X(18) VALUE
                  "PARAMETROS:".
               05  FILLER               PIC X(19) VALUE
                   "ESCORE:".
               05  FILLER               PIC X(02) VALUE SPACES.
      *
           03  CAB04-A.
               05 FILLER                PIC X(41) VALUE
                  "----------------------------------".
               05 FILLER                PIC X(18) VALUE
                  "-----------".
               05 FILLER                PIC X(19) VALUE
                  "-------".
               05  FILLER               PIC X(02) VALUE SPACES.
      *
           03  CAB05.
               05 FILLER                PIC X(28) VALUE
                  "INICIO DO PROCESSAMENTO   : ".
               05 C05-INICIO            PIC 99.99.99.
               05 FILLER                PIC X(05) VALUE SPACES.
               05 FILLER                PIC X(10) VALUE
                  "NUM.ESQ.: ".
               05 C05-ESQUEMA           PIC 99999.
               05 FILLER                PIC X(03) VALUE SPACES.
               05 FILLER                PIC X(13) VALUE "QT.TOT.GRA.: ".
               05 C05-ESCORE            PIC ZZ9,99.
               05 FILLER                PIC X(02) VALUE " %".
      *
           03  CAB06.
               05 FILLER                PIC X(28) VALUE
                  "TERMINO DO PROCESSAMENTO  : ".
               05 C06-TERMINO           PIC 99.99.99.
               05 FILLER                PIC X(25) VALUE SPACES.
               05 FILLER                PIC X(11) VALUE "NAO GRAV.: ".
               05 C06-ESCORE-NAO-BLOQ   PIC ZZ9,99.
               05 FILLER                PIC X(02) VALUE " %".
      *
           03  CAB07.
               05 FILLER                PIC X(28) VALUE
                  "ESTIMATIVA DE TEMPO GASTO :".
               05 C07-ESTIMATIVA        PIC 99.99.99.
               05 FILLER                PIC X(25) VALUE SPACES.
               05 FILLER                PIC X(11) VALUE "PENDENTE : ".
               05 C07-ESCORE-BLOQ       PIC ZZ9,99.
               05 FILLER                PIC X(02) VALUE " %".
      *
           03  TRACO-03.
               05 FILLER                PIC X(80) VALUE ALL "-".
      *
           03  CAB07-C.
               05  FILLER               PIC X(80) VALUE SPACES.
      *
           03  CAB08.
               05 FILLER                PIC X(16) VALUE SPACES.
               05 FILLER                PIC X(39) VALUE
                  "LIDOS ".
               05 FILLER                PIC X(25) VALUE
                  "INVALIDADOS".
      *
           03  CAB09.
               05 FILLER                PIC X(14) VALUE SPACES.
               05 FILLER                PIC X(42) VALUE
                  "----------".
               05 FILLER                PIC X(24) VALUE
                  "----------".
      *
           03  CAB10.
               05 FILLER                PIC X(14) VALUE SPACES.
               05 FILLER                PIC X(42) VALUE
                  "QUANTIDADE".
               05 FILLER                PIC X(24) VALUE
                  "QUANTIDADE".
      *
           03  CAB11.
               05 FILLER                PIC X(23) VALUE
                  "TOTAL CREDITOS LIDOS: ".
               05 C11-QTD-LIDOS         PIC ZZ.ZZZ.ZZ9.
               05 FILLER                PIC X(06) VALUE SPACES.
               05 FILLER                PIC X(30) VALUE
                  "TOTAL CREDITOS GRAVADOS   .:".
               05 C11-QTD-GRAVADOS      PIC ZZ.ZZZ.ZZ9.
      *
           03  CAB12.
               05 FILLER                PIC X(46) VALUE SPACES.
               05 FILLER                PIC X(23) VALUE
                  "QTD. CREDITOS CALAM.:  ".
               05 C12-CRED-CALAM      PIC ZZ.ZZZ.ZZ9.
      *
      *     03  CAB13.
      *         05 FILLER                PIC X(48) VALUE SPACES.
      *         05 FILLER                PIC X(21) VALUE
      *            "BLOQUEADOS........: ".
      *         05 C13-QTD-BLOQ          PIC ZZ.ZZZ.ZZ9.
      *
           03  CAB14.
               05 FILLER                PIC X(45) VALUE SPACES.
               05 FILLER                PIC X(24) VALUE
                  "QTD. ZIKA ESPECIE 60..: ".
               05 C14-QTD-ZIKA           PIC ZZ.ZZZ.ZZ9.

      *     03  CAB15.
      *         05 FILLER                PIC X(41) VALUE SPACES.
      *         05 FILLER                PIC X(28) VALUE
      *            "GLOSAS CANCELADAS B955....: ".
      *        05 C15-QTD-B955          PIC ZZ.ZZZ.ZZ9.

          03 LINHA-DET-RESUMO.
               05 FILLER                PIC X(04) VALUE SPACES.
               05 TEXTO-RESUMO          PIC X(44).
               05 DOIS-PONTOS           PIC X(02).
               05 QTDE-RESUMO           PIC 9(10).
               05 FILLER                PIC X(24) VALUE SPACES.

       77  W77-HORA                     PIC 9(06) BINARY.
       77  W77-RESPOSTA                 PIC 9(01) BINARY.
       77  W77-NUM-FITA                 PIC X(06).
       77  W77-NOME                     PIC X(17) VALUE SPACES.
       77  W77-NU-MUNI                  PIC 9(05) VALUE ZEROS.
       77  W77-CS-MOTIVO-INVAL-OK       PIC 9(01) VALUE ZEROS.
       77  W77-CS-ORIGEM-INVAL-OK       PIC 9(01) VALUE ZEROS.
       77  W77-CS-CRITERIO-INVAL-OK     PIC 9(01) VALUE ZEROS.
       77  W77-TEM-13-SAL               PIC 9(01) VALUE ZEROS.
       77  W77-ESQUEMA                  PIC 9(05) BINARY.
       77  W77-ORIGEM                   PIC X(03).
       77  W77-DATAJUL                  PIC 9(05) BINARY.
       77  W77-IND-431                  PIC 9(02) VALUE ZEROS.
       77  W77-IND-432                  PIC 9(02) VALUE ZEROS.
       77  W77-IND-433                  PIC 9(02) VALUE ZEROS.
       77  W77-FIM-LISTA                PIC 9(01) VALUE ZEROS.
       77  W77-QTD-GRAVADOS             PIC 9(06) VALUE ZEROS.
       77  W77-GRAVADOS-PB5             PIC 9(06) VALUE ZEROS.
       77  W77-CANCELADOS-B955          PIC 9(06) VALUE ZEROS.
       77  W77-QTD-CRED-CALAM           PIC 9(06) VALUE ZEROS.
       77  W77-QTD-BLOQ                 PIC 9(06) VALUE ZEROS.
       77  W77-ESCORE                   PIC 9(03)V99 VALUE ZEROS.
       77  W77-ESCORE-NAO-GRAV          PIC 9(03)V99 VALUE ZEROS.
       77  W77-ESCORE-BLOQ              PIC 9(03)V99 VALUE ZEROS.
       77  W77-CONT-LINHA               PIC 9(02) VALUE 99.
       77  W77-CONT-PAG                 PIC 9(06) VALUE ZEROS.
       77  W77-HORA-INICIO              PIC 9(06).
       77  W77-HORA-FIM                 PIC 9(06).
       77  W77-DATA-FIM                 PIC 9(08).
       77  W77-TEMPO-GASTO              PIC 9(06).
       77  WS-DIAS-INICIO               PIC 9(05) BINARY.
       77  WS-DIAS-FIM                  PIC 9(05) BINARY.
       77  WS-NATU-CM                   PIC 9(01) VALUE ZEROS.
       77  W77-FASE                     PIC X(50) VALUE SPACES.
       01  WS-DATA                         PIC 9(08).
       01  WS-DATA-R REDEFINES WS-DATA.
           03 WS-DIA                       PIC 99.
           03 WS-MES                       PIC 99.
           03 WS-ANO                       PIC 9999.
       01  WS-LIDOS-LISTA               PIC 9(03).
       01  WS-IND-LISTA                 PIC 9(03) VALUE ZEROS.
       01 TAB-LISTA.
         03 WS-LISTA-ARQ OCCURS 20         PIC X(43).
         03 WS-QTD-REGISTROS OCCURS 20     PIC 9(06).

       01 TAB-R230.
         03 WS-NB-R230  OCCURS  20         PIC 9(10).

      /
      *=================================================================
      * ROTINA DE ENVIO DE RELATORIO VIA EMAIL
      *=================================================================

       77 W77-MAIL-OK               USAGE REAL.
       77 W77-MAIL-TAMANHO          PIC S9(11) USAGE BINARY.
       01 WS-LSUBCPG140MAIL-TITLE.
          03 NM-ANEXO               PIC X(039) VA
             "W/SUB/CPG/140/LSUBCPG140UNI ON PKDADOS".
      *       12345678901234567890123456789012345
          03 FILLER                 PIC X      VA ".".

       01 EMAIL                     PIC X(400).

       01 WS-HOST.
         05 W01-SCA-HOSTNAME    PIC X(8).
         05 FILLER              PIC X(248).
      *=================================================================

       PROCEDURE DIVISION.
       INICIO.

      * +--------------------------------------------------------------*
           MOVE ATTRIBUTE HOSTNAME OF MYSELF TO WS-HOST.
      * +--------------------------------------------------------------*

           MOVE SPACES TO WS-MSG
           STRING "*** INICIO PROGRAMA P/SUB/CPG/140/R274 (NOVO) ***"
           DELIMITED BY SIZE INTO WS-MSG
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                     WS-STATUS-BACKUP.



      *    CALL "LAYOUT IN DTP/LBD/TAB" USING   FONTE-LAYOUT-TB00233,
      *                                          CODE-LAYOUT-TB00233
      *                                 GIVING       RETORNO-TB00233.
          IF RETORNO-TB00233 = 2
             MOVE "ERRO ABERTURA TB00233" TO WS-MSG
             CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                             WS-STATUS-BACKUP
             SET MYSELF(STATUS) TO -1.

      *    CALL "LAYOUT IN DTP/LBD/TAB" USING   FONTE-LAYOUT-TB00494,
      *                                          CODE-LAYOUT-TB00494
      *                                 GIVING       RETORNO-TB00494.
          IF RETORNO-TB00494 = 2
             MOVE "ERRO ABERTURA TB00494" TO WS-MSG
             CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                             WS-STATUS-BACKUP
             SET MYSELF(STATUS) TO -1.
           ACCEPT WS-DATA-SIS FROM DATE.
           ACCEPT WS-AAAAMMDD FROM DATE YYYYMMDD.
      *     ACCEPT WS-HORA-SIS FROM TIME.

           CALL "ZHORA IN DTP/L1"
                 GIVING WS-HORA-SIS.

           MOVE WS-DD-SIS   TO WS-DD-SIS-INV
           MOVE WS-MM-SIS   TO WS-MM-SIS-INV
           MOVE WS-AA-SIS   TO WS-AA-SIS-INV

           CALL  "ZHORA IN DTP/L1"          GIVING  W77-HORA
           MOVE  W77-HORA               TO  W77-HORA-INICIO
                                            WS-HORA-R.
           MOVE  WS-HORA                  TO  C02-HORA.
           MOVE  WS-MINUTO                TO  C02-MINUTO.
           CALL  "ZDATA4 IN DTP/L1"   GIVING   W77-DDATA
           MOVE  W77-DDATA              TO  WS-DATA   C01-DATA
                                            W77-DATA-INICIO.

      *
      *    Calculando a data 6 meses ANTES da data corrente, para
      *    posterior utilizacao na selecao dos pabs (em funcao da
      *    virada da global).
      *
           MOVE WS-AAAAMMDD TO WS-DT-ANTES-6-MESES.
           IF WS-MES-ANTES-6-MESES > 6
              SUBTRACT 6 FROM WS-MES-ANTES-6-MESES
           ELSE
              SUBTRACT 1 FROM WS-ANO-ANTES-6-MESES
              ADD      6 TO   WS-MES-ANTES-6-MESES.
           IF WS-DIA-ANTES-6-MESES = 31
              IF WS-MES-ANTES-6-MESES = 2
                 MOVE 28 TO WS-DIA-ANTES-6-MESES
              ELSE
                 IF WS-MES-ANTES-6-MESES = 4 OR 6 OR 9 OR 11
                    MOVE 30 TO WS-DIA-ANTES-6-MESES.
      *
           CALL "ZDATA4 IN DTP/L1" GIVING  W77-DDATA.
           MOVE W77-DDATA         TO     W77-DATA-INICIO
                                         C01-DATA WS-DATA.
      *     IF WS-ANO OF WS-DATA-R = 00
      *        MOVE "MONTANDO DATA DE PROCESSAMENTO COM ANO 2000"
      *          TO WS-MENSAGEM-ZDISPLAY
      *        CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
      *                        WS-INTEIRO-ZDISPLAY.
      *
      *    Calculando a data 11 meses ANTES da data corrente, para
      *    posterior utilizacao na selecao dos pabs (em funcao da
      *    virada da global).
      *
           MOVE W77-DATA-INICIO   TO WS-DATA-ATUAL-DMA.
           MOVE -330              TO WS-DIAS-A-SUBTRAIR.
           CALL "ZQDATA4 IN DTP/L1" USING  WS-DATA-ATUAL-DMA
                                           WS-DIAS-A-SUBTRAIR
                                    GIVING W77-DATA-RESPOSTA.
           MOVE W77-DATA-RESPOSTA TO WS-DT-ANTES-11-MESES-DMA.
           MOVE CORR WS-DT-ANTES-11-MESES-DMA-R
             TO      WS-DT-ANTES-11-MESES-AMD-R.
      *   DISPLAY    WS-DT-ANTES-11-MESES-AMD-R.

           OPEN OUTPUT DSUBCPG999.
           OPEN OUTPUT DSUBCPG999A.

           OPEN OUTPUT DSUBCPG058SPGENT.
           OPEN OUTPUT DSUBCPG058SPGENTA.
           OPEN OUTPUT DSUBCPG058SPGTAL.
           OPEN OUTPUT LSUBCPG140MAIL.
      *     OPEN OUTPUT DSUBCPG058SPGENT301.
           MOVE ZEROS           TO DSUBCPG058-REG-SPGENT-HEADER.
           MOVE ZEROS           TO DSUBCPG058-REG-SPGENT-T.

           PERFORM 9550-LIMPA-RUB-H THRU 9550-LIMPA-RUB-H-EXIT
                   VARYING W77-IND FROM 1 BY 1 UNTIL  W77-IND > 95.

           OPEN INQUIRY BDSISBEN
                ON EXCEPTION
                   MOVE "ERRO NO OPEN DO BDSISBEN" TO WS-MSG
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                                   WS-STATUS-BACKUP
                   CALL SYSTEM DMTERMINATE.

           OPEN INQUIRY BDBENCRED
                ON EXCEPTION
                   MOVE "ERRO NO OPEN DO BDBENCRED" TO WS-MSG
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                                   WS-STATUS-BACKUP
                   CALL SYSTEM DMTERMINATE.

           FIND BDBENCRED.
           MOVE NU-ULT-PROC-ALTER
             TO WS-NU-ULT-PROC-ALTER-SENDO-PR.

      *****DISPLAY "WS-NU-ULT-PROC-ALTER-SENDO-PR = "
      **********WS-NU-ULT-PROC-ALTER-SENDO-PR.

           IF WS-NU-ULT-PROC-ALTER-SENDO-PR = 1
              MOVE 999 TO WS-NU-ULT-PROC-ALTER-SENDO-PR
           ELSE
              COMPUTE WS-NU-ULT-PROC-ALTER-SENDO-PR =
                      WS-NU-ULT-PROC-ALTER-SENDO-PR - 1.

      *    MOVE 866 TO WS-NU-ULT-PROC-ALTER-SENDO-PR.
           PERFORM 300-LE-999 THRU FIM-300-PROCESSAMENTO.

           IF FIM
                MOVE "NADA SELECIONADO EM DS-MOV-CRED-ALTER" TO WS-MSG
                CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                                WS-STATUS-BACKUP.

           PERFORM 300-PROCESSAMENTO THRU FIM-300-PROCESSAMENTO
                   UNTIL FIM.

           CLOSE BDBENCRED
                 ON  EXCEPTION
                     MOVE "ERRO NO CLOSE DO BDBENCRED" TO WS-MSG
                     CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                                     WS-STATUS-BACKUP
                     CALL SYSTEM DMTERMINATE.

           CLOSE DSUBCPG999     SAVE.
           CLOSE DSUBCPG999A    SAVE.

          PERFORM 10000-ORDENA-RUBR THRU 10000-FIM-ORDENA-RUBR
                  VARYING W77-IND FROM 1 BY 1
                  UNTIL W77-IND > WS-QUAIS-RUBR.

           PERFORM 16000-INCLUI-HEADER-SPGENT  THRU
                   16000-INCLUI-HEADER-SPGENT-EX.
           PERFORM 17000-INCLUI-TRAILER-SPGENT THRU
                   17000-INCLUI-TRAILER-SPGENT-EX.

           CLOSE DSUBCPG058SPGENT SAVE.
           CLOSE DSUBCPG058SPGENTA SAVE.
           IF WS-GRAV-SPGENTAL > 2
              CLOSE DSUBCPG058SPGTAL SAVE
           ELSE
              CLOSE DSUBCPG058SPGTAL.
      *     CLOSE DSUBCPG058SPGENT301 SAVE.

           PERFORM 3320-IMP-ENT-CLASSES
              THRU 3320-FIM.

           CLOSE LSUBCPG058SPGENT.

           MOVE SPACES TO WS-MSG.
           MOVE
           "-----------------------------------------------" TO WS-MSG.
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           STRING  "LIDOS DS-MOV-CRED-ALTER= " WS-LD-CRED-PEND
                   DELIMITED BY SIZE INTO WS-MSG.
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           STRING  "SOMATORIO QTDE CREDITOS (TOTAL)        =  "
                    WS-CP50-N-QTDE-CREDITOS
                   DELIMITED BY SIZE INTO WS-MSG.
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           STRING  "SOMATORIO VALORES DE CREDITOS (TOTAL)  =  "
                    WS-CP50-N-VL-LIQ-CREDITOS
                   DELIMITED BY SIZE INTO WS-MSG.
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           STRING  "GRAVADOS DSUBCPG999ESP    = "
                    WS-REGS-058-NOVO-ESP
                   DELIMITED BY SIZE INTO WS-MSG.
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP.
           MOVE SPACES TO WS-MSG.
           STRING  "GRAVADOS DSUBCPG999       = " WS-REGS-058-NOVO
                   DELIMITED BY SIZE INTO WS-MSG.
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP.
           MOVE SPACES TO WS-MSG.
           MOVE WS-GRAV-SPGENTA        TO WS-CONTADOR-X
                STRING "REG GRAVADOS SPG D/SUB/CPG/058/ESP/SPG/R274 = "
                WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           MOVE WS-GRAV-SPGENTAL        TO WS-CONTADOR-X
                STRING "REG GRAVADOS SPG D/SUB/CPG/058/ESP/SPG/TAL = "
                WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP

           MOVE SPACES TO WS-MSG.
           MOVE WS-GRAV-SPGENT        TO WS-CONTADOR-X
                STRING "REG GRAVADOS SPG D/SUB/CPG/058/ALT/SPG/R274 = "
                WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           STRING  "SOMATORIO VALORES DE CREDITOS SISPAGBEN = "
                    WS-SPGENT-TOT-VL-LIQ-T
                   DELIMITED BY SIZE INTO WS-MSG.
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           STRING  "ERROS OP TB0023    " WS-OPS-ERRO-TB0023
                   DELIMITED BY SIZE INTO WS-MSG.
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           STRING  "ERROS MR TB0049    " WS-MRS-ERRO-TB0049
                   DELIMITED BY SIZE INTO WS-MSG.
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP.
           MOVE SPACES TO WS-MSG.
           STRING "QUANTIDADE DE NBS COM BRUTO ZERADOS ="
             WS-CONTA-BRUTO-ZERADO DELIMITED BY SIZE INTO WS-MSG
             CALL "ZDISPLAY IN DTP/L1"
                   USING WS-MSG
                         WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           STRING "QUANTIDADE DE CREDITOS DE CALAMIDADE ="
             WS-TOT-CALAM DELIMITED BY SIZE INTO WS-MSG
             CALL "ZDISPLAY IN DTP/L1"
                   USING WS-MSG
                         WS-STATUS-BACKUP.

          MOVE SPACES TO WS-MSG.
          MOVE GRAVADOS-301 TO WS-CONTADOR-X
              STRING "REG GRAV SISPAGBEN D/SUB/CPG/058/SPGENT RUB 301 ="
               WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
               CALL "ZDISPLAY IN DTP/L1"
                     USING WS-MSG
                            WS-STATUS-BACKUP.

          MOVE SPACES TO WS-MSG.
          MOVE GRAVADOS-ZIKA TO WS-CONTADOR-X
               STRING "REG GRAV SISPAGBEN ESPECIE 60 - TRAT 82,  ZIKA ="
               WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
               CALL "ZDISPLAY IN DTP/L1"
                     USING WS-MSG
                            WS-STATUS-BACKUP.

          MOVE SPACES TO WS-MSG.
          MOVE WS-TOT-R274  TO WS-CONTADOR-X
               STRING "TOTAL DE CREDITOS COM RUBRICA 271 = "
               WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
               CALL "ZDISPLAY IN DTP/L1"
                     USING WS-MSG
                            WS-STATUS-BACKUP.

          MOVE SPACES TO WS-MSG.
          MOVE WS-TOT-R273  TO WS-CONTADOR-X
               STRING "TOTAL DE CREDITOS COM RUBRICA 273 = "
               WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
               CALL "ZDISPLAY IN DTP/L1"
                     USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG
           MOVE "FINAL DO P/SUB/CPG/140/R274    (SISPAGBEN)" TO  WS-MSG
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                          WS-STATUS-BACKUP.

           IF WS-FLAG-BRUTO-ZERO-ABORTA = 1
              MOVE SPACES TO WS-MSG
              MOVE "*** ABORTADO POR ENCONTRAR VALOR-BRUTO ZERADO ***"
              TO WS-MSG
              CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                             WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           MOVE "FINAL DO PROGRAMA P/SUB/CPG/140/R274 (NOVO)" TO WS-MSG.
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           MOVE "--------------------------------------" TO WS-MSG.
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP.

          PERFORM 27000-IMPRIME-RELATORIO THRU 27000-EXIT.

          CLOSE LSUBCPG140MAIL SAVE.
      *=================================================================
      * ROTINA DE ENVIO DE RELATORIO VIA EMAIL
      *=================================================================

           IF W01-SCA-HOSTNAME = "DTPRJCV1"
              PERFORM 5000-EMAIL-CV1 THRU 5000-FIM-EMAIL-CV1
              GO TO INIC-FIM.

           CHANGE ATTRIBUTE LIBACCESS OF "EMAILSUPPORT" TO BYFUNCTION.
           MOVE SPACES TO EMAIL.
           STRING "SEND: BODY ", NM-ANEXO,
                  ", FROM PSUBCPG140UNI@dataprev.gov.br",
      **             " adriano.carneiro@dataprev.gov.br",
                " luciana.Dutra@dataprev.gov.br",
      *          " ricardo.cabral@dataprev.gov.br",
                   ",jose.mfonseca@dataprev.gov.br",
               ",anderson.brito@dataprev.gov.br",
               ",alexandre.gregorio@dataprev.gov.br",
               ",nilson.lima@dataprev.gov.br",
               ",claudio.marques@dataprev.gov.br",
          "//LSUBCPG140UNI- Relatorio de arquivos do PAB para o "
          "Sispagben//"
                  DELIMITED BY SIZE INTO EMAIL.
           MOVE 400 TO W77-MAIL-TAMANHO.
           CALL "EMAIL_COB IN EMAILSUPPORT" USING EMAIL,
                 W77-MAIL-TAMANHO GIVING W77-MAIL-OK.

           IF W77-MAIL-OK IS NOT EQUAL ZERO
              CHANGE ATTRIBUTE STATUS OF MYSELF TO -1.
      *=================================================================
      *
      *
       INIC-FIM.
      *
           STOP RUN.
      $PAGE
       300-PROCESSAMENTO.

           IF NOT ((NU-SEQ-PROC OF DS-MOV-CRED-ALTER =
                                         WS-NU-ULT-PROC-ALTER-SENDO-PR)
              AND (D2-MOV-CREDITO OF DS-MOV-CRED-ALTER NOT <
                                     WS-DT-ANTES-11-MESES-AMD))
              GO TO 300-LE-999.

           ADD 1 TO WS-LD-CRED-PEND       WS-LIDOS-DISPLAY.

           MOVE 0 TO WS-ACHOU-CREDITO.

           PERFORM ACHA-CREDITO THRU ACHA-CREDITO-FIM.

           IF  ACHOU-CREDITO
               PERFORM GERAR-PAB    THRU GERAR-PAB-FIM.

       300-LE-999.

           FIND NEXT DS-MOV-CRED-ALTER
                ON  EXCEPTION
                    IF  DMSTATUS (NOTFOUND)
                    MOVE 1 TO WS-FIM.

           ADD 1 TO W77-LIDOS-ALTER.
      *GATILHO NU-SEQ
      * COMPUTE NU-SEQ-PROC OF DS-MOV-CRED-ALTER =
      *                                   WS-NU-ULT-PROC-ALTER-SENDO-PR.
      *FIM GATILHO NU-SEQ.
       FIM-300-PROCESSAMENTO. EXIT.

      $PAGE
       ACHA-CREDITO.
      **************
           MOVE ID-OL-SOLIC-CRED OF DS-MOV-CRED-ALTER
                                                 TO WS-ID-OL-SOLIC-CRED
           MOVE NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER
                                                 TO WS-NU-SEQ-SOLIC-CRED
           MOVE D2-SOLIC-CREDITO  OF DS-MOV-CRED-ALTER
                                                 TO WS-D2-SOLIC-CREDITO.
           MOVE NU-NB OF DS-MOV-CRED-ALTER TO W77-NU-NB.

           MOVE ID-LOTE-PREGAO OF DS-MOV-CRED-ALTER
                                                 TO AUX1-ID-LOTE-PREGAO.
           IF IN-CRED-DELETADO OF DS-MOV-CRED-ALTER
              MOVE " TRUE" TO WS-IN-CRED-DELETADO
           ELSE
              MOVE "FALSE" TO WS-IN-CRED-DELETADO.

           MOVE ID-BANCO OF DS-MOV-CRED-ALTER TO WS-ID-BANCO.

           IF IN-VAGO-01 OF DS-MOV-CRED-ALTER
              MOVE " TRUE" TO WS-IN-VAGO-01
           ELSE
              MOVE "FALSE" TO WS-IN-VAGO-01.

      *GATILHO INICIO
      *    IF D2-SOLIC-CREDITO  < WS-DT-ANTES-6-MESES AND
      *                                    D2-ENVIO-BANCO  > 20090702
      *       STRING "SOLIC MENOR QUE 6..ID-SOLIC.= "
      *                                      WS-ID-OL-SOLIC-CRED
      *              "...SEQ= "  WS-NU-SEQ-SOLIC-CRED
      *              "...D2-SOLIC= " WS-D2-SOLIC-CREDITO
      *       DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
      *       CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
      *                                       WS-INTEIRO-ZDISPLAY.
      *GATILHO FIM

           IF (CS-NATUR-CREDITO OF DS-MOV-CRED-ALTER = 6)  AND
              (CS-MOTIVO-SOLIC  OF DS-MOV-CRED-ALTER = 72) AND
              (D2-SOLIC-CREDITO OF DS-MOV-CRED-ALTER = 20110714)
               GO TO ACHA-CREDITO-FIM.

            IF (ID-LOTE-PREGAO OF DS-MOV-CRED-ALTER = 97 OR 98 OR 99)
               OR
               ( (ID-BANCO OF DS-MOV-CRED-ALTER NOT = 998 AND 996 AND
                                                      888)
                  AND (ID-LOTE-PREGAO OF DS-MOV-CRED-ALTER = 00)
                  AND (D2-INI-VALIDADE OF DS-MOV-CRED-ALTER > 20110630)
                  AND (NOT IN-CRED-DELETADO OF DS-MOV-CRED-ALTER)
                  AND (NOT IN-VAGO-01 OF DS-MOV-CRED-ALTER)            )
               MOVE SPACES TO WS-MSG
               STRING "ERRO - CREDITO REPRESADO - NB: " W77-NU-NB,
               ">ID-BANCO: "WS-ID-BANCO,
               ">PREGAO: "AUX1-ID-LOTE-PREGAO,
               ">IN-DELETADO: "WS-IN-CRED-DELETADO,
               ">IN-VAGO-01: "WS-IN-VAGO-01
               DELIMITED BY SIZE INTO WS-MSG
              CALL "ZDISPLAY IN DTP/L1" USING WS-MSG WS-STATUS-BACKUP
               GO TO ACHA-CREDITO-FIM.
      *      MOVE 503 TO NU-SEQ-PROC OF DS-MOV-CRED-ALTER.
      *      MOVE 20190725 TO D2-ENVIO-BANCO

           IF NU-SEQ-PROC OF DS-MOV-CRED-ALTER
                                    EQUAL WS-NU-ULT-PROC-ALTER-SENDO-PR
      *---------> 29/12/2009 TER  IF NOT IN-CRED-DELETADO
           IF NOT IN-CRED-EMPR-CONV
           IF ((IN-CRED-ESPECIAL) OR (NOT IN-PAB-DOMIC-BANC))
      *--> IF D2-SOLIC-CREDITO NOT < WS-DT-ANTES-6-MESES (JA ESTAVA C/*)
           IF D2-ENVIO-BANCO   NOT < WS-DT-ANTES-6-MESES

      * A parir de 05/2023 inibir teste de valor liquido
      *--->     IF VL-LIQUIDO-CRED OF DS-MOV-CRED-ALTER > ZEROS

           IF ID-ORGAO-PAGADOR OF  DS-MOV-CRED-ALTER NOT EQUAL 999991
           MOVE 1 TO WS-ACHOU-CREDITO.

      *
       ACHA-CREDITO-FIM.
      *
           EXIT.
      $PAGE
       GERAR-PAB.

            MOVE VL-LIQUIDO-CRED OF DS-MOV-CRED-ALTER
                                             TO CP01-N-VL-LIQ-CREDITOS.
            MOVE VL-BRUTO-CREDITO OF DS-MOV-CRED-ALTER
                                              TO  CP01-N-VL-BRUTO-CRED.

            IF VL-BRUTO-CREDITO OF DS-MOV-CRED-ALTER EQUAL ZEROS
               ADD 1 TO WS-CONTA-BRUTO-ZERADO.
               MOVE 1 TO                SPGENT-QUANT-CREDITOS.
               MOVE 2 TO                         SPGENT-ID-REG.

            MOVE 0 TO WS-IND
                      WS-VL-RUB-301
                      WS-TOT-BRUTO-2
                      WS-TOT-DESC-2
                      WS-VL-RUB-108
                      WS-TOT-DESCIR-2.

                PERFORM MOV-RUBR THRU MOV-RUBR-FIM
                        VARYING WS-IND FROM 1 BY 1
                                UNTIL WS-IND GREATER
                                      QT-RUBRICA OF DS-MOV-CRED-ALTER.

           PERFORM 2631-TOT-CONTAB THRU 2631-FIM.
      *
      *    VOU GRAVAR O(S) REGISTRO(S) DO ARQUIVO DSUBCPG058 NESTE PONTO
      *    PORQUE NESTE MOMENTO A ROTINA 2631 FOI UTILIZADA PARA TODOS O
      *    OS VALORES. A PROXIMA CHAMADA DA ROTINA 2631 EH APENAS PARA A
      *    RUBRICA 105 SER CONTABILIZADA EM SEPARADO, POR CAUSA DA  CON-
      *    TABILIDADE E NAO INTERESSA AO SCF.
      *
           MOVE ALL "00" TO REG-DSUBCPG999.
           MOVE ALL "00" TO REG-DSUBCPG999A.
           MOVE ALL "00" TO DSUBCPG050-REG.
           MOVE ALL ZEROS       TO DSUBCPG058-REG-SPGENT
           MOVE ZEROS           TO OUTROS-CAMPOS-SPGENT.

      ******************************************************************
      * Rotina necessaria para encontrar a AGENCIA PIONEIRA (nivel 88).
      * Inicio para criar um "OBJETO.AGENCIA.PIONEIRA" (CALL SHARING)
      * Ate o momento o processo "GERADOR DO CREDITO PAB" (DS-...-ALTER)
      * nao esta posicionando IN-CRED-PIONEIRA ( e IN-CRED-CART-SOC ).
      * Desta forma foi recriada a rotina neste programa (encontra-se
      * desativada no P/SUB/CPG/050).
      *
      * www.tutorialdocumenta.com.br 20/Abr/2001
      *
      ******************************************************************
           MOVE 2                 TO SPGENT-ID-REG.
           MOVE ID-PAIS           TO CP50-N-ID-PAIS
                                       SPGENT-ID-PAIS.
           MOVE ID-ORGAO-PAGADOR OF  DS-MOV-CRED-ALTER  TO
                                                CP50-N-ID-ORGAO-PAGADOR.

           MOVE NU-NB OF DS-MOV-CRED-ALTER             TO CP50-N-NU-NB.

           IF   ID-ORGAO-PAGADOR OF  DS-MOV-CRED-ALTER
                NOT EQUAL WS-ID-OP-ANT
                MOVE ID-ORGAO-PAGADOR OF  DS-MOV-CRED-ALTER
                                                  TO       WS-ID-OP-ANT
                PERFORM 2100-PESQ-DADOS-OP THRU 2100-FIM.

           IF   AGENCIA-PIONEIRA
                MOVE 1            TO CP50-N-IN-CRED-PIONEIRA
                                      SPGENT-IN-CRED-PIONEIRA
           ELSE
                MOVE 0            TO CP50-N-IN-CRED-PIONEIRA
                                      SPGENT-IN-CRED-PIONEIRA.

      ******************************************************************
      *Incluido em 15/12/2016. Busca do CS-TRATAMENTO para o EPU.
      *Alterado em 30/12/2016. Nao procura se ESPECIE 72 - Peculio.

           IF IN-CREDITO-EPU OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-CREDITO-EPU
           ELSE
              MOVE ZERO TO AUX1-IN-CREDITO-EPU.

           IF CS-ESPECIE OF DS-MOV-CRED-ALTER NOT EQUAL 68
              FIND ST-BENEF-NB AT NU-NB OF DS-BENEFICIO =
                               NU-NB OF DS-MOV-CRED-ALTER
              ON EXCEPTION
                 IF DMSTATUS (NOTFOUND)
                   MOVE SPACES TO WS-MSG
                   MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-CONTADOR-X
                   STRING  "NB DO ALTER NAO ENCONTRADO DS-BENEFICIO = "
                            WS-CONTADOR-X
                           DELIMITED BY SIZE INTO WS-MSG
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                                   WS-STATUS-BACKUP
                    CALL SYSTEM DMTERMINATE.

           IF  CS-ESPECIE OF DS-MOV-CRED-ALTER NOT EQUAL 68
               MOVE  CS-TRATAMENTO OF DS-BENEFICIO TO
                                                     AUX1-CS-TRATAMENTO.

      **************************************************************
      *Fim inclusao do CS-TRATAMENTO.

           MOVE CS-MEIO-PAGTO OF DS-MOV-CRED-ALTER
                                  TO CP50-N-CS-MEIO-PAGTO
                                     SPGENT-CS-MEIO-PAGTO.

           MOVE CS-ORIGEM-CREDITO OF DS-MOV-CRED-ALTER
                                           TO CP50-N-CS-ORIGEM-CREDITO.

           MOVE D2-INI-VALIDADE OF DS-MOV-CRED-ALTER
                                      TO CP50-N-DT-INI-VALIDADE
                                      SPGENT-DT-INI-VALIDADE.

           MOVE D2-FIM-VALIDADE OF DS-MOV-CRED-ALTER
                                           TO AUX1-DT-FIM-VALIDADE.

           IF CP50-N-DT-INI-VALIDADE=20080126 OR
              CP50-N-DT-INI-VALIDADE=20080127
                COMPUTE CP50-N-DT-INI-VALIDADE=20080128.

           MOVE ID-BANCO OF DS-MOV-CRED-ALTER        TO CP50-N-ID-BANCO
                                       SPGENT-ID-BANCO.
           IF   IN-CRED-CART-SOC OF DS-MOV-CRED-ALTER
                MOVE 1            TO CP50-N-IN-CREDITO-SEAS
           ELSE
                MOVE 0            TO CP50-N-IN-CREDITO-SEAS.

           MOVE CS-RAMO-ATIVIDADE OF DS-MOV-CRED-ALTER
                                             TO AUX1-CS-RAMO-ATIVIDADE.

           MOVE CS-ESPECIE OF DS-MOV-CRED-ALTER TO WS-ESPECIES-LOAS
                                                   SPGENT-CS-ESPECIE.

           MOVE CS-ESPECIE OF DS-MOV-CRED-ALTER
                                TO CP50-N-CS-ESPECIE  WS-ESPECIES-LOAS.

           MOVE TAB-GR-ESP(CP50-N-CS-ESPECIE) TO CP50-N-GR-ESPECIES.

           IF CP50-N-CS-MEIO-PAGTO = 2
              MOVE 0                      TO CP50-N-IN-CRED-PIONEIRA.

           MOVE ID-PREGAO OF DS-MOV-CRED-ALTER TO CP50-N-ID-PREGAO.

           MOVE ID-LOTE-PREGAO OF DS-MOV-CRED-ALTER TO CP50-N-ID-LOTE.

           MOVE WS-ID-MICRO-REG-TAB TO CP50-N-ID-MICRO-REG-PREV.

      *13o. SALARIO.
           IF IN-ADIANT-13-SAL OF DS-MOV-CRED-ALTER
              MOVE 1 TO SPGENT-IN-ADIANTE-13-SAL
           ELSE
              MOVE 0 TO SPGENT-IN-ADIANTE-13-SAL.

      *NOVO    FALTA CRIAR NO DS-MOV-CRED-ALTER ID-CALAMIDADE
      *NOVO SO EXISTE NO DS-MOV-CRED-ALTER
      *****MOVE ID-PAIS       OF DS-MOV-CRED-ALTER TO SPGENT-ID-PAIS.
      **   MOVE ID-CALAMIDADE OF DS-MOV-CRED-ALTER TO
      ***                      WS-SPGENT-COD-CALAMIDADE.


      *Nova classificacao para atender exigencias do leilao da folha.
      *-------------------------------------------------------------

      *Assume os valores como sendo orcamento INSS e nao eh rede-ect.

           MOVE 0 TO CP50-N-CS-REDE-ECT
                     SPGENT-IN-CRED-EPU.
           MOVE 1 TO CP50-N-CS-ORIG-ORC
                     SPGENT-CS-ORIG-ORC.

      *Sispagben------------------------------------------------
      *Identifica a origem orcamentaria e se eh rede-ect-misto.

           IF ( ESPECIES-LOAS ) OR  (ESPECIES-RMV)
      *E LOAS.
               MOVE 3 TO CP50-N-CS-ORIG-ORC
                          SPGENT-CS-ORIG-ORC
           ELSE
               IF  AUX1-IN-CREDITO-EPU = 1
      *E EPU.
                  MOVE 2 TO CP50-N-CS-ORIG-ORC
                            SPGENT-CS-ORIG-ORC
                  IF AUX1-CS-TRAT-RFFSA
      *E REDE.
                     MOVE 1 TO SPGENT-CS-ORIG-ORC
                     MOVE 1 TO CP50-N-CS-REDE-ECT
                                SPGENT-IN-CRED-EPU
                  ELSE
                     IF AUX1-CS-TRAT-ECT
      *E ECT
                        MOVE 1 TO SPGENT-CS-ORIG-ORC
                        MOVE 2 TO CP50-N-CS-REDE-ECT
                                  SPGENT-IN-CRED-EPU
                     ELSE
      *********                  IF AUX1-CS-TRAT-MISTO
      *E MISTO
      *E Tratamento MISTO ou possui complemento EPU.
      *Em 14/11/2017 - E misto ou possui complemento EPU informado
      * pelo INSS.
                           MOVE 1 TO SPGENT-CS-ORIG-ORC
                           MOVE 3 TO CP50-N-CS-REDE-ECT
                                     SPGENT-IN-CRED-EPU.

           IF        ESPECIES-EPU
              MOVE 2 TO SPGENT-CS-ORIG-ORC
              MOVE 4 TO SPGENT-IN-CRED-EPU.

      *Em 22/10/2019, tratamento 82 e especie 60, ZIKA. Tipo EPU=5.
           IF CS-ESPECIE OF DS-MOV-CRED-ALTER  = 60
              AND AUX1-CS-TRAT-ZIKA
              MOVE 2 TO SPGENT-CS-ORIG-ORC
              MOVE 5 TO SPGENT-IN-CRED-EPU
              ADD 1 TO GRAVADOS-ZIKA.

      *Pagamentos do Mercosul.
           IF CS-ESPECIE OF DS-MOV-CRED-ALTER = 17
              MOVE 4 TO CP50-N-CS-ORIG-ORC
                         SPGENT-CS-ORIG-ORC
              MOVE 0 TO CP50-N-CS-REDE-ECT
                        SPGENT-IN-CRED-EPU.

      *Verifica se eh urbano ou rural.
           MOVE CS-RAMO-ATIVIDADE OF DS-MOV-CRED-ALTER
                                              TO AUX1-CS-RAMO-ATIVIDADE.

              IF AUX1-RURAL
                 MOVE 2 TO CP50-N-CS-URBANO-RURAL
                                  WS-URBANO-RURAL
                                  AUX1-URBANO-RURAL
              ELSE
                 MOVE 1 TO CP50-N-CS-URBANO-RURAL
                                  WS-URBANO-RURAL
                                AUX1-URBANO-RURAL.

           IF CP50-N-CS-ORIG-ORC NOT EQUAL 1
              MOVE ZEROS TO CP50-N-CS-URBANO-RURAL
                                  WS-URBANO-RURAL.

            MOVE CP01-N-VL-LIQ-CREDITOS TO SPGENT-VALOR-LIQUIDO.
            MOVE CP01-N-VL-BRUTO-CRED TO SPGENT-VALOR-BRUTO.

      *------------------------------------------------------------*
          PERFORM 9590-CARREGA-RUBRICAS THRU 9590-FIM-CARREGA-RUBRICAS.

      *    IF AUX1-CS-ORIG-ORC = 3 OR 4
      *       MOVE 0 TO AUX1-URBANO-RURAL

           MOVE ZEROS TO  WS-PERC-PREV  WS-PERC-UNIAO    W77-ERRO.

           PERFORM 9800-CALC-EPU-PREV THRU 9800-FIM-CALC-EPU-PREV.

      *     IF W77-IND-301 EQUAL ZEROS OR W77-IND-301 GREATER THAN 65
      *        MOVE SPACES TO WS-PARAM1-ZDISPLAY
      *       MOVE D2-FIM-PERIODO OF DS-MOV-CRED-ALTER TO W77-D2-FIM-PER
      *        MOVE 1 TO W77-ERRO
      *        STRING "ERRO NA W77-IND-301 = " W77-IND-301
      *               " - NU-NB = "SPGENT-NB " D2-PER = "W77-D2-FIM-PER
      *            DELIMITED BY SIZE
      *                INTO WS-PARAM1-ZDISPLAY
      *            CALL "ZDISPLAY IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
      *                                        WS-INTEIRO-ZDISPLAY.

      *     IF WS-VL-RUB-301 > ZEROS AND W77-ERRO = 0
      *        MOVE WS-VL-RUB-301 TO TB-VL-RUBRICA(W77-IND-301).

      *------------------------------------------------------------*
      *Rotina acrescentada devido ao novo SCF.

           IF ( ESPECIES-LOAS ) OR  (ESPECIES-RMV)
              MOVE 1 TO CP50-N-QTDE-CREDITOS
              MOVE CP01-N-VL-LIQ-CREDITOS TO CP50-N-VL-LIQ-CREDITOS
      *Verifica se existe a rubrica de salario familia(105).
              PERFORM 4500-VERIFICA-SAL-FAM THRU
                      4500-FIM-VERIFICA-SAL-FAM
           ELSE
                 IF WS-VL-RUB-301 NOT < CP50-N-VL-LIQ-CREDITOS
                    MOVE 1 TO CP50-N-QTDE-CREDITOS
                    MOVE CP01-N-VL-LIQ-CREDITOS TO
                                             CP50-N-VL-LIQ-CREDITOS
      *Verifica se existe a rubrica de salario familia(105).
                    PERFORM 4500-VERIFICA-SAL-FAM THRU
                                          4500-FIM-VERIFICA-SAL-FAM
                    PERFORM VERIFICA-RUBR-IR THRU FIM-VERIFICA-RUBR-IR
                 ELSE
                    IF   WS-VL-RUB-301 > 0,00
      *Sera criado dois registros: um EPU (origem orcamentaria=2)...
      * LINHA NOVA       MOVE 2 TO CP50-N-CS-ORIG-ORC
                         MOVE 2 TO CP50-N-CS-ORIG-ORC
      * LINHA MUDADA     MOVE 1 TO CP50-N-QTDE-CREDITOS
                         MOVE 0 TO CP50-N-QTDE-CREDITOS
      *FIM INCLUSAO QUI 08/09/2011
                         MOVE 0 TO CP50-N-CS-URBANO-RURAL
                         COMPUTE CP50-N-VL-LIQ-CREDITOS =
                                                           WS-VL-RUB-301
                         COMPUTE CP50-N-VL-IR =
                                       WS-IR-UNIAO-2 - WS-RUB-108-UNIAO
                         PERFORM VERIFICA-VALOR  THRU FIM-VERIFICA
      *
                 ADD CP50-N-QTDE-CREDITOS TO WS-CP50-N-QTDE-CREDITOS
                 ADD CP50-N-VL-LIQ-CREDITOS TO
                  WS-CP50-N-VL-LIQ-CREDITOS
      *
                         PERFORM DECIDE-DSUBCPG050 THRU
                                 DECIDE-DSUBCPG050-EXIT
      *e outro INSS (origem orcamentaria=1).
                         MOVE 1 TO CP50-N-CS-ORIG-ORC
                         MOVE 0 TO CP50-N-CS-REDE-ECT
                         MOVE 1  TO CP50-N-QTDE-CREDITOS
                         MOVE WS-URBANO-RURAL  TO CP50-N-CS-URBANO-RURAL
                         COMPUTE CP50-N-VL-LIQ-CREDITOS =
                              CP01-N-VL-LIQ-CREDITOS - WS-VL-RUB-301
                          COMPUTE CP50-N-VL-IR =
                                       WS-IR-UNIAO-2 - WS-RUB-108-PREV
                         PERFORM VERIFICA-VALOR THRU FIM-VERIFICA
                         COMPUTE WS-TOT-PARCIAL=
                              CP01-N-VL-LIQ-CREDITOS - WS-VL-RUB-301 -
                                                      WS-BRUTO-RUB-105
                         IF ( WS-BRUTO-RUB-105 > ZEROS ) AND
                                              ( WS-TOT-PARCIAL > ZEROS )
                            COMPUTE CP50-N-VL-LIQ-CREDITOS =
                                 CP01-N-VL-LIQ-CREDITOS - WS-VL-RUB-301
                                                     - WS-BRUTO-RUB-105
      *
                 ADD CP50-N-QTDE-CREDITOS TO WS-CP50-N-QTDE-CREDITOS
                 ADD CP50-N-VL-LIQ-CREDITOS TO
                  WS-CP50-N-VL-LIQ-CREDITOS
      *
                             PERFORM DECIDE-DSUBCPG050 THRU
                                     DECIDE-DSUBCPG050-EXIT
                             MOVE 0 TO CP50-N-QTDE-CREDITOS
                             MOVE 1 TO CP50-N-ID-SAL-FAMILIA
                             MOVE  WS-BRUTO-RUB-105 TO
                                            CP50-N-VL-LIQ-CREDITOS
      *
                 ADD CP50-N-QTDE-CREDITOS TO WS-CP50-N-QTDE-CREDITOS
                 ADD CP50-N-VL-LIQ-CREDITOS TO
                  WS-CP50-N-VL-LIQ-CREDITOS
      *
                             PERFORM DECIDE-DSUBCPG050 THRU
                                     DECIDE-DSUBCPG050-EXIT
                             ADD 1 TO WS-REGS-058-SAL-FAMILIA
                         ELSE
      *
                 ADD CP50-N-QTDE-CREDITOS TO WS-CP50-N-QTDE-CREDITOS
                 ADD CP50-N-VL-LIQ-CREDITOS TO
                  WS-CP50-N-VL-LIQ-CREDITOS
      *
                            PERFORM DECIDE-DSUBCPG050 THRU
                                    DECIDE-DSUBCPG050-EXIT
                    ELSE
      *Se nao existe rubrica 301, a origem orcamentaria eh = 1, INSS.
                         MOVE 1 TO CP50-N-CS-ORIG-ORC
      *INCLUIDA          MOVE 0 TO CP50-N-CS-REDE-ECT  QUI 08/09/2011
                         MOVE 0 TO CP50-N-CS-REDE-ECT
                         MOVE 1  TO CP50-N-QTDE-CREDITOS
                         COMPUTE CP50-N-VL-LIQ-CREDITOS =
                                            CP01-N-VL-LIQ-CREDITOS
      *INCLUIDA EM QUI 08/09/2011
                         PERFORM 4600-VERIFICA-URB-RURAL THRU
                                 4600-FIM-VERIFICA-URB-RURAL
                         MOVE WS-URBANO-RURAL TO CP50-N-CS-URBANO-RURAL
      *FIM INCLUIDA EM QUI 08/09/2011
      *Verifica se existe a rubrica de salario familia(105).
                    PERFORM 4500-VERIFICA-SAL-FAM THRU
                                          4500-FIM-VERIFICA-SAL-FAM

                         PERFORM VERIFICA-RUBR-IR THRU
                                                   FIM-VERIFICA-RUBR-IR.

      *FIM DA BOOK B/SUB/CPG/050 PROC

           PERFORM 9700-MOV-RUBR-GRAVA THRU 9700-FIM-MOV-RUBR-GRAVA.

       GERAR-PAB-FIM.
           EXIT.

       2631-TOT-CONTAB.
           IF WS-VL-RUB-301 > WS-TOT-BRUTO-2
              MOVE WS-TOT-BRUTO-2 TO WS-VL-RUB-301.

           MOVE CS-ESPECIE OF DS-MOV-CRED-ALTER TO WS-ESPECIES-LOAS
           IF (CS-ESPECIE OF DS-MOV-CRED-ALTER = 89
               OR 54
               OR ESPECIES-LOAS)
               AND WS-VL-RUB-301 NOT = WS-TOT-BRUTO-2
               MOVE WS-TOT-BRUTO-2 TO WS-VL-RUB-301.


           IF WS-VL-RUB-301 = ZEROS
              MOVE 100   TO WS-PERC-PREV-2
           ELSE
              COMPUTE WS-PERC-PREV-2 = 100 -
                     (WS-VL-RUB-301 / WS-TOT-BRUTO-2 * 100).

           MOVE ZEROS TO WS-IR-PREV-2, WS-RUB-108-PREV,
                         WS-RUB-108-UNIAO, WS-IR-UNIAO-2.

           COMPUTE WS-IR-PREV-2 =
              ( WS-TOT-DESCIR-2 * WS-PERC-PREV-2 ) / 100.

           IF WS-PERC-PREV-2 NOT = 100,00000
              COMPUTE WS-IR-UNIAO-2 =
                WS-TOT-DESCIR-2 - WS-IR-PREV-2.

           COMPUTE WS-RUB-108-PREV =
                                 (WS-VL-RUB-108 * WS-PERC-PREV-2)/100.

           COMPUTE WS-RUB-108-UNIAO = (WS-VL-RUB-108 - WS-RUB-108-PREV).


       2631-FIM. EXIT.
      $PAGE
       MOV-RUBR.

           IF CS-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND)
                                               GREATER 100 AND LESS 151
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND)
                                                      TO WS-TOT-BRUTO-2.

           IF CS-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND) EQUAL 108
              MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND)
                                                       TO WS-VL-RUB-108.

           IF CS-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND)
                                                GREATER 200 AND LESS 215
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND)
                                                       TO WS-TOT-DESC-2.
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND)
              EQUAL 201 OR 204 OR 205 OR 207 OR 209 OR 211
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND)
                                                    TO WS-TOT-DESCIR-2.
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND) EQUAL 301
              MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND)
                                                       TO WS-VL-RUB-301.

           IF CS-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND) EQUAL 105
              MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER(WS-IND)
                                                    TO WS-BRUTO-RUB-105.

       MOV-RUBR-FIM.
           EXIT.

       2100-PESQ-DADOS-OP.
      *------------------*
      *
           MOVE ZEROS TO WS-ID-MICRO-REG-TAB
                         WS-CS-MUNIC-SINPAS-TAB
                         WS-TIPO-MICRO-REG.
      *
           MOVE WS-ID-OP-ANT TO TB00233-CHAVE.
           CALL "SETTB00233 IN DTP/LBD/TAB" USING REGISTRO-TB00233,
                  CODE-LAYOUT-TB00233, REGISTRO-TB00233-R
                                            GIVING RETORNO-TB00233.
      *
           IF RETORNO-TB00233 = 0
              MOVE ID-MICRO-REG-PREV OF REGISTRO-TB00233-R
                                     TO WS-ID-MICRO-REG-TAB
                                        TB00494-CHAVE
              MOVE CS-MUNI-SINPAS    OF REGISTRO-TB00233-R
                                     TO WS-CS-MUNIC-SINPAS-TAB
              CALL "SETTB00494 IN DTP/LBD/TAB" USING REGISTRO-TB00494,
                       CODE-LAYOUT-TB00494, REGISTRO-TB00494-R
                                               GIVING RETORNO-TB00494
              IF RETORNO-TB00494 = 0
                 MOVE CS-TIPO-MICRO-REG OF REGISTRO-TB00494-R
                   TO WS-TIPO-MICRO-REG
              ELSE
                 MOVE "MR NAO ENCONTRADA NA TB0049: "
                   TO  WS-PARAM1-ZDISPLAY
                 MOVE  WS-ID-MICRO-REG-TAB
                   TO  WS-PARAM2-ZDISPLAY
                 CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                                  WS-PARAM2-ZDISPLAY,
                                                  WS-INTEIRO-ZDISPLAY
                 ADD 1 TO WS-MRS-ERRO-TB0049
           ELSE
              MOVE 010000 TO WS-ID-MICRO-REG-TAB
              MOVE 01000  TO WS-CS-MUNIC-SINPAS-TAB
              MOVE "OP NAO ENCONTRADO NA TB0023: " TO WS-PARAM1-ZDISPLAY
              MOVE  WS-ID-OP-ANT                   TO WS-PARAM2-ZDISPLAY
              CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
              ADD 1 TO WS-OPS-ERRO-TB0023
              GO 2100-FIM.
      *
       2100-FIM.  EXIT.
      *
      /
      *
       3320-IMP-ENT-CLASSES.
      *----------------------

           OPEN OUTPUT LSUBCPG058SPGENT.

           MOVE WS-DATA-SIS-INV              TO LSPG1A-CAB1-DATA.
           MOVE CORR WS-HH-INTR              TO LSPG1A-CAB2-HORA.
           MOVE 0001                         TO LSPG1A-CAB3-PAG.
           MOVE "ALTE"                       TO LSPG1A-CAB3-MOV.
      *     WRITE LSUBSPGENT-REG         FROM  LSPG1A-SPACES.

           WRITE LSUBSPGENT-REG         FROM LSPG1A-SUB     AFTER 1.

           WRITE LSUBSPGENT-REG         FROM LSPG1A-CAB1    AFTER 1.

           WRITE LSUBSPGENT-REG         FROM LSPG1A-CAB2    AFTER 1.

           WRITE LSUBSPGENT-REG         FROM LSPG1A-CAB3    AFTER 1.

           WRITE LSUBSPGENT-REG         FROM LSPG1A-SUB     AFTER 1.

           WRITE LSUBSPGENT-REG         FROM LSPG-CAB6   AFTER 3.
           WRITE LSUBSPGENT-REG         FROM LSPG-CAB7   AFTER 1.
      *
      *    DETALHE
      *
           MOVE ZEROS TO WS-QT-AUX
                         WS-VL-AUX.

           PERFORM 3320-IMP-VALORES-DET-SPG
              THRU 3320-FIM-IMP
           VARYING WS-IND FROM 1 BY 1
             UNTIL WS-IND > 38.

           WRITE   LSUBSPGENT-REG FROM LSPG-CAB7 AFTER 1.

       3320-FIM. EXIT.
      *------------------------------
       3320-IMP-VALORES-DET-SPG.
      *------------------------------

           MOVE  TAB-NM-ENTIDADE(WS-IND)     TO DESC-DET-SPG.

           MOVE TAB-CS-RUBR-ENT(WS-IND)          TO CS-RUBR-ENT

           MOVE  TB-QT-TOTAL-ENT(WS-IND)     TO QT-DET-SPG.

           MOVE  TB-VL-TOTAL-RUB-ENT(WS-IND) TO  VL-DET-SPG.

           WRITE LSUBSPGENT-REG  FROM LSPG-DET   AFTER 1.

      *------------------------------
       3320-FIM-IMP.
       EXIT.
      *------------------------------

      *-----------------------------------*
       VERIFICA-RUBR-IR.
      *-----------------------------------*

           COMPUTE CP50-N-VL-IR = WS-TOT-DESCIR-2 - WS-VL-RUB-108.

           IF CP50-N-VL-IR = ZEROS
              MOVE 0 TO CP50-N-QTDE-IR
           ELSE
              MOVE 1 TO CP50-N-QTDE-IR.

      *-----------------------------------*
       FIM-VERIFICA-RUBR-IR. EXIT.
      *-----------------------------------*
      *-----------------------------------*
       VERIFICA-VALOR.
      *-----------------------------------*

        IF CP50-N-VL-IR  > 0
           MOVE 1 TO CP50-N-QTDE-IR
        ELSE
           MOVE 0 TO CP50-N-QTDE-IR.

      *-----------------------------------*
       FIM-VERIFICA.
           EXIT.
       4500-VERIFICA-SAL-FAM.
      *-----------------------------------*

            COMPUTE WS-TOT-PARCIAL=
              CP01-N-VL-LIQ-CREDITOS - WS-VL-RUB-301 - WS-BRUTO-RUB-105.
              IF ( WS-BRUTO-RUB-105 > ZEROS ) AND
                 ( WS-IN-EMP-PREGAO = ZEROS ) AND
                 ( WS-TOT-PARCIAL > ZEROS )
                 COMPUTE CP50-N-VL-LIQ-CREDITOS =
                         CP01-N-VL-LIQ-CREDITOS - WS-BRUTO-RUB-105
      *
                 ADD CP50-N-QTDE-CREDITOS TO WS-CP50-N-QTDE-CREDITOS
                 ADD CP50-N-VL-LIQ-CREDITOS TO
                  WS-CP50-N-VL-LIQ-CREDITOS
      *
                 PERFORM DECIDE-DSUBCPG050 THRU
                         DECIDE-DSUBCPG050-EXIT
                 MOVE 0 TO CP50-N-QTDE-CREDITOS
                 MOVE 1 TO CP50-N-ID-SAL-FAMILIA
                 MOVE  WS-BRUTO-RUB-105 TO CP50-N-VL-LIQ-CREDITOS
      *
                 ADD CP50-N-QTDE-CREDITOS TO WS-CP50-N-QTDE-CREDITOS
                 ADD CP50-N-VL-LIQ-CREDITOS TO
                  WS-CP50-N-VL-LIQ-CREDITOS
      *
                 PERFORM DECIDE-DSUBCPG050 THRU
                         DECIDE-DSUBCPG050-EXIT
                 ADD 1 TO WS-REGS-058-SAL-FAMILIA
              ELSE
      *
                 ADD CP50-N-QTDE-CREDITOS TO WS-CP50-N-QTDE-CREDITOS
                 ADD CP50-N-VL-LIQ-CREDITOS TO
                  WS-CP50-N-VL-LIQ-CREDITOS
      *
                 PERFORM DECIDE-DSUBCPG050 THRU
                         DECIDE-DSUBCPG050-EXIT.

      *-----------------------------------*
       4500-FIM-VERIFICA-SAL-FAM. EXIT.
      *-----------------------------------*

       DECIDE-DSUBCPG050.

           IF IN-CRED-ESPECIAL
              MOVE 1 TO CP50-N-CS-ORIGEM-CREDITO
                        AUX1-CRED-ESPECIAL
              IF AUX1-DT-FIM-VALIDADE NOT GREATER THAN 20150227
                  WRITE REG-DSUBCPG999A   FROM DSUBCPG050-REG
                  ADD 1 TO WS-REGS-058-NOVO-ESP
               ELSE
                  NEXT SENTENCE
           ELSE
              IF AUX1-DT-FIM-VALIDADE NOT GREATER THAN 20150227
                 WRITE REG-DSUBCPG999 FROM DSUBCPG050-REG
                 ADD 1 TO WS-REGS-058-NOVO.

       DECIDE-DSUBCPG050-EXIT.
           EXIT.
      *INCLUIDA EM QUI 08/09/2011
      *---------------------------------------------*
       4600-VERIFICA-URB-RURAL.
      *---------------------------------------------*

      *Verifica se eh urbano ou rural.
           IF CP50-N-CS-ORIG-ORC NOT EQUAL 1
              MOVE 0 TO CP50-N-CS-URBANO-RURAL
                        WS-URBANO-RURAL
           ELSE
              IF AUX1-RURAL
                 MOVE 2 TO CP50-N-CS-URBANO-RURAL
                           WS-URBANO-RURAL
              ELSE
                 MOVE 1 TO CP50-N-CS-URBANO-RURAL
                           WS-URBANO-RURAL.

      *---------------------------------------------*
       4600-FIM-VERIFICA-URB-RURAL. EXIT.
      *---------------------------------------------*
      *---------------------------------------------*
       7000-LIMPA-ENT.
      *---------------------------------------------*

           MOVE ZEROS TO TB-VL-RUB-ENT(WS-IND).

      *---------------------------------------------*
       7000-LIMPA-ENT-EXIT.
            EXIT.
      *---------------------------------------------*

      *------------------------------------------------------------*
       9550-LIMPA-RUB-H.
      *-------------------------------------------------------------*

           MOVE ZEROS TO SPGENT-CS-RUBRICA-H ( W77-IND ).
           MOVE ZEROS TO WS-TODAS-RUBR ( W77-IND).
           MOVE ZEROS TO WS-OUTRAS-RUBR (W77-IND).

      *-------------------------------------------------------------*
       9550-LIMPA-RUB-H-EXIT. EXIT.
      *-------------------------------------------------------------*

      *------------------------------------------------------------*
       9550-LIMPA-TAB-RUBR.
      *-------------------------------------------------------------*

           MOVE ZEROS TO TB-VL-RUBRICA(W77-IND).

      *-------------------------------------------------------------*
       9550-FIM-LIMPA-TAB-RUBR. EXIT.
      *-------------------------------------------------------------*

      *------------------------------------------------------------*
       9560-LIMPA-CS-RUB-ARQ.
      *-------------------------------------------------------------*

           MOVE ZEROS TO SPGENT-CS-RUB(W77-IND).

      *------------------------------------------------------------*
       9560-FIM-LIMPA-CS-RUB-ARQ. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       9590-CARREGA-RUBRICAS.
      *-------------------------------------------------------------*

           PERFORM 7000-LIMPA-ENT THRU
                   7000-LIMPA-ENT-EXIT
             VARYING WS-IND FROM 1 BY 1
               UNTIL WS-IND > 38.

           PERFORM 9550-LIMPA-TAB-RUBR THRU 9550-FIM-LIMPA-TAB-RUBR
                   VARYING W77-IND FROM 1 BY 1 UNTIL  W77-IND > 95.
      * A partir de 21/09/2021, so cabem 50 rubricas na descricao para
      * homologacao.
           PERFORM 9560-LIMPA-CS-RUB-ARQ THRU 9560-FIM-LIMPA-CS-RUB-ARQ
                   VARYING W77-IND FROM 1 BY 1 UNTIL  W77-IND > 30.
           MOVE 1 TO W77-TOT-RUBR-CRED.
           MOVE 1 TO W77-IND-RUB.
           MOVE ZEROS TO WS-VL-RUB-301.
           MOVE ZEROS TO WS-ACHOU-RUBRICA.
           MOVE ZEROS TO WS-TOT-BRUTO.
           MOVE ZEROS TO SPGENT-MARCA-CALAM
           MOVE ZEROS TO SPGENT-MARCA-MP87519.
           MOVE ZEROS TO SPGENT-MARCA-R329.

           PERFORM 9600-CARGA-INICIAL THRU 9600-FIM-CARGA-INICIAL
                   VARYING W77-IND FROM 1 BY 1
                   UNTIL  W77-IND > QT-RUBRICA OF DS-MOV-CRED-ALTER.

            COMPUTE SPGENT-OCOR-RUB = W77-IND-RUB - 1.

            IF W77-TOT-RUBR-CRED > W77-TOT-RUBRICAS
               MOVE W77-TOT-RUBR-CRED TO W77-TOT-RUBRICAS.

      *-------------------------------------------------------------*
       9590-FIM-CARREGA-RUBRICAS. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       9600-CARGA-INICIAL.
      *-------------------------------------------------------------*

          MOVE ZEROS TO WS-ACHOU-RUBRICA.

          MOVE CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND)
                                                      TO WS-DESCR-RUBR
                                         SPGENT-CS-RUB(W77-IND-RUB).
      *
      *Jose Fonseca - 20/03/2019.
      *Calamidade de Brumadinho - MG. MP 875/19.
           IF WS-DESCR-RUBR = 328
              MOVE 154 TO WS-DESCR-RUBR
              MOVE 1 TO SPGENT-MARCA-MP87519.

      *
      * Jose Fonseca - 25/07/2019.
      * Atrasados Talidomida - DM.080880.
          IF WS-DESCR-RUBR = 329
             MOVE 1 TO SPGENT-MARCA-R329.

          IF  WS-CS-RUBR
              PERFORM 9650-MOVE-TO-TAB THRU 9650-FIM-MOVE-TO-TAB
                      VARYING W77-IND-TAB FROM 1 BY 1
                      UNTIL W77-IND-TAB > 95

              ADD 1 TO W77-TOT-RUBR-CRED

              PERFORM 9640-MOVE-RUBR-H-INI THRU 9640-FIM-MOVE-RUBR-H-INI
                      VARYING W77-IND-H FROM 1 BY 1
                      UNTIL W77-IND-H > WS-QUAIS-RUBR
                      OR WS-ACHOU-RUBRICA EQUAL 1

              IF WS-ACHOU-RUBRICA EQUAL ZEROS
                 MOVE CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND) TO
                                           WS-TODAS-RUBR(WS-QUAIS-RUBR)
                 ADD 1 TO WS-QUAIS-RUBR
              ELSE
                 NEXT SENTENCE
          ELSE
              IF (WS-DESCR-RUBR > 162 AND WS-DESCR-RUBR < 200)
                 OR (WS-DESCR-RUBR > 274 AND WS-DESCR-RUBR < 300)
                 OR (WS-DESCR-RUBR > 900 AND WS-DESCR-RUBR > 929)
                 MOVE ZEROS TO WS-ACHOU-RUBRICA
                 PERFORM 9670-MOVE-OUTRAS-H-INI THRU
                                              9670-FIM-MOVE-OUTRAS-H-INI
                      VARYING W77-IND-H FROM 1 BY 1
                      UNTIL W77-IND-H > WS-CONT-OUTRAS
                      OR WS-ACHOU-RUBRICA EQUAL 1
                 IF WS-ACHOU-RUBRICA EQUAL ZEROS
                    MOVE CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND) TO
                                          WS-OUTRAS-RUBR(WS-CONT-OUTRAS)
                    ADD 1 TO WS-CONT-OUTRAS.

          ADD 1 TO W77-IND-RUB.

          IF       CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND)
                    GREATER   99  AND  LESS 200
                    ADD  VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND)
                     TO  WS-TOT-BRUTO
          ELSE
             IF       CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND)
                    GREATER   199  AND  LESS 300
                    ADD  VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND)
                     TO  WS-TOT-DESC
             ELSE
                IF       CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND)
                    EQUAL 301
                    MOVE  VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND)
                      TO  WS-VL-RUB-301.

      *-------------------------------------------------------------*
       9600-FIM-CARGA-INICIAL. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       9640-MOVE-RUBR-H-INI.
      *-------------------------------------------------------------*

           IF  CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND) EQUAL
                                              WS-TODAS-RUBR (W77-IND-H)
               MOVE 1 TO WS-ACHOU-RUBRICA.

      *-------------------------------------------------------------*
       9640-FIM-MOVE-RUBR-H-INI. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       9650-MOVE-TO-TAB.
      *-------------------------------------------------------------*

           IF WS-DESCR-RUBR = TAB-CS-RUBR(W77-IND-TAB)
      *       MOVE 50 TO W77-IND-TAB
              IF WS-DESCR-RUBR = 216 OR 217
                 ADD VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND) TO
                                  TB-VL-RUBRICA(W77-IND-TAB)
              ELSE
                 MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND) TO
                                          TB-VL-RUBRICA(W77-IND-TAB).
      *Em 05/06/2017 - Jose Fonseca.
      *DM.062989 - Reten??o de IR para dep?sito em conta judicial
      *DM.064300 - Reten??o de IR para dep?sito em conta judicial
      *Em 25/08/2017 - Jose Fonseca
      *DM.066820 - Retencao de IR para deposito em conta judicial.
      *Em 15/03/2019 - Jose Fonseca
      *DM.078805- Retencao de IR para deposito em conta judicial.

           IF WS-DESCR-RUBR = TAB-CS-RUBR(W77-IND-TAB)
              IF WS-DESCR-RUBR = 201
                 AND (NU-NB OF DS-MOV-CRED-ALTER = 1180589847
                      OR NU-NB OF DS-MOV-CRED-ALTER = 1625337911
                      OR NU-NB OF DS-MOV-CRED-ALTER = 1594335777
                      OR NU-NB OF DS-MOV-CRED-ALTER = 1619329317)
                MOVE ZEROS TO TB-VL-RUBRICA(W77-IND-TAB).

      * A partir de 23/09/2021, zerar a rubrica 230 - entidade  FNTF.
           IF WS-DESCR-RUBR = TAB-CS-RUBR(W77-IND-TAB)
              IF WS-DESCR-RUBR = 230
                  ADD 1 TO SPGENT-MARCA-R230
                  MOVE ZEROS TO TB-VL-RUBRICA(W77-IND-TAB)
                  MOVE SPGENT-NB TO WS-NB-R230(SPGENT-MARCA-R230).

      * Mover rubricas de entidades de classe - 06/03/2017.
           IF WS-DESCR-RUBR = TAB-CS-RUBR(W77-IND-TAB)
               IF WS-DESCR-RUBR = 219
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(01)
               ELSE
               IF WS-DESCR-RUBR = 220
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(02)
               ELSE
               IF WS-DESCR-RUBR = 221
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(03)
               ELSE
               IF WS-DESCR-RUBR = 222
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(04)
               ELSE
               IF WS-DESCR-RUBR = 223
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(05)
               ELSE
               IF WS-DESCR-RUBR = 224
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(06)
               ELSE
               IF WS-DESCR-RUBR = 225
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(07)
               ELSE
               IF WS-DESCR-RUBR = 226
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(08)
               ELSE
               IF WS-DESCR-RUBR = 228
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(09)
               ELSE
               IF WS-DESCR-RUBR = 229
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(10)
               ELSE
               IF WS-DESCR-RUBR = 230
      * A partir de 23/09/2021, zerar a rubrica 230 - entidade  FNTF.
                  MOVE ZEROS TO TB-VL-RUB-ENT(11)
      *            ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
      *            TB-VL-RUB-ENT(11)
               ELSE
               IF WS-DESCR-RUBR = 234
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(12)
               ELSE
               IF WS-DESCR-RUBR = 239
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(13)
               ELSE
               IF WS-DESCR-RUBR = 240
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(14)
               ELSE
               IF WS-DESCR-RUBR = 241
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(15)
               ELSE
               IF WS-DESCR-RUBR = 242
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(16)
               ELSE
               IF WS-DESCR-RUBR = 243
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(17)
               ELSE
               IF WS-DESCR-RUBR = 244
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(18)
               ELSE
               IF WS-DESCR-RUBR = 245
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(19)
               ELSE
               IF WS-DESCR-RUBR = 246
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(20)
               ELSE
               IF WS-DESCR-RUBR = 247
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(21)
               ELSE
               IF WS-DESCR-RUBR = 248
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(22)
               ELSE
               IF WS-DESCR-RUBR = 249
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(23)
               ELSE
               IF WS-DESCR-RUBR = 250
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(24)
               ELSE
               IF WS-DESCR-RUBR = 254
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(25)
               ELSE
               IF WS-DESCR-RUBR = 256
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(26)
               ELSE
               IF WS-DESCR-RUBR = 257
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(27)
               ELSE
               IF WS-DESCR-RUBR = 258
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(28)
                 ELSE
               IF WS-DESCR-RUBR = 259
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(29)
               ELSE
               IF WS-DESCR-RUBR = 264
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(30)
               ELSE
               IF WS-DESCR-RUBR = 266
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(31)
               ELSE
               IF WS-DESCR-RUBR = 267
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(32)
               ELSE
               IF WS-DESCR-RUBR = 269
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(33)
               ELSE
               IF WS-DESCR-RUBR = 270
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(34)
               ELSE
               IF WS-DESCR-RUBR = 271
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(35)
              ELSE
               IF WS-DESCR-RUBR = 272
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(36)
              ELSE
               IF WS-DESCR-RUBR = 273
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(37)
              ELSE
               IF WS-DESCR-RUBR = 274
                  ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND) TO
                  TB-VL-RUB-ENT(38).

           IF WS-DESCR-RUBR = 274
              MOVE 1 TO WS-TOT-R274.

           IF WS-DESCR-RUBR = 273
              ADD 1 TO WS-TOT-R273.

           IF WS-DESCR-RUBR = 231
              OR WS-DESCR-RUBR = 154
              MOVE 1 TO SPGENT-MARCA-CALAM.

           IF WS-DESCR-RUBR = 301
              MOVE W77-IND-TAB TO W77-IND-301.

           IF WS-DESCR-RUBR = TAB-CS-RUBR(W77-IND-TAB)
              MOVE 95 TO W77-IND-TAB.


      *-------------------------------------------------------------*
       9650-FIM-MOVE-TO-TAB. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       9670-MOVE-OUTRAS-H-INI.
      *-------------------------------------------------------------*


           IF  CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND) EQUAL
                                              WS-OUTRAS-RUBR (W77-IND-H)
               MOVE 1 TO WS-ACHOU-RUBRICA.

      *-------------------------------------------------------------*
       9670-FIM-MOVE-OUTRAS-H-INI. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       9700-MOV-RUBR-GRAVA.
      *-------------------------------------------------------------*

           MOVE ZEROS TO  WS-SPGENT-COD-CALAMIDADE.

           MOVE NU-NB OF DS-MOV-CRED-ALTER TO AUX1-NU-NB.

           IF SPGENT-MARCA-CALAM = 1
              ADD  1 TO WS-TOT-CALAM
              PERFORM 9980-PEGA-COD-CALAM THRU
                       9980-PEGA-COD-CALAM-EXIT
              MOVE ZEROS TO SPGENT-MARCA-CALAM.

           MOVE WS-SPGENT-COD-CALAMIDADE     TO   SPGENT-COD-CALAMIDADE.

           IF SPGENT-CS-ORIG-ORC = 1
              MOVE AUX1-URBANO-RURAL TO SPGENT-CS-URBANO-RURAL
           ELSE
              MOVE ZEROS TO SPGENT-CS-URBANO-RURAL.

           MOVE 1 TO SPGENT-QUANT-CREDITOS.

           PERFORM 9710-MOVE-TAB-RUBR THRU 9710-FIM-MOVE-TAB-RUBR
                    VARYING W77-IND FROM 1 BY 1
                     UNTIL W77-IND > 95.

           MOVE "NUNB-"           TO SPGENT-BRANCO1.
           MOVE NU-NB OF DS-MOV-CRED-ALTER             TO  SPGENT-NB.

           IF IN-CRED-ESPECIAL
              IF SPGENT-MARCA-R329 = 0
                 COMPUTE WS-SPGENT-TOT-VL-LIQ-TA =
                 WS-SPGENT-TOT-VL-LIQ-TA +  SPGENT-VALOR-LIQUIDO
                 WRITE REG-058-SPGENTA FROM DSUBCPG058-REG-SPGENT
                 ADD 1 TO WS-GRAV-SPGENTA
              ELSE
                 WRITE REG-058-SPGTAL FROM DSUBCPG058-REG-SPGENT
                 COMPUTE WS-SPG-TOT-VL-LIQ-TAL =
                 WS-SPG-TOT-VL-LIQ-TAL + SPGENT-VALOR-LIQUIDO
                 ADD 1 TO WS-GRAV-SPGENTAL.

           IF NOT IN-CRED-ESPECIAL
              COMPUTE WS-SPGENT-TOT-VL-LIQ-T = WS-SPGENT-TOT-VL-LIQ-T
                                         + SPGENT-VALOR-LIQUIDO
              WRITE REG-058-SPGENT FROM DSUBCPG058-REG-SPGENT
              ADD 1 TO WS-GRAV-SPGENT.

           PERFORM 9990-VERIFICA-ENT-CLASSE THRU
                   9990-FIM-VERIFICA-ENT-CLASSE
                   VARYING WS-IND-ENT FROM 1 BY 1
                   UNTIL WS-IND-ENT > 38.

      *     IF  WS-VL-RUB-301 > ZEROS
      *         AND NOT (ESPECIES-LOAS OR ESPECIES-RMV)
      *              WRITE REG-058-SPGENT-301 FROM DSUBCPG058-REG-SPGENT
      *              ADD 1 TO GRAVADOS-301.

      *-------------------------------------------------------------*
       9700-FIM-MOV-RUBR-GRAVA. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       9710-MOVE-TAB-RUBR.
      *-------------------------------------------------------------*

           IF SPGENT-MARCA-MP87519 = 1
              MOVE ZEROS TO TB-VL-RUBRICA(54)
              MOVE ZEROS TO SPGENT-MARCA-MP87519.

           MOVE TB-VL-RUBRICA(W77-IND) TO SPGENT-VL-RUBRICA(W77-IND).

      *-------------------------------------------------------------*
       9710-FIM-MOVE-TAB-RUBR. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       9800-CALC-EPU-PREV.
      *-------------------------------------------------------------*

           IF  WS-TOT-BRUTO EQUAL ZEROS
               MOVE SPACES TO WS-PARAM1-ZDISPLAY
               STRING "***VALOR BRUTO ZERADO --> NU-NB = ",
               SPGENT-NB
               DELIMITED BY SIZE INTO WS-PARAM1-ZDISPLAY
               CALL "ZDISPLAY IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                              WS-INTEIRO-ZDISPLAY
               GO TO 9800-FIM-CALC-EPU-PREV.

           IF  WS-VL-RUB-301  GREATER        WS-TOT-BRUTO
               MOVE  WS-TOT-BRUTO      TO  WS-VL-RUB-301.
      *
           IF  ((SPGENT-CS-ESPECIE   EQUAL
                  87 OR 11 OR 30 OR 88 OR 12 OR 40 OR 89 OR 54 OR 98)
                AND (WS-VL-RUB-301     NOT  =   WS-TOT-BRUTO))
                 MOVE  WS-TOT-BRUTO    TO  WS-VL-RUB-301.
      *
           IF    WS-VL-RUB-301         EQUAL   ZEROS
                 MOVE  100              TO  WS-PERC-PREV
           ELSE
                 COMPUTE  WS-PERC-PREV  =    100         -
                      (WS-VL-RUB-301   /   WS-TOT-BRUTO * 100).

           COMPUTE WS-PERC-UNIAO = (WS-VL-RUB-301 / WS-TOT-BRUTO)*100.

      *-------------------------------------------------------------*
       9800-FIM-CALC-EPU-PREV. EXIT.
      *-------------------------------------------------------------*
      *-------------------------------------------------------------*
       9980-PEGA-COD-CALAM.
      *-------------------------------------------------------------*

      *Alteracao provisoria ja que o benef-calam nao esta carregado.
      *
      * GO TO BUSCA-CALAM.
        SET ST-BENEF-CALAM-NB TO BEGINNING.
        MOVE ZEROS TO WS-SPGENT-COD-CALAMIDADE.

        FIND FIRST ST-BENEF-CALAM-NB AT NU-NB OF DS-BENEF-CALAM =
                   AUX1-NU-NB
                ON EXCEPTION
                   IF DMSTATUS(NOTFOUND)
                      MOVE SPACES TO WS-PARAM1-ZDISPLAY
                      MOVE "ERRO FIND ST-BENEF-CALAM ROT. 9980"
                        TO  WS-PARAM1-ZDISPLAY
                      MOVE  AUX1-NU-NB TO  WS-PARAM2-ZDISPLAY
                      CALL "ZDISPLAY2 IN DTP/L1" USING
                                                WS-PARAM1-ZDISPLAY,
                                                WS-PARAM2-ZDISPLAY,
                                                WS-INTEIRO-ZDISPLAY
                      MOVE 999 TO WS-SPGENT-COD-CALAMIDADE
                   ELSE
                      CALL SYSTEM DMTERMINATE.

           IF WS-SPGENT-COD-CALAMIDADE = ZEROS
              MOVE ID-CALAMIDADE TO WS-SPGENT-COD-CALAMIDADE.

           IF SPGENT-MARCA-MP87519 = 1
              MOVE 999 TO WS-SPGENT-COD-CALAMIDADE.

      *-------------------------------------------------------------*
       9980-PEGA-COD-CALAM-EXIT. EXIT.
      *-------------------------------------------------------------*

      *-----------------------------------
       9990-VERIFICA-ENT-CLASSE.
      *-----------------------------------

         IF TB-VL-RUB-ENT(WS-IND-ENT) > 0
            MOVE ZEROS TO SPGENT-QUANT-CREDITOS
            MOVE ZEROS TO SPGENT-VALOR-BRUTO
            MOVE ZEROS TO SPGENT-VALOR-LIQUIDO
            PERFORM 10000-ZERA-VALORES THRU
                    10000-FIM-ZERA-VALORES
                   VARYING WS-IND FROM 1 BY 1
                   UNTIL WS-IND > 95
            PERFORM 10010-CRIA-ENT-CLASSE THRU
                    10010-FIM-CRIA-ENT-CLASSE
         ELSE
            MOVE ZEROS TO SPGENT-ENT-CLASS
            MOVE ZEROS TO SPGENT-QTD-ENT-CLASS.

      *-----------------------------------
        9990-FIM-VERIFICA-ENT-CLASSE. EXIT.
      *-----------------------------------
      *-----------------------------------------------
       10000-ZERA-VALORES.
      *-----------------------------------------------

           MOVE ZEROS TO SPGENT-VL-RUBRICA(WS-IND).

      *-----------------------------------------------
       10000-FIM-ZERA-VALORES. EXIT.
      *-----------------------------------------------
      *------------------------------
       10010-CRIA-ENT-CLASSE.
      *------------------------------

           MOVE  TB-VL-RUB-ENT(WS-IND-ENT) TO
                 SPGENT-VL-RUBRICA(TAB-IND-RUBR(WS-IND-ENT)).

           MOVE TAB-CS-RUBR-ENT(WS-IND-ENT) TO SPGENT-ENT-CLASS.

           ADD 1 TO TB-QT-TOTAL-ENT(WS-IND-ENT).

           ADD TB-VL-RUB-ENT(WS-IND-ENT) TO
               TB-VL-TOTAL-RUB-ENT(WS-IND-ENT).

           MOVE 1 TO SPGENT-QTD-ENT-CLASS.

      *     WRITE REG-058-SPGENT FROM DSUBCPG058-REG-SPGENT.

           ADD 1 TO WS-GRAV-ENTIDADES.

           IF IN-CRED-ESPECIAL
              IF SPGENT-MARCA-R329 = 0
                 WRITE REG-058-SPGENTA FROM DSUBCPG058-REG-SPGENT
                 ADD 1 TO WS-GRAV-SPGENTA
              ELSE
                 WRITE REG-058-SPGTAL FROM DSUBCPG058-REG-SPGENT
                 ADD 1 TO WS-GRAV-SPGENTAL.

           IF NOT IN-CRED-ESPECIAL
              WRITE REG-058-SPGENT FROM DSUBCPG058-REG-SPGENT
              ADD 1 TO WS-GRAV-SPGENT.

      *---------------------------------
        10010-FIM-CRIA-ENT-CLASSE. EXIT.
      *---------------------------------
      *------------------------------------
       11000-LIMPA-RUB-ENT.
      *------------------------------------

           MOVE ZEROS TO TB-VL-RUB-ENT(WS-IND).

      *------------------------------------
       11000-LIMPA-RUB-ENT-EXIT. EXIT.
      *------------------------------------

      *-------------------------------------------------------------*
       10000-ORDENA-RUBR.
      *-------------------------------------------------------------*

           COMPUTE W77-IND-K = W77-IND + 1
           PERFORM 10010-ORDENA-MOVE-RUBR THRU
                                           10010-FIM-ORDENA-MOVE-RUBR
           VARYING W77-IND-H FROM W77-IND-K BY 1
           UNTIL W77-IND-H > WS-QUAIS-RUBR - 1.

      *-------------------------------------------------------------*
       10000-FIM-ORDENA-RUBR. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       10010-ORDENA-MOVE-RUBR.
      *-------------------------------------------------------------*

           IF WS-TODAS-RUBR(W77-IND) > WS-TODAS-RUBR(W77-IND-H)
              MOVE WS-TODAS-RUBR (W77-IND) TO WS-CS-RUBR-ANT
              MOVE WS-TODAS-RUBR (W77-IND-H) TO WS-TODAS-RUBR (W77-IND)
              MOVE WS-CS-RUBR-ANT TO WS-TODAS-RUBR (W77-IND-H).

      *-------------------------------------------------------------*
       10010-FIM-ORDENA-MOVE-RUBR. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       10500-MOVE-CALAM.
      *-------------------------------------------------------------*

           FIND ST-CR-MAN-NB-ESP AT CS-ORIGEM-CREDITO = 4
                AND NU-SEQ-PROC = WS-IND-CALAM
                AND NU-NB = AUX1-NU-NB
                ON EXCEPTION
                   IF DMSTATUS(NOTFOUND)
                      GO TO 10500-FIM-MOVE-CALAM.

           MOVE 1 TO WS-ACHOU-CALAM.
           ADD  1 TO WS-TOT-CALAM.

      *-------------------------------------------------------------*
       10500-FIM-MOVE-CALAM. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       10600-PESQ-CALAM-OP.
      *-------------------------------------------------------------*

           PERFORM 10700-OBTEM-OP THRU 10700-FIM-OBTEM-OP
                   VARYING WS-IND-OP FROM 1 BY 1
                   UNTIL ACHOU-CALAM OR WS-IND-OP > 20.

      *-------------------------------------------------------------*
       10600-FIM-PESQ-CALAM-OP. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       10700-OBTEM-OP.
      *-------------------------------------------------------------*

           IF TAB-OP-CALAM(WS-IND-CALAM, WS-IND-OP)= WS-ID-ORGAO-PAGADOR
              MOVE 1 TO WS-ACHOU-CALAM
              IF WS-IND-CALAM > 0 AND < 7
                 MOVE 1 TO SPGENT-COD-CALAMIDADE
              ELSE
                 IF WS-IND-CALAM > 6 AND < 14
                    MOVE 2 TO SPGENT-COD-CALAMIDADE
                 ELSE
                    IF WS-IND-CALAM > 13 AND < 15
                       MOVE 3 TO SPGENT-COD-CALAMIDADE
                    ELSE
                       IF WS-IND-CALAM > 14 AND < 20
                       MOVE 4 TO SPGENT-COD-CALAMIDADE.

      *-------------------------------------------------------------*
       10700-FIM-OBTEM-OP. EXIT.
      *-------------------------------------------------------------*

      *-------------------------------------------------------------*
       16000-INCLUI-HEADER-SPGENT.
      *-------------------------------------------------------------*

           MOVE 1                 TO SPGENT-ID-REG-H.
           MOVE WS-AAAAMMDD       TO SPGENT-DT-PROC-H.
           PERFORM 16000-MOVE-RUBRICA-H THRU 16000-FIM-MOVE-RUBRICA-H.
      *OBS MOVE CP01-DT-INI-VALIDADE   TO SPGENT-DT-INI-VALIDADE-H.
           MOVE ZEROS                  TO SPGENT-DT-INI-VALIDADE-H.
           MOVE AUX1-DT-FIM-VALIDADE   TO SPGENT-DT-FIM-VALIDADE-H.
      *     MOVE ZEROS                  TO SPGENT-DT-FIM-VALIDADE-H.
      *OBS MOVE ID-CALAMIDADE OF DS-CALAMIDADE
      *                                       TO SPGENT-ID-CALAMIDADE-H.
           MOVE ZEROS                         TO SPGENT-ID-CALAMIDADE-H.
      *OBS MOVE NU-PORTARIA   OF DS-CALAMIDADE TO SPGENT-NUM-PORTARIA-H.
           MOVE ZEROS                          TO SPGENT-NUM-PORTARIA-H.
      *OBS MOVE DT-PORTARIA   OF DS-CALAMIDADE TO SPGENT-DT-PORTARIA-H.
           MOVE ZEROS                          TO SPGENT-DT-PORTARIA-H.

           IF AUX1-CRED-ESPECIAL  = 1
              MOVE 4 TO SPGENT-ORIG-CRED-H
              ADD 1 TO WS-GRAV-SPGENTA
           ELSE
              MOVE  3 TO SPGENT-ORIG-CRED-H
              ADD 1 TO WS-GRAV-SPGENT.

              WRITE REG-058-SPGENTA FROM DSUBCPG058-REG-SPGENT-HEADER.

              IF ATTRIBUTE MAXRECSIZE OF DSUBCPG058SPGTAL > 0
                 WRITE REG-058-SPGTAL FROM
                 DSUBCPG058-REG-SPGENT-HEADER.

              WRITE REG-058-SPGENT FROM DSUBCPG058-REG-SPGENT-HEADER.

      *-------------------------------------------------------------*
       16000-INCLUI-HEADER-SPGENT-EX. EXIT.
      *-------------------------------------------------------------*
      *-------------------------------------------------------------*
       16000-MOVE-RUBRICA-H.
      *-------------------------------------------------------------*

              ADD 1 TO WS-IND-TBH
              MOVE 104 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 105 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 107 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 108 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 112 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 136 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 154 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 160 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 161 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 162 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 201 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 202 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 203 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 204 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 205 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 206 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 207 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 208 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 209 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 210 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 214 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 215 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 216 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 217 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 218 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 219 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 220 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 221 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 222 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 223 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 224 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 225 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 226 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 227 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 228 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 229 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 230 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 231 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 232 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 233 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 234 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 236 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 237 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 238 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 239 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 240 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 241 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 242 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 243 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 244 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 245 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 246 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 247 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 248 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 249 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 250 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 251 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 252 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 253 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 254 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 255 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 256 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 257 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 258 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 259 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 260 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 261 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 262 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 263 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 264 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 265 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 266 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 267 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 268 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 269 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 270 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 271 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 272 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 273 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 274 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 301 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 905 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 909 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 910 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 911 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 912 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 916 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 917 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 921 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 925 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 926 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 927 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 928 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 929 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

              ADD 1 TO WS-IND-TBH
              MOVE 933 TO SPGENT-CS-RUBRICA-H ( WS-IND-TBH ).

      *--------------------------------------------------------------*
       16000-FIM-MOVE-RUBRICA-H. EXIT.
      *--------------------------------------------------------------*
      *-------------------------------------------------------------*
       17000-INCLUI-TRAILER-SPGENT.
      *-------------------------------------------------------------*

           IF WS-GRAV-SPGENT > ZEROS
              ADD 1 TO WS-GRAV-SPGENT
              MOVE 9 TO SPGENT-ID-REG-T
              MOVE WS-GRAV-SPGENT    TO SPGENT-QTD-REG-T
              MOVE WS-SPGENT-TOT-VL-LIQ-T TO SPGENT-TOT-VL-LIQ-T
              WRITE REG-058-SPGENT FROM DSUBCPG058-REG-SPGENT-T.

           MOVE ZEROS           TO DSUBCPG058-REG-SPGENT-T.

           IF WS-GRAV-SPGENTA > ZEROS
              ADD 1 TO WS-GRAV-SPGENTA
              MOVE 9 TO SPGENT-ID-REG-T
              MOVE WS-GRAV-SPGENTA    TO SPGENT-QTD-REG-T
              MOVE WS-SPGENT-TOT-VL-LIQ-TA TO SPGENT-TOT-VL-LIQ-T
              WRITE REG-058-SPGENTA FROM DSUBCPG058-REG-SPGENT-T.

          IF WS-GRAV-SPGENTAL > ZEROS
             MOVE ZEROS           TO DSUBCPG058-REG-SPGENT-T
             ADD 1 TO WS-GRAV-SPGENTAL
             MOVE 9 TO SPGENT-ID-REG-T
             MOVE  WS-SPG-TOT-VL-LIQ-TAL TO SPGENT-TOT-VL-LIQ-T
             MOVE WS-GRAV-SPGENTAL    TO SPGENT-QTD-REG-T
             WRITE REG-058-SPGTAL FROM DSUBCPG058-REG-SPGENT-T.

           COMPUTE WS-SPGENT-TOT-VL-LIQ-T = WS-SPGENT-TOT-VL-LIQ-T +
                                         WS-SPGENT-TOT-VL-LIQ-TA.
      *-------------------------------------------------------------*
       17000-INCLUI-TRAILER-SPGENT-EX. EXIT.
      *-------------------------------------------------------------*

      *---->                 RELATORIO DE TOTAIS                  <----*
      *-----------------------*
       27000-IMPRIME-RELATORIO.
      *-----------------------*

           IF (WS-LD-CRED-PEND = 0) THEN
              MOVE 0 TO W77-ESCORE W77-ESCORE-NAO-GRAV W77-ESCORE-BLOQ
           ELSE
              COMPUTE  W77-ESCORE = (WS-CP50-N-QTDE-CREDITOS /
                                  WS-LD-CRED-PEND) * 100
              COMPUTE  W77-ESCORE-NAO-GRAV = (W77-QTD-CRED-CALAM /
                                  WS-LD-CRED-PEND) * 100
              COMPUTE  W77-ESCORE-BLOQ  = (W77-QTD-BLOQ /
                                  WS-LD-CRED-PEND) * 100.
      *
      ***** PEGANDO HORA E DATA FINAL DO PROC. ANTES DE IMPRIMIR
      *
           CALL  "ZHORA IN DTP/L1"          GIVING  W77-HORA
           MOVE  W77-HORA               TO  W77-HORA-FIM.

           CALL  "ZDATA4 IN DTP/L1"   GIVING   W77-DDATA
           MOVE  W77-DDATA              TO  WS-DATA
                                            W77-DATA-FIM.
      *
      **** ROTINA PARA ESTIMATIVA

           CALL  "P/SUB/LIB/2K/601"
                 USING  W77-DATA-INICIO  W77-HORA-INICIO
                        W77-DATA-FIM     W77-HORA-FIM
                        W77-TEMPO-GASTO.

           MOVE  W77-HORA-INICIO        TO  C05-INICIO.
           MOVE  W77-ESCORE             TO  C05-ESCORE
           MOVE  W77-ESQUEMA            TO  C05-ESQUEMA
           MOVE  W77-HORA-FIM           TO  C06-TERMINO
           MOVE  W77-ESCORE-NAO-GRAV    TO  C06-ESCORE-NAO-BLOQ
           MOVE  W77-TEMPO-GASTO        TO  C07-ESTIMATIVA
           MOVE  W77-ESCORE-BLOQ        TO  C07-ESCORE-BLOQ
           MOVE  WS-LD-CRED-PEND
                                        TO  C11-QTD-LIDOS
           MOVE  WS-CP50-N-QTDE-CREDITOS       TO  C11-QTD-GRAVADOS
           MOVE  WS-TOT-CALAM           TO  C12-CRED-CALAM
      *     MOVE  W77-QTD-BLOQ           TO  C13-QTD-BLOQ
           MOVE  GRAVADOS-ZIKA       TO  C14-QTD-ZIKA
      *     MOVE  W77-CANCELADOS-B955    TO  C15-QTD-B955
           MOVE  SPACES                 TO  LSUBSPGENT-REG
           ADD   1                      TO  W77-CONT-PAG
           MOVE  W77-CONT-PAG           TO  C03-PAGINA
           MOVE  ZEROS                  TO  W77-CONT-LINHA
      *     WRITE LSUBSPGENT-REG                 BEFORE PAGE
      *     WRITE LSUBSPGENT-REG  FROM TRACO-01  BEFORE 1
           WRITE REG-LSUBCPG140MAIL FROM TRACO-01
      *     WRITE LSUBSPGENT-REG  FROM CAB01     BEFORE 1
           WRITE REG-LSUBCPG140MAIL FROM CAB01
      *     WRITE LSUBSPGENT-REG  FROM CAB02     BEFORE 1
           WRITE REG-LSUBCPG140MAIL FROM CAB02
      *     WRITE LSUBSPGENT-REG  FROM CAB03     BEFORE 1
           WRITE REG-LSUBCPG140MAIL FROM CAB03
      *     WRITE LSUBSPGENT-REG  FROM TRACO-02  BEFORE 2
           WRITE REG-LSUBCPG140MAIL FROM TRACO-02
      *     WRITE LSUBSPGENT-REG  FROM CAB04     BEFORE 1
           WRITE REG-LSUBCPG140MAIL FROM CAB04
      *     WRITE LSUBSPGENT-REG  FROM CAB04-A   BEFORE 2
           WRITE REG-LSUBCPG140MAIL FROM CAB04-A
      *     WRITE LSUBSPGENT-REG  FROM CAB05     BEFORE 1
           WRITE REG-LSUBCPG140MAIL FROM CAB05
      *     WRITE LSUBSPGENT-REG  FROM CAB06     BEFORE 1
           WRITE REG-LSUBCPG140MAIL FROM CAB06
      *     WRITE LSUBSPGENT-REG  FROM CAB07     BEFORE 1
           WRITE REG-LSUBCPG140MAIL FROM CAB07
      *     WRITE LSUBSPGENT-REG  FROM TRACO-03  BEFORE 2
           WRITE REG-LSUBCPG140MAIL FROM TRACO-03
      *    WRITE LSUBSPGENT-REG  FROM CAB08     BEFORE 1
      *    WRITE LSUBSPGENT-REG  FROM CAB09     BEFORE 1
      *    WRITE LSUBSPGENT-REG  FROM CAB10     BEFORE 1
      *    WRITE LSUBSPGENT-REG  FROM CAB09     BEFORE 2
      *     WRITE LSUBSPGENT-REG  FROM CAB11     BEFORE 1.
           WRITE REG-LSUBCPG140MAIL FROM CAB11.
      *     WRITE LSUBSPGENT-REG  FROM CAB12     BEFORE 1.
           WRITE REG-LSUBCPG140MAIL FROM CAB12.
      *     WRITE LSUBSPGENT-REG  FROM CAB13     BEFORE 1.
      *     WRITE LSUBSPGENT-REG  FROM CAB14     BEFORE 1.
      *     WRITE LSUBSPGENT-REG  FROM CAB15     BEFORE 1.
           WRITE REG-LSUBCPG140MAIL FROM CAB14.
      *     WRITE REG-LSUBCPG140MAIL FROM CAB15.
      *
            MOVE SPACES TO REG-LSUBCPG140MAIL.

            WRITE REG-LSUBCPG140MAIL.

            MOVE "BENEFICIOS COM DESCONTOS NA RUBRICA 230 - FNTF" TO
      *        TO  LSUBSPGENT-REG
              REG-LSUBCPG140MAIL.
      *      WRITE LSUBSPGENT-REG AFTER 3.
            WRITE REG-LSUBCPG140MAIL.
            MOVE "----------------------------------------------" TO
      *        TO  LSUBSPGENT-REG
              REG-LSUBCPG140MAIL.
      *      WRITE LSUBSPGENT-REG AFTER 1.
            WRITE REG-LSUBCPG140MAIL.

           IF SPGENT-MARCA-R230 > 0
              PERFORM 6000-INFORMA-LISTA THRU 6000-FIM-INFORMA-LISTA
                   VARYING WS-IND FROM 1 BY 1
                   UNTIL WS-IND > SPGENT-MARCA-R230.
      *
      *-----------------------*
       27000-EXIT.
           EXIT.
      *-----------------------*

      *-----------------------*
       5000-EMAIL-CV1.
      *-----------------------*

           CHANGE ATTRIBUTE LIBACCESS OF "EMAILSUPPORT" TO BYFUNCTION.
           MOVE SPACES TO EMAIL.

           STRING "SEND: BODY ", NM-ANEXO,
                  ",FROM PSUBCPG140@dataprev.gov.br ",
                  " jose.mfonseca@dataprev.gov.br",
      *            ",anderson.brito@dataprev.gov.br",
      *            ",alexandre.gregorio@dataprev.gov.br",
      *            ",andre.terra@dataprev.gov.br",
      "//LSUBCPG140R274-Relatorio de arquivos PAB para o Sispagben//",
      "TESTE - CV1"
      *         "//LSUBCPG606 - Relatorio de emprestimos TESTE//"
                  DELIMITED BY SIZE INTO EMAIL.
           MOVE 400 TO W77-MAIL-TAMANHO.
           CALL "EMAIL_COB IN EMAILSUPPORT" USING EMAIL,
                 W77-MAIL-TAMANHO GIVING W77-MAIL-OK.

           IF W77-MAIL-OK IS NOT EQUAL ZERO
              CHANGE ATTRIBUTE STATUS OF MYSELF TO -1.

      *-----------------------*
       5000-FIM-EMAIL-CV1.
       EXIT.
      *-----------------------*

       6000-INFORMA-LISTA.

      *      MOVE WS-LISTA-ARQ(WS-IND)  TO TEXTO-RESUMO.
      *      MOVE  WS-QTD-REGISTROS(WS-IND) TO QTDE-RESUMO.
      *      WRITE LSUBSPGENT-REG FROM LINHA-DET-RESUMO AFTER 2.
            MOVE WS-NB-R230(WS-IND) TO QTDE-RESUMO.
            WRITE REG-LSUBCPG140MAIL FROM LINHA-DET-RESUMO.

       6000-FIM-INFORMA-LISTA. EXIT.

      *=================================================================
       FIM-DO-PROGRAMA.
      *-----------------------------------*
      *  FIM DO PROGRAMA P/SUB/CPG/140/RMC
      *-----------------------------------*


      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%   REFERENCIAS CRUZADAS DO FONTE     %%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      * LIBRARY - B/SUB/CPG/050/SUL           CICLO = 0001VERSAO =   00

      * LIBRARY - B/SDC/LBD/SETTB00494        CICLO = 0001VERSAO =   09

      * LIBRARY - B/SDC/LBD/SETTB00233        CICLO = 0001VERSAO =   10

      * LIBRARY - B/SUB/CPG/058/SPG/R274      CICLO = 0001VERSAO =   01

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
