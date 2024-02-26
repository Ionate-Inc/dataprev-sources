      $$SET LIST
      *$$SET LIST LIST$ WARNFATAL
      $$SET ERRORLIMIT = 5
      $$SET FREE
      $$SET FEDLEVEL = 5
      **********$SET TADS
       IDENTIFICATION DIVISION.

       PROGRAM-ID.     PSUBCPG001.
      *PROGRAM-ID.     PSUBCPG001SPG.
       AUTHOR.         ADEMAR LIMA.
       INSTALLATION.   POOL-DDEAFP.
       DATE-WRITTEN.   21/02/93.
      *DATE-WRITTEN.   08/08/2013 QUINTA FEIRA.
       DATE-COMPILED.

      ******************************************************************
      *  OBJETIVO:   A PARTIR DA LEITURA DO BDBENCRED GRAVAR O ARQUIVO *
      *              CONTENDO OS DADOS NECESSARIOS PARA  CARGA  DO  DS-*
      *              CREDITO-PAGTO.                                    *
      *  HOSTNAME PRODUCAO:                                            *
      *  USERCODE PRODUCAO:                                            *
      *  DIVISAO:                                                      *
      *  ANALISTA:                                                     *
      *  PROGRAMADOR:                                                  *
      *  GRAU DIFICULDADE:                                             *
      ******************************************************************
      *  ALTERACAO EM    MARKID EPU-REL                                *
      *  SETEMBRO/2000 - GERAR UMA TABELA (E UM RELATORIO) COM VALORES *
      *                  DE ENCARGOS DA UNIAO PARA A NOTA TECNICA.     *
      *                  EH IMPORTANTE RESSALTAR QUE, PARA A GRAVACAO  *
      *                  DO ARQUIVO DSUBCPG058 (POSTERIORMENTE AGREGADO*
      *                  PARA O SISTEMA SCF), O VALOR USADO EH O LIQUI-*
      *                  DO, ENQUANTO QUE PARA O RELATORIO DE ENCARGOS *
      *                  DA UNIAO, O VALOR USADO EH O DA RUBRICA 301.  *
      ******************************************************************
      * ALTERACAO EM    MARKID PAB                                     *
      * OUTUBRO/2000  - NA CARGA DOS PABS, A SELECAO PASSA A SER IN-   *
      *                 CRED-DELETADO=FALSE E NU-SEQ-PROC <            *
      *                 NU-ULT-PROC-ALTER.                             *
      ******************************************************************
      * ALTERACAO EM    MARKID VIRA-GBL                                *
      * OUTUBRO/2003  - CARGA DOS PABS:                                *
      *                 * CASO A CONDICAO NU-SEQ-PROC DO PAB <         *
      *                   NU-ULT-PROC-ALTER NAO SEJA SATISFEITA (E O   *
      *                   IN-CRED-DELETADO ESTEJA  DESLIGADO), SERAH   *
      *                   VERIFICADO SE O NU-SEQ-PROC EH > 990 E A     *
      *                   D2-SOLIC-CREDITO EH, NO MAXIMO, 6 MESES ANTE-*
      *                   RIOR A DATA CORRENTE. SE ISTO NAO FOR SATIS- *
      *                   FEITO, O PAB NAO SERAH CARREGADO...          *
      ******************************************************************

      ******************************************************************
      * Os servicos de manutencao realizados pela Tutorial, NESTE CODIGO
      * foram meramente corretivos e se primam pela menor
      * mudanca possivel na estrutura do presente codigo.
      *
      * www.tutorialdocumenta.com.br
      *
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
        SOURCE-COMPUTER.  A10.
        OBJECT-COMPUTER.  A15.
        SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.


       INPUT-OUTPUT SECTION.
        FILE-CONTROL.

           SELECT DSUBCPG001  ASSIGN TO DISK.

           SELECT DSUBCPG003  ASSIGN TO DISK.

           SELECT DSUBCPG078  ASSIGN TO DISK.

           SELECT DSUBCPG079  ASSIGN TO DISK.

           SELECT DSUBCPG071  ASSIGN TO DISK.

           SELECT DSUBCPG072  ASSIGN TO DISK.

           SELECT DSUBCPG073  ASSIGN TO DISK.

           SELECT DSUBCPG075  ASSIGN TO DISK.

      * INCLUI ESSE ARQUIVO PARA GRAVAR O IR COMO ERA ANTES DA
      * ALTERACAO PARA O SISPAGBEN
           SELECT DSUBCPG075AUX  ASSIGN TO DISK.

      *NOVO SELECT DSUBCPG058SPGENT     ASSIGN TO DISK.
           SELECT DSUBCPG058SISPG      ASSIGN TO DISK.
           SELECT DSUBCPG058SPGENT      ASSIGN TO DISK.

           SELECT LSUBCPG058SISPG      ASSIGN TO PRINTER.
      *NOVO FIM SELECT NOVA
           SELECT DSUBCPG081  ASSIGN TO DISK.

           SELECT DSUBDNT001 ASSIGN TO DISK.

           SELECT WOFIS       ASSIGN TO DISK.

           SELECT LSUBCPG001A   ASSIGN TO PRINTER.

           SELECT LSUBCPG001B   ASSIGN TO PRINTER.

      *
       DATA DIVISION.
       FILE SECTION.

        FD DSUBCPG001
           BLOCK  CONTAINS 180  RECORDS
           RECORD CONTAINS 125  CHARACTERS
           VALUE OF TITLE IS "D/SUB/CPG/001."
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 40000
           SECURITYTYPE PUBLIC.
         COPY "*B/SUB/CPG/2K/401 ON PKBDSGF".
      *  COPY "*B/SUB/CPG/2K/401/JCM ON PKBDSGF".
      *  COPY "B/SUB/CPG/2K/401 ON PKPRG".

        FD DSUBCPG003
           BLOCK  CONTAINS 600  RECORDS
           RECORD CONTAINS 65 CHARACTERS
           VALUE OF TITLE IS "D/SUB/CPG/003."
           VA OF AREAS IS 200
           VA OF AREASIZE IS 1000
           SECURITYTYPE PUBLIC.

        COPY "*B/SUB/CPG/2K/410 ON PKBDSGF".
      * COPY "B/SUB/CPG/2K/410 ON PKPRG".

        FD DSUBCPG078
           BLOCK  CONTAINS 250  RECORDS
           RECORD CONTAINS 144 CHARACTERS
           VALUE OF TITLE IS "D/SUB/CPG/078."
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 33000
           SECURITYTYPE PUBLIC.


        COPY "*B/SUB/CPG/2K/430 ON PKBDSGF".

        FD DSUBCPG079
           BLOCK  CONTAINS  72  RECORDS
           RECORD CONTAINS 250 CHARACTERS
           VALUE OF TITLE IS "D/SUB/CPG/079."
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 10000
           SECURITYTYPE PUBLIC.

        COPY "*B/SUB/CPG/2K/450 ON PKBDSGF".
      * COPY "B/SUB/CPG/2K/450 ON PKPRG".

        FD DSUBCPG071
           BLOCK  CONTAINS 237 RECORDS
           RECORD CONTAINS 76 CHARACTERS
           VALUE OF TITLE IS "D/SUB/CPG/071."
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 10000
           SECURITYTYPE PUBLIC.

        COPY "*B/SUB/CPG/2K/477 ON PKBDSGF".

        FD DSUBCPG072
           BLOCK  CONTAINS  450 RECORDS
           RECORD CONTAINS   48 CHARACTERS
           VALUE  OF TITLE "D/SUB/CPG/072"
           VA OF AREAS IS 100
           VA OF AREASIZE IS 1000
           SECURITYTYPE PUBLIC.

        01 REG-DSUBCPG072.
           03 CPG072-COMPET            PIC 9(06).
           03 CPG072-SIGL-UF           PIC X(02).
           03 CPG072-TIPO              PIC 9(01).
           03 CPG072-ESPECIE           PIC 9(03).
           03 CPG072-BRUTO             PIC 9(12)V99.
           03 CPG072-DESC              PIC 9(12)V99.
           03 CPG072-TOT-BENEF         PIC 9(08).

        FD DSUBCPG073
           BLOCK  CONTAINS   40 RECORDS
           RECORD CONTAINS  450 CHARACTERS
           VALUE  OF TITLE "D/SUB/CPG/073"
           VA OF AREAS IS 300
           VA OF AREASIZE IS 500
           SECURITYTYPE PUBLIC.

        01 REG-DSUBCPG073.
           03 CPG073-COMPET            PIC 9(06).
           03 CPG073-SIGL-UF           PIC X(02).
           03 CPG073-TIPO              PIC 9(01).
           03 CPG073-IR                PIC 9(12)V99.
           03 CPG073-IR-UNI            PIC 9(12)V99.
           03 CPG073-C-PREV            PIC 9(12)V99.
           03 CPG073-PA                PIC 9(12)V99.
           03 CPG073-13-SAL            PIC 9(12)V99.
           03 CPG073-SF                PIC 9(12)V99.
           03 CPG073-CONSIG            PIC 9(12)V99.
           03 CPG073-CPMF              PIC 9(12)V99.
           03 CPG073-TP-CONSIG OC 16   PIC 9(12)V99.
           03 FILLER                   PIC X(105).

       FD DSUBCPG075
          RECORD CONTAINS 90 CHARACTERS
          BLOCK CONTAINS 200 RECORDS
          VA OF TITLE "D/SUB/CPG/075"
          VA OF AREAS IS 1000
          VA OF AREASIZE IS 10000
          SECURITYTYPE PUBLIC.

       01 REG-DSUBCPG075.
           03 CPG075-NU-NB             PIC 9(10).
           03 CPG075-DT-INI-PERIODO    PIC 9(08).
           03 CPG075-DT-FIM-PERIODO    PIC 9(08).
           03 CPG075-CS-NATUR-CREDITO  PIC 9(01).
           03 CPG075-DT-INI-VALID      PIC 9(08).
           03 CPG075-VL-BRUTO          PIC 9(15)V99.
           03 CPG075-VL-IR             PIC 9(15)V99.
           03 CPG075-TIPO-IR           PIC 9(02).
           03 CPG075-CS-ORIGEM-CREDITO PIC 9(01).
           03 CPG075-D2-MOV-CREDITO    PIC 9(08).
           03 CPG075-CS-OPERACAO       PIC 9(01).
           03 FILLER                   PIC 9(09).

      * INCLUI ESSE ARQUIVO PARA GRAVAR O IR COMO ERA ANTES DA
      * ALTERACAO PARA O SISPAGBEN
       FD DSUBCPG075AUX
          RECORD CONTAINS 90 CHARACTERS
          BLOCK CONTAINS 200 RECORDS
          VA OF TITLE "D/SUB/CPG/075/AUX"
          VA OF AREAS IS 1000
          VA OF AREASIZE IS 10000
          SECURITYTYPE PUBLIC.

       01 REG-DSUBCPG075AUX.
           03 CPG075AUX-NU-NB             PIC 9(10).
           03 CPG075AUX-DT-INI-PERIODO    PIC 9(08).
           03 CPG075AUX-DT-FIM-PERIODO    PIC 9(08).
           03 CPG075AUX-CS-NATUR-CREDITO  PIC 9(01).
           03 CPG075AUX-DT-INI-VALID      PIC 9(08).
           03 CPG075AUX-VL-BRUTO          PIC 9(15)V99.
           03 CPG075AUX-VL-IR             PIC 9(15)V99.
           03 CPG075AUX-TIPO-IR           PIC 9(02).
           03 CPG075AUX-CS-ORIGEM-CREDITO PIC 9(01).
           03 CPG075AUX-D2-MOV-CREDITO    PIC 9(08).
           03 CPG075AUX-CS-OPERACAO       PIC 9(01).
           03 FILLER                      PIC 9(09).


       FD DSUBCPG081
          RECORD CONTAINS 72 CHARACTERS
          BLOCK CONTAINS 250 RECORDS
          VA OF TITLE "D/SUB/CPG/081"
          VA OF AREAS IS 1000
          VA OF AREASIZE IS 40000
          SECURITYTYPE PUBLIC.

       01 REG-DSUBCPG081.
          03 CPG081-CHAVE.
             05 CPG081-DT-MOV-CREDITO    PIC 9(08)    COMP.
             05 CPG081-NU-NB             PIC 9(10)    COMP.
             05 CPG081-CS-NATUR-CREDITO  PIC 9(02)    COMP.
             05 CPG081-DT-FIM-PERIODO    PIC 9(08)    COMP.
             05 CPG081-DT-INI-PERIODO    PIC 9(08)    COMP.
             05 CPG081-ID-NIT            PIC 9(10)    COMP.
             05 CPG081-SEQ-REG           PIC 9(09)    COMP.
             05 FILLER                   PIC 9(01)    COMP.
          03 CPG081-ID-EMPRESTIMO        PIC X(20).
          03 CPG081-VL-PARCELA-CONSIG    PIC 9(10)V99 COMP.
          03 CPG081-NU-PARCELA-CONSIG    PIC 9(02)    COMP.
          03 CPG081-QT-TOT-PAR-CONSIG    PIC 9(02)    COMP.
          03 CPG081-CS-NAO-DESCONTO      PIC 9(02)    COMP.
          03 CPG081-ID-BANCO-EMPREST     PIC 9(03)    COMP.
          03 CPG081-D2-INI-DESCONTO      PIC 9(06)    COMP.
          03 CPG081-CS-CONSIGNACAO       PIC 9(02)    COMP.
          03 CPG081-ID-BCO-GLOSA         PIC 9(03)    COMP.
          03 FILLER                      PIC 9(14)    COMP.

        FD DSUBCPG058SISPG
           RECORD CONTAINS  1800 CHARACTERS
           BLOCK  CONTAINS    10 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 40000
           SECURITYTYPE PUBLIC
           VALUE OF TITLE IS "D/SUB/CPG/058/SPG/001/R275 ON PKDADOS.".

       01  REG-058-SISPG PIC X(1800).

        FD DSUBCPG058SPGENT
           RECORD CONTAINS  1800 CHARACTERS
           BLOCK  CONTAINS    10 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 40000
           SECURITYTYPE PUBLIC
           VALUE OF TITLE IS "D/SUB/CPG/058/SPG/001/R274 ON PKDADOS.".

       01  REG-058-SPGENT PIC X(1800).

       FD DSUBDNT001
          RECORD CONTAINS 80 CHARACTERS
          BLOCK CONTAINS   33 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 40000
          SECURITYTYPE PUBLIC
          VALUE OF TITLE IS "D/SUB/DNT/001/AAAAMM ON PACK.".

       01 REG-DNT.

          03 COD-DESCRICAO              PIC 9(02).
          03 QTD-CREDITOS               PIC 9(08).
          03 VL-CREDITOS                PIC 9(15)V99.
          03 FILLER                     PIC X(53).

        FD WOFIS
           RECORD CONTAINS 080 CHARACTERS
           BLOCK  CONTAINS 010 RECORDS
           VALUE  OF TITLE IS WS-OFIS-TITLE.

        01 REG-WOFIS                   PIC X(080).

        FD LSUBCPG001A.
        01 LSUB001-REG                 PIC X(132).

        FD LSUBCPG001B.
        01 LSUB001B-REG                PIC X(080).
      *
       FD  LSUBCPG058SISPG.
       01  LSUBSISPG-REG               PIC X(132).

       DATA-BASE SECTION.
       DB  BDBENCRED.
      * DB  BDBENCRED = BDBENCREDTR.
      * DB  BDBENCRED = BDBENCREDTST.
      *  DB  BDBENCRED = BDBENCREDMIGPAB.
       01  DS-RESTART.
       01  DS-CALAMIDADE.
       01  DS-MOV-CRED-CALAM.
       01  DS-MOV-CRED-CONC.
       01  DS-MOV-CRED-MANUT.
       01  DS-MOV-CRED-ALTER.
       01  DS-MOV-CRED-SEAS.
       01  DS-CRED-PENDENTE.
       01  DS-CTRL-PROC-CRED.
       01  DS-BENEF-CALAM.

       DB  BDSISBEN.
      * DB  BDSISBEN = BDSISBENTR.
      *** DB  BDSISBEN = BDSISBEN3TST.
      * DB  BDSISBEN = BDSISBENMIGPAB.
       01  DS-CALC-ACAO-JUD.
       01  DS-BENEFICIO.
       01  DS-PECULIO.
       01  DS-CONSIGNACAO.
       01  DS-CENSO-BEN-REC.

       DB  BDCONPAGBEN.
      * DB  BDCONPAGBEN = BDCONPAGBENTR.
      * DB  BDCONPAGBEN = BDCONPAGBENMIGPAB.
       01 DS-CREDITO-PAGTO USING S2-CREDITO-NB.
       01 DS-CRED-PAGTO-EXP.
       01  DS-MEIO-PAGTO.
      /
      *
       WORKING-STORAGE SECTION.

      *     COPY "*B/SUB/CPG/058/SPG ON PKBDSGF".

      *NOVO LAYOUT REGISTRO DO DSUBCPG058SPGENT

      *     COPY "*B/SUB/CPG/058/SPG/ENT ON PKBDSGF".
      *     COPY "*B/SUB/CPG/058/SPG/R274 ON PKBDSGF".
            COPY "*B/SUB/CPG/058/SPG/R275 ON PKBDSGF".
            COPY "*B/SUB/CPG/058/SPG/R274 ON PKBDSGF".


      *     COPY "*B/SUB/CPG/058/SPG/CNF ON PKBDSGF".
      *     COPY "*B/SUB/CPG/058/SPG/ABSP ON PKBDSGF".
      **  COPY "B/SUB/CPG/058/SPG/JCM/RUB7 ON PKPRG".

      *VARIAVEIS DO HEADER DO SISPAGBEN.
           COPY "*B/SUB/CPG/SPG/RUB ON PKBDSGF".


       77 W77-QT-GRUPO            PIC 9(08).
       77 W77-INDICE              PIC 9(08).

      *Jose Fonseca - DM.096405 - INSS em numeros
       01  WS-TITLE-DSUBDNT001.
           03 FILLER                PIC X(14) VA "D/SUB/DNT/001/".
           03 WS-COMPET-CRED        PIC X(06).
           03 FILLER                PIC X(12) VA " ON PACK.".

       77 WS-QTD-PAB-TOT            PIC 9(08).
       77 WS-VL-PAB-TOT             PIC 9(15)V99.
       01 WS-TAB-DNT.
          03 WS-TAB-DNT-R OCCURS 12 TIMES.
             05 WS-QTD-DNT PIC 9(08).
             05 WS-VL-DNT  PIC 9(15)V99.
             05 WS-QTD-DNT-LIDO PIC 9(08).
             05 WS-VL-DNT-LIDO  PIC 9(15)V99.
       77 WS-GRAVADOS-DNT           PIC 9(08) VA ZEROS.

       77 WS-QT-AUX                    PIC 9(08).
       77 WS-VL-AUX                    PIC 9(15)V99.
       77 WS-QT-AUX-LIDO               PIC 9(08).
       77 WS-VL-AUX-LIDO               PIC 9(15)V99.

      *Para exclusoes de consignacoes. 01/09/2021.
       01  WS-CONS-INICIO           PIC  9(06).
       01  WS-CONS-INICIO-R  REDEFINES  WS-CONS-INICIO.
           03  WS-CONS-INI-ANO      PIC  9(04).
           03  WS-CONS-INI-MES      PIC  9(02).
       77  WS-NU-PARCELA-CONSIG     PIC  9(02).
       77  WS-FIM-CONSIG            PIC  9(01).
       77  WS-ACHOU-CONSIG          PIC  9(01).
       77  WS-IN-EXC-CONSIG         PIC  9(02).

      *Pagamentos do Mercosul.   Jose Carlos Fonseca 20/09/2012 QUI
       01  AUX1-ID-PAIS                PIC 9(003) VALUE ZEROS.

       01  WS-E-CRED-CALAMIDADE           PIC X(03).
           88 E-CREDITO-DE-CALAMIDADE     VALUE "SIM".
           88 NAO-E-CREDITO-DE-CALAMIDADE VALUE "NAO".

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
         03 FILLER PIC X(22) VA "3172266CINAAP        ".
         03 FILLER PIC X(22) VA "3273267CAAP          ".
         03 FILLER PIC X(22) VA "3375269AP BRASIL     ".
         03 FILLER PIC X(22) VA "3476270CBPA          ".
         03 FILLER PIC X(22) VA "3577271AMAR BRASIL   ".
         03 FILLER PIC X(22) VA "3678272ACOLHER       ".
         03 FILLER PIC X(22) VA "3779273CEBAP         ".
         03 FILLER PIC X(22) VA "3880274ABENPREV      ".
         03 FILLER PIC X(22) VA "3981275ASABASP BRASIL".

       01 WS-TAB-ENT-R REDEFINES TAB-ENTID.
          03 WS-TAB-ENT OCCURS 39 TIMES.
             07 TAB-NUM-ENTIDADE        PIC 99.
             07 TAB-IND-RUBR            PIC 99.
             07 TAB-CS-RUBR-ENT             PIC 9(003).
             07 TAB-NM-ENTIDADE         PIC X(015).

        01 WS-TAB-RUBR-ENT.
           03 WS-TB-RUBRICAS-ENT OC 39.
              05 TB-VL-RUB-ENT  PIC 9(12)V99.

        01 WS-TAB-QTD-VL-ENT.
           03 WS-TB-QT-VL-ENT OC 39.
              05 TB-QT-TOTAL-ENT      PIC 9(08).
              05 TB-VL-TOTAL-RUB-ENT  PIC 9(12)V99.

       01 WS-TITLE-DSUBCPG058R275.
          03 WS-NOME-ROTINA-UNI  PIC X(23) VALUE
                                              "D/SUB/CPG/058/SPG/R275/".
          03 WS-ROTINA                PIC X(04).
          03 FILLER                   PIC X(01) VALUE ".".

       01 WS-TITLE-DSUBCPG058R274.
          03 WS-NOME-ROTI-SPG-R274    PIC X(23) VALUE
                                              "D/SUB/CPG/058/SPG/R274/".
          03 WS-ROTINA-R274           PIC X(04).
          03 FILLER                   PIC X(01) VALUE ".".

       77  WS-FLAG-RUB                 PIC 9(01) VALUE ZEROS.
      *SE  WS-FLAG-LISTA-RUB = ZEROS NAO LISTA AS RUBRICAS SE 1 SIM
       77  WS-FLAG-LISTA-RUB           PIC 9(01) VALUE 1.
       77  WS-NU-NB-REG                PIC 9(10).
       77  WS-NU-NB-MSG                PIC 9(10).
       77  WS-MSG                      PIC X(150).
       77  WS-STATUS-BACKUP            PIC 9(11) BINARY VA 0.
       77  WS-CONTADOR-Y               PIC X(12).
       77  WS-CONTADOR-X               PIC X(12).
       77  WS-EPU-PREV                 PIC X(04)       VALUE SPACES.
       77  WS-SISPG-PERC-PREV          PIC 9(03)V9(05) VALUE ZEROS.
       77  WS-SISPG-PERC-EPU           PIC 9(03)V9(05) VALUE ZEROS.
       77  WS-SISPG-PERC-X             PIC 9(03)V9(05) VALUE ZEROS.
       77  WS-SISPG-VALOR-RUB301       PIC 9(15)V99    VALUE ZEROS.
       77  WS-SISPG-COD-CALAMIDADE     PIC 9(05)       VALUE ZEROS.
       77  IX                          PIC 9(02) VALUE ZEROS.
       77  IXX                         PIC 9(02) VALUE ZEROS.
       77  IND-TB                      PIC 9(02) VALUE ZEROS.
       77  IND-TB2                     PIC 9(02) VALUE ZEROS.
       77  IND-TB-ENT                  PIC 9(02) VALUE ZEROS.
       77  WS-CONTA-DESCONTO           PIC 9(11) VALUE ZEROS.
       77  WS-GRAV-ENTIDADES           PIC 9(06) VALUE ZEROS.
       77  WS-GRAV-SISPG               PIC 9(11) VALUE ZEROS.
       77  WS-GRAV-SISPG-RUB301        PIC 9(11) VALUE ZEROS.
       77  WS-GRAV-SISPG-RUB267        PIC 9(11) VALUE ZEROS.
       77  WS-GRAV-SISPG-RUB273        PIC 9(11) VALUE ZEROS.
       77  WS-GRAV-SPGENT-RUB274       PIC 9(11) VALUE ZEROS.
       77  WS-GRAV-SPGENT-RUB275       PIC 9(11) VALUE ZEROS.
       77  WS-SISPG-QUANT-CREDITOS     PIC 9(10) VALUE 1.
       77  WS-QUANT-LINHA              PIC 9(12) VALUE ZEROS.
       77  WS-GRAV-MAIOR0-RUB301       PIC 9(11) VALUE ZEROS.
       77  WS-CONTA-BRUTO              PIC 9(11) VALUE ZEROS.
       77  WS-CONTA-BRUTO-ZERADO       PIC 9(11) VALUE ZEROS.
       77  WS-FLAG-BRUTO-ZERO-ABORTA   PIC 9(01) VALUE ZEROS.
       77  WS-SISPG-CS-REDE-ECT        PIC 9(01) VALUE ZEROS.
       77  W77-FIM-MAIOR-CRED          PIC 9(06) VALUE ZEROS.
       01  LIN-SISPG.
           03 NB-I                     PIC 999.999.999.9.
           03 FILLER                   PIC X(01) VALUE SPACES.
           03 N-RUB-I                  PIC 99.
           03 RUBRICAS-I.
              05 RUBR OCCURS 6 TIMES.
                 07 BRANCO-I  PIC X(01).
                 07 CS-RUB-I  PIC 999B.
                 07 VL-RUB-I  PIC Z.ZZZ.ZZ9,99.

       01  SISPG-MARCA-GRUPO-RUBRICAS.
           03  SISPG-MARCA-RUB104   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB105   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB107   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB108   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB112   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB136   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB154   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB160   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB161   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB162   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB201   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB202   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB203   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB204   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB205   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB206   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB207   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB208   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB209   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB210   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB214   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB215   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB216   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB217   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB218   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB219   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB220   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB221   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB222   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB223   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB224   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB225   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB226   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB227   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB228   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB229   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB230   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB231   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB232   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB233   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB234   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB236   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB237   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB238   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB239   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB240   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB241   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB242   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB243   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB244   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB245   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB246   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB247   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB248   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB249   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB250   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB251   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB252   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB253   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB254   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB255   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB256   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB257   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB258   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB259   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB260   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB261   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB262   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB263   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB264   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB265   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB266   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB267   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB268   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB269   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB270   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB271   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB272   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB273   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB274   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB275   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB301   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB905   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB909   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB910   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB911   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB912   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB916   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB917   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB921   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB925   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB926   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB927   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB928   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB929   PIC 9(01) VALUE ZEROS.
           03  SISPG-MARCA-RUB933   PIC 9(01) VALUE ZEROS.

       01  LIN-SISPG-LIQ.
           03  FILLER PIC X(07) VALUE "BRUTO =".
           03  LIN-BRUTO  PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
           03  FILLER PIC X(01) VALUE SPACES.
           03  FILLER PIC X(07) VALUE "LIQUI =".
           03  LIN-LIQUIDO PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
           03  FILLER PIC X(01) VALUE SPACES.
           03  FILLER PIC X(07) VALUE "RUB301=".
           03  LIN-RUB301  PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
           03  FILLER PIC X(01) VALUE SPACES.
           03  FILLER PIC X(07) VALUE "%%PREV=".
           03  LIN-PERC-PREV PIC 9(03),9(05).
           03  FILLER PIC X(01) VALUE SPACES.
           03  FILLER PIC X(07) VALUE "%%%EPU=".
           03  LIN-PERC-EPU  PIC 9(03),9(05).

       77  WS-CONTA-ORIG-ORC-1         PIC 9(11) VALUE ZEROS.
       77  WS-CONTA-ORIG-ORC-2         PIC 9(11) VALUE ZEROS.
       77  WS-CONTA-ORIG-ORC-3         PIC 9(11) VALUE ZEROS.
       77  WS-CONTA-ORIG-ORC-4         PIC 9(11) VALUE ZEROS.
       77  WS-CONTA-ORIG-ORC-OUTROS    PIC 9(11) VALUE ZEROS.
      *
       77  WS-CONTA-NAO-CONSIG         PIC 9(11) VALUE ZEROS.
       77  TB-IND-SISPG                PIC 9(02) VALUE ZEROS.

       01  SISPG-VALOR-GRUPO-RUBR-GUARDA PIC X(1344).
       01  TAB-SISPG-GR-RUB            PIC X(1344).
       01  TAB-SISPG-GR-RUB-R REDEFINES
           TAB-SISPG-GR-RUB.
           03 TAB-SISPG-RUBS OCCURS 96 TIMES.
              05 TB-SISPG-RUBR         PIC 9(12)V99.

       77  WS-QT-RUBRICA               PIC 9(02)     VALUE ZEROS.
       77  WS-QT-NU-NB                 PIC 9(10)     VALUE ZEROS.
       77  WS-SISPG-VALOR-LIQUIDO      PIC 9(15)V99  VALUE ZEROS.
       77  WS-SISPG-VALOR-LIQUIDO-PREV PIC 9(15)V99  VALUE ZEROS.
       77  WS-SISPG-VALOR-BRUTO        PIC 9(15)V99  VALUE ZEROS.
       77  WS-SISPG-VALOR-BRUTO-PREV   PIC 9(15)V99  VALUE ZEROS.
       77  WS-TB-SISPG-RUBR-ORIGINAL   PIC 9(12)V99  VALUE ZEROS.
       77  WS-TB-SISPG-RUBR-PREV       PIC 9(12)V99  VALUE ZEROS.
       77  WS-SISPG-QTD-REG-T          PIC 9(08)     VALUE ZEROS.
       77  WS-CONTA-EPU-ZERO           PIC 9(08)     VALUE ZEROS.
       77  WS-SISPG-TOT-VL-LIQ-T       PIC 9(15)V99  VALUE ZEROS.
       77  WS-FLAG-PREV-EPU-2REG-301   PIC 9(01)     VALUE ZEROS.
       77  WS-SISPG-ORIG-CRED-H        PIC 9(01)     VALUE ZEROS.

       01  WS-ACHOU-CALAM              PIC 9(01).
          88  ACHOU-CALAM                 VALUE 1.

       77  WS-VL-LIQUIDO-CRED          PIC 9(15)V99  VALUE ZEROS.
       77  WS-VL-LIQUIDO-CRED-ZZ       PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       77  WS-VL-BRUTO-CREDITO         PIC 9(15)V99  VALUE ZEROS.
       77  WS-VL-BRUTO-CREDITO-ZZ      PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       77  WS-VIRGULA                  PIC X(01)     VALUE ",".
       01  WS-INTEIRO-DECIMAL          PIC 9(15)V99  VALUE ZEROS.
       01  WS-INTEIRO-DECIMAL-R  REDEFINES
           WS-INTEIRO-DECIMAL.
           03 WS-INTEIRO               PIC 9(15).
           03 WS-DECIMAL               PIC 9(02).

      *ALTERACAO TEMPORARIA
       77 WS-NU-NB    PIC 9(10).
       77 WS-OL       PIC 9(08).
       77 WS-DT-SOLIC PIC 9(08).
       77 WS-NU-SEQ   PIC 9(04).
       77 WS-INDICE   PIC 9(03).
       77 WS-ORIG     PIC 9(01).
      *
       77 WS-JA-PROCESSADO PIC 9(01).
       77 WS-MSG-LIDOS                PIC X(132).
       77 W77-MSG                     PIC X(132).
       77 WSS-NB      PIC 9(10).
       77 WSS-FIM-PER PIC 9(08).
       77 WSS-INI-PER PIC 9(08).
       77 WSS-NATUR   PIC 9(02).
       77 WS-COMP                      PIC 9(01).
       77 WS-ESP                       PIC 9(03).
       77 WS-PERC-PREV                 PIC 9(03)V9(05).
       77 WS-TOT-BRUTO                 PIC 9(12)V99.
       77 WS-TOT-DESC                  PIC 9(12)V99.
       77 WS-IR-PREV                   PIC 9(12)V99.
       77 WS-IR-UNIAO                  PIC 9(12)V99.
       77 WS-RUB-NOVA                  PIC 9(12)V99.
       77 WS-PERC-PREV-2               PIC 9(03)V9(05).
       77 WS-TOT-BRUTO-2               PIC 9(12)V99.
       77 WS-TOT-DESC-2                PIC 9(12)V99.
       77 WS-IR-PREV-2                 PIC 9(12)V99.
       77 WS-IR-UNIAO-2                PIC 9(12)V99.
       77 WS-TOT-DESCIR-2              PIC 9(12)V99.
       77 WS-VL-RUB-301                PIC 9(12)V99.
       77 WS-VL-RUB-301-SEM-ALT        PIC 9(12)V99.
       77 WS-BRUTO-RUB-105             PIC 9(12)V99.
       77 WS-RUB-108-PREV              PIC 9(12)V99.
       77 WS-RUB-108-UNIAO             PIC 9(12)V99.
       77 WS-VL-RUB-108                PIC 9(12)V99.
       77 WS-TOT-VL-CONS               PIC 9(12)V99.
       77 WS-TOT-PARCIAL               PIC 9(15)V99.
       77 WS-ACORDO-INTERNAC           PIC 9(01).
       01 WS-TIPO                      PIC 9(02).
       01 WS-TIPO-R REDEFINES WS-TIPO.
          03 FILLER                    PIC 9(01).
          03 WS-TIPO-1                 PIC 9(01).
       01 WS-COMP-13                   PIC 9(06).
       01 WS-COMP-13-R REDEFINES WS-COMP-13.
          03 FILLER                    PIC 9(04).
          03 WS-COMP-13-MES            PIC 9(02).

       77 W77-DT-MAX-PER-NAT-01        PIC 9(08) VALUE ZEROS.
       77 W77-NU-SEQ-CONCESSAO         PIC 9(02).
       77 INDICE-PENDENCIA             PIC 9(02) VALUE ZEROS.
       77 WS-INTERVALO-LIB601          PIC 9(06) VALUE ZEROS.
       77 WS-QT-ACEITO                 PIC 9(03)V99 VALUE ZEROS.
       77 WS-DATA-INICIO-LIB601        PIC 9(08) VALUE ZEROS.
       77 WS-HORA-INICIO-LIB601        PIC 9(06) VALUE ZEROS.
       77 WS-DATA-FIM-LIB601           PIC 9(08) VALUE ZEROS.
       77 WS-HORA-FIM-LIB601           PIC 9(06) VALUE ZEROS.
       77 WS-CONT-LIN                  PIC 9(06) VALUE 99.
       77 WS-CONT-PAG                  PIC 9(06) VALUE ZEROS.
       77 WS-DESCONTO                  PIC 9(12)V99 VALUE ZEROS.
       77 W77-IND-TAB                  PIC 9(03) VALUE ZEROS.
       77 W77-IND                      PIC 9(03) VALUE ZEROS.
       77 W77-IND-CP                   PIC 9(03) VALUE ZEROS.
       77 COMPR                        PIC 9(02) BINARY.
       77 WS-IND                       PIC 9(03) VALUE ZEROS.
       77 WS-IND-ENT                   PIC 9(03).
       77 WS-IND-DNT                   PIC 9(02) VALUE ZEROS.
      $SET MAP
       77 W77-DESPREZ-CONC             PIC 9(11) BINARY.
       77 W77-LIDOS-MANUT              PIC 9(11) BINARY.
       77 W77-LIDOS-CONC               PIC 9(11) BINARY.
       77 W77-LIDOS-ALTE               PIC 9(11) BINARY.
       77 W77-LIDOS-AP                 PIC 9(11) BINARY.
       77 W77-LIDOS-SEAS               PIC 9(11) BINARY.

       77 WS-LIDOS-MANUT               PIC 9(11) VALUE ZEROS.
       77 WS-LIDOS-CONC                PIC 9(11) VALUE ZEROS.
       77 WS-LIDOS-ALTE                PIC 9(11) VALUE ZEROS.

      $RESET MAP
       77 W77-FASE                     PIC X(30) VALUE
          "   FASE   UNICA   ".
       77 W77-GRAVADOS                 PIC 9(11) BINARY.
       77 W77-GR-CPG072                PIC 9(11) BINARY.
       77 W77-GR-CPG073                PIC 9(11) BINARY.
       77 WS-REG-LIDOS                 PIC 9(06) VALUE ZEROS.
       77 W77-IMPRESSOS                PIC 9(11) BINARY.
       77 AUX-QT-MARCA-ERRO            PIC 9(08) VALUE ZEROS.
       77 AUX-DIGITO                   PIC 9(001) BINARY.
       77 AUX-IN-CREDITO-OK            PIC 9(001) BINARY.
       77 AUX-QT-DELETADOS             PIC 9(008) VALUE ZEROS.
       77 AUX-QT-NU-SEQ-PROC           PIC 9(008) VALUE ZEROS.
       77 AUX-IN-NATUR-CREDITO-OK      PIC 9(001) BINARY.
       77 AUX-IN-PERIODO-OK            PIC 9(001) BINARY.
       77 AUX-IN-INI-PERIODO-OK        PIC 9(001) BINARY.
       77 AUX-IN-FIM-PERIODO-OK        PIC 9(001) BINARY.
       77 AUX-IN-VALIDADE-OK           PIC 9(001) BINARY.
       77 AUX-IN-INI-VALIDADE-OK       PIC 9(001) BINARY.
       77 AUX-IN-FIM-VALIDADE-OK       PIC 9(001) BINARY.
       77 AUX-IN-MEIO-PAGTO-OK         PIC 9(001) BINARY.
       77 AUX-IN-NB-OK                 PIC 9(001) BINARY.
       77 AUX-IN-BANCO-OK              PIC 9(001) BINARY.
       77 AUX-IN-UF-OK                 PIC 9(001) BINARY.
       77 AUX-IN-VALOR-OK              PIC 9(001) BINARY.
       77 AUX-IN-NIT-OK                PIC 9(001) BINARY.
       77 PAR-ESQUEMA                  PIC 9(05)  BINARY.
       77 PAR-MOV-CREDITO              PIC X(04).
       77 PAR-SEAS                     PIC X(04).
       77 W77-OFIS-CLASSE              PIC 9(11) BINARY.
       77 W77-OFIS-PRAZO               PIC 9(11) BINARY VA 365.
       77 W77-OFIS-RETORNO             PIC S9(11) BINARY.
       77 W77-IND-RUBRICAS             PIC 9(02).
       77 W77-IND-RUB                  PIC 9(03).
       77 W77-IND-TAB-RUB              PIC 9(02).
       77 W77-IND-CONS                 PIC 9(02).
      *
       77  WS-MENSAGEM-ZDISPLAY            PIC  X(80).
       77  WS-PARAM1-ZDISPLAY              PIC  X(50).
       77  WS-PARAM2-ZDISPLAY              PIC  X(15).
       77  WS-INTEIRO-ZDISPLAY             PIC  9(01) BINARY VALUE 0.
      *
      $SET MAP
       77 WS-REGS-058-NOVO                 PIC  9(08) BINARY.
       77 WS-REGS-058-LOAS-DEF             PIC  9(08) BINARY.
       77 WS-REGS-058-LOAS-IDOSO           PIC  9(08) BINARY.
       77 WS-REGS-058-RMV-DEF              PIC  9(08) BINARY.
       77 WS-REGS-058-RMV-ID               PIC  9(08) BINARY.
       77 WS-REGS-058-SO-EPU               PIC  9(08) BINARY.
       77 WS-REGS-058-SO-PREV              PIC  9(08) BINARY.
       77 WS-REGS-058-MISTO                PIC  9(08) BINARY.
       77 WS-REGS-058-MISTO-SO-EPU         PIC  9(08) BINARY.
       77 WS-REGS-DESP-EPU                 PIC  9(08) BINARY.
       77 WS-REGS-SAL-FAMILIA              PIC  9(08) BINARY.
       77 WS-VL-DESP-EPU                   PIC  9(15)V99  BINARY.
      $RESET MAP
       77 WS-INCONS-CONSIG                 PIC  9(01).
       77 WS-CONT-CONSIG                   PIC  9(02).
       77 WS-ACHOU-NAO-DESC-DIF-97-98      PIC  9(01).
       77 WS-ACHOU-FIM-DESC-OK             PIC  9(01).
       77 WS-OCORR-ERRO                    PIC  9(01).
       77 WS-TEM-MOV                       PIC  9(01) VALUE ZEROS.
       01 AUX-NU-NB-INT             PIC 9(10).
       01 AUX-NU-NB-INTR REDEFINES AUX-NU-NB-INT.
          03 AUX-NU                 PIC 9(09).
          03 AUX-DIG                PIC 9(01).

       01 AUX-IN-FIM-ARQ            PIC 9(001) VALUE ZEROS.
       01 AUX-DT-INI-PERIODO        PIC 9(008) VALUE ZEROS.
       01 AUX-DT-FIM-PERIODO        PIC 9(008) VALUE ZEROS.
       01 AUX-DT-FIM-VALIDADE       PIC 9(008) VALUE ZEROS.
       01 AUX-DT-INI-VALIDADE       PIC 9(008) VALUE ZEROS.
       01 AUX-COMP-CRED             PIC 9(011) VALUE ZEROS.
       01 AUX-NU-CONT-CHECKPOINT    PIC 9(011) VALUE ZEROS.
       01 AUX-ORIGEM                PIC 9(011) VALUE ZEROS.
       01 AUX-QT-ERRO               PIC 9(008) VALUE ZEROS.
       01 AUX-QT-ERRO-NB            PIC 9(008) VALUE ZEROS.
       01 AUX-QT-ERRO-PAGTO         PIC 9(008) VALUE ZEROS.
       01 AUX-QT-ERRO-NATUR-CREDITO PIC 9(008) VALUE ZEROS.
       01 AUX-QT-ERRO-PERIODO       PIC 9(008) VALUE ZEROS.
       01 AUX-QT-ERRO-VALIDADE      PIC 9(008) VALUE ZEROS.
       01 AUX-QT-ERRO-BANCO         PIC 9(008) VALUE ZEROS.
       01 AUX-QT-ERRO-UF            PIC 9(008) VALUE ZEROS.
       01 AUX-QT-ERRO-VALOR         PIC 9(008) VALUE ZEROS.
       01 AUX-QT-ERRO-NIT           PIC 9(008) VALUE ZEROS.
       01 AUX-SEQ-REG               PIC 9(009) VALUE ZEROS.
       01 AUX1-SEQ-PROC             PIC 9(011) VALUE ZEROS.
       01 AUX1-VL-LIQUIDO-CRED      PIC 9(010)V99.
       01 AUX1-IN-CREDITO-EPU       PIC 9(001).
       01 AUX1-DT-MOV-CREDITO       PIC 9(008).
       01 AUX1-NU-NB                PIC 9(010).
       01 AUX-NB                    PIC 9(009).
       01 AUX1-ID-EMPRESTIMO        PIC X(020).
       01 AUX-NIT                   PIC 9(010).
       01 AUX1-IN-CR-BLOQUEADO      PIC 9(001).
       01 AUX1-DT-INI-PERIODO       PIC 9(008).
       01 AUX1-DT-FIM-PERIODO       PIC 9(008).
       01 AUX1-DT-INI-VALIDADE      PIC 9(008).
       01 AUX1-DT-FIM-VALIDADE      PIC 9(008).
       01 AUX1-ID-BANCO             PIC 9(003).
       01 AUX1-NU-CONTA-CORRENTE    PIC X(010).
       01 AUX1-CS-ESPECIE           PIC 9(003).
       01 AUX1-ID-PREGAO            PIC 9(002).
       01 AUX1-ID-LOTE-PREGAO       PIC 9(002).
       01 AUX1-CS-ORIGEM-ORCAMENTO  PIC 9(002).
      *
       01 AUX1-NU-CPF-TITULAR       PIC 9(011).
       01 AUX1-NU-NIT-TITULAR       PIC 9(011).
      *
       01 AUX1-DT-COMP-ATU-VL-CR    PIC 9(006).

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
          88 AUX1-CS-TRAT-ESTAT-SPG     VALUE 07 09 16 26 70 71 72 73 74
                                              75.
          88 AUX1-CS-TRAT-ZIKA          VALUE 82.

      *

       01 AUX1-DT-COMPET-CREDITO    PIC 9(006).
       01 AUX1-CS-ORIGEM-CREDITO    PIC 9(001).
       01 AUX1-QT-RUBRICA           PIC 9(002).
       01 AUX1-CS-RUBRICA1          PIC 9(003).
       01 AUX1-CS-RUBRICA2          PIC 9(003).
       01 AUX1-VL-RUBRICA1          PIC 9(010)V99.
       01 AUX1-VL-RUBRICA2          PIC 9(010)V99.
       01 AUX1-CS-NATUR-CREDITO     PIC 9(002).
       01 AUX1-MEIO-PAGTO           PIC 9(002).
       01 AUX1-DT-DDB               PIC 9(008).
       01 AUX1-IN-RRA-EXERC-ANT     PIC 9(001).
       01 AUX1-IN-CREDITO-RRA       PIC 9(001).
       01 AUX1-IN-RRA-PARCELADO     PIC 9(001).
       01 AUX1-IN-CRED-SUBSTIT      PIC 9(001).
       01 AUX1-IN-RRA-ATRAS-CONC    PIC 9(001).

       01 AUX1-CS-RAMO-ATIVIDADE    PIC 9(002).
          88 AUX1-RURAL                 VALUE 08.
       01 WS-URBANO-RURAL           PIC 9(001) VA 0.

       01 AUX1-ID-ORGAO-PAGADOR     PIC 9(006).
          88 IN-ACORDO-INTERNACIONAL VALUE 101908 101840
                                           367791 367804
                                           367811 367828
                                           408577 408584
                                           408591 752781
                                           767637 772874
                                           795757 763868
                                           817392 796729
                                           763875 817474
                                           822995 824364
                                           840979.

       01 AUX1-ID-OL-MANUTENCAO     PIC 9(008).
      *01 AUX1-ID-OL-MANUTENCAO-R REDEFINES AUX1-ID-OL-MANUTENCAO.
      *   03 FILLER                 PIC X(001).
      *   03 AUX1-ID-OL-MANUT       PIC 9(007).
       01 AUX1-DT-SOLIC-CREDITO     PIC 9(008).
       01 AUX1-NU-SEQ-SOLIC-CRED    PIC 9(008).
       01 AUX1-CS-UF                PIC 9(002).
       01 AUX1-IN-CRED-ISENTO-IR    PIC 9(001).
       01 AUX1-IN-CRED-CANCELADO    PIC 9(001).
       01 AUX1-IN-CRED-ATRASADO     PIC 9(001).
       01 AUX1-ID-CALAMIDADE        PIC 9(005).
       01 AUX1-IN-CRED-CART-SOC     PIC 9(001).
       01 AUX1-IN-CRED-PIONEIRA     PIC 9(001).
       01 AUX1-ID-NIT               PIC 9(010).
       01 AUX1-NU-NIT-DV1           PIC 9(001).
       01 AUX1-IN-CRED-PROG-SEAS    PIC 9(001).
       01 AUX1-CS-ROTINA-PAGTO      PIC 9(001).
       01 AUX1-CS-UF-OL-ACAO-JUD    PIC X(002).
       01 AUX1-CS-MUNICIPIO         PIC 9(006).
       01 AUX1-NU-VARA-ACAO-JUD     PIC 9(002).
       01 AUX1-NU-PROC-ACAO-JUD     PIC X(020).
       01 AUX1-IN-ADIANT-13-SAL     PIC 9(001).
       01 AUX1-IN-IR-ADIAN-13SAL    PIC 9(001).
       01 AUX1-IN-BANCO-POSTAL      PIC 9(001).
        01 WS-OFIS-TITLE             PIC X(030) VA
          "W/SUB/CPG/001/OFIS ON PKDADOS.".
       01 WS-OFIS-REMETENTE         PIC X(012) VA "RCTRLSUB".
      *01 WS-OFIS-REMETENTE         PIC X(012) VA "XRCTRLSUB".
      *01 WS-OFIS-LISTA             PIC X(012) VA "SUBTESTE".
       01 WS-OFIS-LISTA             PIC X(012) VA "SUBCPG001".
       01 WS-OFIS-GAVETA            PIC X(012) VA "ZERO".
       01 WS-OFIS-PASTA             PIC X(012) VA "CPG".
       01 WS-OFIS-ASSUNTO.
          03 FILLER                 PIC X(004) VA "ESQ".
          03 WS-OFIS-ESQUEMA        PIC 9(005).
          03 FILLER                 PIC X(012) VA "-LSUBCPG001A".
          03 FILLER                 PIC X(019) VA SPACES.
       01  TABELA-MEIO-PAGTO.
           03 FILLER PIC X(45) VA
           "01CARTAO MAGNETICO                        CMG".
           03 FILLER PIC X(45) VA
           "02CONTA CORRENTE FITA MAGNETICA           CCF".
           03 FILLER PIC X(45) VA
           "03CONTA CORRENTE LISTAGEM                 CCL".
           03 FILLER PIC X(45) VA
           "04ORDEM BANCARIA DE PAGAMENTO             OBP".
           03 FILLER PIC X(45) VA
           "05AUTORIZACAO DE PAGAMENTO DE BENEFICIO   APB".
           03 FILLER PIC X(45) VA
           "06CHEQUE DE PAGAMENTO DE BENEFICIO        CPB".
           03 FILLER PIC X(45) VA
           "07ORDEM DE PAGAMENTO DE BENEFICIO         OPB".
           03 FILLER PIC X(45) VA
           "08RELACAO DE PAGAMENTO DE BENEFICIO       RPB".
       01 WS-TABELAS REDEFINES TABELA-MEIO-PAGTO.
          03 WS-TAB-MEIO-PAGTO OCCURS 08 TIMES.
             07 TAB-CS-MEIO-PAGTO       PIC 99.
             07 TAB-TE-MEIO-PAGTO       PIC X(040).
             07 TAB-TE-ABRE-PAGTO       PIC X(003).

       01  WS-TABELA-RUBRICAS           COMP.
           03 WS-TAB-RUBRICAS OCCURS 50 TIMES.
              05  WS-TAB-CS-RUBR-ENTICA     PIC 9(003).
              05  WS-TAB-VL-RUBRICA     PIC 9(010)V99.
              05 FILLER                 PIC 9(001).

      *----------------------------------------------------------------*
      *       TABELAS TOTALIZACOES PARA ARQUIVO DE CONTABILIDADE       *
      *----------------------------------------------------------------*

       01 WS-TAB-TOTAL-ESTESP-1 BINARY.
          03 WS-TB-TOT-ESP-EST-1-C      OC 05.
             05 WS-TB-TOT-ESP-ESTADO-1  OC 28.
                07 WS-TB-TOT-ESP-1      OC 99.
                   09 WS-TB-TOT-BRUTO-ESP1       PIC 9(12)V99.
                   09 WS-TB-TOT-DESC-ESP1        PIC 9(12)V99.
                07 WS-TB-TOT-GER-BRUTO-ESP1      PIC 9(12)V99.
                07 WS-TB-TOT-GER-DESC-ESP1       PIC 9(12)V99.

       01 WS-TAB-TOTAL-ESTESP-1A BINARY.
          03 WS-TB-TOT-ESP-EST-1-CA     OC 05.
             05 WS-TB-TOT-ESP-ESTADO-1A OC 28.
                07 WS-TB-TOT-ESP-1A     OC 99.
                   09 WS-TB-TOT-ESP1             PIC 9(08).
                07 WS-TB-TOT-GER-ESP1            PIC 9(08).

       01 WS-TAB-TOTAL-ESTESP-2 BINARY.
          03 WS-TB-TOT-ESP-EST-2-C      OC 05.
             05 WS-TB-TOT-ESP-ESTADO-2  OC 28.
                07 WS-TB-TOT-ESP-2      OC 99.
                   09 WS-TB-TOT-BRUTO-ESP2       PIC 9(12)V99.
                   09 WS-TB-TOT-DESC-ESP2        PIC 9(12)V99.
                07 WS-TB-TOT-GER-BRUTO-ESP2      PIC 9(12)V99.
                07 WS-TB-TOT-GER-DESC-ESP2       PIC 9(12)V99.

       01 WS-TAB-TOTAL-ESTESP-2A BINARY.
          03 WS-TB-TOT-ESP-EST-2-CA     OC 05.
             05 WS-TB-TOT-ESP-ESTADO-2A OC 28.
                07 WS-TB-TOT-ESP-2A     OC 99.
                   09 WS-TB-TOT-ESP2             PIC 9(08).
                07 WS-TB-TOT-GER-ESP2            PIC 9(08).

       01 WS-TAB-TOTAL-ESTESP-3 BINARY.
          03 WS-TB-TOT-ESP-EST-3-C      OC 05.
             05 WS-TB-TOT-ESP-ESTADO-3  OC 28.
                07 WS-TB-TOT-ESP-3      OC 99.
                   09 WS-TB-TOT-BRUTO-ESP3       PIC 9(12)V99.
                   09 WS-TB-TOT-DESC-ESP3        PIC 9(12)V99.
                07 WS-TB-TOT-GER-BRUTO-ESP3      PIC 9(12)V99.
                07 WS-TB-TOT-GER-DESC-ESP3       PIC 9(12)V99.

       01 WS-TAB-TOTAL-ESTESP-3A BINARY.
          03 WS-TB-TOT-ESP-EST-3-CA     OC 05.
             05 WS-TB-TOT-ESP-ESTADO-3A OC 28.
                07 WS-TB-TOT-ESP-3A     OC 99.
                   09 WS-TB-TOT-ESP3             PIC 9(08).
                07 WS-TB-TOT-GER-ESP3            PIC 9(08).

       01 WS-TAB-TOTAL-ESTESP-4 BINARY.
          03 WS-TB-TOT-ESP-EST-4-C      OC 05.
             05 WS-TB-TOT-ESP-ESTADO-4  OC 28.
                07 WS-TB-TOT-ESP-4      OC 99.
                   09 WS-TB-TOT-BRUTO-ESP4       PIC 9(12)V99.
                   09 WS-TB-TOT-DESC-ESP4        PIC 9(12)V99.
                07 WS-TB-TOT-GER-BRUTO-ESP4      PIC 9(12)V99.
                07 WS-TB-TOT-GER-DESC-ESP4       PIC 9(12)V99.

       01 WS-TAB-TOTAL-ESTESP-4A BINARY.
          03 WS-TB-TOT-ESP-EST-4-CA     OC 05.
             05 WS-TB-TOT-ESP-ESTADO-4A OC 28.
                07 WS-TB-TOT-ESP-4A     OC 99.
                   09 WS-TB-TOT-ESP4             PIC 9(08).
                07 WS-TB-TOT-GER-ESP4            PIC 9(08).

       01 WS-TAB-TOTAL-ESTESP-5 BINARY.
          03 WS-TB-TOT-ESP-EST-5-C      OC 05.
             05 WS-TB-TOT-ESP-ESTADO-5  OC 28.
                07 WS-TB-TOT-ESP-5      OC 99.
                   09 WS-TB-TOT-BRUTO-ESP5       PIC 9(12)V99.
                   09 WS-TB-TOT-DESC-ESP5        PIC 9(12)V99.
                07 WS-TB-TOT-GER-BRUTO-ESP5      PIC 9(12)V99.
                07 WS-TB-TOT-GER-DESC-ESP5       PIC 9(12)V99.

       01 WS-TAB-TOTAL-ESTESP-5A BINARY.
          03 WS-TB-TOT-ESP-EST-5-CA     OC 05.
             05 WS-TB-TOT-ESP-ESTADO-5A OC 28.
                07 WS-TB-TOT-ESP-5A     OC 99.
                   09 WS-TB-TOT-ESP5             PIC 9(08).
                07 WS-TB-TOT-GER-ESP5            PIC 9(08).

       01 WS-TAB-TOTAL-ESTESP-6 BINARY.
          03 WS-TB-TOT-ESP-EST-6-C      OC 05.
             05 WS-TB-TOT-ESP-ESTADO-6  OC 28.
                07 WS-TB-TOT-ESP-6      OC 99.
                   09 WS-TB-TOT-BRUTO-ESP6       PIC 9(12)V99.
                   09 WS-TB-TOT-DESC-ESP6        PIC 9(12)V99.
                07 WS-TB-TOT-GER-BRUTO-ESP6      PIC 9(12)V99.
                07 WS-TB-TOT-GER-DESC-ESP6       PIC 9(12)V99.

       01 WS-TAB-TOTAL-ESTESP-6A BINARY.
          03 WS-TB-TOT-ESP-EST-6-CA     OC 05.
             05 WS-TB-TOT-ESP-ESTADO-6A OC 28.
                07 WS-TB-TOT-ESP-6A     OC 99.
                   09 WS-TB-TOT-ESP6             PIC 9(08).
                07 WS-TB-TOT-GER-ESP6            PIC 9(08).

       01 WS-TAB-TOTAL-ESTESP-7 BINARY.
          03 WS-TB-TOT-ESP-EST-7-C      OC 05.
             05 WS-TB-TOT-ESP-ESTADO-7  OC 28.
                07 WS-TB-TOT-ESP-7      OC 99.
                   09 WS-TB-TOT-BRUTO-ESP7       PIC 9(12)V99.
                   09 WS-TB-TOT-DESC-ESP7        PIC 9(12)V99.
                07 WS-TB-TOT-GER-BRUTO-ESP7      PIC 9(12)V99.
                07 WS-TB-TOT-GER-DESC-ESP7       PIC 9(12)V99.

       01 WS-TAB-TOTAL-ESTESP-7A BINARY.
          03 WS-TB-TOT-ESP-EST-7-CA     OC 05.
             05 WS-TB-TOT-ESP-ESTADO-7A OC 28.
                07 WS-TB-TOT-ESP-7A     OC 99.
                   09 WS-TB-TOT-ESP7             PIC 9(08).
                07 WS-TB-TOT-GER-ESP7            PIC 9(08).


       01 WS-TAB-TOTAL-ESTESP-8 BINARY.
          03 WS-TB-TOT-ESP-EST-8-C      OC 05.
             05 WS-TB-TOT-ESP-ESTADO-8  OC 28.
                07 WS-TB-TOT-ESP-8      OC 99.
                   09 WS-TB-TOT-BRUTO-ESP8       PIC 9(12)V99.
                   09 WS-TB-TOT-DESC-ESP8        PIC 9(12)V99.
                07 WS-TB-TOT-GER-BRUTO-ESP8      PIC 9(12)V99.
                07 WS-TB-TOT-GER-DESC-ESP8       PIC 9(12)V99.

       01 WS-TAB-TOTAL-ESTESP-8A BINARY.
          03 WS-TB-TOT-ESP-EST-8-CA     OC 05.
             05 WS-TB-TOT-ESP-ESTADO-8A OC 28.
                07 WS-TB-TOT-ESP-8A     OC 99.
                   09 WS-TB-TOT-ESP8             PIC 9(08).
                07 WS-TB-TOT-GER-ESP8            PIC 9(08).

       01 WS-TAB-TOTAL-ESTESP-9 BINARY.
          03 WS-TB-TOT-ESP-EST-9-C      OC 05.
             05 WS-TB-TOT-ESP-ESTADO-9  OC 28.
                07 WS-TB-TOT-ESP-9      OC 99.
                   09 WS-TB-TOT-BRUTO-ESP9       PIC 9(12)V99.
                   09 WS-TB-TOT-DESC-ESP9        PIC 9(12)V99.
                07 WS-TB-TOT-GER-BRUTO-ESP9      PIC 9(12)V99.
                07 WS-TB-TOT-GER-DESC-ESP9       PIC 9(12)V99.

       01 WS-TAB-TOTAL-ESTESP-9A BINARY.
          03 WS-TB-TOT-ESP-EST-9-CA     OC 05.
             05 WS-TB-TOT-ESP-ESTADO-9A OC 28.
                07 WS-TB-TOT-ESP-9A     OC 99.
                   09 WS-TB-TOT-ESP9             PIC 9(08).
                07 WS-TB-TOT-GER-ESP9            PIC 9(08).

       01 WS-TAB-TOTAL-DIVTIP BINARY.
          03 WS-TB-TOT-ESP-EST-D-C       OC 05.
             05 WS-TB-TOT-ESP-ESTADO-DIV OC 28.
                07 WS-TB-TOT-ESP-EST-DIV-TP OC 10.
                   09 WS-TOT-IR-PREV             PIC 9(12)V99.
                   09 WS-TOT-IR-UNIAO            PIC 9(12)V99.
                   09 WS-TOT-CONT-PREV           PIC 9(12)V99.
                   09 WS-TOT-PA                  PIC 9(12)V99.
                   09 WS-TOT-13-SAL              PIC 9(12)V99.
                   09 WS-TOT-SF                  PIC 9(12)V99.
                   09 WS-TOT-CPMF                PIC 9(12)V99.
                   09 WS-TOT-CONSIG              PIC 9(12)V99.

       01 WS-TAB-TOTAL-DIVTIP2 BINARY.
          03 WS-TB-TOT-ESP-EST-D-C2          OC 05.
             05 WS-TB-TOT-ESP-ESTADO-DIV2    OC 28.
                07 WS-TB-TOT-ESP-EST-DIV-TP2 OC 10.
                   09 WS-TOT-COD-CONSIG OC 16    PIC 9(12)V99.
      *
      *   A TABELA CRIADA ABAIXO EH COM O OBJETIVO DE EMITIR O RELATORIO
      *   DE EPU. AS OCORRENCIAS ESTAO DISTRIBUIDAS DA SEGUINTE FORMA:
      *   1 - ECT              (TRATAMENTO 64 OU 65)
      *   2 - REDE FERROVIARIA (TRATAMENTO 54, 59 OU 60)
      *   3 - ESTATUTARIO      (TRATAMENTO 07, 09, 16, 26, 70 AO 75)
      *   4 - ANISTIADOS       (ESPECIES 58 OU 59)
      *   5 - TALIDOMIDA       (ESPECIE  56)
      *   6 - LOAS             (ESPECIES 87 OU 88)
      *   8 - RMV-DEF          (ESPECIES 11, 30)
      *   9 - RMV-ID           (ESPECIES 12, 40)
      *   10- HANSENIASE       (ESPECIE  96)
      *

       01 WS-TABELA-EPU-REL.
          03 WS-ESTADO-TABELA-EPU-REL  OCCURS 28 TIMES.
             05 WS-ITEM-TABELA-EPU-REL OCCURS 12 TIMES.
                07 WS-QT-EPU-REL      PIC 9(08).
                07 WS-VL-EPU-REL      PIC 9(15)V99.
      *
       01 WS-TABELA-DESC-EPU-REL.
          03 FILLER    PIC X(19) VALUE "ECT                ".
          03 FILLER    PIC X(19) VALUE "REDE FERROVIARIA   ".
          03 FILLER    PIC X(19) VALUE "ESTATUTARIO        ".
          03 FILLER    PIC X(19) VALUE "ANISTIADOS         ".
          03 FILLER    PIC X(19) VALUE "TALIDOMIDA         ".
          03 FILLER    PIC X(19) VALUE "LOAS-87-88         ".
          03 FILLER    PIC X(19) VALUE "HEMODIALISE-CARUARU".
          03 FILLER    PIC X(19) VALUE "RMV  11-30         ".
          03 FILLER    PIC X(19) VALUE "RMV  12-40         ".
          03 FILLER    PIC X(19) VALUE "HANSENIASE (ESP 96)".
          03 FILLER    PIC X(19) VALUE "ZIKA(ESP 60 TRT 82)".
       01 WS-TABELA-DESC-EPU-REL-R REDEFINES WS-TABELA-DESC-EPU-REL.
          03 WS-DESC-EPU-TAB OCCURS 11 TIMES PIC X(19).

        01 WS-TAB-RUBRICAS-AUX.
           03 WS-TB-RUBRICAS-AUX OC 999.
              05 WS-TB-VL-RUBRICA-AUX            PIC 9(12)V99.

        01 WS-TAB-COMPET.
           03 WS-TB-COMPET OC 5                  PIC 9(06).

        01 WS-TAB-SIGL-UF.
           03 FILLER                             PIC X(56) VALUE
           "ALAMBACEMSESGOMAMTMGPAPBPRPEPIRJRNRSSCSPSEDFACAPRORRTOBR".

        01 WS-TAB-SIGL-UF-R REDEFINES WS-TAB-SIGL-UF.
           03 WS-TB-SIGL-UF OC 28                PIC X(02).

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
            03 FILLER                    PIC X(02) VA "31".
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

         01 TAB-GR-R REDEFINES TAB-GRUPO-ESPECIE.
            03 TAB-GR-ESP            OC 99  PIC 9(02).

      *----------------------------------------------------------------
       01 WS-DATA-SIS                   PIC 9(08).
       01 WS-DATA-SISR REDEFINES WS-DATA-SIS.
          05 WS-AA-SIS                  PIC 9999.
          05 WS-MM-SIS                  PIC 99.
          05 WS-DD-SIS                  PIC 99.

       01 TAB-CREDPROC.
          05 TAB-CP OCCURS 100 TIMES.
             10 TCP-DATA                PIC 9(08).
             10 TCP-LIDOS-QT            PIC 9(08).
             10 TCP-ACEITOS-QT          PIC 9(08).
             10 TCP-REJ-QT              PIC 9(08).
             10 TCP-LIDOS-VR            PIC 9(15)V99.
             10 TCP-ACEITOS-VR          PIC 9(15)V99.
             10 TCP-REJ-VR              PIC 9(15)V99.

       01 WS-DATA-SIS-INV               PIC 9(08).
       01 WS-DATA-SIS-INVR REDEFINES WS-DATA-SIS-INV.
          05 WS-AA-SIS-INV              PIC 9999.
          05 WS-MM-SIS-INV              PIC 99.
          05 WS-DD-SIS-INV              PIC 99.
       01 WS-DATA-SIS-INVRR REDEFINES WS-DATA-SIS-INV.
          05 WS-COMP-SIS-INV            PIC 9(06).
          05 FILLER                     PIC 9(02).

       01 WS-DT-ANTES-6-MESES           PIC 9(08).
       01 WS-DT-ANTES-6-MESES-R REDEFINES WS-DT-ANTES-6-MESES.
          03 WS-ANO-ANTES-6-MESES       PIC 9(04).
          03 WS-MES-ANTES-6-MESES       PIC 9(02).
          03 WS-DIA-ANTES-6-MESES       PIC 9(02).
      *
       01 WS-OL-MANUT                   PIC 9(08).
       01 WS-OL-MANUTR REDEFINES WS-OL-MANUT.
          05 WS-UF                      PIC 9(02).
          05 FILLER                     PIC 9(06).

       01 WS-HORA-SIS                   PIC 9(08).
       01 WS-HORA-SISR REDEFINES WS-HORA-SIS.
          05 WS-HH                      PIC 9(06).
          05 FILLER                     PIC 9(02).

       01 WS-HH-INT                    PIC 9(06).
       01 WS-HH-INTR REDEFINES WS-HH-INT.
          05 WS-HH-MM                  PIC 9(04).
          05 FILLER                    PIC 9(02).

       01 WS-DATAS.
          03 WS-DT-AUX1-DDMMAA         PIC 9(08).
          03 WS-DT-AUX1-DDMMAAR REDEFINES WS-DT-AUX1-DDMMAA.
             05 WS-DT-AUX1-DIA         PIC 9(02).
             05 WS-DT-AUX1-MES         PIC 9(02).
             05 WS-DT-AUX1-ANO         PIC 9(04).
          03 WS-DT-AUX2-AAMMDD         PIC 9(08).
          03 WS-DT-AUX2-AAMMDDR REDEFINES WS-DT-AUX2-AAMMDD.
             05 WS-DT-AUX2-ANO         PIC 9(04).
             05 WS-DT-AUX2-MES         PIC 9(02).
             05 WS-DT-AUX2-DIA         PIC 9(02).
          03 WS-DT-AUX3-DDMMAA         PIC 9(08).
          03 WS-DT-AUX3-DDMMAAR REDEFINES WS-DT-AUX3-DDMMAA.
             05 WS-DT-AUX3-DD          PIC 99.
             05 WS-DT-AUX3-MM          PIC 99.
             05 WS-DT-AUX3-AA          PIC 9999.
          03 WS-DT-AUX4-AAMMDD         PIC 9(08).
          03 WS-DT-AUX4-AAMMDDR REDEFINES WS-DT-AUX4-AAMMDD.
             05 WS-DT-AUX4-COMPET      PIC 9(06).
             05 WS-DT-AUX4-DD          PIC 9(02).

       01  WS-DT-DDMMAA              PIC 9(08).
       01  WS-DT-DDMMAA-R REDEFINES WS-DT-DDMMAA.
           05 WS-DT-DIA              PIC 9(02).
           05 WS-DT-MES              PIC 9(02).
           05 WS-DT-ANO              PIC 9(04).

       01  WS-DATA-1                    PIC 9(08).
       01  WS-DATA-1-R  REDEFINES WS-DATA-1.
           03 WS-ANO                    PIC 9(04).
           03 WS-MES                    PIC 9(02).
           03 WS-DIA                    PIC 9(02).

       01  WS-DATA-11                   PIC 9(08).
       01  WS-DATA-11-R  REDEFINES WS-DATA-11.
           03 WS-DIA                    PIC 9(02).
           03 WS-MES                    PIC 9(02).
           03 WS-ANO                    PIC 9(04).

       01  WS-COMPET                    PIC 9(06).
       01  WS-COMPET-R  REDEFINES WS-COMPET.
           03 WS-AA-COMPET              PIC 9(04).
           03 WS-MM-COMPET              PIC 9(02).

       77  W77-MEIO-OK                  PIC 9(01).
       77  W77-IND-FERIADO              PIC 9(01) VALUE 0.
       77  W77-NUM-DIAS                 PIC 9(03) BINARY.
       77  W77-DATA-RESPOSTA            PIC 9(08) BINARY.
       77  W77-DATA-INIC                PIC 9(08) BINARY.
       77  W77-CONT-DIA-UTIL            PIC 9(02).

       77  W77-DIASEM-RESP              PIC 9(01) BINARY.
           88 ERRO-NA-DATA-INFORMADA    VALUE 0.
           88 DOMINGO                   VALUE 1.
           88 SEGUNDA                   VALUE 2.
           88 TERCA                     VALUE 3.
           88 QUARTA                    VALUE 4.
           88 QUINTA                    VALUE 5.
           88 SEXTA                     VALUE 6.
           88 SABADO                    VALUE 7.

       77  W77-FLAG-DIA-UTIL            PIC 9(01).
           88 DIA-UTIL                  VALUE 1.
           88 FERIADO                   VALUE 2.

       01  WS-PERGUNTA                  PIC 9(03) COMP.
       01  WS-RESPOSTA                  PIC X(25).
       01  W77-BANCO-OK-X.
           03 W77-BANCO-OK              PIC 9.

       COPY "*B/SUB/CPG/2K/042 ON PKBDSGF".
      *--------------------------------------------------------*
      *                                                        *
      *  RELATORIO DE CONTROLE DA ROTINA DE CARGA DO MOVIMENTO *
      *  DE CREDITOS.                                          *
      *                                                        *
      *--------------------------------------------------------*

       01 LINHAS-RELATORIO.
          03 L001A-SPACES.
             05 FILLER                        PIC X(080) VALUE SPACES.

          03 L001A-SUB.
             05 FILLER                        PIC X(080) VALUE ALL "-".

          03 L001A-CAB1.
             05 FILLER                        PIC X(016) VALUE
                "MPAS/DATAPREV".
             05 FILLER                        PIC X(048) VALUE
                "INSS - INSTITUTO NACIONAL DO SEGURO SOCIAL".
             05 FILLER                        PIC X(006) VALUE
                "DATA: ".
             05 L001A-CAB1-DATA               PIC 99/99/9999.

          03 L001A-CAB2.
             05 FILLER                        PIC X(026) VALUE
                "P/SUB/CPG/001".
             05 FILLER                        PIC X(040) VALUE
                "SISTEMA UNICO DE BENEFICIOS".
             05 FILLER                        PIC X(009) VALUE
                "HORA:".
             05 L001A-CAB2-HORA               PIC 99.99.

          03 L001A-CAB3.
             05 FILLER                        PIC X(013) VALUE
                "LSUBCPG001A".
             05 FILLER                        PIC X(053) VALUE
                "CONTROLE DA ROTINA DE CARGA DO MOVIMENTO DE CREDITOS".
             05 FILLER                        PIC X(007) VALUE
                "PAG.:".
             05 L001A-CAB3-PAG                PIC ZZZ.ZZ9.

          03 L001B-CAB4.
             05 FILLER                        PIC X(013) VALUE
                "LSUBCPG001B".
             05 FILLER                        PIC X(053) VALUE
                "*****    ENCARGOS PREVIDENCIARIOS DA UNIAO     *****".
             05 FILLER                        PIC X(007) VALUE
                "PAG.:".
             05 L001B-CAB-PAG                 PIC ZZZ.ZZ9.
      *
          03 L001B-CAB5.
             05 FILLER                        PIC X(005) VALUE
                "UF - ".
             05 NM-SIGLA-UF-CAB5              PIC X(002).
             05 FILLER                        PIC X(073) VALUE SPACES.
      *
          03 L001B-CAB6.
             05 FILLER                        PIC X(006) VALUE SPACES.
             05 FILLER                        PIC X(024) VALUE
                "DISCRIMINACAO".
             05 FILLER                        PIC X(023) VALUE
                "QUANTIDADE".
             05 FILLER                        PIC X(026) VALUE
                "VALOR R$".
      *
          03 L001B-CAB7.
             05 FILLER                        PIC X(006) VALUE SPACES.
             05 FILLER                        PIC X(020) VALUE
                "------------------- ".
             05 FILLER                        PIC X(054) VALUE ALL "-".
      *
          03 L001B-DET.
             05 FILLER                       PIC X(006) VALUE SPACES.
             05 DESC-DET-001B                PIC X(019).
             05 FILLER                       PIC X(006) VALUE ":     ".
             05 QT-DET-001B                  PIC ZZ.ZZZ.ZZ9.
             05 FILLER                       PIC X(006) VALUE SPACES.
             05 VL-DET-001B                  PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
             05 FILLER                       PIC X(011) VALUE SPACES.
      *
          03 L001A-RESUP1.
             05 FILLER                        PIC X(041) VALUE
                "TEMPO DE PROCESSAMENTO (HH:MM:SS):".
             05 FILLER                        PIC X(020) VALUE
                "PARAMETROS:".
             05 FILLER                        PIC X(019) VALUE
                "ESCORE:".

          03 L001A-RESUP2.
             05 FILLER                        PIC X(034) VALUE ALL "-".
             05 FILLER                        PIC X(007) VALUE SPACES.
             05 FILLER                        PIC X(011) VALUE ALL "-".
             05 FILLER                        PIC X(009) VALUE SPACES.
             05 FILLER                        PIC X(007) VALUE ALL "-".
             05 FILLER                        PIC X(012) VALUE SPACES.

          03 L001A-RESUP3.
             05 FILLER                        PIC X(028) VALUE
                "INICIO  DO PROCESSAMENTO : ".
             05 L001A-RESUP3-INICP            PIC 99.99.99.
             05 FILLER                        PIC X(005) VALUE SPACES.
             05 FILLER                        PIC X(011) VALUE
                "NUM.ESQ. : ".
             05 L001A-RESUP3-NRESQ            PIC 9(05).
             05 FILLER                        PIC X(015) VALUE
                "    QT-ACEITO: ".
             05 L001A-RESUP3-QT-AC            PIC ZZ9,99.

          03 L001A-RESUP4.
             05 FILLER                        PIC X(028) VALUE
                "TERMINO DO PROCESSAMENTO : ".
             05 L001A-RESUP4-FIMP             PIC 99.99.99.
             05 FILLER                        PIC X(005) VALUE SPACES.
             05 FILLER                        PIC X(011) VALUE
                "MOV.CRED.: ".
             05 L001A-RESUP4-MOVCRED          PIC X(05).
             05 FILLER                        PIC X(012) VALUE SPACES.

          03 L001A-RESUP5.
             05 FILLER                        PIC X(028) VALUE
                "ESTIMATIVA DE TEMPO GASTO: ".
             05 L001A-RESUP5-ESTIMAT          PIC 99.99.99.
             05 FILLER                        PIC X(005) VALUE SPACES.
             05 FILLER                        PIC X(011) VALUE
                "SEAS     : ".
             05 L001A-RESUP5-SEAS             PIC X(03).
             05 FILLER                        PIC X(014) VALUE SPACES.

          03 L001A-REJ0.
             05 FILLER                        PIC X(16) VALUE SPACES.
             05 FILLER                        PIC X(38) VALUE
                "MOVIMENTO                          :".
             05 L001A-REJ0-MOV                PIC X(11).
             05 FILLER                        PIC X(15) VALUE SPACES.

          03 L001A-REJ1.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "TOTAL DE CREDITOS REJEITADOS       :".
             05 L001A-REJ-REJEITADOS          PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-REJ1A.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "TOTAL DE CREDITOS DELETADOS        :".
             05 L001A-REJ-DELETADOS           PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-REJ1A2.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "TOTAL DE CREDS. DESPREZ. NO PAB     ".
             05 FILLER                        PIC X(026) VALUE SPACES.

          03 L001A-REJ1A3.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "(NU-SEQ-PROC > GLOBAL)             :".
             05 L001A-REJ-NU-SEQ-PROC-PAB     PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.
      *
          03 L001A-REJ1B.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "TOTAL DE CREDITOS MARCA ERRO       :".
             05 L001A-REJ-MARCA-ERRO          PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-REJ2.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(048) VALUE
                "ERROS DE REJEICAO                    QUANTIDADES".

          03 L001A-REJ3.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "1- ERRO NO NB".
             05 L001A-REJ3-ERRO-NB            PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-REJ4.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "2- ERRO NA NATUREZA DO CREDITO".
             05 L001A-REJ4-ERRO-NAT           PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-REJ5.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "3- ERRO NO MEIO DE PAGAMENTO  ".
             05 L001A-REJ5-ERRO-MEIO          PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-REJ6.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "4- ERRO NO PERIODO            ".
             05 L001A-REJ6-ERRO-PER           PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-REJ7.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "5- ERRO NA VALIDADE           ".
             05 L001A-REJ7-ERRO-VAL           PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-REJ8.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "6- ERRO NO BANCO              ".
             05 L001A-REJ8-ERRO-BANCO         PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-REJ9.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "7- ERRO NA UF                 ".
             05 L001A-REJ9-ERRO-UF            PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-REJ10.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "8- ERRO NO VALOR              ".
             05 L001A-REJ10-ERRO-VALOR        PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-REJ11.
             05 FILLER                        PIC X(016) VALUE SPACES.
             05 FILLER                        PIC X(038) VALUE
                "9- ERRO NO NIT                ".
             05 L001A-REJ11-ERRO-NIT          PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(016) VALUE SPACES.

          03 L001A-CRED1.
             05 FILLER                        PIC X(035) VALUE SPACES.
             05 FILLER                        PIC X(045) VALUE
                "CREDITOS PROCESSADOS".

          03 L001A-CRED2.
             05 FILLER                        PIC X(011) VALUE SPACES.
             05 FILLER                        PIC X(068) VALUE ALL "-".
             05 FILLER                        PIC X(001) VALUE SPACES.

          03 L001A-CRED3.
             05 FILLER                        PIC X(001) VALUE SPACES.
             05 FILLER                        PIC X(039) VALUE
                "DATA DO".
             05 FILLER                        PIC X(040) VALUE
                "QUANTIDADES".

          03 L001A-CRED4.
             05 FILLER                        PIC X(001) VALUE SPACES.
             05 FILLER                        PIC X(041) VALUE
                "MOVIMENTO".
             05 FILLER                        PIC X(038) VALUE
                "VALORES".

          03 L001A-CRED6.
             05 FILLER                        PIC X(001) VALUE SPACES.
             05 FILLER                        PIC X(019) VALUE
                "MENSAGEM".
             05 FILLER                        PIC X(022) VALUE
                "LIDOS".
             05 FILLER                        PIC X(021) VALUE
                "ACEITOS".
             05 FILLER                        PIC X(017) VALUE
                "REJEITADOS".

          03 L001A-CRED7.
      *      05 FILLER                        PIC X(001) VALUE SPACES.
             05 FILLER                        PIC X(010) VALUE ALL "-".
             05 FILLER                        PIC X(001) VALUE SPACES.
             05 FILLER                        PIC X(022) VALUE ALL "-".
             05 FILLER                        PIC X(001) VALUE SPACES.
             05 FILLER                        PIC X(022) VALUE ALL "-".
             05 FILLER                        PIC X(001) VALUE SPACES.
             05 FILLER                        PIC X(022) VALUE ALL "-".
             05 FILLER                        PIC X(001) VALUE SPACES.

          03 L001A-CRED8.
      *      05 FILLER                        PIC X(001) VALUE SPACES.
      ********** UNISYS Y2K - Linha alterada em 24/06/1999 12:27:25
      *      05 L001A-CRED8-DTAMOV            PIC 99/99/99.
             05 L001A-CRED8-DTAMOV            PIC 99/99/9999.
      ********** UNISYS Y2K - Linha alterada em 24/06/1999 12:27:25
      *      05 FILLER                        PIC X(014) VALUE SPACES.
             05 FILLER                        PIC X(013) VALUE SPACES.
             05 L001A-CRED8-LIDOS             PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(013) VALUE SPACES.
             05 L001A-CRED8-ACEITOS           PIC ZZ.ZZZ.ZZ9.
             05 FILLER                        PIC X(013) VALUE SPACES.
             05 L001A-CRED8-REJEITADOS        PIC ZZ.ZZZ.ZZ9.

          03 L001A-CRED9.
             05 FILLER                        PIC X(003) VALUE SPACES.
             05 L001A-CRED9-MENSAGEM          PIC X(004).
             05 FILLER                        PIC X(004) VALUE SPACES.
             05 L001A-CRED9-LIDOSVR          PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
             05 FILLER                        PIC X(001) VALUE SPACES.
             05 L001A-CRED9-ACEITOSVR        PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
             05 FILLER                        PIC X(001) VALUE SPACES.
             05 L001A-CRED9-REJEITADOSVR     PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
             05 FILLER                        PIC X(001) VALUE SPACES.

      *
       01    LINHA-DET.
             03 FILLER                        PIC X(10) VALUE SPACES.
             03 TEXTO-DET                     PIC X(35) VALUE SPACES.
             03 FILLER                        PIC X(01) VALUE ":".
             03 QTDE-DET                      PIC ZZ.ZZZ.ZZ9.
             03 QTDE-DET-R REDEFINES QTDE-DET PIC X(10).
             03 VALOR-DET                    PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
             03 VALOR-DET-R REDEFINES VALOR-DET PIC X(22).
      *
             COPY "*B/DTP/PAD/01/CHECKPOINT/WS ON PKBDSGF".

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
                "P/SUB/CPG/001".
             05 FILLER                        PIC X(038) VALUE
                "SISTEMA UNICO DE BENEFICIOS".
             05 FILLER                        PIC X(009) VALUE
                "HORA:".
             05 LSPG1A-CAB2-HORA               PIC 99.99.

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

      ******************************************************************
      *                                                                *
      *           COMENTARIO SOBRE O CONTEUDO DO PARAMETRO             *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION
           USING  PAR-ESQUEMA , PAR-MOV-CREDITO ,
                                 W77-OFIS-CLASSE, PAR-SEAS.
      *
      * LINHA       124300*GATILHO PARA PRODUCAO AVULSO
      * E INIBIR OS PARAMETROS ABAIXO DEIXANDO SO PROCEDURE.
      * OS PARAMETROS MOVIDOS ESTAO ABAIXO DA DECLARATIVES..
      * PROCEDURE DIVISION.

       DECLARATIVES.
       DE-INTERRUPT SECTION.
           USE INTERRUPT PROCEDURE.
       DE-INTERROMPE.
           MOVE "LIDOS CONCESSAO ==>"     TO   WS-PARAM1-ZDISPLAY
           MOVE  W77-LIDOS-CONC           TO   WS-PARAM2-ZDISPLAY
           MOVE  1                        TO   WS-INTEIRO-ZDISPLAY
           CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE "LIDOS MANUTENCAO ==>"    TO   WS-PARAM1-ZDISPLAY
           MOVE  W77-LIDOS-MANUT          TO   WS-PARAM2-ZDISPLAY
           CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE "LIDOS ALTERNATIVO ==>"   TO   WS-PARAM1-ZDISPLAY
           MOVE  W77-LIDOS-ALTE           TO   WS-PARAM2-ZDISPLAY
           CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE "LIDOS ACAO JUDICIAL ==>" TO   WS-PARAM1-ZDISPLAY
           MOVE  W77-LIDOS-AP             TO   WS-PARAM2-ZDISPLAY
           CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE "LIDOS SEAS          ==>" TO   WS-PARAM1-ZDISPLAY
           MOVE  W77-LIDOS-SEAS           TO   WS-PARAM2-ZDISPLAY
           CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE "GRAVADOS         ==>"    TO   WS-PARAM1-ZDISPLAY
           MOVE  W77-GRAVADOS             TO   WS-PARAM2-ZDISPLAY
           CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE "IMPRESSOS        ==>"    TO   WS-PARAM1-ZDISPLAY
           MOVE  W77-IMPRESSOS            TO   WS-PARAM2-ZDISPLAY
           CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE  "FASE             ==>"   TO   WS-MENSAGEM-ZDISPLAY
           CALL "ZDISPLAY IN DTP/L1" USING     WS-MENSAGEM-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE  W77-FASE                 TO   WS-MENSAGEM-ZDISPLAY
           CALL "ZDISPLAY IN DTP/L1" USING     WS-MENSAGEM-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE "DESPREZADOS CONC.==>"    TO   WS-PARAM1-ZDISPLAY
           MOVE  W77-DESPREZ-CONC         TO   WS-PARAM2-ZDISPLAY
           CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE "W77-NU-SEQ-CONCES==>"    TO   WS-PARAM1-ZDISPLAY
           MOVE  W77-NU-SEQ-CONCESSAO     TO   WS-PARAM2-ZDISPLAY
           CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE "NU-SEQ-PROC      ==>"    TO   WS-PARAM1-ZDISPLAY
           MOVE  NU-SEQ-PROC OF DS-MOV-CRED-CONC TO WS-PARAM2-ZDISPLAY
           CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
           MOVE 0                         TO   WS-INTEIRO-ZDISPLAY.
           RESET ATTRIBUTE EXCEPTIONEVENT OF MYSELF.
       END DECLARATIVES.

       PRINCIPAL   SECTION.
      *----------------------
       0000-INICIO.
      *GATILHO PARA PRODUCAO
      *     MOVE 12345  TO PAR-ESQUEMA
      *     MOVE "MANU" TO PAR-MOV-CREDITO
      *     MOVE 309    TO W77-OFIS-CLASSE
      *     MOVE "    " TO PAR-SEAS.

           COPY "*B/DTP/PAD/01/CHECKPOINT/PD1 ON PKBDSGF".
           ATTACH  DE-INTERRUPT TO ATTRIBUTE EXCEPTIONEVENT OF MYSELF.

           MOVE SPACES TO WS-MSG
           MOVE "INICIO S/SUB/CPG/001/SISPG (SISPAGBEN)" TO  WS-MSG
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                    WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG
           MOVE " PARA LISTAR AS RUBRICAS SETAR WS-FLAG-LISTA-RUB NA"
           TO WS-MSG
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                          WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG
           MOVE " WORK PARA 1 PARA NAO LISTAR SETAR 0." TO WS-MSG
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                          WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG
                STRING "FLAG RUBRICA SETADA NO MOMENTO ="
                WS-FLAG-LISTA-RUB  DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           PERFORM 1000-INICIALIZACAO THRU  1000-FIM.

           PERFORM 2000-PROCESSAR     THRU  2000-FIM.

           PERFORM 3000-FINALIZACAO   THRU  3000-FIM.

           PERFORM 4000-ENVIA-OFIS    THRU  4000-FIM.

           MOVE SPACES TO WS-MSG.
           STRING "LIDOS DS-MOV-CRED-MANUT             ="
           WS-LIDOS-MANUT             DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           STRING "LIDOS DS-MOV-CRED-CONC              ="
           WS-LIDOS-CONC              DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           STRING "LIDOS DS-MOV-CRED-ALTER             ="
           WS-LIDOS-ALTE              DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           MOVE WS-VL-LIQUIDO-CRED   TO WS-INTEIRO-DECIMAL
                                        WS-VL-LIQUIDO-CRED-ZZ
                STRING "TOTAL VALOR LIQUIDO DE TODOS OS REGISTROS     ="
      *** WS-INTEIRO WS-VIRGULA WS-DECIMAL DELIMITED BY SIZE INTO WS-MSG
                WS-VL-LIQUIDO-CRED-ZZ      DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           MOVE WS-VL-BRUTO-CREDITO  TO WS-INTEIRO-DECIMAL
                                        WS-VL-BRUTO-CREDITO-ZZ
                STRING "TOTAL VALOR  BRUTO  DE TODOS OS REGISTROS     ="
      *** WS-INTEIRO WS-VIRGULA WS-DECIMAL DELIMITED BY SIZE INTO WS-MSG
                WS-VL-BRUTO-CREDITO-ZZ     DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           MOVE WS-GRAV-SISPG        TO WS-CONTADOR-X
                STRING "REG GRAVADOS SISPAGBEN D/SUB/CPG/058/SISPG    ="
                WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           MOVE WS-GRAV-SISPG-RUB267       TO WS-CONTADOR-X
                STRING "QTDE. RUBRICA 267 GRAVADOS NO SISPAGBEN       ="
                WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           MOVE WS-GRAV-SISPG-RUB273       TO WS-CONTADOR-X
                STRING "QTDE. RUBRICA 273 GRAVADOS NO SISPAGBEN       ="
                WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           MOVE WS-GRAV-SPGENT-RUB274       TO WS-CONTADOR-X
                STRING "QTDE. RUBRICA 274 GRAVADOS NO SISPAGBEN       ="
                WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG.
           MOVE WS-GRAV-SPGENT-RUB275       TO WS-CONTADOR-X
                STRING "QTDE. RUBRICA 275 GRAVADOS NO SISPAGBEN       ="
                WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.

          MOVE SPACES TO WS-MSG.
          MOVE WS-CONTA-NAO-CONSIG   TO WS-CONTADOR-X
               STRING "LIDOS BANCO SEM EMPRESTIMO (SS-CONSIG-EMPREST)-="
               WS-CONTADOR-X DELIMITED BY SIZE INTO WS-MSG
               CALL "ZDISPLAY IN DTP/L1"
                     USING WS-MSG
                            WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG
           MOVE "FINAL DO P/SUB/CPG/001       (SISPAGBEN)" TO  WS-MSG
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                          WS-STATUS-BACKUP.

           IF WS-FLAG-BRUTO-ZERO-ABORTA = 1
              MOVE SPACES TO WS-MSG
              MOVE "*** ABORTADO POR ENCONTRAR VALOR-BRUTO ZERADO ***"
              TO WS-MSG
              CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                             WS-STATUS-BACKUP.

           IF WS-OCORR-ERRO = 1 OR WS-INCONS-CONSIG = 1
              MOVE SPACES TO WS-MSG
              MOVE "*** ABORTADO POR OCORR-ERRO OU INCONS-CONSIG ***"
              TO WS-MSG
              CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                             WS-STATUS-BACKUP.
      *** Alteracao Temporaria - 05/05/2023 ***
           IF PAR-MOV-CREDITO = "ALTE"
              STOP RUN.
      ***
           IF WS-OCORR-ERRO = 1 OR WS-INCONS-CONSIG = 1 OR
              WS-FLAG-BRUTO-ZERO-ABORTA = 1
                SET MYSELF (STATUS) TO -1
              ELSE
                MOVE SPACES TO WS-MSG
                MOVE "*** PROGRAMA P/SUB/CPG/001     TERMINOU OK ***"
                TO WS-MSG
                CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                                WS-STATUS-BACKUP
                STOP RUN.
      *
      /
      *****************************************************************
      *                                                               *
      *      DESCRICAO  DA  ROTINA DE INICIALIZACAO  (OPCIONAL)       *
      *                                                               *
      *****************************************************************
       1000-INICIALIZACAO.
      *
            MOVE "FASE INICIALIZACAO " TO W77-FASE.

            IF PAR-ESQUEMA  = 0
               MOVE "P/SUB/CPG/001- PAR ESQUEMA INV" TO
                                                     WS-PARAM1-ZDISPLAY
               MOVE PAR-ESQUEMA                   TO WS-PARAM2-ZDISPLAY
               CALL "ZDISPLAY2 IN DTP/L1" USING      WS-PARAM1-ZDISPLAY,
                                                     WS-PARAM2-ZDISPLAY,
                                                     WS-INTEIRO-ZDISPLAY
               SET MYSELF (STATUS) TO -1.

             IF PAR-MOV-CREDITO NOT = "CONC" AND "MANU" AND "ALTE" AND
                                      "APAG" AND "CONV" AND "CALA"
               MOVE "P/SUB/CPG/001-PAR MOV CRED INV" TO
                                                     WS-PARAM1-ZDISPLAY
               MOVE PAR-MOV-CREDITO               TO WS-PARAM2-ZDISPLAY
               CALL "ZDISPLAY2 IN DTP/L1" USING      WS-PARAM1-ZDISPLAY,
                                                     WS-PARAM2-ZDISPLAY,
                                                     WS-INTEIRO-ZDISPLAY
               SET MYSELF (STATUS) TO -1.

            IF PAR-SEAS NOT = SPACES AND "SEAS"
               MOVE "P/SUB/CPG/001- PAR SEAS INV" TO
                                                     WS-PARAM1-ZDISPLAY
               MOVE PAR-SEAS                      TO WS-PARAM2-ZDISPLAY
               CALL "ZDISPLAY2 IN DTP/L1" USING      WS-PARAM1-ZDISPLAY,
                                                     WS-PARAM2-ZDISPLAY,
                                                     WS-INTEIRO-ZDISPLAY
               SET MYSELF (STATUS) TO -1.

            MOVE SPACES TO WS-MSG
              MOVE "-----------------------------------------" TO WS-MSG
            CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                           WS-STATUS-BACKUP
            MOVE SPACES TO WS-MSG

           IF PAR-SEAS = "SEAS"
              MOVE SPACES TO WS-MSG
              MOVE "PARAMETRO ( SEAS ) LENDO DS-MOV-CRED-SEAS" TO WS-MSG
              CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                             WS-STATUS-BACKUP
             ELSE
           IF PAR-MOV-CREDITO = "CONC"
              MOVE SPACES TO WS-MSG
              MOVE "PARAMETRO ( CONC ) LENDO DS-MOV-CRED-CONC" TO WS-MSG
              CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                             WS-STATUS-BACKUP
             ELSE
           IF PAR-MOV-CREDITO = "MANU"
              MOVE SPACES TO WS-MSG
             MOVE "PARAMETRO ( MANU ) LENDO DS-MOV-CRED-MANUT" TO WS-MSG
              CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                              WS-STATUS-BACKUP
             ELSE
           IF PAR-MOV-CREDITO = "ALTE"
              MOVE SPACES TO WS-MSG
             MOVE "PARAMETRO ( ALTE ) LENDO DS-MOV-CRED-ALTER" TO WS-MSG
              CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                              WS-STATUS-BACKUP
             ELSE
           IF PAR-MOV-CREDITO = "APAG"
              MOVE SPACES TO WS-MSG
             MOVE "PARAMETRO ( APAG ) LENDO DS-CALC-ACAO-JUD" TO WS-MSG
              CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                              WS-STATUS-BACKUP
             ELSE
           IF PAR-MOV-CREDITO = "CALA"
              MOVE SPACES TO WS-MSG
             MOVE "PARAMETRO ( CALA ) LENDO DS-MOV-CRED-CALAM" TO WS-MSG
              CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                              WS-STATUS-BACKUP.

           MOVE SPACES TO WS-MSG
             MOVE "------------------------------------------" TO WS-MSG
           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                          WS-STATUS-BACKUP.

           IF PAR-MOV-CREDITO = "CALA"
              MOVE "MANU" TO PAR-MOV-CREDITO
              MOVE "SIM"  TO WS-E-CRED-CALAMIDADE
           ELSE
              MOVE "NAO"  TO WS-E-CRED-CALAMIDADE.

            IF   PAR-MOV-CREDITO        =   "ALTE"
                 OPEN  INQUIRY BDCONPAGBEN
                 OPEN  INQUIRY BDSISBEN
                 FIND  BDCONPAGBEN
                 MOVE  D2-MAX-PER-NAT-01 TO W77-DT-MAX-PER-NAT-01.

           IF   (PAR-MOV-CREDITO = "CONC") AND (PAR-SEAS = SPACES)
                 OPEN  INQUIRY BDSISBEN
                 FIND  BDSISBEN
                 MOVE  NU-SEQ-CONCESSAO TO W77-NU-SEQ-CONCESSAO.

           IF   (PAR-MOV-CREDITO = "MANU" OR "CONV") AND
                (PAR-SEAS = SPACES)
                 OPEN INQUIRY BDSISBEN.

           IF    PAR-MOV-CREDITO        =    "APAG"
                 OPEN OUTPUT  DSUBCPG071
                 OPEN INQUIRY BDSISBEN
                           ON EXCEPTION
                              MOVE "P/SUB/CPG/001-ERRO OPEN BDSISBEN"
                                TO WS-MENSAGEM-ZDISPLAY
                              CALL "ZDISPLAY IN DTP/L1" USING
                                    WS-MENSAGEM-ZDISPLAY,
                                    WS-INTEIRO-ZDISPLAY
           ELSE
                 OPEN OUTPUT DSUBCPG079
                 OPEN UPDATE BDBENCRED
                          ON EXCEPTION
                             MOVE "P/SUB/CPG/001-ERRO OPEN BDBENCRED" TO
                                   WS-MENSAGEM-ZDISPLAY
                             CALL "ZDISPLAY IN DTP/L1" USING
                                   WS-MENSAGEM-ZDISPLAY,
                                   WS-INTEIRO-ZDISPLAY
                             CALL SYSTEM DMTERMINATE.

           IF PAR-SEAS = SPACES
              OPEN OUTPUT DSUBCPG075
                          DSUBCPG075AUX.

           IF PAR-MOV-CREDITO = "MANU" OR "CONV"
              OPEN OUTPUT DSUBCPG081.

           IF E-CREDITO-DE-CALAMIDADE
              PERFORM 9980-PEGA-COD-CALAM THRU 9980-PEGA-COD-CALAM-EXIT.

      *     IF PAR-MOV-CREDITO = "MAN2"
      *        CHANGE  ATTRIBUTE TITLE OF  DSUBCPG058SPGENT301 TO
      *                "D/SUB/CPG/058/SPG/301/ENT/MAN2 ON PKDADOS."
      *        CHANGE  ATTRIBUTE TITLE OF  DSUBCPG058SPGCONSIG TO
      *                "D/SUB/CPG/058/SPG/CONSIG/001/MAN2 ON PKDADOS."
      *        CHANGE  ATTRIBUTE TITLE OF  DSUBCPG058ANTIGO    TO
      *                "D/SUB/CPG/058/ANTIGO/MAN2"
      *        CHANGE  ATTRIBUTE TITLE OF  DSUBCPG058SAIDA     TO
      *                "D/SUB/CPG/058/SAIDA/MAN2."
      *        CHANGE  ATTRIBUTE TITLE OF  DSUBCPG058NSAIDA    TO
      *                "D/SUB/CPG/058/NOV/SAIDA/MAN2.".

           OPEN OUTPUT DSUBCPG001
           OPEN OUTPUT DSUBCPG003
           OPEN OUTPUT DSUBCPG078
           OPEN OUTPUT WOFIS.

      *    NOVO  OPEN OUTPUT DSUBCPG058SPGENT.
      *    OPEN OUTPUT DSUBCPG058SISPG.

           PERFORM 9010-ABRE-DSUBCPG058SPG
              THRU 9010-FIM.

           IF   WS-FLAG-LISTA-RUB = 1
                OPEN OUTPUT LSUBCPG058SISPG.

      **** PERFORM 1100-TRATA-RESTART THRU 1100-FIM.

           ACCEPT WS-DATA-SIS FROM DATE YYYYMMDD.
           MOVE WS-DD-SIS   TO WS-DT-AUX1-DIA
           MOVE WS-DD-SIS   TO WS-DT-AUX2-DIA
           MOVE WS-DD-SIS   TO WS-DT-AUX3-DD
           MOVE WS-MM-SIS   TO WS-DT-AUX1-MES
           MOVE WS-MM-SIS   TO WS-DT-AUX2-MES
           MOVE WS-MM-SIS   TO WS-DT-AUX3-MM
           MOVE WS-AA-SIS   TO WS-DT-AUX1-ANO
           MOVE WS-AA-SIS   TO WS-DT-AUX2-ANO
           MOVE WS-AA-SIS   TO WS-DT-AUX3-AA

           MOVE WS-DD-SIS   TO WS-DD-SIS-INV
           MOVE WS-MM-SIS   TO WS-MM-SIS-INV
           MOVE WS-AA-SIS   TO WS-AA-SIS-INV

           MOVE WS-DATA-SIS TO WS-DT-ANTES-6-MESES.

           IF WS-AA-SIS OF WS-DATA-SISR > WS-ANO-TAB-FERIADO
              MOVE "TAB FERIADO NAO ATUALIZ.P/ O ANO DE "
                TO  WS-PARAM1-ZDISPLAY
              MOVE  WS-AA-SIS OF WS-DATA-SISR TO WS-PARAM2-ZDISPLAY
              CALL "ZDISPLAY2 IN DTP/L1" USING   WS-PARAM1-ZDISPLAY,
                                                 WS-PARAM2-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
              MOVE "ANO DA TABELA: "   TO WS-PARAM1-ZDISPLAY
              MOVE  WS-ANO-TAB-FERIADO TO WS-PARAM2-ZDISPLAY
              CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
              SET MYSELF(STATUS) TO -1.

           MOVE WS-DT-AUX3-DDMMAA TO WS-DT-DDMMAA
           MOVE 1 TO W77-NUM-DIAS
           MOVE 0 TO W77-CONT-DIA-UTIL

           PERFORM 1100-SOMAR-MAIS-X-DIAS THRU
                   1100-FIM-SOMAR-MAIS-X-DIAS UNTIL
                   W77-CONT-DIA-UTIL = 1

           MOVE W77-DATA-RESPOSTA       TO WS-DATA-11
           MOVE CORR WS-DATA-11-R       TO WS-DATA-1-R
      *
           ACCEPT WS-HORA-SIS FROM TIME.

           COMPUTE WS-DATA-INICIO-LIB601 = (WS-DD-SIS * 1000000) +
                                           (WS-MM-SIS * 10000)   +
                                           WS-AA-SIS.

           MOVE WS-HH             TO WS-HORA-INICIO-LIB601.

           MOVE ZEROS    TO WS-TAB-TOTAL-ESTESP-1
                            WS-TAB-TOTAL-ESTESP-2
                            WS-TAB-TOTAL-ESTESP-3
                            WS-TAB-TOTAL-ESTESP-4
                            WS-TAB-TOTAL-ESTESP-5
                            WS-TAB-TOTAL-ESTESP-6
                            WS-TAB-TOTAL-ESTESP-7
                            WS-TAB-TOTAL-ESTESP-8
                            WS-TAB-TOTAL-DIVTIP
                            WS-TAB-TOTAL-DIVTIP2
                            WS-INCONS-CONSIG.

           IF PAR-MOV-CREDITO = "ALTE"
              FIND BDBENCRED.
      *
      *    Calculando a data 6 meses ANTES da data corrente, para
      *    posterior utilizacao na selecao dos pabs (em funcao da
      *    virada da global).
      *
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

      *NOVO MOVENDO DATA DO SISTEMA INVERTIDO PARA O HEADER E 1 ID-REG.
           MOVE ZEROS           TO DSUBCPG058-REG-SISPG-HEADER
                                   DSUBCPG058-REG-SPGENT-HEADER.

           MOVE ALL @00@        TO DSUBCPG058-REG-SISPG
                                   DSUBCPG058-REG-SPGENT.
           MOVE ZEROS           TO OUTROS-CAMPOS-SPGENT.
           MOVE ZEROS           TO OUTROS-CAMPOS-SISPG.

           MOVE SPACES          TO DSUBCPG058-REG-SISPG-T
                                    DSUBCPG058-REG-SPGENT-T.
           MOVE WS-DATA-SIS-INV   TO SISPG-DT-PROC-H
                                     SPGENT-DT-PROC-H.
           MOVE 1                 TO SISPG-ID-REG-H
                                     SPGENT-ID-REG-H.
           MOVE 9                 TO SISPG-ID-REG-T
                                     SPGENT-ID-REG-T.

           PERFORM 9550-LIMPA-RUB-H THRU
                   9550-LIMPA-RUB-H-EXIT
            VARYING IX FROM 1 BY 1
             UNTIL  IX > 96.

       1000-FIM.  EXIT.

       1100-SOMAR-MAIS-X-DIAS.

           MOVE WS-DT-DDMMAA TO W77-DATA-INIC

           CALL "ZQDATA4 IN DTP/L1" USING  W77-DATA-INIC W77-NUM-DIAS
                                    GIVING W77-DATA-RESPOSTA

           IF W77-DATA-RESPOSTA = 0
              MOVE "DATA INVALIDA PARA ZQDATA ->" TO WS-PARAM1-ZDISPLAY
              MOVE  W77-DATA-INIC                 TO WS-PARAM2-ZDISPLAY
              CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
              SET MYSELF(STATUS) TO -1.

           MOVE W77-DATA-RESPOSTA   TO WS-DT-DDMMAA

           CALL "ZDIADASEMANA4 IN DTP/L1" USING  WS-DT-DDMMAA
                                          GIVING W77-DIASEM-RESP

           IF ERRO-NA-DATA-INFORMADA
              MOVE "DATA INVALIDA NA PESQ. DO DIA DA SEMANA "
                TO  WS-PARAM1-ZDISPLAY
              MOVE  WS-DT-DDMMAA TO WS-PARAM2-ZDISPLAY
              CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
              SET MYSELF(STATUS) TO -1
           ELSE
              IF (NOT SABADO) AND (NOT DOMINGO)
                  MOVE 1 TO W77-IND-FERIADO
                  MOVE 0 TO W77-FLAG-DIA-UTIL
                  PERFORM 1100-VE-SE-EH-FERIADO THRU
                          1100-FIM-VE-SE-EH-FERIADO UNTIL FERIADO OR
                                                          DIA-UTIL
                  IF DIA-UTIL
                     ADD 1 TO W77-CONT-DIA-UTIL.

       1100-FIM-SOMAR-MAIS-X-DIAS. EXIT.

       1100-VE-SE-EH-FERIADO.

           IF TAB-FERIADO (WS-DT-MES OF WS-DT-DDMMAA-R, W77-IND-FERIADO)
                         = WS-DT-DIA OF WS-DT-DDMMAA-R
              MOVE 2 TO W77-FLAG-DIA-UTIL
           ELSE
              ADD 1 TO W77-IND-FERIADO
              IF W77-IND-FERIADO > 4
                 MOVE 1 TO W77-FLAG-DIA-UTIL.

       1100-FIM-VE-SE-EH-FERIADO. EXIT.

       2000-PROCESSAMENTO  SECTION.
      ******************************************************************
      *                                                                *
      *      DESCRICAO DA ROTINA DE PROCESSAMENTO (OPCIONAL)           *
      *                                                                *
      ******************************************************************
       2000-PROCESSAR.


           MOVE "FASE PROCESSAR " TO W77-FASE.

           MOVE ZEROS          TO            AUX-SEQ-REG.

           PERFORM 2500-DETERMINA-LEITURA        THRU 2500-FIM.

           MOVE AUX1-DT-COMPET-CREDITO           TO  AUX-COMP-CRED

           PERFORM 2600-TRATA-CREDITOS         THRU 2600-FIM
                                   UNTIL AUX-IN-FIM-ARQ = 1.

           PERFORM 2650-ATUAL-DATA-MOV-CREDITO THRU 2650-FIM.

           IF E-CREDITO-DE-CALAMIDADE
              PERFORM 2653-ATUAL-PROCESSAMENTO-CALAM THRU 2653-FIM.

       2000-FIM.  EXIT.
      /
       2500-DETERMINA-LEITURA.

           MOVE "FASE DETERMINA LEITURA " TO W77-FASE.

      *NOVO ARQUIVO PARA SUBSTITUIR SCF atual
      *     (SISPAGBEN) DSUBCPG058-REG-SISPG
           MOVE ALL @00@ TO DSUBCPG058-REG-SISPG
                            DSUBCPG058-REG-SPGENT.
           MOVE SPACES   TO OUTROS-CAMPOS-SISPG.

           MOVE SPACES   TO OUTROS-CAMPOS-SPGENT.

           IF PAR-SEAS = "SEAS"
              PERFORM 2550-LE-MOV-SEAS THRU 2550-FIM
           ELSE
              IF PAR-MOV-CREDITO = "CONC"
                 PERFORM 2510-LE-MOV-CONC THRU 2510-FIM
              ELSE
                 IF PAR-MOV-CREDITO = "MANU" OR "CONV"
                    PERFORM 2520-LE-MOV-MANUT THRU 2520-FIM
                 ELSE
                    IF PAR-MOV-CREDITO = "ALTE"
                       PERFORM 2530-LE-MOV-ALT THRU 2530-FIM
                    ELSE
                       IF PAR-MOV-CREDITO = "APAG"
                          PERFORM 2540-LE-CALC-ACAO-JUD THRU 2540-FIM
                       ELSE
                          MOVE "P/SUB/CPG/001-PAR-MOV-CRED INV"
                            TO WS-PARAM1-ZDISPLAY
                          MOVE PAR-MOV-CREDITO TO WS-PARAM2-ZDISPLAY
                          CALL "ZDISPLAY2 IN DTP/L1" USING
                                                     WS-PARAM1-ZDISPLAY,
                                                     WS-PARAM2-ZDISPLAY,
                                                    WS-INTEIRO-ZDISPLAY.

            IF AUX-IN-FIM-ARQ = 1
               GO TO 2500-FIM.

            ADD 1 TO AUX-NU-CONT-CHECKPOINT
                     WS-REG-LIDOS.
      ***************************************GATILHO PROC
            MOVE ZEROS          TO W77-IND-TAB.

      *       MOVE 1          TO W77-IND-TAB.
           PERFORM 2605-POSICIONA-TAB-CREDPROC THRU 2605-FIM.
      *****************************************FIM GATILO PROC
           IF PAR-SEAS = "SEAS"
               MOVE D2-MOV-CREDITO OF DS-MOV-CRED-SEAS
                                      TO TCP-DATA (W77-IND-TAB)
               ADD VL-LIQUIDO-CRED OF DS-MOV-CRED-SEAS
                                      TO TCP-LIDOS-VR (W77-IND-TAB)
               ADD VL-LIQUIDO-CRED OF DS-MOV-CRED-SEAS
                                      TO WS-VL-LIQUIDO-CRED
      *******  ADD VL-BRUTO-CREDITO OF DS-MOV-CRED-SEAS
      ******* NAO TEM BRUTO            TO WS-VL-BRUTO-CREDITO
            ELSE
               IF PAR-MOV-CREDITO = "CONC"
                  MOVE D2-MOV-CREDITO OF DS-MOV-CRED-CONC
                                         TO TCP-DATA (W77-IND-TAB)
                  ADD VL-LIQUIDO-CRED OF DS-MOV-CRED-CONC
                                         TO TCP-LIDOS-VR (W77-IND-TAB)
                  ADD VL-LIQUIDO-CRED OF DS-MOV-CRED-CONC
                                         TO WS-VL-LIQUIDO-CRED
                  ADD VL-BRUTO-CREDITO OF DS-MOV-CRED-CONC
                                          TO WS-VL-BRUTO-CREDITO
               ELSE
                  IF PAR-MOV-CREDITO = "MANU" OR "CONV"
                     MOVE D2-MOV-CREDITO OF DS-MOV-CRED-MANUT
                                         TO TCP-DATA (W77-IND-TAB)
                     ADD VL-LIQUIDO-CRED OF DS-MOV-CRED-MANUT
                                      TO TCP-LIDOS-VR (W77-IND-TAB)
                     ADD VL-LIQUIDO-CRED OF DS-MOV-CRED-MANUT
                                            TO WS-VL-LIQUIDO-CRED
                     ADD VL-BRUTO-CREDITO OF DS-MOV-CRED-MANUT
                                             TO WS-VL-BRUTO-CREDITO
                  ELSE
                     IF PAR-MOV-CREDITO = "ALTE"
                        MOVE D2-MOV-CREDITO OF DS-MOV-CRED-ALTER
                                            TO TCP-DATA (W77-IND-TAB)
                        ADD VL-LIQUIDO-CRED OF DS-MOV-CRED-ALTER
                                         TO TCP-LIDOS-VR (W77-IND-TAB)
                        ADD VL-LIQUIDO-CRED OF DS-MOV-CRED-ALTER
                                               TO WS-VL-LIQUIDO-CRED
                        ADD VL-BRUTO-CREDITO OF DS-MOV-CRED-ALTER
                                                TO WS-VL-BRUTO-CREDITO
                     ELSE
                        IF PAR-MOV-CREDITO = "APAG"
                           MOVE D2-PAGTO-ACAO-JUD OF DS-CALC-ACAO-JUD
                                               TO TCP-DATA (W77-IND-TAB)
                           COMPUTE AUX1-VL-LIQUIDO-CRED =
                                   VL-BENEF-ACAO-JUD - VL-IR-ACAO-JUD -
                                   VL-CTB-PREV-ACAO
                           ADD AUX1-VL-LIQUIDO-CRED TO
                               TCP-LIDOS-VR (W77-IND-TAB).

            ADD 1                TO TCP-LIDOS-QT (W77-IND-TAB).

       2500-FIM.  EXIT.
      /
       2510-LE-MOV-CONC.

            MOVE "FASE LE-MOV-CONC " TO W77-FASE.

            FIND NEXT DS-MOV-CRED-CONC
                      ON EXCEPTION
                         IF DMSTATUS(NOTFOUND)
                            MOVE 1 TO AUX-IN-FIM-ARQ
                            GO TO 2510-FIM
                         ELSE
                            MOVE "FIND NEXT DS-MOV-CRED-CONC.LIDOS:"
                              TO  WS-PARAM1-ZDISPLAY
                            MOVE W77-LIDOS-CONC
                              TO  WS-PARAM2-ZDISPLAY
                            CALL "ZDISPLAY2 IN DTP/L1" USING
                                  WS-PARAM1-ZDISPLAY,
                                  WS-PARAM2-ZDISPLAY,
                                  WS-INTEIRO-ZDISPLAY
                            CALL  SYSTEM DMTERMINATE.
      *
          IF W77-NU-SEQ-CONCESSAO NOT = NU-SEQ-PROC OF DS-MOV-CRED-CONC
             ADD 1 TO W77-DESPREZ-CONC
             MOVE "W77-NU-SEQ-CONCESSAO =>"    TO   WS-PARAM1-ZDISPLAY
             MOVE  W77-NU-SEQ-CONCESSAO        TO   WS-PARAM2-ZDISPLAY
             CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                                 WS-PARAM2-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
             MOVE "NU-SEQ-PROC         ==>"    TO   WS-PARAM1-ZDISPLAY
             MOVE  NU-SEQ-PROC OF DS-MOV-CRED-CONC TO WS-PARAM2-ZDISPLAY
             CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                                 WS-PARAM2-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
             MOVE "DESPREZADOS CONC.   ==>"    TO   WS-PARAM1-ZDISPLAY
             MOVE  W77-DESPREZ-CONC            TO   WS-PARAM2-ZDISPLAY
             CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
                                                 WS-PARAM2-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
             GO TO 2510-LE-MOV-CONC.

           ADD 1 TO W77-LIDOS-CONC WS-LIDOS-CONC.

      *     PERFORM 7000-LIMPA-RUB THRU
      *             7000-LIMPA-RUB-EXIT
      *       VARYING IX FROM 1 BY 1
      *         UNTIL IX > 72.

           MOVE ALL "00" TO OUTROS-CAMPOS-SPGENT
                                  OUTROS-CAMPOS-SISPG
                                  WS-TAB-RUBR-ENT.

            MOVE VL-LIQUIDO-CRED OF DS-MOV-CRED-CONC  TO
                 SISPG-VALOR-LIQUIDO
                 SPGENT-VALOR-LIQUIDO
                  LIN-LIQUIDO.

            MOVE VL-BRUTO-CREDITO OF DS-MOV-CRED-CONC  TO
                 SISPG-VALOR-BRUTO
                 SPGENT-VALOR-BRUTO
                  LIN-BRUTO.

            MOVE CS-ORIGEM-CREDITO OF DS-MOV-CRED-CONC
              TO WS-SISPG-ORIG-CRED-H.

          MOVE ZEROS TO WS-SISPG-VALOR-RUB301.
          IF QT-RUBRICA OF DS-MOV-CRED-CONC > 0
             MOVE ZEROS TO WS-TAB-RUBRICAS-AUX
                           WS-VL-RUB-301-SEM-ALT
      *NOVO  MOVE ZEROS TO SISPG-VALOR-GRUPO-RUBRICAS
             MOVE ZEROS TO SISPG-VALOR-GRUPO-RUBRICAS IX
                           SPGENT-VALOR-GRUPO-RUBRICAS.
             MOVE QT-RUBRICA OF DS-MOV-CRED-CONC TO SISPG-OCOR-RUB
                                                    SPGENT-OCOR-RUB

             MOVE ZEROS TO WS-VL-RUB-301-SEM-ALT WS-SISPG-VALOR-RUB301
                LIN-RUB301  WS-BRUTO-RUB-105  SISPG-MARCA-RUB231

             PERFORM 2790-SALVA-RUB THRU 2790-FIM
              VARYING W77-IND-RUBRICAS FROM 1 BY 1
              UNTIL  W77-IND-RUBRICAS > QT-RUBRICA OF DS-MOV-CRED-CONC.

           MOVE SPACES TO LIN-SISPG.
           MOVE " NB#" TO SISPG-BRANCO1
                          SPGENT-BRANCO1.
           MOVE NU-NB OF DS-MOV-CRED-CONC TO NB-I WS-NU-NB-REG SISPG-NB
                                                              SPGENT-NB.
           MOVE WS-SISPG-VALOR-RUB301 TO LIN-RUB301.

           IF   WS-FLAG-LISTA-RUB = 0
                GO TO PULA-LISTA-RUBR-CONC.

           ADD 1 TO WS-QUANT-LINHA.

      *     IF  WS-QUANT-LINHA > 100
            IF  WS-QUANT-LINHA > ZEROS
               GO TO PULA-LISTA-RUBR-CONC.

           IF WS-QUANT-LINHA = 1
              MOVE "LISTA TODAS AS RUBRICAS DO DS-MOV-CRED-CONC" TO
              LSUBSISPG-REG
              WRITE LSUBSISPG-REG MOVE SPACES TO LSUBSISPG-REG.

           MOVE QT-RUBRICA OF DS-MOV-CRED-CONC  TO N-RUB-I

             PERFORM ZZZZ-LISTA-RUBR-CONC THRU
                     ZZZZ-LISTA-RUBR-CONC-EXIT
               VARYING IX FROM 1 BY 1
               UNTIL   IX > 42 OR
                       IX > QT-RUBRICA OF DS-MOV-CRED-CONC.
          WRITE LSUBSISPG-REG FROM LIN-SISPG AFTER 1.
          MOVE ZEROS TO IND-TB.
          WRITE LSUBSISPG-REG FROM LIN-SISPG-LIQ AFTER 1.

       PULA-LISTA-RUBR-CONC.
          IF QT-RUBRICA OF DS-MOV-CRED-CONC > 0
             MOVE LOW-VALUES TO WS-TABELA-RUBRICAS
             MOVE 0 TO W77-IND-RUBRICAS  W77-IND-TAB-RUB
             PERFORM 2710-TIRA-RUBRICA-900-MOV-CONC THRU 2710-FIM
              UNTIL W77-IND-RUBRICAS = QT-RUBRICA OF DS-MOV-CRED-CONC
             MOVE 0 TO W77-IND-RUBRICAS
             PERFORM 2770-LIMPA-RUBRICAS-CONC THRU 2770-EXIT
              UNTIL W77-IND-RUBRICAS = QT-RUBRICA OF DS-MOV-CRED-CONC
             MOVE W77-IND-TAB-RUB TO QT-RUBRICA OF DS-MOV-CRED-CONC
             MOVE 0 TO W77-IND-RUBRICAS
             PERFORM 2720-ALTERA-UWA-MOV-CONC THRU 2720-EXIT
                     UNTIL W77-IND-RUBRICAS = W77-IND-TAB-RUB.

          IF IN-CRED-DELETADO OF DS-MOV-CRED-CONC
             ADD 1 TO AUX-QT-DELETADOS
             GO TO 2510-LE-MOV-CONC.

          IF CS-ERRO OF DS-MOV-CRED-CONC > 0
             ADD 1 TO AUX-QT-MARCA-ERRO
             GO TO 2510-LE-MOV-CONC.

          IF CS-AUDITORIA-CRED OF DS-MOV-CRED-CONC = ZERO
             MOVE 2           TO  AUX1-IN-CR-BLOQUEADO
          ELSE
             MOVE 1           TO  AUX1-IN-CR-BLOQUEADO.

          MOVE D2-DDB OF DS-MOV-CRED-CONC TO  AUX1-DT-DDB.
          MOVE VL-LIQUIDO-CRED OF DS-MOV-CRED-CONC
                                 TO  AUX1-VL-LIQUIDO-CRED.

          IF IN-CREDITO-EPU OF DS-MOV-CRED-CONC
             MOVE 1 TO AUX1-IN-CREDITO-EPU
          ELSE
             MOVE ZERO TO AUX1-IN-CREDITO-EPU.

          IF IN-CRED-CART-SOC OF DS-MOV-CRED-CONC
             MOVE 1 TO AUX1-IN-CRED-CART-SOC
          ELSE
             MOVE ZERO TO AUX1-IN-CRED-CART-SOC.

          IF (IN-VAGO-03 OF DS-MOV-CRED-CONC) AND
             (ID-BANCO OF DS-MOV-CRED-CONC = 104 OR 237)
             MOVE 4 TO AUX1-IN-CRED-PIONEIRA
          ELSE

             IF IN-CRED-PIONEIRA OF DS-MOV-CRED-CONC
                MOVE 1 TO AUX1-IN-CRED-PIONEIRA
             ELSE
                MOVE ZERO TO AUX1-IN-CRED-PIONEIRA.

          MOVE ID-PREGAO OF DS-MOV-CRED-CONC TO AUX1-ID-PREGAO.

          MOVE ID-LOTE-PREGAO OF DS-MOV-CRED-CONC
                                             TO AUX1-ID-LOTE-PREGAO.

          MOVE D2-MOV-CREDITO  OF DS-MOV-CRED-CONC
                                 TO  AUX1-DT-MOV-CREDITO.
          MOVE NU-NB           OF DS-MOV-CRED-CONC
                                 TO  AUX1-NU-NB.
          MOVE D2-INI-PERIODO  OF DS-MOV-CRED-CONC
                                 TO  AUX1-DT-INI-PERIODO.
          MOVE D2-FIM-PERIODO  OF DS-MOV-CRED-CONC
                                 TO  AUX1-DT-FIM-PERIODO.
          MOVE D2-INI-VALIDADE OF DS-MOV-CRED-CONC
                                 TO  AUX1-DT-INI-VALIDADE.
          MOVE D2-FIM-VALIDADE OF DS-MOV-CRED-CONC
                                 TO  AUX1-DT-FIM-VALIDADE.
          MOVE ID-BANCO        OF DS-MOV-CRED-CONC
                                 TO  AUX1-ID-BANCO.
          MOVE NU-CONTA-CORRENTE OF DS-MOV-CRED-CONC
                                 TO  AUX1-NU-CONTA-CORRENTE.
          MOVE CS-ESPECIE      OF DS-MOV-CRED-CONC
                               TO AUX1-CS-ESPECIE.
          MOVE CS-TRATAMENTO   OF DS-MOV-CRED-CONC
                               TO AUX1-CS-TRATAMENTO.
          MOVE D2-COMPET-CREDITO OF DS-MOV-CRED-CONC
                                 TO  AUX1-DT-COMPET-CREDITO.
          MOVE CS-ORIGEM-CREDITO OF DS-MOV-CRED-CONC
                                 TO  AUX1-CS-ORIGEM-CREDITO.
          MOVE ID-OL-MANUTENCAO OF DS-MOV-CRED-CONC
                                 TO WS-OL-MANUT.

      *O ramo de atividade determina se o beneficio eh urbano ou rural
      *serah usado no novo arquivo do SCF.
          MOVE CS-RAMO-ATIVIDADE OF DS-MOV-CRED-CONC
                                 TO AUX1-CS-RAMO-ATIVIDADE.

          MOVE WS-UF          TO AUX1-CS-UF.

          MOVE QT-RUBRICA        OF DS-MOV-CRED-CONC
                                 TO  AUX1-QT-RUBRICA.

          MOVE ZEROS             TO  AUX1-CS-RUBRICA1
                                     AUX1-CS-RUBRICA2
                                     AUX1-VL-RUBRICA1
                                     AUX1-VL-RUBRICA2.

          IF QT-RUBRICA OF DS-MOV-CRED-CONC > 0
             IF (CS-RUBRICA OF DS-MOV-CRED-CONC (1) > 0) AND
                (VL-RUBRICA OF DS-MOV-CRED-CONC (1) IS NULL)
                 MOVE "BENEFICIO COM RUBRICA ZERADA - NB =" TO
                       WS-PARAM1-ZDISPLAY
                 MOVE NU-NB OF DS-MOV-CRED-CONC TO WS-PARAM2-ZDISPLAY
                 CALL "ZDISPLAY2 IN DTP/L1" USING
                                            WS-PARAM1-ZDISPLAY,
                                            WS-PARAM2-ZDISPLAY,
                                            WS-INTEIRO-ZDISPLAY
                 SET MYSELF (STATUS) TO -1
             ELSE
                 MOVE CS-RUBRICA OF DS-MOV-CRED-CONC (1)
                                 TO AUX1-CS-RUBRICA1
                 MOVE VL-RUBRICA OF DS-MOV-CRED-CONC(1)
                                 TO  AUX1-VL-RUBRICA1.

          IF QT-RUBRICA OF DS-MOV-CRED-CONC > 1
             IF (CS-RUBRICA OF DS-MOV-CRED-CONC (2) > 0) AND
                (VL-RUBRICA OF DS-MOV-CRED-CONC (2) IS NULL)
                 MOVE "BENEFICIO COM RUBRICA ZERADA - NB =" TO
                       WS-PARAM1-ZDISPLAY
                 MOVE NU-NB OF DS-MOV-CRED-CONC TO WS-PARAM2-ZDISPLAY
                 CALL "ZDISPLAY2 IN DTP/L1" USING
                                            WS-PARAM1-ZDISPLAY,
                                            WS-PARAM2-ZDISPLAY,
                                            WS-INTEIRO-ZDISPLAY
                 SET MYSELF (STATUS) TO -1
             ELSE
                 MOVE CS-RUBRICA OF DS-MOV-CRED-CONC (2)
                                 TO  AUX1-CS-RUBRICA2
                 MOVE VL-RUBRICA OF DS-MOV-CRED-CONC (2)
                                 TO  AUX1-VL-RUBRICA2.

          MOVE CS-NATUR-CREDITO OF DS-MOV-CRED-CONC
                                TO  AUX1-CS-NATUR-CREDITO.
          MOVE CS-MEIO-PAGTO OF DS-MOV-CRED-CONC TO AUX1-MEIO-PAGTO
          MOVE D2-MOV-CREDITO   OF DS-MOV-CRED-CONC
                                TO  AUX1-DT-MOV-CREDITO.
          MOVE ID-ORGAO-PAGADOR OF DS-MOV-CRED-CONC
                                TO  AUX1-ID-ORGAO-PAGADOR.
          MOVE CS-RAMO-ATIVIDADE OF DS-MOV-CRED-CONC
                                TO  AUX1-CS-RAMO-ATIVIDADE.
          MOVE ID-OL-MANUTENCAO OF DS-MOV-CRED-CONC
                                TO  AUX1-ID-OL-MANUTENCAO.
          MOVE NU-SEQ-PROC      OF DS-MOV-CRED-CONC
                                TO  AUX1-SEQ-PROC.
          MOVE IN-CRED-ATRASADO OF DS-MOV-CRED-CONC
                                TO AUX1-IN-CRED-ATRASADO
          MOVE ID-NIT           OF DS-MOV-CRED-CONC
                                TO  AUX1-ID-NIT
          MOVE NU-NIT-DV1       OF DS-MOV-CRED-CONC
                                TO  AUX1-NU-NIT-DV1
          MOVE 1                TO  AUX1-CS-ROTINA-PAGTO

          IF IN-CRED-ISENTO-IR OF DS-MOV-CRED-CONC
             MOVE 1 TO AUX1-IN-CRED-ISENTO-IR
          ELSE
             MOVE 0 TO AUX1-IN-CRED-ISENTO-IR.

      *   IF IN-VAGO-03 OF DS-MOV-CRED-CONC
      *      MOVE 1 TO AUX1-IN-ADIANT-13-SAL
      *   ELSE
      *      MOVE 0 TO AUX1-IN-ADIANT-13-SAL.

      *    IF IN-SEGUNDA-PARC13 OF DS-MOV-CRED-CONC
      *       MOVE 1 TO AUX1-IN-ADIANT-13-SAL
      *    ELSE
      *       MOVE 0 TO AUX1-IN-ADIANT-13-SAL.

          IF IN-ADIANT-13-SAL OF DS-MOV-CRED-CONC
             MOVE 1 TO AUX1-IN-ADIANT-13-SAL
          ELSE
             MOVE 0 TO AUX1-IN-ADIANT-13-SAL.


          IF IN-VAGO-03 OF DS-MOV-CRED-CONC
             MOVE 1 TO AUX1-IN-BANCO-POSTAL
          ELSE
             MOVE 0 TO AUX1-IN-BANCO-POSTAL.

          IF IN-CREDITO-RRA OF DS-MOV-CRED-CONC
             MOVE 1 TO AUX1-IN-CREDITO-RRA
             MOVE 1 TO AUX1-IN-RRA-ATRAS-CONC
          ELSE
             MOVE 0 TO AUX1-IN-CREDITO-RRA
             MOVE 0 TO AUX1-IN-RRA-ATRAS-CONC.

          MOVE CS-ORIGEM-ORCAMENTO OF DS-MOV-CRED-CONC
                                   TO AUX1-CS-ORIGEM-ORCAMENTO.
      **
          MOVE NU-CPF-TITULAR OF DS-MOV-CRED-CONC
                              TO AUX1-NU-CPF-TITULAR.
          MOVE NU-NIT-TITULAR OF DS-MOV-CRED-CONC
                              TO AUX1-NU-NIT-TITULAR.
      **
          MOVE D2-COMP-ATU-VL-CR OF DS-MOV-CRED-CONC
                                 TO AUX1-DT-COMP-ATU-VL-CR.

           IF CS-ORIGEM-CREDITO OF DS-MOV-CRED-CONC = 4
              FIND S2-CRED-ALTER-OL  AT
                   ID-OL-SOLIC-CRED  OF DS-MOV-CRED-ALTER =
                   ID-OL-SOLIC-CRED  OF DS-MOV-CRED-CONC   AND
                   D2-SOLIC-CREDITO  OF DS-MOV-CRED-ALTER =
                   D2-SOLIC-CREDITO  OF DS-MOV-CRED-CONC   AND
                   NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER =
                   NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-CONC
                   ON EXCEPTION
                      MOVE
                "P/SUB/CPG/001 - ERRO FIND DS-MOV-CRED-ALTER - ROT 2510"
                      TO WS-MENSAGEM-ZDISPLAY
                      CALL "ZDISPLAY IN DTP/L1" USING
                                                WS-MENSAGEM-ZDISPLAY,
                                                WS-INTEIRO-ZDISPLAY
                      CALL SYSTEM DMTERMINATE.

       2510-FIM. EXIT.
      /
       2520-LE-MOV-MANUT.

            MOVE "FASE LE-MOV-MANUT " TO W77-FASE.

            IF E-CREDITO-DE-CALAMIDADE
               PERFORM 2525-FIND-CALA THRU 2525-FIM
            ELSE
               PERFORM 2523-FIND-MANU THRU 2523-FIM.

            IF AUX-IN-FIM-ARQ = 1
               GO TO 2520-FIM.

      *      FIND NEXT DS-MOV-CRED-MANUT
      *                ON EXCEPTION
      *                   IF DMSTATUS(NOTFOUND)
      *                      MOVE 1 TO AUX-IN-FIM-ARQ
      *                      GO TO 2520-FIM
      *                   ELSE
      *                      MOVE "FIND NEXT DS-MOV-CRED-MANUT.LIDOS:"
      *                        TO  WS-PARAM1-ZDISPLAY
      *                      MOVE W77-LIDOS-MANUT
      *                        TO  WS-PARAM2-ZDISPLAY
      *                      CALL "ZDISPLAY2 IN DTP/L1" USING
      *                            WS-PARAM1-ZDISPLAY,
      *                            WS-PARAM2-ZDISPLAY,
      *                            WS-INTEIRO-ZDISPLAY
      *                      CALL  SYSTEM DMTERMINATE.
      *

           IF (PAR-MOV-CREDITO = "MANU") AND
              (ID-BANCO OF DS-MOV-CRED-MANUT = 998 OR 996)
              GO TO 2520-LE-MOV-MANUT.

           IF (PAR-MOV-CREDITO = "CONV") AND
              (ID-BANCO OF DS-MOV-CRED-MANUT NOT = 998 AND 996)
              GO TO 2520-LE-MOV-MANUT.

           IF D2-INI-VALIDADE OF DS-MOV-CRED-MANUT < 20100131
      *        D2-INI-VALIDADE OF DS-MOV-CRED-MANUT > 20100206
              GO TO 2520-LE-MOV-MANUT.
      ***ALTERACAO TEMPORARIA PARA PULAR NB COM PROBLEMA NA RUBR***
           IF (NU-NB OF DS-MOV-CRED-MANUT = 1610163238) AND
              (D2-FIM-PERIODO OF DS-MOV-CRED-MANUT= 20121130)
              GO TO 2520-LE-MOV-MANUT.
      ***ALTERACAO TEMPORARIA PARA PULAR NB COM PROBLEMA NA RUBR***
           ADD 1 TO W77-LIDOS-MANUT WS-LIDOS-MANUT.

      *Jose Fonseca - DM.096405 - INSS EM NUMEROS.
           IF PAR-MOV-CREDITO = "MANU"
              AND WS-LIDOS-MANUT = 1
              MOVE D2-COMPET-CREDITO OF DS-MOV-CRED-MANUT
                                                      TO WS-COMPET-CRED
              CHANGE ATTRIBUTE TITLE OF DSUBDNT001
                                                TO WS-TITLE-DSUBDNT001
              OPEN     OUTPUT    DSUBDNT001
           ELSE
              IF PAR-MOV-CREDITO = "CONV"
                 AND WS-LIDOS-MANUT = 1
                  MOVE D2-COMPET-CREDITO OF DS-MOV-CRED-MANUT
                                                      TO WS-COMPET-CRED
      *            MOVE 201410 TO WS-COMPET-CRED
                  CHANGE ATTRIBUTE TITLE OF DSUBDNT001
                                                TO WS-TITLE-DSUBDNT001
                  IF (ATTRIBUTE RESIDENT OF DSUBDNT001 = VALUE TRUE)
                     OPEN INPUT DSUBDNT001
                     PERFORM 3319-LER-DNT THRU 3319-FIM-LER-DNT
                          VARYING WS-IND-DNT FROM 1 BY 1
                          UNTIL WS-IND-DNT > 13
                      MOVE QTD-CREDITOS  TO WS-QT-AUX-LIDO
                      MOVE VL-CREDITOS   TO WS-VL-AUX-LIDO
                      CLOSE DSUBDNT001 RELEASE
                      OPEN     OUTPUT    DSUBDNT001
                  ELSE
                      STRING
                      "DSUBDNT001 FOI REMOVIDO: "WS-TITLE-DSUBDNT001
                      DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                      CALL "ZDISPLAY IN DTP/L1" USING
                                                WS-MENSAGEM-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY.

      ***ALTERACAO TEMPORARIA*************************
           MOVE ZEROS TO SISPG-MARCA-GRUPO-RUBRICAS.
      ***ALTERACAO TEMPORARIA*************************
      *     IF W77-LIDOS-MANUT > 199000
      *        MOVE "NU-NB      ==>"            TO WS-PARAM1-ZDISPLAY
      *        MOVE  NU-NB OF DS-MOV-CRED-MANUT TO WS-PARAM2-ZDISPLAY
      *        CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY,
      *                                            WS-PARAM2-ZDISPLAY,
      *                                            WS-INTEIRO-ZDISPLAY
      *        MOVE 0                           TO WS-INTEIRO-ZDISPLAY.
      ***ALTERACAO TEMPORARIA*************************

           MOVE CS-ORIGEM-CREDITO OF DS-MOV-CRED-MANUT
             TO WS-SISPG-ORIG-CRED-H.

           MOVE VL-LIQUIDO-CRED OF DS-MOV-CRED-MANUT TO
                SISPG-VALOR-LIQUIDO
                SPGENT-VALOR-LIQUIDO
                LIN-LIQUIDO.

           MOVE VL-BRUTO-CREDITO OF DS-MOV-CRED-MANUT TO
                SISPG-VALOR-BRUTO
                SPGENT-VALOR-BRUTO
                LIN-BRUTO.

           MOVE ALL "00" TO OUTROS-CAMPOS-SPGENT
                                  OUTROS-CAMPOS-SISPG
                                  WS-TAB-RUBR-ENT.

      *     PERFORM 7000-LIMPA-RUB THRU
      *             7000-LIMPA-RUB-EXIT
      *         VARYING IX FROM 1 BY 1
      *         UNTIL IX > 72.

      *     PERFORM 7000-LIMPA-ENT THRU
      *             7000-LIMPA-ENT-EXIT
      *       VARYING IX FROM 1 BY 1
      *         UNTIL IX > 24.

           MOVE ZEROS TO WS-SISPG-VALOR-RUB301.
           IF QT-RUBRICA OF DS-MOV-CRED-MANUT > 0
              MOVE ZEROS TO WS-TAB-RUBRICAS-AUX  IX
                            WS-VL-RUB-301-SEM-ALT
      *NOVO  MOVE ZEROS TO SISPG-VALOR-GRUPO-RUBRICAS
             MOVE ZEROS TO SISPG-VALOR-GRUPO-RUBRICAS
                           SPGENT-VALOR-GRUPO-RUBRICAS.

             MOVE QT-RUBRICA OF DS-MOV-CRED-MANUT TO SISPG-OCOR-RUB
                                                     SPGENT-OCOR-RUB.

             MOVE ZEROS TO WS-VL-RUB-301-SEM-ALT WS-SISPG-VALOR-RUB301
                LIN-RUB301  WS-BRUTO-RUB-105  SISPG-MARCA-RUB231

           MOVE NU-NB OF DS-MOV-CRED-MANUT TO WS-NU-NB-MSG.

           IF QT-RUBRICA OF DS-MOV-CRED-MANUT > WS-QT-RUBRICA
              MOVE QT-RUBRICA OF DS-MOV-CRED-MANUT TO WS-QT-RUBRICA
              MOVE NU-NB OF DS-MOV-CRED-MANUT TO WS-QT-NU-NB.

              PERFORM 2792-SALVA-RUB THRU 2792-FIM
               VARYING W77-IND-RUBRICAS FROM 1 BY 1
               UNTIL W77-IND-RUBRICAS > QT-RUBRICA OF DS-MOV-CRED-MANUT.

          MOVE SPACES TO LIN-SISPG.
          MOVE " NB#" TO SISPG-BRANCO1
                         SPGENT-BRANCO1.
          MOVE NU-NB OF DS-MOV-CRED-MANUT TO NB-I WS-NU-NB-REG SISPG-NB
           SPGENT-NB.
           MOVE WS-SISPG-VALOR-RUB301 TO LIN-RUB301.

          IF   WS-FLAG-LISTA-RUB = 0
               GO TO PULA-LISTA-RUBR-MANU.

           ADD 1 TO WS-QUANT-LINHA.
      *     IF  WS-QUANT-LINHA > 100
            IF  WS-QUANT-LINHA > ZEROS
               GO TO PULA-LISTA-RUBR-MANU.
           IF  WS-QUANT-LINHA = 1
               MOVE "LISTA TODAS AS RUBRICAS DO DS-MOV-CRED-MANUT" TO
               LSUBSISPG-REG
               WRITE LSUBSISPG-REG MOVE SPACES TO LSUBSISPG-REG.

           MOVE QT-RUBRICA OF DS-MOV-CRED-MANUT TO N-RUB-I

             PERFORM ZZZZ-LISTA-RUBR-MANU THRU
                     ZZZZ-LISTA-RUBR-MANU-EXIT
               VARYING IX FROM 1 BY 1
               UNTIL   IX > 42 OR
                       IX > QT-RUBRICA OF DS-MOV-CRED-MANUT.
          WRITE LSUBSISPG-REG FROM LIN-SISPG AFTER 1.
          MOVE ZEROS TO IND-TB.
          WRITE LSUBSISPG-REG FROM LIN-SISPG-LIQ AFTER 1.
       PULA-LISTA-RUBR-MANU.

           IF QT-RUBRICA OF DS-MOV-CRED-MANUT > 0
              MOVE LOW-VALUES TO WS-TABELA-RUBRICAS
              MOVE 0 TO W77-IND-RUBRICAS  W77-IND-TAB-RUB
              PERFORM 2730-TIRA-RUBRICA-900-MOV-MANU THRU 2730-FIM
               UNTIL W77-IND-RUBRICAS = QT-RUBRICA OF DS-MOV-CRED-MANUT
              MOVE 0 TO W77-IND-RUBRICAS
              PERFORM 2775-LIMPA-RUBRICAS-MANU THRU 2775-EXIT
               UNTIL W77-IND-RUBRICAS = QT-RUBRICA OF DS-MOV-CRED-MANUT
              MOVE W77-IND-TAB-RUB TO QT-RUBRICA OF DS-MOV-CRED-MANUT
              MOVE 0 TO W77-IND-RUBRICAS
              PERFORM 2740-ALTERA-UWA-MOV-MANU THRU 2740-EXIT
                     UNTIL W77-IND-RUBRICAS = W77-IND-TAB-RUB.
      *****GATILHO PERIODO
      *    IF  D2-FIM-PERIODO OF DS-MOV-CRED-MANUT < 20230131
      *       GO TO 2520-LE-MOV-MANUT.
      *****FIM GATILHO PERIODO.
           IF IN-CRED-DELETADO OF DS-MOV-CRED-MANUT
              ADD 1 TO AUX-QT-DELETADOS
              GO TO 2520-LE-MOV-MANUT.

           IF CS-ERRO OF DS-MOV-CRED-MANUT > 0
              ADD 1 TO AUX-QT-MARCA-ERRO
              GO TO 2520-LE-MOV-MANUT.

           IF CS-AUDITORIA-CRED OF DS-MOV-CRED-MANUT = ZERO
           OR E-CREDITO-DE-CALAMIDADE
           OR CS-ORIG-BLOQUEIO OF DS-MOV-CRED-MANUT = 9
              MOVE 2           TO  AUX1-IN-CR-BLOQUEADO
           ELSE
              MOVE 1           TO  AUX1-IN-CR-BLOQUEADO.

           MOVE VL-LIQUIDO-CRED OF DS-MOV-CRED-MANUT
                                TO AUX1-VL-LIQUIDO-CRED.

           IF IN-CREDITO-EPU OF DS-MOV-CRED-MANUT
              MOVE 1 TO AUX1-IN-CREDITO-EPU
           ELSE
              MOVE ZERO TO AUX1-IN-CREDITO-EPU.

           MOVE ID-OL-MANUTENCAO OF DS-MOV-CRED-MANUT
                                 TO WS-OL-MANUT.
           MOVE WS-UF          TO  AUX1-CS-UF.
           MOVE D2-MOV-CREDITO OF DS-MOV-CRED-MANUT
                               TO AUX1-DT-MOV-CREDITO.
           MOVE NU-NB          OF DS-MOV-CRED-MANUT
                               TO AUX1-NU-NB.
           MOVE D2-INI-PERIODO OF DS-MOV-CRED-MANUT
                               TO  AUX1-DT-INI-PERIODO.
           MOVE D2-FIM-PERIODO OF DS-MOV-CRED-MANUT
                               TO AUX1-DT-FIM-PERIODO.
           MOVE D2-INI-VALIDADE OF DS-MOV-CRED-MANUT
                                TO AUX1-DT-INI-VALIDADE.
           MOVE D2-FIM-VALIDADE OF DS-MOV-CRED-MANUT
                                TO AUX1-DT-FIM-VALIDADE.
           MOVE ID-BANCO OF DS-MOV-CRED-MANUT TO AUX1-ID-BANCO.
           MOVE NU-CONTA-CORRENTE OF DS-MOV-CRED-MANUT
                                  TO  AUX1-NU-CONTA-CORRENTE.
           MOVE CS-ESPECIE      OF DS-MOV-CRED-MANUT
                                TO AUX1-CS-ESPECIE.
           MOVE CS-TRATAMENTO   OF DS-MOV-CRED-MANUT
                                TO AUX1-CS-TRATAMENTO.
           MOVE D2-COMPET-CREDITO OF DS-MOV-CRED-MANUT
                                  TO AUX1-DT-COMPET-CREDITO.
           MOVE CS-ORIGEM-CREDITO OF DS-MOV-CRED-MANUT
                                  TO AUX1-CS-ORIGEM-CREDITO.

           IF IN-CRED-CART-SOC OF DS-MOV-CRED-MANUT
              MOVE 1 TO AUX1-IN-CRED-CART-SOC
           ELSE
              MOVE ZERO TO AUX1-IN-CRED-CART-SOC.

           IF (IN-VAGO-03 OF DS-MOV-CRED-MANUT) AND
              (ID-BANCO OF DS-MOV-CRED-MANUT = 104 OR 237)
              MOVE 4 TO AUX1-IN-CRED-PIONEIRA
           ELSE
              IF IN-CRED-PIONEIRA OF DS-MOV-CRED-MANUT
                 MOVE 1 TO AUX1-IN-CRED-PIONEIRA
              ELSE
                 MOVE ZERO TO AUX1-IN-CRED-PIONEIRA.


           MOVE QT-RUBRICA        OF DS-MOV-CRED-MANUT
                                  TO AUX1-QT-RUBRICA.

           MOVE ZEROS             TO AUX1-CS-RUBRICA1
                                     AUX1-CS-RUBRICA2
                                     AUX1-VL-RUBRICA1
                                     AUX1-VL-RUBRICA2.

           IF QT-RUBRICA OF DS-MOV-CRED-MANUT > 0
              IF (CS-RUBRICA OF DS-MOV-CRED-MANUT (1) > 0) AND
                 (VL-RUBRICA OF DS-MOV-CRED-MANUT (1) IS NULL)
                  MOVE "BENEFICIO COM RUBRICA ZERADA - NB=" TO
                        WS-PARAM1-ZDISPLAY
                  MOVE  NU-NB OF DS-MOV-CRED-MANUT TO WS-PARAM2-ZDISPLAY
                  CALL "ZDISPLAY2 IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                  SET MYSELF (STATUS) TO -1
              ELSE
                  MOVE CS-RUBRICA OF DS-MOV-CRED-MANUT (1)
                                  TO AUX1-CS-RUBRICA1
                  MOVE VL-RUBRICA OF DS-MOV-CRED-MANUT (1)
                                  TO AUX1-VL-RUBRICA1.

           IF QT-RUBRICA OF DS-MOV-CRED-MANUT > 1
              IF (CS-RUBRICA OF DS-MOV-CRED-MANUT (2) > 0) AND
                 (VL-RUBRICA OF DS-MOV-CRED-MANUT (2) IS NULL)
                  MOVE "BENEFICIO COM RUBRICA ZERADA - NB=" TO
                        WS-PARAM1-ZDISPLAY
                  MOVE NU-NB OF DS-MOV-CRED-MANUT TO WS-PARAM2-ZDISPLAY
                  CALL "ZDISPLAY2 IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                  SET MYSELF (STATUS) TO -1
              ELSE
                  MOVE CS-RUBRICA OF DS-MOV-CRED-MANUT (2)
                                  TO AUX1-CS-RUBRICA2
                  MOVE VL-RUBRICA OF DS-MOV-CRED-MANUT (2)
                                  TO AUX1-VL-RUBRICA2.

           MOVE CS-NATUR-CREDITO OF DS-MOV-CRED-MANUT
                                 TO AUX1-CS-NATUR-CREDITO.
           MOVE CS-MEIO-PAGTO OF DS-MOV-CRED-MANUT TO AUX1-MEIO-PAGTO
           MOVE D2-MOV-CREDITO   OF DS-MOV-CRED-MANUT
                                 TO AUX1-DT-MOV-CREDITO.
           MOVE ID-ORGAO-PAGADOR OF DS-MOV-CRED-MANUT
                                 TO AUX1-ID-ORGAO-PAGADOR.
           MOVE CS-RAMO-ATIVIDADE OF DS-MOV-CRED-MANUT
                                  TO AUX1-CS-RAMO-ATIVIDADE.
           MOVE ID-OL-MANUTENCAO OF DS-MOV-CRED-MANUT
                                 TO AUX1-ID-OL-MANUTENCAO.
           MOVE NU-SEQ-PROC      OF DS-MOV-CRED-MANUT
                                 TO AUX1-SEQ-PROC.
           MOVE IN-CRED-ATRASADO OF DS-MOV-CRED-MANUT
                                 TO AUX1-IN-CRED-ATRASADO.
           MOVE ID-NIT           OF DS-MOV-CRED-MANUT
                                 TO AUX1-ID-NIT.
           MOVE NU-NIT-DV1       OF DS-MOV-CRED-MANUT
                                 TO AUX1-NU-NIT-DV1.
           MOVE 2                TO AUX1-CS-ROTINA-PAGTO.

           IF IN-CRED-ISENTO-IR OF DS-MOV-CRED-MANUT
              MOVE 1 TO AUX1-IN-CRED-ISENTO-IR
           ELSE
              MOVE 0 TO AUX1-IN-CRED-ISENTO-IR.

      *    IF IN-VAGO-03 OF DS-MOV-CRED-MANUT
      *       MOVE 1 TO AUX1-IN-ADIANT-13-SAL
      *    ELSE
      *       MOVE 0 TO AUX1-IN-ADIANT-13-SAL.

           IF IN-VAGO-03 OF DS-MOV-CRED-MANUT
              MOVE 1 TO AUX1-IN-BANCO-POSTAL
           ELSE
              MOVE 0 TO AUX1-IN-BANCO-POSTAL.

           MOVE D2-COMPET-CREDITO OF DS-MOV-CRED-MANUT TO WS-COMPET.

      *     IF (IN-SEGUNDA-PARC13 OF DS-MOV-CRED-MANUT)
      *        AND                                  (WS-MM-COMPET = 08)
      *        MOVE 1 TO AUX1-IN-ADIANT-13-SAL
      *     ELSE
      *        MOVE 0 TO AUX1-IN-ADIANT-13-SAL.
      *
      *** ALTERACAO TEMPORARIA - 08/12/2015
      *
      *     IF IN-ADIANT-13-SAL OF DS-MOV-CRED-MANUT
      *        MOVE 1 TO AUX1-IN-ADIANT-13-SAL
      *     ELSE
      *        MOVE 0 TO AUX1-IN-ADIANT-13-SAL.

           IF IN-ADIANT-13-SAL OF DS-MOV-CRED-MANUT
              MOVE 1 TO AUX1-IN-IR-ADIAN-13SAL
           ELSE
              MOVE 0 TO AUX1-IN-IR-ADIAN-13SAL.
      *
      ***
      *     IF (IN-SEGUNDA-PARC13 OF DS-MOV-CRED-MANUT) AND
      *        (WS-MM-COMPET NOT = 08)
      *        MOVE 1 TO AUX1-IN-IR-ADIAN-13SAL
      *     ELSE
      *        MOVE 0 TO AUX1-IN-IR-ADIAN-13SAL.
      *
      *** ALTERACAO TEMPORARIA - 08/12/2015
      *
      *     IF IN-IR-ADIAN-13SAL OF DS-MOV-CRED-MANUT
      *        MOVE 1 TO AUX1-IN-IR-ADIAN-13SAL
      *     ELSE
      *        MOVE 0 TO AUX1-IN-IR-ADIAN-13SAL.

           IF IN-IR-ADIAN-13SAL OF DS-MOV-CRED-MANUT
              MOVE 1 TO AUX1-IN-ADIANT-13-SAL
           ELSE
              MOVE 0 TO AUX1-IN-ADIANT-13-SAL.
      *
      ***
           IF (PAR-MOV-CREDITO = "MANU") AND
              (ID-BANCO OF DS-MOV-CRED-MANUT NOT = 998 AND 996)
             MOVE ID-PREGAO OF DS-MOV-CRED-MANUT TO AUX1-ID-PREGAO
             MOVE ID-LOTE-PREGAO OF DS-MOV-CRED-MANUT
                                                 TO AUX1-ID-LOTE-PREGAO.

           IF IN-CREDITO-RRA OF DS-MOV-CRED-MANUT
              MOVE 1 TO AUX1-IN-CREDITO-RRA
           ELSE
              MOVE 0 TO AUX1-IN-CREDITO-RRA.

           MOVE CS-ORIGEM-ORCAMENTO OF DS-MOV-CRED-MANUT
                                    TO AUX1-CS-ORIGEM-ORCAMENTO.
      **
           MOVE NU-CPF-TITULAR OF DS-MOV-CRED-MANUT
                               TO AUX1-NU-CPF-TITULAR.
           MOVE NU-NIT-TITULAR OF DS-MOV-CRED-MANUT
                               TO AUX1-NU-NIT-TITULAR.
      **
           MOVE D2-COMP-ATU-VL-CR OF DS-MOV-CRED-MANUT
                                  TO AUX1-DT-COMP-ATU-VL-CR.

           IF CS-ORIGEM-CREDITO OF DS-MOV-CRED-MANUT = 4
           AND NAO-E-CREDITO-DE-CALAMIDADE
              FIND S2-CRED-ALTER-OL  AT
                   ID-OL-SOLIC-CRED  OF DS-MOV-CRED-ALTER =
                   ID-OL-SOLIC-CRED  OF DS-MOV-CRED-MANUT  AND
                   D2-SOLIC-CREDITO  OF DS-MOV-CRED-ALTER =
                   D2-SOLIC-CREDITO  OF DS-MOV-CRED-MANUT  AND
                   NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER =
                   NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-MANUT
                   ON EXCEPTION
                      MOVE
                "P/SUB/CPG/001 - ERRO FIND DS-MOV-CRED-ALTER - ROT 2510"
                      TO WS-MENSAGEM-ZDISPLAY
                      CALL "ZDISPLAY IN DTP/L1" USING
                                                WS-MENSAGEM-ZDISPLAY,
                                                WS-INTEIRO-ZDISPLAY
                      CALL SYSTEM DMTERMINATE.
      *  IF SPGENT-NB = 7028623751
      *  MOVE 1 TO AUX-IN-FIM-ARQ.
       2520-FIM. EXIT.

       2523-FIND-MANU.

            FIND NEXT DS-MOV-CRED-MANUT
                      ON EXCEPTION
                         IF DMSTATUS(NOTFOUND)
                            MOVE 1 TO AUX-IN-FIM-ARQ
                            GO TO 2523-FIM
                         ELSE
                            MOVE "FIND NEXT DS-MOV-CRED-MANUT.LIDOS:"
                              TO  WS-PARAM1-ZDISPLAY
                            MOVE W77-LIDOS-MANUT
                              TO  WS-PARAM2-ZDISPLAY
                            CALL "ZDISPLAY2 IN DTP/L1" USING
                                  WS-PARAM1-ZDISPLAY,
                                  WS-PARAM2-ZDISPLAY,
                                  WS-INTEIRO-ZDISPLAY
                            CALL  SYSTEM DMTERMINATE.

      *Jose Fonseca - DM.096405 - INSS EM NUMEROS.
           IF PAR-MOV-CREDITO = "MANU"
             AND CS-MEIO-PAGTO OF DS-MOV-CRED-MANUT = 04
             AND NAO-E-CREDITO-DE-CALAMIDADE
                 IF CS-RAMO-ATIVIDADE OF DS-MOV-CRED-MANUT = 8
                    ADD 1 TO WS-QTD-DNT(2) WS-QTD-PAB-TOT
                    ADD VL-LIQUIDO-CRED OF DS-MOV-CRED-MANUT TO
                         WS-VL-DNT(2) WS-VL-PAB-TOT
                 ELSE
                    ADD 1 TO WS-QTD-DNT(1) WS-QTD-PAB-TOT
                    ADD VL-LIQUIDO-CRED OF DS-MOV-CRED-MANUT TO
                         WS-VL-DNT(1) WS-VL-PAB-TOT.
      *Fim DM.096405.

      *GATILHO, TIRAR PARA PRODUCAO.
      *     IF D2-INI-VALIDADE OF DS-MOV-CRED-MANUT < 20160301
      *        GO TO 2523-FIND-MANU.

       2523-FIM. EXIT.

       2525-FIND-CALA.

            LOCK NEXT ST-CRED-CALAM-NB
                   AT ID-CALAMIDADE     = WS-SISPG-COD-CALAMIDADE
                  AND CS-ORIGEM-CREDITO = 4
                  AND NU-SEQ-PROC       = NU-SEQ-PROC   OF DS-CALAMIDADE
                      ON EXCEPTION
                         IF DMSTATUS(NOTFOUND)
                            MOVE 1 TO AUX-IN-FIM-ARQ
                            GO TO 2525-FIM
                         ELSE
                            MOVE "FIND NEXT DS-MOV-CRED-CALAM.LIDOS:"
                              TO  WS-PARAM1-ZDISPLAY
                            MOVE W77-LIDOS-MANUT
                              TO  WS-PARAM2-ZDISPLAY
                            CALL "ZDISPLAY2 IN DTP/L1" USING
                                  WS-PARAM1-ZDISPLAY,
                                  WS-PARAM2-ZDISPLAY,
                                  WS-INTEIRO-ZDISPLAY
                            CALL  SYSTEM DMTERMINATE.

           MOVE CORR DS-MOV-CRED-CALAM TO DS-MOV-CRED-MANUT.

           MOVE  QT-CONSIGNACAO OF DS-MOV-CRED-CALAM
             TO W77-QT-GRUPO.

           PERFORM MOVE-GR-CONSIGNACAO THRU FIM-GR-CONSIGNACAO
           VARYING W77-INDICE FROM 1 BY 1
             UNTIL W77-INDICE > W77-QT-GRUPO
                OR W77-QT-GRUPO = 0.

           MOVE  QT-DIFERENCA OF DS-MOV-CRED-CALAM
             TO W77-QT-GRUPO.

           PERFORM MOVE-GR-DIFERENCA THRU FIM-GR-DIFERENCA
           VARYING W77-INDICE FROM 1 BY 1
             UNTIL W77-INDICE > W77-QT-GRUPO
                OR W77-QT-GRUPO = 0.

           MOVE  QT-RUBRICA OF DS-MOV-CRED-CALAM
             TO W77-QT-GRUPO.

           PERFORM MOVE-GR-RUBRICA THRU FIM-GR-RUBRICA
           VARYING W77-INDICE FROM 1 BY 1
             UNTIL W77-INDICE > W77-QT-GRUPO
                OR W77-QT-GRUPO = 0.

       2525-FIM. EXIT.

       MOVE-GR-CONSIGNACAO.

           MOVE NU-SEQ-CONSIG     OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO NU-SEQ-CONSIG      OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE VL-SALDO-CONSIG   OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO VL-SALDO-CONSIG    OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE CS-CONSIGNACAO    OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO CS-CONSIGNACAO     OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE VL-CONSIGNACAO    OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO VL-CONSIGNACAO     OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE QT-COMP-DESCONTOS OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO QT-COMP-DESCONTOS  OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE D2-INI-DESCONTO   OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO D2-INI-DESCONTO    OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE QT-TOT-PAR-CONSIG OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO QT-TOT-PAR-CONSIG  OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE CS-NAO-DESCONTO   OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO CS-NAO-DESCONTO    OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE ID-EMPRESTIMO     OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO ID-EMPRESTIMO      OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE NU-PARCELA-CONSIG OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO NU-PARCELA-CONSIG  OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE ID-BANCO-EMPREST  OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO ID-BANCO-EMPREST   OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE ID-CONSIG         OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO ID-CONSIG          OF DS-MOV-CRED-MANUT(W77-INDICE).

       FIM-GR-CONSIGNACAO. EXIT.

       MOVE-GR-DIFERENCA.

           MOVE CS-MOTIVO-DIFER    OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO CS-MOTIVO-DIFER     OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE QT-TOTAL-PARCELA   OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO QT-TOTAL-PARCELA    OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE QT-PARCELA-PAGA    OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO QT-PARCELA-PAGA     OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE VL-TOTAL-PAGO      OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO VL-TOTAL-PAGO       OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE VL-PARCELA-ATUAL   OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO VL-PARCELA-ATUAL    OF DS-MOV-CRED-MANUT(W77-INDICE).

       FIM-GR-DIFERENCA. EXIT.

       MOVE-GR-RUBRICA.

           MOVE CS-RUBRICA    OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO CS-RUBRICA     OF DS-MOV-CRED-MANUT(W77-INDICE)
           MOVE VL-RUBRICA    OF DS-MOV-CRED-CALAM(W77-INDICE)
            TO VL-RUBRICA     OF DS-MOV-CRED-MANUT(W77-INDICE).

       FIM-GR-RUBRICA. EXIT.
      /
       2530-LE-MOV-ALT.

            MOVE "FASE LE-MOV-ALTE " TO W77-FASE.

            FIND NEXT DS-MOV-CRED-ALTER
                      ON EXCEPTION
                         IF DMSTATUS(NOTFOUND)
                            MOVE 1 TO AUX-IN-FIM-ARQ
                            GO TO 2530-FIM
                         ELSE
                            MOVE "FIND NEXT DS-MOV-CRED-ALTER.LIDOS:"
                              TO  WS-PARAM1-ZDISPLAY
                            MOVE W77-LIDOS-ALTE
                              TO  WS-PARAM2-ZDISPLAY
                            CALL "ZDISPLAY2 IN DTP/L1" USING
                                  WS-PARAM1-ZDISPLAY,
                                  WS-PARAM2-ZDISPLAY,
                                  WS-INTEIRO-ZDISPLAY
                            CALL  SYSTEM DMTERMINATE.
      *

           ADD 1 TO W77-LIDOS-ALTE WS-LIDOS-ALTE.
      *****
           MOVE ZEROS TO SISPG-MARCA-GRUPO-RUBRICAS.
           IF ID-OL-SOLIC-CRED  OF DS-MOV-CRED-ALTER = 23001040 AND
              D2-SOLIC-CREDITO  OF DS-MOV-CRED-ALTER = 20101206 AND
              NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER = 9998
              GO TO 2530-CONTINUA-CRIT-LEITURA.
      *****
             IF D2-FIM-PERIODO OF DS-MOV-CRED-ALTER = 20061131
                MOVE 20061130 TO D2-FIM-PERIODO OF DS-MOV-CRED-ALTER.
      ******************************************************************
             IF (D2-MOV-CREDITO OF DS-MOV-CRED-ALTER = 20080130) AND
                (D2-INI-VALIDADE OF DS-MOV-CRED-ALTER = 20080230)
                 MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                 STRING "NB D2-INI-VALIDADE ERRADA:" WS-NU-NB
                 DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                 CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
                 MOVE 20080303 TO D2-INI-VALIDADE OF DS-MOV-CRED-ALTER.

             IF (D2-MOV-CREDITO OF DS-MOV-CRED-ALTER = 20080130) AND
                (D2-INI-VALIDADE OF DS-MOV-CRED-ALTER = 20080231)
                 MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                 STRING "NB D2-INI-VALIDADE ERRADA:" WS-NU-NB
                 DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                 CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
                 MOVE 20080303 TO D2-INI-VALIDADE OF DS-MOV-CRED-ALTER.
      ******************************************************************
             IF QT-RUBRICA OF DS-MOV-CRED-ALTER < 1 OR > 99
                MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                STRING "NB C/ QT-RUBRICA ZERAD0:" WS-NU-NB
                        DELIMITED BY SIZE INTO  WS-MENSAGEM-ZDISPLAY
                CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
                                                WS-INTEIRO-ZDISPLAY
                GO TO 2530-LE-MOV-ALT.

             IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 1
                IF CS-RUBRICA OF DS-MOV-CRED-ALTER(2) = 0
                   MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                   STRING "NB C/ RUBRICA(2) ZERADA:" WS-NU-NB
                   DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                   WS-INTEIRO-ZDISPLAY
                   MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(2).

             IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 2
                IF CS-RUBRICA OF DS-MOV-CRED-ALTER(3) = 0
                   MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                    STRING "NB C/ RUBRICA(3) ZERADA:" WS-NU-NB
                    DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                    CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                    WS-INTEIRO-ZDISPLAY
                    MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(3).

           IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 3
              IF CS-RUBRICA OF DS-MOV-CRED-ALTER(4) = 0
                  MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                  STRING "NB C/ RUBRICA(4) ZERADA:" WS-NU-NB
                  DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                WS-INTEIRO-ZDISPLAY
                MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(4).

           IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 4
              IF CS-RUBRICA OF DS-MOV-CRED-ALTER(5) = 0
                 MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                  STRING "NB C/ RUBRICA(5) ZERADA:" WS-NU-NB
                  DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                WS-INTEIRO-ZDISPLAY
                MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(5).

           IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 5
              IF CS-RUBRICA OF DS-MOV-CRED-ALTER(6) = 0
                 MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                  STRING "NB C/ RUBRICA(6) ZERADA:" WS-NU-NB
                  DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                WS-INTEIRO-ZDISPLAY
                MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(6).

           IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 6
              IF CS-RUBRICA OF DS-MOV-CRED-ALTER(7) = 0
                 MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                  STRING "NB C/ RUBRICA(7) ZERADA:" WS-NU-NB
                  DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                WS-INTEIRO-ZDISPLAY
                MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(7).

           IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 7
              IF CS-RUBRICA OF DS-MOV-CRED-ALTER(8) = 0
                 MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                  STRING "NB C/ RUBRICA(8) ZERADA:" WS-NU-NB
                  DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                WS-INTEIRO-ZDISPLAY
                MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(8).

           IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 8
              IF CS-RUBRICA OF DS-MOV-CRED-ALTER(9) = 0
                 MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                  STRING "NB C/ RUBRICA(9) ZERADA:" WS-NU-NB
                  DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                      WS-INTEIRO-ZDISPLAY
                MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(9).

             IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 9
                IF CS-RUBRICA OF DS-MOV-CRED-ALTER(10) = 0
                   MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                   STRING "NB C/ RUBRICA(10) ZERADA:" WS-NU-NB
                   DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                   WS-INTEIRO-ZDISPLAY
                   MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(10).

             IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 10
                IF CS-RUBRICA OF DS-MOV-CRED-ALTER(11) = 0
                   MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                   STRING "NB C/ RUBRICA(11) ZERADA:" WS-NU-NB
                   DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                   WS-INTEIRO-ZDISPLAY
                   MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(11).

             IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 11
                IF CS-RUBRICA OF DS-MOV-CRED-ALTER(12) = 0
                   MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                   STRING "NB C/ RUBRICA(12) ZERADA:" WS-NU-NB
                   DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                   WS-INTEIRO-ZDISPLAY
                   MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(12).

             IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 12
                IF CS-RUBRICA OF DS-MOV-CRED-ALTER(13) = 0
                   MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                   STRING "NB C/ RUBRICA(13) ZERADA:" WS-NU-NB
                   DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                   WS-INTEIRO-ZDISPLAY
                   MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(13).

             IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 13
                IF CS-RUBRICA OF DS-MOV-CRED-ALTER(14) = 0
                   MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                   STRING "NB C/ RUBRICA(14) ZERADA:" WS-NU-NB
                   DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                   WS-INTEIRO-ZDISPLAY
                   MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(14).

             IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 15
                IF CS-RUBRICA OF DS-MOV-CRED-ALTER(16) = 0
                   MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                   STRING "NB C/ RUBRICA(16) ZERADA:" WS-NU-NB
                   DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                   WS-INTEIRO-ZDISPLAY
                   MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(16).

             IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 16
                IF CS-RUBRICA OF DS-MOV-CRED-ALTER(17) = 0
                   MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                   STRING "NB C/ RUBRICA(17) ZERADA:" WS-NU-NB
                   DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                                   WS-INTEIRO-ZDISPLAY
                   MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(17).

             IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 17
                IF CS-RUBRICA OF DS-MOV-CRED-ALTER(18) = 0
                   MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
                   STRING "NB C/ RUBRICA(18) ZERADA:" WS-NU-NB
                   DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
                   CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY
                                             WS-INTEIRO-ZDISPLAY
                   MOVE 999 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(18).
      *
            IF CS-RUBRICA OF DS-MOV-CRED-ALTER(1) = 0
               MOVE 101 TO CS-RUBRICA OF DS-MOV-CRED-ALTER(1)
               MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
               STRING "NB C/ RUBRICA(1) ZERADA:" WS-NU-NB
                      DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
               CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY.

      *ALTERACAO TEMPORARIA
      *    IF (D2-SOLIC-CREDITO OF DS-MOV-CRED-ALTER < 20070101) AND
      *       (NOT IN-CRED-DELETADO OF DS-MOV-CRED-ALTER)
      *       MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
      *       MOVE ID-OL-SOLIC-CRED OF DS-MOV-CRED-ALTER  TO WS-OL
      *       MOVE D2-SOLIC-CREDITO OF DS-MOV-CRED-ALTER  TO WS-DT-SOLIC
      *       MOVE NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER TO WS-NU-SEQ
      *       STRING "NB:" WS-NU-NB " OL:" WS-OL " DT:" WS-DT-SOLIC
      *              " SEQ: " WS-NU-SEQ
      *              DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
      *       CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
      *                                       WS-INTEIRO-ZDISPLAY
      *       GO TO 2530-LE-MOV-ALT.

      *ALTERACAO TEMPORARIA  21/01/2023
           IF (NU-SEQ-PROC OF DS-MOV-CRED-ALTER = 794) AND
              (NOT IN-CRED-DELETADO OF DS-MOV-CRED-ALTER)
              MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
              MOVE ID-OL-SOLIC-CRED OF DS-MOV-CRED-ALTER  TO WS-OL
              MOVE D2-SOLIC-CREDITO OF DS-MOV-CRED-ALTER  TO WS-DT-SOLIC
              MOVE NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER TO WS-NU-SEQ
              STRING "NB REJEITADO:" WS-NU-NB
                     " OL:" WS-OL " DT:" WS-DT-SOLIC " SEQ: " WS-NU-SEQ
                     DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
              CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
                                              WS-INTEIRO-ZDISPLAY
              GO TO 2530-LE-MOV-ALT.

      *
           IF (IN-CRED-DELETADO  OF DS-MOV-CRED-ALTER)
              ADD 1 TO AUX-QT-DELETADOS
              GO TO 2530-LE-MOV-ALT.

      *    IF (ID-OL-SOLIC-CRED  OF DS-MOV-CRED-ALTER = 18001030) AND
      *       (D2-SOLIC-CREDITO  OF DS-MOV-CRED-ALTER = 20070726) AND
      *       (NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER = 0001)
      *       MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-NU-NB
      *       STRING "NB C/ RUB. ZERADA:" WS-NU-NB
      *       DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
      *       CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
      *                                       WS-INTEIRO-ZDISPLAY
      *       GO TO 2530-LE-MOV-ALT.

           IF NU-SEQ-PROC OF DS-MOV-CRED-ALTER < NU-ULT-PROC-ALTER
              GO 2530-CONTINUA-CRIT-LEITURA.
      *****
           IF ID-OL-SOLIC-CRED  OF DS-MOV-CRED-ALTER = 18001030 AND
              D2-SOLIC-CREDITO  OF DS-MOV-CRED-ALTER = 20070726 AND
              NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER = 1
              GO TO 2530-CONTINUA-CRIT-LEITURA.
      *****
           IF ID-OL-SOLIC-CRED  OF DS-MOV-CRED-ALTER = 08021140 AND
              D2-SOLIC-CREDITO  OF DS-MOV-CRED-ALTER = 20111104 AND
              NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER = 1
              GO TO 2530-CONTINUA-CRIT-LEITURA.
      *****
      ***19/12/2019
           IF ID-OL-SOLIC-CRED  OF DS-MOV-CRED-ALTER = 11027010 AND
              D2-SOLIC-CREDITO  OF DS-MOV-CRED-ALTER = 20180827 AND
              NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER = 9993
              GO TO 2530-CONTINUA-CRIT-LEITURA.
      *****
           IF NU-SEQ-PROC OF DS-MOV-CRED-ALTER < 990
              ADD 1 TO AUX-QT-NU-SEQ-PROC
              MOVE "PAB DESP.NU-SEQ-PROC < 990. NB:"
                TO  WS-PARAM1-ZDISPLAY
              MOVE  NU-NB OF DS-MOV-CRED-ALTER TO WS-PARAM2-ZDISPLAY
              CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY
                                                  WS-PARAM2-ZDISPLAY
                                                  WS-INTEIRO-ZDISPLAY
              GO TO 2530-LE-MOV-ALT.

           IF D2-MOV-CREDITO OF DS-MOV-CRED-ALTER NOT <
              WS-DT-ANTES-6-MESES
              GO 2530-CONTINUA-CRIT-LEITURA
           ELSE
              MOVE "PAB DESP.DT-SOLIC + DE 6 MESES.NB:"
                TO  WS-PARAM1-ZDISPLAY
              MOVE  NU-NB OF DS-MOV-CRED-ALTER TO WS-PARAM2-ZDISPLAY
              CALL "ZDISPLAY2 IN DTP/L1" USING    WS-PARAM1-ZDISPLAY
                                                  WS-PARAM2-ZDISPLAY
                                                  WS-INTEIRO-ZDISPLAY
              ADD 1 TO AUX-QT-NU-SEQ-PROC
              GO TO 2530-LE-MOV-ALT.
      *
       2530-CONTINUA-CRIT-LEITURA.
      *--------------------------*
      *Pagamentos do Mercosul.   Jose Carlos Fonseca 20/09/2012 QUI
           MOVE ID-PAIS OF DS-MOV-CRED-ALTER   TO AUX1-ID-PAIS.

           MOVE ID-ORGAO-PAGADOR OF DS-MOV-CRED-ALTER
             TO AUX1-ID-ORGAO-PAGADOR.

           MOVE ALL "00" TO OUTROS-CAMPOS-SPGENT
                                  OUTROS-CAMPOS-SISPG
                                  WS-TAB-RUBR-ENT.

      *     PERFORM 7000-LIMPA-RUB THRU
      *             7000-LIMPA-RUB-EXIT
      *       VARYING IX FROM 1 BY 1
      *         UNTIL IX > 72.

           MOVE VL-LIQUIDO-CRED OF DS-MOV-CRED-ALTER TO
                SISPG-VALOR-LIQUIDO
                SPGENT-VALOR-LIQUIDO
                LIN-LIQUIDO.

           MOVE CS-ORIGEM-CREDITO OF DS-MOV-CRED-ALTER
             TO WS-SISPG-ORIG-CRED-H.
           MOVE VL-BRUTO-CREDITO OF DS-MOV-CRED-ALTER TO
                SISPG-VALOR-BRUTO
                SPGENT-VALOR-BRUTO
                 LIN-BRUTO.

           MOVE ZEROS TO WS-SISPG-VALOR-RUB301.
           IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 0
              MOVE ZEROS TO WS-TAB-RUBRICAS-AUX IX
                            WS-VL-RUB-301-SEM-ALT
      *NOVO   MOVE ZEROS TO SISPG-VALOR-GRUPO-RUBRICAS
              MOVE ZEROS TO SISPG-VALOR-GRUPO-RUBRICAS
                            SPGENT-VALOR-GRUPO-RUBRICAS.

              MOVE QT-RUBRICA OF DS-MOV-CRED-ALTER TO SISPG-OCOR-RUB
                                                      SPGENT-OCOR-RUB.
              MOVE ZEROS TO WS-VL-RUB-301-SEM-ALT WS-SISPG-VALOR-RUB301
                LIN-RUB301  WS-BRUTO-RUB-105  SISPG-MARCA-RUB231

              PERFORM 2794-SALVA-RUB THRU 2794-FIM
              VARYING W77-IND-RUBRICAS FROM 1 BY 1
               UNTIL W77-IND-RUBRICAS > QT-RUBRICA OF DS-MOV-CRED-ALTER.

           MOVE SPACES TO LIN-SISPG.
           MOVE " NB#" TO SISPG-BRANCO1
                          SPGENT-BRANCO1.
           MOVE NU-NB OF DS-MOV-CRED-ALTER TO NB-I WS-NU-NB-REG SISPG-NB
            SPGENT-NB.
           MOVE WS-SISPG-VALOR-RUB301 TO LIN-RUB301.

           IF  WS-FLAG-LISTA-RUB = 0
               GO TO PULA-LISTA-RUBR-ALTER.

           ADD 1 TO WS-QUANT-LINHA.
      *     IF  WS-QUANT-LINHA > 100
           IF  WS-QUANT-LINHA > ZEROS
               GO TO PULA-LISTA-RUBR-ALTER.

           IF  WS-QUANT-LINHA = 1
               MOVE "LISTA TODAS AS RUBRICAS DO DS-MOV-CRED-ALTER" TO
               LSUBSISPG-REG
               WRITE LSUBSISPG-REG MOVE SPACES TO LSUBSISPG-REG.

           MOVE QT-RUBRICA OF DS-MOV-CRED-ALTER TO N-RUB-I

           PERFORM ZZZZ-LISTA-RUBR-ALTER THRU
                   ZZZZ-LISTA-RUBR-ALTER-EXIT
              VARYING IX FROM 1 BY 1
              UNTIL   IX > 42 OR
                      IX > QT-RUBRICA OF DS-MOV-CRED-ALTER.
          WRITE LSUBSISPG-REG FROM LIN-SISPG AFTER 1.
          MOVE ZEROS TO IND-TB.
          WRITE LSUBSISPG-REG FROM LIN-SISPG-LIQ AFTER 1.
       PULA-LISTA-RUBR-ALTER.

           IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 0
              MOVE LOW-VALUES TO WS-TABELA-RUBRICAS
              MOVE 0 TO W77-IND-RUBRICAS  W77-IND-TAB-RUB
              PERFORM 2750-TIRA-RUBRICA-900-MOV-ALTE THRU 2750-FIM
                UNTIL W77-IND-RUBRICAS = QT-RUBRICA OF DS-MOV-CRED-ALTER
              MOVE 0 TO W77-IND-RUBRICAS
              PERFORM 2780-LIMPA-RUBRICAS-ALTE THRU 2780-EXIT
                UNTIL W77-IND-RUBRICAS = QT-RUBRICA OF DS-MOV-CRED-ALTER
              MOVE W77-IND-TAB-RUB TO QT-RUBRICA OF DS-MOV-CRED-ALTER
              MOVE 0 TO W77-IND-RUBRICAS
              PERFORM 2760-ALTERA-UWA-MOV-ALTE THRU 2760-EXIT
                UNTIL W77-IND-RUBRICAS = W77-IND-TAB-RUB.

           IF  IN-PAB-DOMIC-BANC OF DS-MOV-CRED-ALTER
               IF IN-CRED-ESPECIAL OF DS-MOV-CRED-ALTER
                  NEXT SENTENCE
               ELSE
                  IF IN-CRED-EMPR-CONV OF DS-MOV-CRED-ALTER
                     IF NOT IN-ACORDO-INTERNACIONAL
                        NEXT SENTENCE
                     ELSE
                        ADD 1 TO AUX-QT-DELETADOS
                        GO TO 2530-LE-MOV-ALT
                  ELSE
                     ADD 1 TO AUX-QT-DELETADOS
                     GO TO 2530-LE-MOV-ALT.

           MOVE ZEROS           TO AUX1-DT-DDB.
           MOVE VL-LIQUIDO-CRED OF DS-MOV-CRED-ALTER
                                TO AUX1-VL-LIQUIDO-CRED.

           IF IN-CREDITO-EPU OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-CREDITO-EPU
           ELSE
              MOVE ZERO TO AUX1-IN-CREDITO-EPU.

           MOVE D2-MOV-CREDITO OF DS-MOV-CRED-ALTER
                               TO AUX1-DT-MOV-CREDITO.
           MOVE NU-NB          OF DS-MOV-CRED-ALTER
                               TO AUX1-NU-NB.
           MOVE D2-INI-PERIODO OF DS-MOV-CRED-ALTER
                               TO AUX1-DT-INI-PERIODO.
           MOVE D2-FIM-PERIODO OF DS-MOV-CRED-ALTER
                               TO AUX1-DT-FIM-PERIODO.
           MOVE D2-INI-VALIDADE OF DS-MOV-CRED-ALTER
                                TO AUX1-DT-INI-VALIDADE.
           MOVE D2-FIM-VALIDADE OF DS-MOV-CRED-ALTER
                                TO AUX1-DT-FIM-VALIDADE.
           MOVE ID-BANCO        OF DS-MOV-CRED-ALTER
                                TO AUX1-ID-BANCO.

           IF IN-CRED-ESPECIAL OF DS-MOV-CRED-ALTER
              MOVE NU-CONTA-CORRENTE OF DS-MOV-CRED-ALTER
                                     TO AUX1-NU-CONTA-CORRENTE
           ELSE
              MOVE SPACES       TO  AUX1-NU-CONTA-CORRENTE.

           MOVE CS-ESPECIE      OF DS-MOV-CRED-ALTER
                                TO AUX1-CS-ESPECIE.
           MOVE D2-COMPET-CREDITO OF DS-MOV-CRED-ALTER
                                  TO AUX1-DT-COMPET-CREDITO.
           MOVE CS-ORIGEM-CREDITO OF DS-MOV-CRED-ALTER
                                  TO AUX1-CS-ORIGEM-CREDITO.
           MOVE ID-OL-SOLIC-CRED  OF DS-MOV-CRED-ALTER
                                  TO WS-OL-MANUT.
           MOVE WS-UF             TO AUX1-CS-UF.

           IF IN-CRED-CART-SOC OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-CRED-CART-SOC
           ELSE
              MOVE ZERO TO AUX1-IN-CRED-CART-SOC.

           IF (IN-VAGO-03 OF DS-MOV-CRED-ALTER AND CS-MEIO-PAGTO OF
               DS-MOV-CRED-ALTER NOT EQUAL 4) AND
              (ID-BANCO OF DS-MOV-CRED-ALTER = 104 OR 237)
              MOVE 4 TO AUX1-IN-CRED-PIONEIRA
           ELSE
              IF IN-CRED-PIONEIRA OF DS-MOV-CRED-ALTER
                 MOVE 1 TO AUX1-IN-CRED-PIONEIRA
              ELSE
                 MOVE ZERO TO AUX1-IN-CRED-PIONEIRA.

           MOVE QT-RUBRICA        OF DS-MOV-CRED-ALTER
                                  TO AUX1-QT-RUBRICA.

           MOVE ZEROS             TO AUX1-CS-RUBRICA1
                                     AUX1-CS-RUBRICA2
                                     AUX1-VL-RUBRICA1
                                     AUX1-VL-RUBRICA2.

           IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 0
              IF (CS-RUBRICA OF DS-MOV-CRED-ALTER (1) > 0) AND
                 (VL-RUBRICA OF DS-MOV-CRED-ALTER (1) IS NULL)
                  MOVE "BENEFICIO COM RUBRICA ZERADA - NB=" TO
                        WS-PARAM1-ZDISPLAY
                  MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-PARAM2-ZDISPLAY
                  CALL "ZDISPLAY2 IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                  MOVE ZERO TO AUX1-VL-RUBRICA1
                  MOVE CS-RUBRICA OF DS-MOV-CRED-ALTER (1)
                                  TO AUX1-CS-RUBRICA1
              ELSE
                  MOVE CS-RUBRICA OF DS-MOV-CRED-ALTER (1)
                                  TO AUX1-CS-RUBRICA1
                  MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER (1)
                                  TO AUX1-VL-RUBRICA1.

           IF QT-RUBRICA OF DS-MOV-CRED-ALTER > 1
              IF (CS-RUBRICA OF DS-MOV-CRED-ALTER (2) > 0) AND
                 (VL-RUBRICA OF DS-MOV-CRED-ALTER (2) IS NULL)
                  MOVE "BENEFICIO COM RUBRICA ZERADA - NB=" TO
                        WS-PARAM1-ZDISPLAY
                  MOVE NU-NB OF DS-MOV-CRED-ALTER TO WS-PARAM2-ZDISPLAY
                  CALL "ZDISPLAY2 IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                  MOVE ZERO TO AUX1-VL-RUBRICA2
                  MOVE CS-RUBRICA OF DS-MOV-CRED-ALTER (2)
                                  TO AUX1-CS-RUBRICA2
              ELSE
                  MOVE CS-RUBRICA    OF DS-MOV-CRED-ALTER (2)
                                     TO AUX1-CS-RUBRICA2
                  MOVE VL-RUBRICA    OF DS-MOV-CRED-ALTER (2)
                                     TO AUX1-VL-RUBRICA2.

           MOVE CS-NATUR-CREDITO OF DS-MOV-CRED-ALTER
                                 TO AUX1-CS-NATUR-CREDITO.
           MOVE CS-MEIO-PAGTO OF DS-MOV-CRED-ALTER TO AUX1-MEIO-PAGTO
           MOVE D2-MOV-CREDITO   OF DS-MOV-CRED-ALTER
                                 TO AUX1-DT-MOV-CREDITO.
           MOVE ID-ORGAO-PAGADOR OF DS-MOV-CRED-ALTER
                                 TO AUX1-ID-ORGAO-PAGADOR.
      ***  MOVE ID-PAIS OF DS-MOV-CRED-ALTER TO AUX1-ID-ORGAO-PAGADOR.

           IF CS-MOTIVO-SOLIC OF DS-MOV-CRED-ALTER = 26
              IF (CS-ESPECIE  OF DS-MOV-CRED-ALTER > 00 AND
                  CS-ESPECIE  OF DS-MOV-CRED-ALTER < 14) OR
                  CS-ESPECIE  OF DS-MOV-CRED-ALTER = 15
                  MOVE 8 TO CS-RAMO-ATIVIDADE OF DS-MOV-CRED-ALTER.

           MOVE CS-RAMO-ATIVIDADE OF DS-MOV-CRED-ALTER
                                  TO AUX1-CS-RAMO-ATIVIDADE.
           MOVE ID-OL-SOLIC-CRED OF DS-MOV-CRED-ALTER
                                 TO AUX1-ID-OL-MANUTENCAO.
           MOVE NU-SEQ-PROC      OF DS-MOV-CRED-ALTER
                                 TO AUX1-SEQ-PROC.
           MOVE D2-SOLIC-CREDITO OF DS-MOV-CRED-ALTER
                                 TO AUX1-DT-SOLIC-CREDITO.
           MOVE NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER
                                  TO AUX1-NU-SEQ-SOLIC-CRED.
           MOVE 1                 TO AUX1-IN-CR-BLOQUEADO.
      **ALTERACAO PROVISORIA P/ EVITAR ERRO QUE TEM OCORRIDO NO RETORNO*
           IF (IN-CRED-ESPECIAL OF DS-MOV-CRED-ALTER) AND
              (CS-MEIO-PAGTO OF DS-MOV-CRED-ALTER = 02)
               MOVE ZEROS TO AUX1-ID-NIT AUX1-NU-NIT-DV1
           ELSE
      ******************************************************************
               MOVE ID-NIT            OF DS-MOV-CRED-ALTER
                                      TO AUX1-ID-NIT
               MOVE NU-NIT-DV1        OF DS-MOV-CRED-ALTER
                                      TO AUX1-NU-NIT-DV1.

           MOVE 4                 TO AUX1-CS-ROTINA-PAGTO.

           IF IN-ISENTO-IR OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-CRED-ISENTO-IR
           ELSE
              MOVE 0 TO AUX1-IN-CRED-ISENTO-IR.

           IF IN-VAGO-03 OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-BANCO-POSTAL
           ELSE
              MOVE 0 TO AUX1-IN-BANCO-POSTAL.

           MOVE ID-PREGAO OF DS-MOV-CRED-ALTER TO AUX1-ID-PREGAO.
           MOVE ID-LOTE-PREGAO OF DS-MOV-CRED-ALTER
                                               TO AUX1-ID-LOTE-PREGAO.

           IF IN-RRA-EXERC-ANT OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-RRA-EXERC-ANT
           ELSE
              MOVE 0 TO AUX1-IN-RRA-EXERC-ANT.

           IF IN-CREDITO-RRA OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-CREDITO-RRA
           ELSE
              MOVE 0 TO AUX1-IN-CREDITO-RRA.

           IF IN-RRA-PARCELADO OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-RRA-PARCELADO
           ELSE
              MOVE 0 TO AUX1-IN-RRA-PARCELADO.

           IF IN-CRED-SUBSTIT OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-CRED-SUBSTIT
           ELSE
              MOVE 0 TO AUX1-IN-CRED-SUBSTIT.

           IF IN-RRA-ATRAS-CONC OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-RRA-ATRAS-CONC
           ELSE
              MOVE 0 TO AUX1-IN-RRA-ATRAS-CONC.

           IF IN-ADIANT-13-SAL OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-ADIANT-13-SAL
           ELSE
              MOVE 0 TO AUX1-IN-ADIANT-13-SAL.

           IF IN-IR-ADIAN-13SAL OF DS-MOV-CRED-ALTER
              MOVE 1 TO AUX1-IN-IR-ADIAN-13SAL
           ELSE
              MOVE 0 TO AUX1-IN-IR-ADIAN-13SAL.

           MOVE IN-CRED-ATRASADO OF DS-MOV-CRED-ALTER
                                 TO AUX1-IN-CRED-ATRASADO.

           MOVE CS-ORIGEM-ORCAMENTO OF DS-MOV-CRED-ALTER
                                 TO AUX1-CS-ORIGEM-ORCAMENTO.
      **
           MOVE NU-CPF-TITULAR OF DS-MOV-CRED-ALTER
                               TO AUX1-NU-CPF-TITULAR.
           MOVE NU-NIT-TITULAR OF DS-MOV-CRED-ALTER
                               TO AUX1-NU-NIT-TITULAR.
      **
           MOVE D2-COMP-ATU-VL-CR OF DS-MOV-CRED-ALTER
                                  TO AUX1-DT-COMP-ATU-VL-CR.

           PERFORM LER-CREDITO-PAGTO THRU FIM-LER-CREDITO-PAGTO.

           IF WS-JA-PROCESSADO = 1
              GO 2530-LE-MOV-ALT.

      *MOVO    FALTA CRIAR NO DS-MOV-CRED-ALTER ID-CALAMIDADE
      *NOVO SO EXISTE NO DS-MOV-CRED-ALTER
      *****MOVE ID-PAIS       OF DS-MOV-CRED-ALTER TO SISPG-ID-PAIS.
      **   MOVE ID-CALAMIDADE OF DS-MOV-CRED-ALTER TO
      ***                      WS-SISPG-COD-CALAMIDADE.
       2530-FIM. EXIT.

       LER-CREDITO-PAGTO.

           MOVE 1 TO WS-JA-PROCESSADO.

           FIND FIRST S2-CREDITO-NB  AT
                NU-NB          = NU-NB OF DS-MOV-CRED-ALTER  AND
                D2-FIM-PERIODO = D2-FIM-PERIODO OF DS-MOV-CRED-ALTER AND
                D2-INI-PERIODO = D2-INI-PERIODO OF DS-MOV-CRED-ALTER AND
                CS-NATUR-CREDITO = CS-NATUR-CREDITO OF DS-MOV-CRED-ALTER
                ON EXCEPTION
                   IF DMSTATUS(NOTFOUND)
      ****************MOVE ZEROS TO WS-JA-PROCESSADO
                      GO LER-DS-CRED-PAGTO-EXP
                   ELSE
                      CALL SYSTEM DMTERMINATE.

          MOVE NU-NB OF DS-CREDITO-PAGTO TO WSS-NB.
          MOVE D2-FIM-PERIODO OF DS-CREDITO-PAGTO TO WSS-FIM-PER.
          MOVE D2-INI-PERIODO OF DS-CREDITO-PAGTO TO WSS-INI-PER.
          MOVE CS-NATUR-CREDITO OF DS-CREDITO-PAGTO TO WSS-NATUR.
          STRING "PGTO NB: ", WSS-NB , " FIM-PER: ", WSS-FIM-PER
                 " INI-PER: ", WSS-INI-PER, " NATUR: "WSS-NATUR
                 DELIMITED BY SIZE INTO WS-MENSAGEM-ZDISPLAY
          CALL "ZDISPLAY IN DTP/L1" USING    WS-MENSAGEM-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY.
           GO FIM-LER-CREDITO-PAGTO.
      *
       LER-DS-CRED-PAGTO-EXP.
      *---------------------*
      *
           FIND FIRST S2-CRED-NB-EXP AT
                NU-NB          = NU-NB OF DS-MOV-CRED-ALTER  AND
                D2-FIM-PERIODO = D2-FIM-PERIODO OF DS-MOV-CRED-ALTER AND
                D2-INI-PERIODO = D2-INI-PERIODO OF DS-MOV-CRED-ALTER AND
                CS-NATUR-CREDITO = CS-NATUR-CREDITO OF DS-MOV-CRED-ALTER
                ON EXCEPTION
                   IF DMSTATUS(NOTFOUND)
                      MOVE ZEROS TO WS-JA-PROCESSADO
                      GO FIM-LER-CREDITO-PAGTO
                   ELSE
                      CALL SYSTEM DMTERMINATE.

          MOVE NU-NB            OF DS-CRED-PAGTO-EXP TO WSS-NB.
          MOVE D2-FIM-PERIODO   OF DS-CRED-PAGTO-EXP TO WSS-FIM-PER.
          MOVE D2-INI-PERIODO   OF DS-CRED-PAGTO-EXP TO WSS-INI-PER.
          MOVE CS-NATUR-CREDITO OF DS-CRED-PAGTO-EXP TO WSS-NATUR.
          STRING "EXP  NB: ", WSS-NB , " FIM-PER: ", WSS-FIM-PER
                 " INI-PER: ", WSS-INI-PER, " NATUR: "WSS-NATUR
                 DELIMITED BY SIZE INTO WS-MENSAGEM-ZDISPLAY
          CALL "ZDISPLAY IN DTP/L1" USING    WS-MENSAGEM-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY.
      *
       FIM-LER-CREDITO-PAGTO.
      /
       2540-LE-CALC-ACAO-JUD.

            MOVE "FASE LE-CALC-ACAO-JUD" TO W77-FASE

            FIND NEXT DS-CALC-ACAO-JUD
                      ON EXCEPTION
                         IF DMSTATUS(NOTFOUND)
                            MOVE 1 TO AUX-IN-FIM-ARQ
                            GO TO 2540-FIM.

            ADD 1 TO W77-LIDOS-AP

            IF IN-CRED-DELETADO OF DS-CALC-ACAO-JUD
               ADD 1 TO AUX-QT-DELETADOS
               GO TO 2540-LE-CALC-ACAO-JUD.

            MOVE D2-PAGTO-ACAO-JUD OF DS-CALC-ACAO-JUD TO
                 AUX1-DT-MOV-CREDITO
            MOVE NU-NB OF DS-CALC-ACAO-JUD TO AUX1-NU-NB
            MOVE D2-INI-PERIODO OF DS-CALC-ACAO-JUD TO
                 AUX1-DT-INI-PERIODO
            MOVE D2-FIM-PERIODO OF DS-CALC-ACAO-JUD TO
                 AUX1-DT-FIM-PERIODO
            MOVE D2-PAGTO-ACAO-JUD TO AUX1-DT-INI-VALIDADE
                                      AUX1-DT-FIM-VALIDADE
            MOVE 1                 TO AUX1-ID-BANCO

            PERFORM 2541-ACESSA-ESPECIE THRU 2541-FIM

            MOVE D2-PAGTO-ACAO-JUD TO WS-DT-AUX4-AAMMDD
            MOVE WS-DT-AUX4-COMPET TO AUX1-DT-COMPET-CREDITO
            MOVE 6                 TO AUX1-CS-ORIGEM-CREDITO
            MOVE AUX1-ID-OL-MANUTENCAO TO WS-OL-MANUT
            MOVE WS-UF             TO AUX1-CS-UF

            IF (VL-BENEF-ACAO-JUD = 0) AND (VL-IR-ACAO-JUD = 0) AND
               (VL-CTB-PREV-ACAO  = 0)
                MOVE 0 TO AUX1-QT-RUBRICA
            ELSE
               IF (VL-BENEF-ACAO-JUD NOT = 0 AND VL-IR-ACAO-JUD = 0 AND
                   VL-CTB-PREV-ACAO = 0) OR
                  (VL-IR-ACAO-JUD NOT = 0 AND VL-BENEF-ACAO-JUD = 0 AND
                   VL-CTB-PREV-ACAO = 0) OR
                  (VL-CTB-PREV-ACAO NOT = 0 AND VL-BENEF-ACAO-JUD = 0
                   AND VL-CTB-PREV-ACAO = 0)
                   MOVE 1 TO AUX1-QT-RUBRICA
               ELSE
                IF (VL-BENEF-ACAO-JUD NOT = 0 AND VL-IR-ACAO-JUD NOT = 0
                    AND VL-CTB-PREV-ACAO = 0) OR
                   (VL-BENEF-ACAO-JUD NOT = 0 AND VL-CTB-PREV-ACAO
                    NOT = 0 AND VL-IR-ACAO-JUD = 0) OR
                   (VL-IR-ACAO-JUD NOT = 0 AND VL-CTB-PREV-ACAO NOT = 0
                    AND VL-BENEF-ACAO-JUD = 0)
                    MOVE 2 TO AUX1-QT-RUBRICA
                ELSE
                   MOVE  3 TO AUX1-QT-RUBRICA.

             MOVE 0 TO AUX1-CS-RUBRICA1
                       AUX1-CS-RUBRICA2
                       AUX1-VL-RUBRICA1
                       AUX1-VL-RUBRICA2

            IF VL-BENEF-ACAO-JUD NOT = 0
               MOVE 101 TO AUX1-CS-RUBRICA1
               MOVE VL-BENEF-ACAO-JUD TO AUX1-VL-RUBRICA1
               IF VL-IR-ACAO-JUD NOT = 0
                  MOVE 201 TO AUX1-CS-RUBRICA2
                  MOVE VL-IR-ACAO-JUD TO AUX1-VL-RUBRICA2
               ELSE
                  IF VL-CTB-PREV-ACAO NOT = 0
                     MOVE 206 TO AUX1-CS-RUBRICA2
                     MOVE VL-CTB-PREV-ACAO TO AUX1-VL-RUBRICA2
                  ELSE
                    NEXT SENTENCE
            ELSE
               IF VL-IR-ACAO-JUD NOT = 0
                  MOVE 201 TO AUX1-CS-RUBRICA1
                  MOVE VL-IR-ACAO-JUD TO AUX1-VL-RUBRICA1
                  IF VL-CTB-PREV-ACAO NOT = 0
                     MOVE 206 TO AUX1-CS-RUBRICA2
                     MOVE VL-CTB-PREV-ACAO TO AUX1-VL-RUBRICA2
                  ELSE
                     NEXT SENTENCE
               ELSE
                  IF VL-CTB-PREV-ACAO NOT = 0
                     MOVE 206 TO AUX1-CS-RUBRICA1
                     MOVE VL-CTB-PREV-ACAO TO AUX1-VL-RUBRICA1.

            MOVE CS-NATUR-CREDITO OF DS-CALC-ACAO-JUD TO
                 AUX1-CS-NATUR-CREDITO
            MOVE 9 TO AUX1-MEIO-PAGTO
            MOVE D2-PAGTO-ACAO-JUD TO AUX1-DT-MOV-CREDITO
            MOVE 1                 TO AUX1-ID-ORGAO-PAGADOR
            MOVE 0                 TO AUX1-SEQ-PROC
            MOVE 1                 TO AUX1-IN-CR-BLOQUEADO
            MOVE CS-UF-OL-ACAO-JUD TO AUX1-CS-UF-OL-ACAO-JUD
            MOVE CS-MUNICIPIO OF DS-CALC-ACAO-JUD
                                   TO AUX1-CS-MUNICIPIO
            MOVE NU-VARA-ACAO-JUD  TO AUX1-NU-VARA-ACAO-JUD
            MOVE NU-PROC-ACAO-JUD  TO AUX1-NU-PROC-ACAO-JUD

            IF IN-CRED-CANCELADO
               MOVE 1 TO AUX1-IN-CRED-CANCELADO
            ELSE
               MOVE 0 TO AUX1-IN-CRED-CANCELADO.

            IF VL-IR-ACAO-JUD = 0
               MOVE 1 TO AUX1-IN-CRED-ISENTO-IR
            ELSE
               MOVE 0 TO AUX1-IN-CRED-ISENTO-IR.

            MOVE ZEROS             TO WS-TAB-RUBRICAS-AUX.

            IF AUX1-CS-RUBRICA1 NOT = 0
               MOVE AUX1-VL-RUBRICA1 TO
                    WS-TB-VL-RUBRICA-AUX(AUX1-CS-RUBRICA1).

            IF AUX1-CS-RUBRICA2 NOT = 0
               MOVE AUX1-VL-RUBRICA2 TO
                    WS-TB-VL-RUBRICA-AUX(AUX1-CS-RUBRICA2).

       2540-FIM. EXIT.

       2541-ACESSA-ESPECIE.

           FIND ST-BENEF-NB AT NU-NB = NU-NB OF DS-CALC-ACAO-JUD
                ON EXCEPTION
                   IF DMSTATUS(NOTFOUND)
                      PERFORM 2541-1-ACESSA-DS-PECULIO THRU 2541-1-FIM
                      GO TO 2541-FIM
                   ELSE
                      MOVE
                   "P/SUB/CPG/001 - ERRO FIND ST-BENEF-NB - ROT 2541" TO
                      WS-MENSAGEM-ZDISPLAY
                      CALL "ZDISPLAY IN DTP/L1" USING
                                                WS-MENSAGEM-ZDISPLAY,
                                                WS-INTEIRO-ZDISPLAY
                      CALL SYSTEM DMTERMINATE.

            MOVE CS-ESPECIE OF DS-BENEFICIO TO AUX1-CS-ESPECIE
            MOVE ID-OL-MANUTENCAO OF DS-BENEFICIO TO
                 AUX1-ID-OL-MANUTENCAO.
            MOVE CS-RAMO-ATIVIDADE OF DS-BENEFICIO TO
                 AUX1-CS-RAMO-ATIVIDADE.

       2541-FIM. EXIT.

       2541-1-ACESSA-DS-PECULIO.

           FIND ST-PECULIO-NB AT NU-NB = NU-NB OF DS-CALC-ACAO-JUD
                ON EXCEPTION
                   IF DMSTATUS(NOTFOUND)
                      MOVE 99       TO AUX1-CS-ESPECIE
                      MOVE 99999999 TO AUX1-ID-OL-MANUTENCAO
                      GO TO 2541-1-FIM
                   ELSE
                      MOVE
                 "P/SUB/CPG/001 - ERRO FIND ST-PECULIO-NB - ROT 2542" TO
                      WS-MENSAGEM-ZDISPLAY
                      CALL "ZDISPLAY IN DTP/L1" USING
                                                WS-MENSAGEM-ZDISPLAY,
                                                WS-INTEIRO-ZDISPLAY
                      CALL SYSTEM DMTERMINATE.

           MOVE 68 TO AUX1-CS-ESPECIE
           MOVE ID-OL-MANUTENCAO OF DS-PECULIO TO AUX1-ID-OL-MANUTENCAO.

       2541-1-FIM. EXIT.

       2550-LE-MOV-SEAS.

           MOVE "FASE LE-MOV-SEAS  " TO W77-FASE.

           FIND NEXT DS-MOV-CRED-SEAS
                     ON EXCEPTION
                        IF DMSTATUS(NOTFOUND)
                           MOVE 1 TO AUX-IN-FIM-ARQ
                           GO TO 2550-FIM
                        ELSE
                           MOVE "FIND NEXT DS-MOV-CRED-SEAS.LIDOS:"
                             TO  WS-PARAM1-ZDISPLAY
                           MOVE W77-LIDOS-SEAS
                             TO  WS-PARAM2-ZDISPLAY
                           CALL "ZDISPLAY2 IN DTP/L1" USING
                                 WS-PARAM1-ZDISPLAY,
                                 WS-PARAM2-ZDISPLAY,
                                 WS-INTEIRO-ZDISPLAY
                           CALL  SYSTEM DMTERMINATE.

           ADD 1 TO W77-LIDOS-SEAS.

           MOVE CS-ORIGEM-CREDITO OF DS-MOV-CRED-SEAS
             TO WS-SISPG-ORIG-CRED-H.

           MOVE VL-LIQUIDO-CRED OF DS-MOV-CRED-SEAS  TO
                SISPG-VALOR-LIQUIDO
                SPGENT-VALOR-LIQUIDO.
      *
      *NAO TEM VL-CREDITO-BRUTO OF DS-MOV-CRED-SEAS.
      *****MOVE VL-BRUTO-CREDITO OF DS-MOV-CRED-SEAS  TO
           MOVE ZEROS                                 TO
                SISPG-VALOR-BRUTO
                SPGENT-VALOR-BRUTO.

      *****MOVE NU-NB OF DS-MOV-CRED-SEAS TO WS-NU-NB-REG SISPG-NB.
           MOVE ZEROS                     TO WS-NU-NB-REG SISPG-NB.

           MOVE ZEROS TO WS-SISPG-VALOR-RUB301.
           IF QT-RUBRICA OF DS-MOV-CRED-SEAS > 0
              MOVE ZEROS TO WS-TAB-RUBRICAS-AUX
                            WS-VL-RUB-301-SEM-ALT
      *NOVO   MOVE ZEROS TO SISPG-VALOR-GRUPO-RUBRICAS
              MOVE ZEROS TO SISPG-VALOR-GRUPO-RUBRICAS
                            SPGENT-VALOR-GRUPO-RUBRICAS.

              MOVE QT-RUBRICA OF DS-MOV-CRED-SEAS TO SISPG-OCOR-RUB
                                                     SPGENT-OCOR-RUB.
              MOVE ZEROS TO WS-VL-RUB-301-SEM-ALT WS-SISPG-VALOR-RUB301
                LIN-RUB301  WS-BRUTO-RUB-105  SISPG-MARCA-RUB231

              PERFORM 2796-SALVA-RUB THRU 2796-FIM
               VARYING W77-IND-RUBRICAS FROM 1 BY 1
               UNTIL W77-IND-RUBRICAS > QT-RUBRICA OF DS-MOV-CRED-SEAS.

           IF QT-RUBRICA OF DS-MOV-CRED-SEAS > 0
              MOVE LOW-VALUES TO WS-TABELA-RUBRICAS
              MOVE 0 TO W77-IND-RUBRICAS  W77-IND-TAB-RUB
              PERFORM 2762-TIRA-RUBRICA-900-MOV-SEAS THRU 2762-FIM
               UNTIL W77-IND-RUBRICAS = QT-RUBRICA OF DS-MOV-CRED-SEAS
              MOVE 0 TO W77-IND-RUBRICAS
              PERFORM 2785-LIMPA-RUBRICAS-SEAS THRU 2785-EXIT
               UNTIL W77-IND-RUBRICAS = QT-RUBRICA OF DS-MOV-CRED-SEAS
              MOVE W77-IND-TAB-RUB TO QT-RUBRICA OF DS-MOV-CRED-SEAS
              MOVE 0 TO W77-IND-RUBRICAS
              PERFORM 2764-ALTERA-UWA-MOV-SEAS THRU 2764-EXIT
                     UNTIL W77-IND-RUBRICAS = W77-IND-TAB-RUB.

           IF IN-CRED-DELETADO OF DS-MOV-CRED-SEAS
              ADD 1 TO AUX-QT-DELETADOS
              GO TO 2550-LE-MOV-SEAS.

           IF CS-ERRO OF DS-MOV-CRED-SEAS > 0
              ADD 1 TO AUX-QT-MARCA-ERRO
              GO TO 2550-LE-MOV-SEAS.

           MOVE 1 TO AUX1-IN-CR-BLOQUEADO.
           MOVE VL-LIQUIDO-CRED OF DS-MOV-CRED-SEAS
                                TO AUX1-VL-LIQUIDO-CRED.
           MOVE 1 TO AUX1-IN-CREDITO-EPU
           MOVE 22000000 TO WS-OL-MANUT
           MOVE WS-UF  TO AUX1-CS-UF
           MOVE D2-MOV-CREDITO OF DS-MOV-CRED-SEAS
                               TO AUX1-DT-MOV-CREDITO.
           MOVE D2-INI-PERIODO OF DS-MOV-CRED-SEAS
                               TO  AUX1-DT-INI-PERIODO.
           MOVE D2-FIM-PERIODO OF DS-MOV-CRED-SEAS
                               TO AUX1-DT-FIM-PERIODO.
           MOVE D2-INI-VALIDADE OF DS-MOV-CRED-SEAS
                                TO AUX1-DT-INI-VALIDADE.
           MOVE D2-FIM-VALIDADE OF DS-MOV-CRED-SEAS
                                TO AUX1-DT-FIM-VALIDADE.
           MOVE ID-BANCO        OF DS-MOV-CRED-SEAS TO AUX1-ID-BANCO
           MOVE SPACES
                                TO  AUX1-NU-CONTA-CORRENTE
           MOVE CS-PROGRAMA     OF DS-MOV-CRED-SEAS TO AUX1-CS-ESPECIE.
           MOVE D2-COMPET-CREDITO OF DS-MOV-CRED-SEAS
                                  TO AUX1-DT-COMPET-CREDITO.
           MOVE CS-ORIGEM-CREDITO OF DS-MOV-CRED-SEAS
                                  TO AUX1-CS-ORIGEM-CREDITO.
           MOVE 1 TO AUX1-IN-CRED-CART-SOC AUX1-IN-CRED-PROG-SEAS
                     AUX1-IN-CRED-ISENTO-IR

           IF IN-CRED-PIONEIRA OF DS-MOV-CRED-SEAS
              MOVE 1 TO AUX1-IN-CRED-PIONEIRA
           ELSE
              MOVE ZERO TO AUX1-IN-CRED-PIONEIRA.

           MOVE QT-RUBRICA OF DS-MOV-CRED-SEAS TO AUX1-QT-RUBRICA
           MOVE ZEROS TO AUX1-CS-RUBRICA1 AUX1-CS-RUBRICA2
                         AUX1-VL-RUBRICA1 AUX1-VL-RUBRICA2

           IF QT-RUBRICA OF DS-MOV-CRED-SEAS > 0
              IF (CS-RUBRICA OF DS-MOV-CRED-SEAS (1) > 0) AND
                 (VL-RUBRICA OF DS-MOV-CRED-SEAS (1) IS NULL)
                  MOVE "BENEFICIO COM RUBRICA ZERADA - NIT=" TO
                        WS-PARAM1-ZDISPLAY
                  MOVE  ID-NIT OF DS-MOV-CRED-SEAS TO WS-PARAM2-ZDISPLAY
                  CALL "ZDISPLAY2 IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                  SET MYSELF (STATUS) TO -1
              ELSE
                  MOVE CS-RUBRICA OF DS-MOV-CRED-SEAS (1)
                                  TO AUX1-CS-RUBRICA1
                  MOVE VL-RUBRICA OF DS-MOV-CRED-SEAS (1)
                                  TO AUX1-VL-RUBRICA1.

           IF QT-RUBRICA OF DS-MOV-CRED-SEAS > 1
              IF (CS-RUBRICA OF DS-MOV-CRED-SEAS (2) > 0) AND
                 (VL-RUBRICA OF DS-MOV-CRED-SEAS (2) IS NULL)
                  MOVE "BENEFICIO COM RUBRICA ZERADA - NIT=" TO
                        WS-PARAM1-ZDISPLAY
                  MOVE ID-NIT OF DS-MOV-CRED-SEAS TO WS-PARAM2-ZDISPLAY
                  CALL "ZDISPLAY2 IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                  SET MYSELF (STATUS) TO -1
              ELSE
                  MOVE CS-RUBRICA OF DS-MOV-CRED-SEAS (2)
                                  TO AUX1-CS-RUBRICA2
                  MOVE VL-RUBRICA OF DS-MOV-CRED-SEAS (2)
                                  TO AUX1-VL-RUBRICA2.

           MOVE CS-NATUR-CREDITO OF DS-MOV-CRED-SEAS
                                 TO AUX1-CS-NATUR-CREDITO.
           MOVE CS-MEIO-PAGTO OF DS-MOV-CRED-SEAS TO AUX1-MEIO-PAGTO.
           MOVE D2-MOV-CREDITO   OF DS-MOV-CRED-SEAS
                                 TO AUX1-DT-MOV-CREDITO.
           MOVE ID-ORGAO-PAGADOR OF DS-MOV-CRED-SEAS
                                 TO AUX1-ID-ORGAO-PAGADOR.
           MOVE 22000000         TO AUX1-ID-OL-MANUTENCAO.
           MOVE NU-SEQ-PROC      OF DS-MOV-CRED-SEAS
                                 TO AUX1-SEQ-PROC.

           IF IN-CRED-ATRASADO OF DS-MOV-CRED-SEAS
              MOVE 1 TO AUX1-IN-CRED-ATRASADO
           ELSE
              MOVE 0 TO AUX1-IN-CRED-ATRASADO.

           MOVE ID-NIT           OF DS-MOV-CRED-SEAS
                                 TO AUX1-ID-NIT.
           MOVE NU-NIT-DV1       OF DS-MOV-CRED-SEAS
                                 TO AUX1-NU-NIT-DV1.

           IF PAR-MOV-CREDITO = "CONC"
              MOVE 1 TO AUX1-CS-ROTINA-PAGTO
           ELSE
              IF PAR-MOV-CREDITO = "MANU"
                 MOVE 2 TO AUX1-CS-ROTINA-PAGTO.

       2550-FIM. EXIT.

       2600-TRATA-CREDITOS.

            MOVE "FASE TRATA CREDITOS " TO W77-FASE

            PERFORM 2610-CRITICA-CREDITO THRU 2610-FIM.

            IF AUX-IN-CREDITO-OK = 1
                COMPUTE AUX-SEQ-REG  = AUX-SEQ-REG + 1
              IF AUX1-QT-RUBRICA < 3
                  PERFORM 2620-MONTA-REG-TMP-CREDITOS THRU 2620-FIM
                  ADD AUX1-VL-LIQUIDO-CRED
                                         TO TCP-ACEITOS-VR (W77-IND-TAB)
                  ADD 1                  TO TCP-ACEITOS-QT (W77-IND-TAB)
               ELSE
                  PERFORM 2620-MONTA-REG-TMP-CREDITOS  THRU 2620-FIM
                  MOVE 2                 TO W77-IND
                  MOVE ZEROS             TO W77-IND-CP
                  PERFORM 2630-MONTA-REG-TMP-CRED-REST THRU 2630-FIM
                         UNTIL W77-IND > AUX1-QT-RUBRICA
                  ADD AUX1-VL-LIQUIDO-CRED
                                         TO TCP-ACEITOS-VR (W77-IND-TAB)
                  ADD 1                  TO TCP-ACEITOS-QT (W77-IND-TAB)
            ELSE
               ADD AUX1-VL-LIQUIDO-CRED TO TCP-REJ-VR (W77-IND-TAB)
               ADD 1                    TO TCP-REJ-QT (W77-IND-TAB)
               PERFORM 2640-MONTA-REG-REJ-CREDITOS THRU 2640-FIM.

            IF AUX-NU-CONT-CHECKPOINT = 10000000
               PERFORM 2660-ROTINA-CHECKPOINT THRU 2660-FIM.

            PERFORM 2500-DETERMINA-LEITURA   THRU 2500-FIM.

       2600-FIM.  EXIT.

       2605-POSICIONA-TAB-CREDPROC.

            ADD 1 TO W77-IND-TAB.

            IF W77-IND-TAB > 100
               MOVE "OVERFLOW TAB CREDPROC" TO WS-MENSAGEM-ZDISPLAY
               CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
               SET MYSELF (STATUS) TO -1.

            IF AUX1-DT-MOV-CREDITO  = TCP-DATA (W77-IND-TAB) OR
               TCP-DATA (W77-IND-TAB) = ZEROS
               GO TO 2605-FIM.

            GO TO 2605-POSICIONA-TAB-CREDPROC.


       2605-FIM. EXIT.

       2610-CRITICA-CREDITO.

            MOVE AUX1-NU-NB    TO AUX-NU-NB-INT.
            MOVE AUX-NU        TO AUX-NB.
            MOVE AUX-DIG       TO AUX-DIGITO.

            MOVE 9 TO COMPR.
            MOVE 1 TO AUX-IN-NB-OK.

            CALL "ZMOD11 IN DTP/L1" USING   AUX-NB
                                            COMPR
                                            AUX-DIGITO
                                     GIVING AUX-IN-NB-OK.

            IF AUX-IN-NB-OK = 1
               IF AUX-DIGITO NOT = AUX-DIG
                  MOVE 0 TO AUX-IN-NB-OK.

            IF AUX1-CS-NATUR-CREDITO NOT  = 0
               MOVE 1 TO AUX-IN-NATUR-CREDITO-OK
            ELSE
               MOVE 0 TO AUX-IN-NATUR-CREDITO-OK.

            IF AUX1-ID-BANCO NOT = 0
               MOVE 1 TO AUX-IN-BANCO-OK
            ELSE
               MOVE 0 TO AUX-IN-BANCO-OK.

            IF AUX1-CS-UF NOT = 0
               MOVE 1 TO AUX-IN-UF-OK
            ELSE
               MOVE 0 TO AUX-IN-UF-OK.

            MOVE 0    TO AUX-IN-MEIO-PAGTO-OK.

            IF PAR-MOV-CREDITO NOT = "APAG"
               PERFORM 2612-TESTA-TAB-MEIO-PAGTO THRU 2612-FIM
            ELSE
               IF AUX1-MEIO-PAGTO = 9
                  MOVE 1 TO AUX-IN-MEIO-PAGTO-OK
               ELSE
                  MOVE 0 TO AUX-IN-MEIO-PAGTO-OK.

            PERFORM 2613-CRITICA-PERIODO      THRU 2613-FIM.

            PERFORM 2614-CRITICA-VALIDADE     THRU 2614-FIM.

            MOVE 1 TO AUX-IN-VALOR-OK

            IF PAR-MOV-CREDITO = "APAG"
               COMPUTE WS-DESCONTO = VL-IR-ACAO-JUD + VL-CTB-PREV-ACAO
               IF VL-BENEF-ACAO-JUD > WS-DESCONTO
                  MOVE 1 TO AUX-IN-VALOR-OK
               ELSE
                  MOVE 0 TO AUX-IN-VALOR-OK.

           MOVE AUX1-ID-NIT     TO AUX-NIT
           MOVE AUX1-NU-NIT-DV1 TO AUX-DIGITO
           MOVE 10              TO COMPR
           MOVE 1               TO AUX-IN-NIT-OK

           CALL "ZMOD11 IN DTP/L1" USING  AUX-NIT
                                          COMPR
                                          AUX-DIGITO
                                   GIVING AUX-IN-NIT-OK.

           IF AUX-IN-NIT-OK = 1
              IF AUX-DIGITO NOT = AUX1-NU-NIT-DV1
                 MOVE 0 TO AUX-IN-NIT-OK.

           PERFORM 2615-ACUMULA-QT-ERRO      THRU 2615-FIM.

       2610-FIM. EXIT.

       2612-TESTA-TAB-MEIO-PAGTO.

            ADD 1 TO WS-IND.

            IF (WS-IND > 08 OR TAB-CS-MEIO-PAGTO(WS-IND) = 0)
               MOVE 0 TO WS-IND
               MOVE 0 TO AUX-IN-MEIO-PAGTO-OK
               GO TO 2612-FIM.

            IF (AUX1-MEIO-PAGTO = TAB-CS-MEIO-PAGTO(WS-IND))
               MOVE 1 TO AUX-IN-MEIO-PAGTO-OK
               MOVE 0 TO WS-IND
            ELSE
               GO TO 2612-TESTA-TAB-MEIO-PAGTO.

       2612-FIM. EXIT.

       2613-CRITICA-PERIODO.

            MOVE 0 TO AUX-IN-PERIODO-OK.
            MOVE AUX1-DT-INI-PERIODO TO WS-DT-AUX2-AAMMDD.
            MOVE WS-DT-AUX2-ANO      TO WS-DT-AUX1-ANO.
            MOVE WS-DT-AUX2-MES      TO WS-DT-AUX1-MES.
            MOVE WS-DT-AUX2-DIA      TO WS-DT-AUX1-DIA.
            MOVE WS-DT-AUX1-DDMMAA   TO AUX-DT-INI-PERIODO.

            CALL "ZDATVL4 IN DTP/L1"  USING  AUX-DT-INI-PERIODO
                                     GIVING AUX-IN-INI-PERIODO-OK.

            MOVE AUX1-DT-FIM-PERIODO TO WS-DT-AUX2-AAMMDD.
            MOVE WS-DT-AUX2-ANO      TO WS-DT-AUX1-ANO.
            MOVE WS-DT-AUX2-MES      TO WS-DT-AUX1-MES.
            MOVE WS-DT-AUX2-DIA      TO WS-DT-AUX1-DIA.
            MOVE WS-DT-AUX1-DDMMAA   TO AUX-DT-FIM-PERIODO.

            CALL "ZDATVL4 IN DTP/L1"  USING  AUX-DT-FIM-PERIODO
                                     GIVING AUX-IN-FIM-PERIODO-OK.

            IF (AUX1-DT-INI-PERIODO NOT > AUX1-DT-FIM-PERIODO) AND
               (AUX-IN-INI-PERIODO-OK = 1)        AND
               (AUX-IN-FIM-PERIODO-OK = 1)
               MOVE 1 TO AUX-IN-PERIODO-OK.

       2613-FIM. EXIT.

       2614-CRITICA-VALIDADE.

            MOVE 0 TO AUX-IN-VALIDADE-OK.
            MOVE AUX1-DT-INI-VALIDADE TO WS-DT-AUX2-AAMMDD.
            MOVE WS-DT-AUX2-ANO       TO WS-DT-AUX1-ANO.
            MOVE WS-DT-AUX2-MES       TO WS-DT-AUX1-MES.
            MOVE WS-DT-AUX2-DIA       TO WS-DT-AUX1-DIA.
            MOVE WS-DT-AUX1-DDMMAA    TO AUX-DT-INI-VALIDADE.

            CALL "ZDATVL4 IN DTP/L1" USING  AUX-DT-INI-VALIDADE
                                    GIVING AUX-IN-INI-VALIDADE-OK.

            MOVE AUX1-DT-FIM-VALIDADE TO WS-DT-AUX2-AAMMDD.
            MOVE WS-DT-AUX2-ANO       TO WS-DT-AUX1-ANO.
            MOVE WS-DT-AUX2-MES       TO WS-DT-AUX1-MES.
            MOVE WS-DT-AUX2-DIA       TO WS-DT-AUX1-DIA.
            MOVE WS-DT-AUX1-DDMMAA    TO AUX-DT-FIM-VALIDADE.

            CALL "ZDATVL4 IN DTP/L1" USING  AUX-DT-FIM-VALIDADE
                                    GIVING AUX-IN-FIM-VALIDADE-OK.

            IF (AUX1-DT-INI-VALIDADE NOT > AUX1-DT-FIM-VALIDADE) AND
               (AUX-IN-INI-VALIDADE-OK = 1)        AND
               (AUX-IN-FIM-VALIDADE-OK = 1)
               MOVE 1 TO AUX-IN-VALIDADE-OK.
      ******* ALTERACAO TEMPORARIA SOLICITADA PELO NELSON ****18/08/93**
      *        MOVE 1 TO AUX-IN-VALIDADE-OK.
      ******************************************************************

       2614-FIM. EXIT.

       2615-ACUMULA-QT-ERRO.

            MOVE 1 TO AUX-IN-CREDITO-OK.

            IF (AUX-IN-NB-OK            = 0 OR
                AUX-IN-NATUR-CREDITO-OK = 0 OR
                AUX-IN-PERIODO-OK       = 0 OR
                AUX-IN-VALIDADE-OK      = 0 OR
                AUX-IN-MEIO-PAGTO-OK    = 0 OR
                AUX-IN-BANCO-OK         = 0 OR
                AUX-IN-UF-OK            = 0 OR
                AUX-IN-VALOR-OK         = 0 OR
                AUX-IN-NIT-OK           = 0)
                MOVE 0 TO AUX-IN-CREDITO-OK
                COMPUTE AUX-QT-ERRO = AUX-QT-ERRO + 1.

            IF AUX-IN-NB-OK = 0
               COMPUTE AUX-QT-ERRO-NB = AUX-QT-ERRO-NB + 1.

            IF AUX-IN-NATUR-CREDITO-OK = 0
               COMPUTE AUX-QT-ERRO-NATUR-CREDITO =
                       AUX-QT-ERRO-NATUR-CREDITO  + 1.

            IF AUX-IN-PERIODO-OK = 0
               COMPUTE AUX-QT-ERRO-PERIODO = AUX-QT-ERRO-PERIODO + 1.

            IF AUX-IN-VALIDADE-OK = 0
               COMPUTE AUX-QT-ERRO-VALIDADE = AUX-QT-ERRO-VALIDADE + 1.

            IF AUX-IN-MEIO-PAGTO-OK = 0
               COMPUTE AUX-QT-ERRO-PAGTO = AUX-QT-ERRO-PAGTO + 1.

            IF AUX-IN-BANCO-OK = 0
               COMPUTE AUX-QT-ERRO-BANCO = AUX-QT-ERRO-BANCO + 1.

            IF AUX-IN-UF-OK = 0
               COMPUTE AUX-QT-ERRO-UF = AUX-QT-ERRO-UF + 1.

            IF AUX-IN-VALOR-OK = 0
               COMPUTE AUX-QT-ERRO-VALOR = AUX-QT-ERRO-VALOR + 1.

            IF AUX-IN-NIT-OK = 0
               COMPUTE AUX-QT-ERRO-NIT = AUX-QT-ERRO-NIT + 1.

       2615-FIM. EXIT.

       2620-MONTA-REG-TMP-CREDITOS.

            MOVE "FASE MONTA-REG-TMP-CREDITOS " TO W77-FASE.

            MOVE ALL @00@ TO CP01-REG1.
            MOVE AUX1-ID-PREGAO         TO CP01-ID-PREGAO.
            MOVE AUX1-ID-LOTE-PREGAO    TO CP01-ID-LOTE-PREGAO
            MOVE AUX1-DT-MOV-CREDITO    TO CP01-DT-MOV-CREDITO.
            MOVE AUX1-NU-NB             TO CP01-NU-NB.
            MOVE AUX1-CS-NATUR-CREDITO  TO CP01-CS-NATUR-CREDITO.
            MOVE AUX1-DT-FIM-PERIODO    TO CP01-DT-FIM-PERIODO.
            MOVE AUX1-DT-INI-PERIODO    TO CP01-DT-INI-PERIODO.
            MOVE AUX1-ID-BANCO          TO CP01-ID-BANCO.
            MOVE AUX1-NU-CONTA-CORRENTE TO CP01-NU-CONTA-CORRENTE.
            MOVE AUX-SEQ-REG            TO CP01-SEQ-REG.
            MOVE AUX1-ID-ORGAO-PAGADOR  TO CP01-ID-ORGAO-PAGADOR.
            MOVE AUX1-CS-UF             TO CP01-CS-UF-OP.
            MOVE AUX1-MEIO-PAGTO        TO CP01-CS-MEIO-PAGTO.
            MOVE AUX1-CS-ESPECIE        TO CP01-CS-ESPECIE.
            MOVE AUX1-DT-COMPET-CREDITO TO CP01-DT-COMPET-CREDITO.
            MOVE AUX1-DT-FIM-VALIDADE   TO CP01-DT-FIM-VALIDADE.
            MOVE AUX1-DT-INI-VALIDADE   TO CP01-DT-INI-VALIDADE.
            MOVE AUX1-CS-ORIGEM-CREDITO TO CP01-CS-ORIGEM-CREDITO.
            MOVE AUX1-QT-RUBRICA        TO CP01-QT-RUBRICA.
            MOVE AUX1-CS-RUBRICA1       TO CP01-CS-RUBRICA1.
            MOVE AUX1-CS-RUBRICA2       TO CP01-CS-RUBRICA2.
            MOVE AUX1-IN-CR-BLOQUEADO   TO CP01-IN-CR-BLOQUEADO.
            MOVE AUX1-IN-CRED-CART-SOC  TO CP01-IN-CRED-CART-SOC.
            MOVE AUX1-IN-CRED-PIONEIRA  TO CP01-IN-CRED-PIONEIRA.
            MOVE AUX1-IN-CRED-PROG-SEAS TO CP01-IN-CRED-PROG-SEAS.
      *
      *     A PARTIR DA GERACAO DO ARQUIVO DSUBCPG058 (MACICA 08/2000),
      *     ESTE CAMPO TORNA-SE DESNECESSARIO.
      *
      *     IF CP01-CS-ESPECIE = 87 OR 88
      *        MOVE 3                   TO CP01-CS-ESP-LOAS
      *     ELSE
      *        MOVE 1                   TO CP01-CS-ESP-LOAS.
      *
      * ALTERACAO TEMPORARIA DEVIDO A TROCA DE MOEDA
      * INICIO DA ALTERACAO
      *
      *
      *   IF PAR-MOV-CREDITO  =  "CONC"      AND
      *      AUX1-SEQ-PROC    >  2           AND
      *      AUX1-DT-DDB      <  930810
      *      COMPUTE AUX1-VL-LIQUIDO-CRED = AUX1-VL-LIQUIDO-CRED / 1000
      *      COMPUTE AUX1-VL-RUBRICA1     = AUX1-VL-RUBRICA1     / 1000
      *      COMPUTE AUX1-VL-RUBRICA2     = AUX1-VL-RUBRICA2     / 1000
      *      MOVE 930802 TO AUX1-DT-MOV-CREDITO
      *      MOVE AUX1-DT-MOV-CREDITO TO CP01-DT-MOV-CREDITO.
      *
      * FIM DA ALTERACAO

            MOVE AUX1-VL-LIQUIDO-CRED   TO CP01-VL-LIQUIDO-CRED.
            MOVE AUX1-IN-CREDITO-EPU    TO CP01-IN-CREDITO-EPU.
            MOVE AUX1-VL-RUBRICA1       TO CP01-VL-RUBRICA1.
            MOVE AUX1-VL-RUBRICA2       TO CP01-VL-RUBRICA2.
            MOVE AUX1-IN-CRED-ISENTO-IR TO CP01-IN-CRED-ISENTO-IR.
            MOVE AUX1-IN-CRED-CANCELADO TO CP01-IN-CRED-CANCELADO.
            MOVE AUX1-ID-NIT            TO CP01-ID-NIT
            MOVE AUX1-NU-NIT-DV1        TO CP01-NU-NIT-DV1
            MOVE AUX1-CS-ROTINA-PAGTO   TO CP01-CS-ROTINA-PAGTO.
            MOVE AUX1-IN-ADIANT-13-SAL  TO CP01-IN-ADIANT-13-SAL.
            MOVE AUX1-IN-BANCO-POSTAL   TO CP01-IN-BANCO-POSTAL.
            MOVE AUX1-IN-IR-ADIAN-13SAL TO CP01-IN-IR-ADIAN-13SAL.

            IF AUX1-QT-RUBRICA < 3
               MOVE 0 TO CP01-IN-RUBRICA-CREDITO
            ELSE
               MOVE 1 TO CP01-IN-RUBRICA-CREDITO.

           MOVE AUX1-IN-RRA-EXERC-ANT    TO CP01-IN-RRA-EXERC-ANT.
           MOVE AUX1-IN-CREDITO-RRA      TO CP01-IN-CREDITO-RRA.
           MOVE AUX1-IN-RRA-PARCELADO    TO CP01-IN-RRA-PARCELADO.
           MOVE AUX1-IN-CRED-SUBSTIT     TO CP01-IN-CRED-SUBSTIT.
           MOVE AUX1-IN-RRA-ATRAS-CONC   TO CP01-IN-RRA-ATRAS-CONC.
           MOVE AUX1-IN-CRED-ATRASADO    TO CP01-IN-CRED-ATRASADO.
           MOVE AUX1-ID-CALAMIDADE       TO CP01-ID-CALAMIDADE.
           MOVE AUX1-CS-ORIGEM-ORCAMENTO TO CP01-CS-ORIGEM-ORCAMENTO.
      **
           MOVE AUX1-NU-CPF-TITULAR      TO CP01-NU-CPF-TITULAR.
           MOVE AUX1-NU-NIT-TITULAR      TO CP01-NU-NIT-TITULAR.
      **
      *GATILHO
      *     MOVE AUX1-DT-COMP-ATU-VL-CR   TO CP01-DT-COMP-ATU-VL-CR.
      *FIM-GATILHO
           IF PAR-SEAS = "    "
              AND (PAR-MOV-CREDITO = "MANU" OR PAR-MOV-CREDITO = "CONV")
              AND NAO-E-CREDITO-DE-CALAMIDADE
                IF IN-DESC-EMPREST OF DS-MOV-CRED-MANUT
                   MOVE 0 TO WS-CONT-CONSIG WS-ACHOU-NAO-DESC-DIF-97-98
                           WS-ACHOU-FIM-DESC-OK
                   PERFORM 2620-MONTA-REG-CONSIG THRU 2620-FIM-CONSIG
                         VARYING W77-IND FROM 1 BY 1 UNTIL W77-IND >
                         QT-CONSIGNACAO OF DS-MOV-CRED-MANUT
                 IF PAR-MOV-CREDITO = "MANU" AND WS-CONT-CONSIG = 0
                    AND IN-DESC-EMPREST OF DS-MOV-CRED-MANUT
                   IF CS-ORIGEM-CREDITO OF DS-MOV-CRED-MANUT = 2
                    MOVE "NAO EXISTE EMPRESTIMO P/O NB" TO
                                                     WS-PARAM1-ZDISPLAY
                    MOVE NU-NB OF DS-MOV-CRED-MANUT TO
                                                     WS-PARAM2-ZDISPLAY
                    CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                                     WS-PARAM2-ZDISPLAY,
                                                     WS-INTEIRO-ZDISPLAY
                    MOVE 1 TO WS-INCONS-CONSIG
                    MOVE 0 TO CP01-IN-DESC-EMPREST
                   ELSE
                    MOVE 0 TO CP01-IN-DESC-EMPREST
                 ELSE
                    IF (WS-ACHOU-NAO-DESC-DIF-97-98 = 1) AND
                       (WS-ACHOU-FIM-DESC-OK = 1)
                       MOVE 1 TO CP01-IN-DESC-EMPREST
                    ELSE
                       MOVE 0 TO CP01-IN-DESC-EMPREST.

           IF PAR-MOV-CREDITO = "ALTE"
              IF IN-CRED-ESPECIAL OF DS-MOV-CRED-ALTER
                 MOVE 1 TO CP01-IN-CRED-ESPECIAL
              ELSE
                 MOVE 0 TO CP01-IN-CRED-ESPECIAL.

           IF PAR-MOV-CREDITO = "ALTE"
              IF IN-VAGO-01 OF DS-MOV-CRED-ALTER
                 MOVE 1 TO CP01-IN-CRE-CALAMIDADE
              ELSE
                 MOVE 0 TO CP01-IN-CRE-CALAMIDADE.

           IF PAR-SEAS = "    " AND PAR-MOV-CREDITO = "MANU"
              PERFORM 2620-BLOQUEIO THRU 2620-FIM-BLOQUEIO.

            WRITE CP01-REG1.
            ADD 1 TO W77-GRAVADOS.

            IF PAR-SEAS = "SEAS"
               NEXT SENTENCE
            ELSE
               IF (PAR-MOV-CREDITO = "ALTE")  OR
                  (PAR-MOV-CREDITO = "CONC"   AND
                   CS-ORIGEM-CREDITO OF DS-MOV-CRED-CONC = 4)  OR
                  (PAR-MOV-CREDITO = "MANU"    AND
                   NAO-E-CREDITO-DE-CALAMIDADE AND
                   CS-ORIGEM-CREDITO OF DS-MOV-CRED-MANUT = 4) OR
                  (PAR-MOV-CREDITO = "CONV"   AND
                   CS-ORIGEM-CREDITO OF DS-MOV-CRED-MANUT = 4)
                   PERFORM  2625-MONTA-REG-ALTE THRU 2625-FIM.

           IF PAR-MOV-CREDITO = "APAG"
              PERFORM 2629-MONTA-REG-AP THRU 2629-FIM.

           PERFORM 2631-TOT-CONTAB THRU 2631-FIM.
      *
      *    VOU GRAVAR O(S) REGISTRO(S) DO ARQUIVO DSUBCPG058 NESTE PONTO
      *    PORQUE NESTE MOMENTO A ROTINA 2631 FOI UTILIZADA PARA TODOS O
      *    OS VALORES. A PROXIMA CHAMADA DA ROTINA 2631 EH APENAS PARA A
      *    RUBRICA 105 SER CONTABILIZADA EM SEPARADO, POR CAUSA DA  CON-
      *    TABILIDADE E NAO INTERESSA AO SCF.

      *Pagamentos do Mercosul.   Jose Carlos Fonseca 20/09/2012 QUI

      *Nova classificacao para atender exigencias do leilao da folha.
      *-------------------------------------------------------------

      *Assume os valores como sendo orcamento INSS e nao eh rede-ect.

           MOVE 0 TO SISPG-IN-CRED-EPU
                     SPGENT-IN-CRED-EPU.
           MOVE 1 TO SISPG-CS-ORIG-ORC
                     SPGENT-CS-ORIG-ORC.
           MOVE 0 TO SISPG-CS-URBANO-RURAL
                     SPGENT-CS-URBANO-RURAL.
      *Identifica a origem orcamentaria e se eh rede-ect-misto.

           IF ( ESPECIES-LOAS ) OR  (ESPECIES-RMV)
      *Eh LOAS.
               MOVE 3 TO SISPG-CS-ORIG-ORC
                         SPGENT-CS-ORIG-ORC
           ELSE
               IF  AUX1-IN-CREDITO-EPU = 1
      *Eh EPU.
                  MOVE 2 TO SISPG-CS-ORIG-ORC
                            SPGENT-CS-ORIG-ORC
                  IF AUX1-CS-TRAT-RFFSA
      *Eh REDE.
                    MOVE 1 TO SISPG-CS-ORIG-ORC
                              SPGENT-CS-ORIG-ORC
                    MOVE 1 TO SISPG-IN-CRED-EPU
                               SPGENT-IN-CRED-EPU
                  ELSE
                     IF AUX1-CS-TRAT-ECT
      *Eh ECT
                        MOVE 1 TO SISPG-CS-ORIG-ORC
                                  SPGENT-CS-ORIG-ORC
                        MOVE 2 TO SISPG-IN-CRED-EPU
                                  SPGENT-IN-CRED-EPU
                     ELSE
      *                  IF AUX1-CS-TRAT-MISTO
      *Eh Tratamento MISTO ou possui complemento EPU.
                           MOVE 1 TO SISPG-CS-ORIG-ORC
                                     SPGENT-CS-ORIG-ORC
                           MOVE 3 TO SISPG-IN-CRED-EPU
                                     SPGENT-IN-CRED-EPU.

           IF        ESPECIES-EPU
              MOVE 2 TO SISPG-CS-ORIG-ORC
                        SPGENT-CS-ORIG-ORC
              MOVE 4 TO SISPG-IN-CRED-EPU
                        SPGENT-IN-CRED-EPU.

      *Em 22/10/2019, tratamento 82 e especie 60, ZIKA. Tipo EPU=5.
           IF CP01-CS-ESPECIE = 60
              AND AUX1-CS-TRAT-ZIKA
              MOVE 2 TO SISPG-CS-ORIG-ORC
                        SPGENT-CS-ORIG-ORC
              MOVE 5 TO SISPG-IN-CRED-EPU
                        SPGENT-IN-CRED-EPU.

      *Pagamentos do Mercosul.
           IF CP01-CS-ESPECIE = 17
               MOVE 4 TO SISPG-CS-ORIG-ORC
                        SPGENT-CS-ORIG-ORC
              MOVE 0 TO SISPG-IN-CRED-EPU
                        SPGENT-IN-CRED-EPU.

      *Verifica se eh urbano ou rural.
           PERFORM 4600-VERIFICA-URB-RURAL THRU
                       4600-FIM-VERIFICA-URB-RURAL.

           IF SISPG-CS-ORIG-ORC EQUAL 1
              IF AUX1-RURAL
                 MOVE 2 TO SISPG-CS-URBANO-RURAL
                           SPGENT-CS-URBANO-RURAL
              ELSE
                 MOVE 1 TO SISPG-CS-URBANO-RURAL
                           SPGENT-CS-URBANO-RURAL.

      *-----------------------------------------------------------*

      *------------------------------------------------------------*
      *Rotina acrescentada devido ao novo SCF.

           PERFORM 2620-PEGA-CAMPOS-FIXO THRU
                       2620-PEGA-CAMPOS-FIXO-EXIT.

           PERFORM 9990-VERIFICA-ENT-CLASSE THRU
                   9990-FIM-VERIFICA-ENT-CLASSE
                   VARYING WS-IND-ENT FROM 1 BY 1
                   UNTIL WS-IND-ENT > 39.

      * Em junho de 2013 o INSS pediu para gravar o regiistro no arquivo
      * D/SUB/CPG/058/SISPG/301 com o registro da rubrica 301 quando for
      * maior que zeros.

      *     IF  WS-SISPG-VALOR-RUB301 > ZEROS
      *         AND NOT (ESPECIES-LOAS OR ESPECIES-RMV)
      *         WRITE REG-058-SISPG-301  FROM DSUBCPG058-REG-SISPG
      *         ADD 1 TO WS-GRAV-SISPG-RUB301.

           IF PAR-SEAS = SPACES
              PERFORM 2639-MONTA-REG-IR THRU 2639-FIM-MONTA-REG-IR.

           IF PAR-SEAS = SPACES
              IF WS-VL-RUB-301-SEM-ALT > ZEROS
                 PERFORM 2639-II-GRAVA-EPU-REL
                    THRU 2639-II-FIM.
      *
           IF WS-BRUTO-RUB-105 NOT = ZEROS
              MOVE 73               TO AUX1-CS-ESPECIE
              MOVE ZEROS            TO WS-TAB-RUBRICAS-AUX
              MOVE WS-BRUTO-RUB-105 TO WS-TB-VL-RUBRICA-AUX(105)
              MOVE ZEROS            TO WS-BRUTO-RUB-105
              PERFORM 2631-TOT-CONTAB THRU 2631-FIM.

       2620-FIM.
           EXIT.
       2620-PEGA-CAMPOS-FIXO.

           MOVE 2                           TO   SISPG-ID-REG
                                                 SPGENT-ID-REG.
           MOVE AUX1-ID-BANCO             TO   SISPG-ID-BANCO
                                                 SPGENT-ID-BANCO.

      *Pagamentos do Mercosul.   Jose Carlos Fonseca 20/09/2012 QUI
           MOVE AUX1-ID-PAIS              TO   SISPG-ID-PAIS
                                                 SPGENT-ID-PAIS.

           MOVE AUX1-IN-CRED-PIONEIRA     TO   SISPG-IN-CRED-PIONEIRA
                                               SPGENT-IN-CRED-PIONEIRA.
           MOVE AUX1-MEIO-PAGTO        TO   SISPG-CS-MEIO-PAGTO
                                                SPGENT-CS-MEIO-PAGTO.
           MOVE AUX1-DT-INI-VALIDADE      TO   SISPG-DT-INI-VALIDADE
                                               SPGENT-DT-INI-VALIDADE.

           MOVE 1                           TO   SISPG-QUANT-CREDITOS
                                               SPGENT-QUANT-CREDITOS.

           IF NAO-E-CREDITO-DE-CALAMIDADE
              IF SISPG-MARCA-RUB231 = 1
                 PERFORM 5000-BUSCA-CALAM THRU 5000-FIM-BUSCA-CALAM
                 MOVE ZEROS TO SISPG-MARCA-RUB231
              ELSE
                 MOVE ZEROS TO WS-SISPG-COD-CALAMIDADE.

           MOVE WS-SISPG-COD-CALAMIDADE     TO   SISPG-COD-CALAMIDADE
                                                 SPGENT-COD-CALAMIDADE.
           MOVE AUX1-IN-ADIANT-13-SAL       TO  SISPG-IN-ADIANTE-13-SAL
                                              SPGENT-IN-ADIANTE-13-SAL.
           MOVE AUX1-CS-ESPECIE           TO   SISPG-CS-ESPECIE
                                                 SPGENT-CS-ESPECIE.
           MOVE ZEROS TO SPGENT-ENT-CLASS
                         SISPG-ENT-CLASS.

           MOVE ZEROS TO SPGENT-QTD-ENT-CLASS
                         SISPG-QTD-ENT-CLASS.

           IF SPGENT-VALOR-RUB267 > ZEROS
      *        MOVE 1 TO AUX-IN-FIM-ARQ
              ADD 1 TO WS-GRAV-SISPG-RUB267.

           IF SPGENT-VALOR-RUB273 > ZEROS
              ADD 1 TO WS-GRAV-SISPG-RUB273.

           IF SPGENT-VALOR-RUB274 > ZEROS
      *        MOVE 1 TO AUX-IN-FIM-ARQ
      *         DISPLAY "R274: "SPGENT-NB, SISPG-VALOR-RUB274
              ADD 1 TO WS-GRAV-SPGENT-RUB274.

           IF SISPG-VALOR-RUB275 > ZEROS
      *        DISPLAY "R275: "SISPG-NB,SISPG-VALOR-RUB275
              ADD 1 TO WS-GRAV-SPGENT-RUB275.

           WRITE REG-058-SISPG FROM DSUBCPG058-REG-SISPG.

           WRITE REG-058-SPGENT FROM DSUBCPG058-REG-SPGENT.

           ADD 1 TO WS-GRAV-SISPG.

           COMPUTE WS-SISPG-TOT-VL-LIQ-T = WS-SISPG-TOT-VL-LIQ-T
                                         + SISPG-VALOR-LIQUIDO.

           ADD 1 TO WS-SISPG-QTD-REG-T.

       2620-PEGA-CAMPOS-FIXO-EXIT.
           EXIT.
       2620-MONTA-REG-CONSIG.
      ******************************************************************
           IF CS-ORIGEM-CREDITO OF DS-MOV-CRED-MANUT NOT = 2
              MOVE "CREDITO NAO TEM ORIGEM = 2" TO WS-PARAM1-ZDISPLAY
              MOVE  NU-NB OF DS-MOV-CRED-MANUT  TO WS-PARAM2-ZDISPLAY
              CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                    WS-PARAM2-ZDISPLAY, WS-INTEIRO-ZDISPLAY
              GO TO 2620-FIM-CONSIG.
      ******************************************************************
           IF CS-CONSIGNACAO OF DS-MOV-CRED-MANUT(W77-IND) NOT = 98 AND
                             77 AND 71 AND 99
              GO TO 2620-FIM-CONSIG.

      *Monta consignacoes que serao excluidas.Em 02/09/2021.
           IF W77-IND = 1
              MOVE 0 TO WS-ACHOU-CONSIG
              MOVE 0 TO WS-FIM-CONSIG
              MOVE 00 TO WS-IN-EXC-CONSIG
              SET SS-CONSIG-EMPREST TO BEGINNING
              PERFORM 8000-BUSCA-CONSIG-EXC THRU
              8000-FIM-BUSCA-CONSIG-EXC
                   UNTIL WS-FIM-CONSIG = 1.

           ADD 1 TO WS-CONT-CONSIG

           IF CS-NAO-DESCONTO OF DS-MOV-CRED-MANUT(W77-IND) = 97 OR 98
              GO TO 2620-FIM-CONSIG.

           MOVE 1 TO WS-ACHOU-NAO-DESC-DIF-97-98.

           FIND SS-CONSIG-EMPREST AT
                NU-NB           = NU-NB OF DS-MOV-CRED-MANUT AND
                ID-EMPRESTIMO   = ID-EMPRESTIMO OF
                                  DS-MOV-CRED-MANUT(W77-IND) AND
                D2-INI-DESCONTO = D2-INI-DESCONTO OF
                                  DS-MOV-CRED-MANUT(W77-IND)
                ON EXCEPTION
                ADD 1 TO WS-CONTA-NAO-CONSIG
                PERFORM 11500-MOSTRA-CONSIG THRU 11500-FIM-MOSTRA-CONSIG
                GO 2620-FIM-CONSIG.

           IF D2-FIM-DESCONTO NOT = ZEROS
             IF D2-FIM-DESCONTO < D2-COMPET-CREDITO OF DS-MOV-CRED-MANUT
                ADD 1 TO W77-FIM-MAIOR-CRED
                IF W77-FIM-MAIOR-CRED < 10000
                   MOVE ID-EMPRESTIMO OF DS-MOV-CRED-MANUT(W77-IND) TO
                                                     AUX1-ID-EMPRESTIMO
                   MOVE SPACES TO WS-MENSAGEM-ZDISPLAY
                   STRING "D2-FIM-DESCONTO < D2-COMPET-CREDITO - NB ",
                           AUX1-NU-NB," ID-EMP.: ",AUX1-ID-EMPRESTIMO
                           DELIMITED BY SIZE
                           INTO WS-MENSAGEM-ZDISPLAY
                           CALL "ZDISPLAY IN DTP/L1" USING
                           WS-MENSAGEM-ZDISPLAY,
                           WS-INTEIRO-ZDISPLAY
                   GO 2620-FIM-CONSIG.

           MOVE 1 TO WS-ACHOU-FIM-DESC-OK

           MOVE ALL @00@ TO REG-DSUBCPG081
           MOVE CP01-DT-MOV-CREDITO    TO CPG081-DT-MOV-CREDITO
           MOVE CP01-NU-NB             TO CPG081-NU-NB
           MOVE CP01-CS-NATUR-CREDITO  TO CPG081-CS-NATUR-CREDITO
           MOVE CP01-DT-FIM-PERIODO    TO CPG081-DT-FIM-PERIODO
           MOVE CP01-DT-INI-PERIODO    TO CPG081-DT-INI-PERIODO
           MOVE CP01-ID-NIT            TO CPG081-ID-NIT
           MOVE CP01-SEQ-REG           TO CPG081-SEQ-REG
           MOVE ID-EMPRESTIMO     OF DS-MOV-CRED-MANUT(W77-IND)
                                       TO CPG081-ID-EMPRESTIMO
           MOVE VL-CONSIGNACAO    OF DS-MOV-CRED-MANUT(W77-IND)
                                       TO CPG081-VL-PARCELA-CONSIG
           MOVE NU-PARCELA-CONSIG OF DS-MOV-CRED-MANUT(W77-IND)
                                       TO CPG081-NU-PARCELA-CONSIG
           MOVE QT-TOT-PAR-CONSIG OF DS-MOV-CRED-MANUT(W77-IND)
                                       TO CPG081-QT-TOT-PAR-CONSIG
           MOVE CS-NAO-DESCONTO   OF DS-MOV-CRED-MANUT(W77-IND)
                                       TO CPG081-CS-NAO-DESCONTO
           MOVE ID-BANCO-EMPREST  OF DS-MOV-CRED-MANUT(W77-IND)
                                       TO CPG081-ID-BANCO-EMPREST
           MOVE D2-INI-DESCONTO   OF DS-MOV-CRED-MANUT(W77-IND)
                                       TO CPG081-D2-INI-DESCONTO
           MOVE CS-CONSIGNACAO    OF DS-MOV-CRED-MANUT(W77-IND)
                                       TO CPG081-CS-CONSIGNACAO

           MOVE ID-BANCO-SISPAG OF DS-CONSIGNACAO TO CPG081-ID-BCO-GLOSA

           WRITE REG-DSUBCPG081.

       2620-FIM-CONSIG. EXIT.

       2620-BLOQUEIO.

           IF E-CREDITO-DE-CALAMIDADE
              MOVE 8 TO CP01-IN-BLOQUEIO
              GO TO 2620-FIM-BLOQUEIO.

           IF CS-ORIG-BLOQUEIO OF DS-MOV-CRED-MANUT = 9
              MOVE 9 TO CP01-IN-BLOQUEIO
              GO TO 2620-FIM-BLOQUEIO.

           FIND SS-BLOQUEIO-CENSO AT NU-NB = NU-NB OF DS-MOV-CRED-MANUT
                ON EXCEPTION
                   IF DMSTATUS(NOTFOUND)
                      MOVE 0 TO CP01-IN-BLOQUEIO
                      GO TO 2620-FIM-BLOQUEIO
                   ELSE
                      MOVE "ERRO FIND SS-BLOQUEIO-CENSO-CENSO - NB"
                        TO  WS-PARAM1-ZDISPLAY
                      MOVE NU-NB OF DS-MOV-CRED-MANUT
                        TO  WS-PARAM2-ZDISPLAY
                      CALL "ZDISPLAY2 IN DTP/L1" USING
                            WS-PARAM1-ZDISPLAY,
                            WS-PARAM2-ZDISPLAY,
                            WS-INTEIRO-ZDISPLAY
                      CALL  SYSTEM DMTERMINATE.

           MOVE 0 TO W77-BANCO-OK
           MOVE ID-BANCO OF DS-MOV-CRED-MANUT TO WS-PERGUNTA

           CALL "P/SUB/LIB/BANCO/TPBLOQUEIO" USING WS-PERGUNTA
                                                   WS-RESPOSTA

           MOVE WS-RESPOSTA TO W77-BANCO-OK-X
           MOVE 0 TO W77-MEIO-OK

           PERFORM 2620-VERIFICA-MEIO-PAGTO THRU
                   2620-FIM-VERIFICA-MEIO-PAGTO

           IF (W77-MEIO-OK = 0) OR
              (W77-BANCO-OK NOT = 1) OR
              (D2-FIM-VALIDADE OF DS-MOV-CRED-MANUT NOT > WS-DATA-1) OR
              (VL-LIQUIDO-CRED OF DS-MOV-CRED-MANUT = 0)
               MOVE "CREDITO DE NB NAO BLOQUEADO - NB"
                                               TO WS-PARAM1-ZDISPLAY
               MOVE NU-NB OF DS-MOV-CRED-MANUT TO WS-PARAM2-ZDISPLAY
               CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                                WS-PARAM2-ZDISPLAY,
                                                WS-INTEIRO-ZDISPLAY
               MOVE 0 TO CP01-IN-BLOQUEIO
               GO TO 2620-FIM-BLOQUEIO.

           MOVE 1 TO WS-TEM-MOV.

           SET ST-CENSO-BEN-REC TO BEGINNING.

           PERFORM 2620-PESQ-MOV THRU 2620-FIM-PESQ-MOV.

           IF WS-TEM-MOV = 1
              IF CS-RECADAST-BENEF = 2 OR 3 OR 5
                 MOVE 3 TO CP01-IN-BLOQUEIO
              ELSE
                 IF CS-RECADAST-BENEF = 1 AND WS-TEM-MOV = 1
                    MOVE 2 TO CP01-IN-BLOQUEIO
                 ELSE
                    MOVE 1 TO CP01-IN-BLOQUEIO.

       2620-FIM-BLOQUEIO. EXIT.

       2620-VERIFICA-MEIO-PAGTO.

           IF CS-MEIO-PAGTO OF DS-MOV-CRED-MANUT NOT = 1 AND 2 AND 4
              GO TO 2620-FIM-VERIFICA-MEIO-PAGTO.

           IF CS-MEIO-PAGTO OF DS-MOV-CRED-MANUT = 02
              IF D2-INI-VALIDADE OF DS-MOV-CRED-MANUT NOT >
                 WS-DATA-SIS-INV
                 GO TO 2620-FIM-VERIFICA-MEIO-PAGTO.

           MOVE 1 TO W77-MEIO-OK.

       2620-FIM-VERIFICA-MEIO-PAGTO. EXIT.

       2620-PESQ-MOV.

           FIND NEXT ST-CENSO-BEN-REC AT NU-NB =
                                         NU-NB OF DS-MOV-CRED-MANUT
                ON EXCEPTION
                   IF DMSTATUS(NOTFOUND)
                      MOVE 0 TO WS-TEM-MOV
                      GO TO 2620-FIM-PESQ-MOV
                   ELSE
                      CALL SYSTEM DMTERMINATE.

           IF CS-MOV-CENSO OF DS-CENSO-BEN-REC = 9
              GO TO 2620-PESQ-MOV.

        2620-FIM-PESQ-MOV. EXIT.

        2625-MONTA-REG-ALTE.

           MOVE  ALL @00@               TO  CP50-REG1
           MOVE  SPACES                 TO  CP50-NM-RECEBEDOR
           MOVE  CP01-CHAVE             TO  CP50-CHAVE
           MOVE  ID-OL-SOLIC-CRED       OF  DS-MOV-CRED-ALTER
                                        TO  CP50-ID-OL-SOLIC-CRED
           MOVE  D2-SOLIC-CREDITO       OF  DS-MOV-CRED-ALTER
                                        TO  CP50-DT-SOLIC-CRED
           MOVE  NU-SEQ-SOLIC-CRED      OF  DS-MOV-CRED-ALTER
                                        TO  CP50-NU-SEQ-SOLIC-CRED
           MOVE  NU-MATRIC-SOLIC        OF  DS-MOV-CRED-ALTER
                                        TO  CP50-NU-MATRIC-SOLIC
           MOVE  CS-MOTIVO-SOLIC        OF  DS-MOV-CRED-ALTER
                                        TO  CP50-CS-MOTIVO-SOLIC
           MOVE  NU-NB-ANTERIOR         OF  DS-MOV-CRED-ALTER
                                        TO  CP50-NU-NB-ANTERIOR
           MOVE  NM-RECEBEDOR           OF  DS-MOV-CRED-ALTER
                                        TO  CP50-NM-RECEBEDOR
           MOVE  ID-FONETICO            OF  DS-MOV-CRED-ALTER
                                        TO  CP50-ID-FONETICO
           MOVE  DT-NASCIMENTO          OF  DS-MOV-CRED-ALTER
                                        TO  CP50-DT-NASCIMENTO
           MOVE CS-MUN-NASCIMENTO       OF  DS-MOV-CRED-ALTER
                                        TO  CP50-CS-MUN-NASCIMENTO
           MOVE ID-FONETICO-MAE         OF  DS-MOV-CRED-ALTER
                                        TO  CP50-ID-FONETICO-MAE
           MOVE D2-EVENTO               OF  DS-MOV-CRED-ALTER
                                        TO  CP50-DT-EVENTO
           MOVE D2-ENVIO-PRISMA         OF  DS-MOV-CRED-ALTER
                                        TO  CP50-DT-ENVIO-PRISMA
           MOVE D2-ENVIO-BANCO          OF  DS-MOV-CRED-ALTER
                                        TO  CP50-DT-ENVIO-BANCO
           MOVE NM-SEGURADO             OF  DS-MOV-CRED-ALTER
                                        TO  CP50-NM-SEGURADO
           MOVE AUX1-ID-NIT             TO  CP50-ID-NIT
           MOVE AUX-SEQ-REG             TO  CP50-SEQ-REG

           IF PAR-MOV-CREDITO = "ALTE"
              MOVE NU-SEQ-REG-LOTE      OF  DS-MOV-CRED-ALTER
                                        TO  CP50-NU-SEQ-REG-LOTE
              MOVE N-SEQ-REG-CADAST     OF  DS-MOV-CRED-ALTER
                                        TO  CP50-NU-SEQ-REG-CADAST.

           IF IN-DIVERGE-RUBR OF DS-MOV-CRED-ALTER
              MOVE 1 TO CP50-IN-DIVERGE-RUBR
           ELSE
              MOVE 0 TO CP50-IN-DIVERGE-RUBR.

           IF IN-ORIG-SOLIC-PAB OF DS-MOV-CRED-ALTER
              MOVE 1 TO CP50-IN-ORIG-SOLIC-PAB
           ELSE
              MOVE 0 TO CP50-IN-ORIG-SOLIC-PAB.

           IF IN-CRED-EMPR-CONV OF DS-MOV-CRED-ALTER
              MOVE 1 TO CP50-IN-CRED-EMPR-CONV
           ELSE
              MOVE 0 TO CP50-IN-CRED-EMPR-CONV.

           IF IN-PAB-DOMIC-BANC OF DS-MOV-CRED-ALTER
              MOVE 1 TO CP50-IN-CRED-MPG-CONC
           ELSE
              MOVE 0 TO CP50-IN-CRED-MPG-CONC.

           MOVE CS-ORIG-CONTG-PAB       OF DS-MOV-CRED-ALTER
                                        TO CP50-CS-ORIG-CONTG-PAB
           MOVE CS-DESVIO-CREDITO       OF DS-MOV-CRED-ALTER
                                        TO CP50-CS-DESVIO-CREDITO
           MOVE NU-SEQ-DEP-IRSM         OF DS-MOV-CRED-ALTER
                                        TO CP50-NU-SEQ-DEP-IRSM
           MOVE NU-SEQ-HERD-IRSM        OF DS-MOV-CRED-ALTER
                                        TO CP50-NU-SEQ-HERD-IRSM
           MOVE ID-PAIS                 OF DS-MOV-CRED-ALTER
                                        TO CP50-ID-PAIS

           IF IN-DADOS-ACAO-JUD OF DS-MOV-CRED-ALTER
              MOVE 1 TO CP50-IN-DADOS-ACAO-JUD
           ELSE
               MOVE 0 TO CP50-IN-DADOS-ACAO-JUD.

           MOVE NU-NB-PENSAO      OF DS-MOV-CRED-ALTER
                                  TO CP50-NU-NB-PENSAO
           MOVE QT-MESES-RRA      OF DS-MOV-CRED-ALTER
                                  TO CP50-QT-MESES-RRA
           MOVE NU-SEQ-SOLI-COMPL OF DS-MOV-CRED-ALTER
                                  TO CP50-NU-SEQ-SOLI-COMPL
           MOVE D2-SOL-CRED-ORIG  OF DS-MOV-CRED-ALTER
                                  TO CP50-D2-SOL-CRED-ORIG
           MOVE NU-SEQ-SOL-ORIG   OF DS-MOV-CRED-ALTER
                                  TO CP50-NU-SEQ-SOL-ORIG
           MOVE NU-MATR-VAL-ANT   OF DS-MOV-CRED-ALTER
                                  TO CP50-NU-MATR-VAL-ANT
           MOVE HR-VALID-ANT      OF DS-MOV-CRED-ALTER
                                  TO CP50-HR-VALID-ANT
           MOVE DT-VALID-ANT      OF DS-MOV-CRED-ALTER
                                  TO CP50-DT-VALID-ANT
           MOVE CS-MOTIVO-CP      OF DS-MOV-CRED-ALTER
                                  TO CP50-CS-MOTIVO-CP

           IF IN-PAB-REM-AU-NPG OF DS-MOV-CRED-ALTER
              MOVE 1 TO CP50-IN-PAB-REM-AUT-NPG
           ELSE
              MOVE 0 TO CP50-IN-PAB-REM-AUT-NPG.

           IF IN-ERRO-ADMIN OF DS-MOV-CRED-ALTER
              MOVE 1 TO CP50-IN-ERRO-ADMIN
           ELSE
              MOVE 0 TO CP50-IN-ERRO-ADMIN.

           IF IN-PAB-NB-CESS OF DS-MOV-CRED-ALTER
              MOVE 1 TO CP50-IN-PAB-NB-CESS
           ELSE
              MOVE 0 TO CP50-IN-PAB-NB-CESS.

           IF IN-ORI-PLAT-BAIXA OF DS-MOV-CRED-ALTER
              MOVE 1 TO CP50-IN-ORI-PLAT-BAIXA
           ELSE
              MOVE 0 TO CP50-IN-ORI-PLAT-BAIXA.

           IF   NOT IN-CRED-VALIDADO    OF DS-MOV-CRED-ALTER
                GO                      TO 2625-GRAVA.

           FIND S2-CRED-PEND-OL  AT
                ID-OL-SOLIC-CRED  OF DS-CRED-PENDENTE =
                ID-OL-SOLIC-CRED  OF DS-MOV-CRED-ALTER             AND
                D2-SOLIC-CREDITO  OF DS-CRED-PENDENTE =
                D2-SOLIC-CREDITO  OF DS-MOV-CRED-ALTER             AND
                NU-SEQ-SOLIC-CRED OF DS-CRED-PENDENTE =
                NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER
                ON EXCEPTION IF DMSTATUS(NOTFOUND)
                                  GO 2625-GRAVA.

           MOVE  D2-VALIDACAO-CRED      OF  DS-CRED-PENDENTE
                                        TO  CP50-DT-VALIDACAO-CRED
           MOVE  NU-MATRIC-VALID        OF  DS-CRED-PENDENTE
                                        TO  CP50-NU-MATR-VALIDACAO
           MOVE  NU-MATRIC-REVISOR      OF  DS-CRED-PENDENTE
                                        TO  CP50-NU-MATRIC-REVISOR
           MOVE  CS-GRAU-PENDENCIA      OF  DS-CRED-PENDENTE
                                        TO  CP50-CS-GRAU-PENDENCIA
           MOVE  D2-LIM-VALIDACAO       OF  DS-CRED-PENDENTE
                                        TO  CP50-DT-LIM-VALIDACAO
           MOVE  HR-VALIDACAO-CRED      OF  DS-CRED-PENDENTE
                                        TO  CP50-HR-VALIDACAO-CRED
           MOVE DT-COMP-COR-MON         OF DS-CRED-PENDENTE
                                        TO CP50-DT-COMP-COR-MON

           IF  IN-ERRO-ADMIN OF DS-CRED-PENDENTE
               MOVE 1 TO CP50-IN-ERRO-ADMIN
           ELSE
               MOVE 0 TO CP50-IN-ERRO-ADMIN.

           IF  QT-MOTIVO-PEND > 0
               MOVE  ZERO             TO INDICE-PENDENCIA
               MOVE  QT-MOTIVO-PEND   TO  CP50-QT-MOT-PENDENCIA
               PERFORM 2628-MONTA-PENDENCIAS THRU 2628-FIM
                       QT-MOTIVO-PEND TIMES.

       2625-GRAVA.

            WRITE CP50-REG1.

       2625-FIM. EXIT.

       2628-MONTA-PENDENCIAS.

            ADD 1 TO INDICE-PENDENCIA.
            MOVE CS-MOTIVO-PEND OF DS-CRED-PENDENTE(INDICE-PENDENCIA) TO
                 CP50-CS-MOT-PENDENCIA (INDICE-PENDENCIA).

       2628-FIM. EXIT.

       2629-MONTA-REG-AP.

           MOVE ALL @00@   TO CP77-REG
           MOVE CP01-DT-MOV-CREDITO   TO CP77-DT-MOV-CREDITO
           MOVE CP01-NU-NB            TO CP77-NU-NB
           MOVE CP01-CS-NATUR-CREDITO TO CP77-CS-NATUR-CREDITO
           MOVE CP01-DT-FIM-PERIODO   TO CP77-DT-FIM-PERIODO
           MOVE CP01-DT-INI-PERIODO   TO CP77-DT-INI-PERIODO
           MOVE CP01-SEQ-REG          TO CP77-SEQ-REG
           MOVE CS-MUNICIPIO      OF DS-CALC-ACAO-JUD  TO
                CP77-CS-MUNICIPIO
           MOVE NU-VARA-ACAO-JUD  OF DS-CALC-ACAO-JUD  TO
                CP77-NU-VARA-ACAO-JUD
           MOVE D2-PAGTO-ACAO-JUD OF DS-CALC-ACAO-JUD  TO
                CP77-DT-PAGTO-ACAO-JUD
           MOVE NU-AP-ACAO-JUD    OF DS-CALC-ACAO-JUD  TO
                CP77-NU-AP-ACAO-JUD
           MOVE NU-AP-IR-ACAO-JUD OF DS-CALC-ACAO-JUD  TO
                CP77-NU-AP-IR-ACAO-JUD
           MOVE NU-MATR-SOL-CALC  OF DS-CALC-ACAO-JUD  TO
                CP77-NU-MATR-SOL-CALC
           MOVE CS-PRAZO-PAGTO    OF DS-CALC-ACAO-JUD  TO
                CP77-CS-PRAZO-PAGTO
           MOVE CS-UF-OL-ACAO-JUD OF DS-CALC-ACAO-JUD  TO
                CP77-CS-UF-OL-ACAO-JUD
           MOVE NU-PROC-ACAO-JUD  OF DS-CALC-ACAO-JUD  TO
                CP77-NU-PROC-ACAO-JUD.
           WRITE CP77-REG.

       2629-FIM. EXIT.

       2630-MONTA-REG-TMP-CRED-REST.

           ADD 1 TO W77-IND
                    W77-IND-CP.

           IF W77-IND > AUX1-QT-RUBRICA
              GO TO 2630-FIM.

           IF  PAR-SEAS = "SEAS"
               IF CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND) > 0 AND
                  VL-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND) IS NULL
                  MOVE "BENEFICIO COM RUBRICA ZERADA - NIT=" TO
                        WS-PARAM1-ZDISPLAY
                  MOVE  ID-NIT OF DS-MOV-CRED-SEAS TO WS-PARAM2-ZDISPLAY
                  CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                                   WS-PARAM2-ZDISPLAY,
                                                   WS-INTEIRO-ZDISPLAY
                  SET MYSELF (STATUS) TO -1
               ELSE
                  NEXT SENTENCE
           ELSE
              IF (PAR-MOV-CREDITO = "CONC" AND
                  CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND) > 0 AND
                  VL-RUBRICA OF DS-MOV-CRED-CONC(W77-IND) IS NULL) OR
                 (PAR-MOV-CREDITO = "MANU" AND
                  CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND) > 0 AND
                  VL-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND) IS NULL) OR
                 (PAR-MOV-CREDITO = "ALTE" AND
                  CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND) > 0 AND
                  VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND) IS NULL) OR
                 (PAR-MOV-CREDITO = "CONV" AND
                  CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND) > 0 AND
                  VL-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND) IS NULL)
                  MOVE "BENEFICIO COM RUBRICA ZERADA - NB=" TO
                        WS-PARAM1-ZDISPLAY
                  MOVE AUX1-NU-NB TO WS-PARAM2-ZDISPLAY
                  CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                                   WS-PARAM2-ZDISPLAY,
                                                   WS-INTEIRO-ZDISPLAY
                  SET MYSELF (STATUS) TO -1.

           IF PAR-SEAS = "SEAS"
              MOVE CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND)
                              TO CP430-CS-RUBRICA(W77-IND-CP)
              MOVE VL-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND)
                              TO CP430-VL-RUBRICA(W77-IND-CP)
           ELSE
              IF PAR-MOV-CREDITO = "CONC"
                 MOVE CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND)
                                 TO CP430-CS-RUBRICA(W77-IND-CP)
                 MOVE VL-RUBRICA OF DS-MOV-CRED-CONC(W77-IND)
                                 TO CP430-VL-RUBRICA(W77-IND-CP)
              ELSE
                 IF PAR-MOV-CREDITO = "MANU" OR "CONV"
                    MOVE CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND)
                                    TO CP430-CS-RUBRICA(W77-IND-CP)
                    MOVE VL-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND)
                                    TO CP430-VL-RUBRICA(W77-IND-CP)
                 ELSE
                    IF PAR-MOV-CREDITO = "ALTE"
                       MOVE CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND)
                                       TO CP430-CS-RUBRICA(W77-IND-CP)
                       MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND)
                                       TO CP430-VL-RUBRICA(W77-IND-CP)
                    ELSE
                       IF PAR-MOV-CREDITO = "APAG"
                          MOVE 206 TO CP430-CS-RUBRICA(W77-IND-CP)
                          MOVE VL-CTB-PREV-ACAO OF DS-CALC-ACAO-JUD TO
                                           CP430-VL-RUBRICA(W77-IND-CP).

               IF PAR-MOV-CREDITO = "CONC"   AND
                  AUX1-SEQ-PROC   >   2      AND
                  AUX1-DT-DDB     <   19930810
                  COMPUTE  CP430-VL-RUBRICA(W77-IND-CP) =
                            CP430-VL-RUBRICA(W77-IND-CP)  / 1000.

           IF W77-IND-CP = 13 OR W77-IND = AUX1-QT-RUBRICA
               MOVE AUX1-NU-NB            TO  CP430-NU-NB
               MOVE AUX1-CS-NATUR-CREDITO TO  CP430-CS-NATUR-CREDITO
               MOVE AUX1-DT-MOV-CREDITO   TO  CP430-DT-MOV-CREDITO
               MOVE AUX1-DT-FIM-PERIODO   TO  CP430-DT-FIM-PERIODO
               MOVE AUX1-DT-INI-PERIODO   TO  CP430-DT-INI-PERIODO
               MOVE AUX1-ID-NIT           TO  CP430-ID-NIT
               MOVE AUX-SEQ-REG           TO  CP430-SEQ-REG
               WRITE CP430-RUBRICA-REG
               MOVE ALL @00@              TO  CP430-RUBRICA-REG
               MOVE ZERO                  TO  W77-IND-CP.

       2630-FIM. EXIT.
      /
       2631-TOT-CONTAB.

      *    AS  VARIAVEIS WS-TOT-BRUTO-2 E WS-TOT-DESC-2 FORAM  CRIADAS
      *    POR CAUSA DO ARQUIVO DSUBCPG058 (SCF). PARA A CONTABILIDADE,
      *    A RUBRICA 105 EH CONTABILIZADA A PARTE, ENQUANTO QUE NO AR-
      *    QUIVO DSUBCPG058 ESTA DEVERAH  SER  UTILIZADA  JUNTO COM AS
      *    DEMAIS RUBRICAS.  SE ESTA RUBRICA  DEIXAR DE EXISTIR, ESTAS
      *    NOVAS VARIAVEIS PODERAO SER SUBSTITUIDAS POR WS-TOT-BRUTO E
      *    WS-TOT-DESC.
      *
           MOVE ZEROS TO WS-TOT-BRUTO
                         WS-RUB-NOVA
                         WS-TOT-DESC
                         WS-VL-RUB-108
                         WS-TOT-BRUTO-2
                         WS-TOT-DESC-2.

           PERFORM 2636-TOT-VL-BRUTO THRU 2636-FIM
                   VARYING W77-IND-RUB FROM 101 BY 1
                     UNTIL W77-IND-RUB > 199.

           COMPUTE WS-TOT-BRUTO-2 = WS-TOT-BRUTO + WS-BRUTO-RUB-105.
      ******************************************************************
           COMPUTE WS-TOT-BRUTO = WS-TOT-BRUTO -
                                  WS-TB-VL-RUBRICA-AUX(218).
      ******************************************************************
           PERFORM 2637-TOT-VL-DESC  THRU 2637-FIM
                   VARYING W77-IND-RUB FROM 201 BY 1
                     UNTIL W77-IND-RUB > 299.

           COMPUTE WS-TOT-DESC-2 = WS-TOT-DESC.
      ******************************************************************
           COMPUTE WS-TOT-DESC = WS-TOT-DESC -
                                 WS-TB-VL-RUBRICA-AUX(218).
      ******************************************************************

      ***  ALTERACAO PROVISORIA PARA IR DE ANISTIADOS
           IF AUX1-NU-NB = 0571793533 OR 0479083720 OR 0866418490 OR
                           0479009996 OR 0548577579 OR 0433200650 OR
             0879533617 OR 0684836173 OR 0635056313 OR 0796730156
              COMPUTE WS-TOT-DESC = WS-TOT-DESC -
                (WS-TB-VL-RUBRICA-AUX(201) + WS-TB-VL-RUBRICA-AUX(204) +
                 WS-TB-VL-RUBRICA-AUX(205) + WS-TB-VL-RUBRICA-AUX(207) +
                 WS-TB-VL-RUBRICA-AUX(209) + WS-TB-VL-RUBRICA-AUX(211)).
      ***
           MOVE WS-TB-VL-RUBRICA-AUX(301) TO WS-VL-RUB-301.

           MOVE WS-TB-VL-RUBRICA-AUX(108) TO WS-VL-RUB-108.

           IF WS-TB-VL-RUBRICA-AUX(301) > WS-TOT-BRUTO
              MOVE WS-TOT-BRUTO TO WS-TB-VL-RUBRICA-AUX(301).

           IF WS-VL-RUB-301  > WS-TOT-BRUTO-2
              MOVE WS-TOT-BRUTO-2 TO WS-VL-RUB-301.

           MOVE AUX1-CS-ESPECIE TO WS-ESPECIES-LOAS.

           IF (AUX1-CS-ESPECIE = 89 OR 54 OR ESPECIES-LOAS OR
               ESPECIES-RMV) AND
              WS-TB-VL-RUBRICA-AUX(301) NOT = WS-TOT-BRUTO
              MOVE WS-TOT-BRUTO TO WS-TB-VL-RUBRICA-AUX(301).

           IF (AUX1-CS-ESPECIE = 89 OR 54 OR ESPECIES-LOAS OR
               ESPECIES-RMV) AND
              WS-VL-RUB-301 NOT = WS-TOT-BRUTO-2
              MOVE WS-TOT-BRUTO-2 TO WS-VL-RUB-301.

           PERFORM 2632-FIXA-IND THRU 2632-FIM.

           PERFORM 2633-CALC-IR  THRU 2633-FIM.

           PERFORM 2634-TOT-ESP  THRU 2634-FIM.

           PERFORM 2635-TOT-DESC THRU 2635-FIM.

           IF WS-TIPO = 2
              NEXT SENTENCE
           ELSE
              IF AUX1-CS-ESPECIE = 89 OR 54 OR ESPECIES-LOAS OR
                 ESPECIES-RMV
                 MOVE 7 TO WS-TIPO
                 PERFORM 2635-TOT-DESC THRU 2635-FIM
              ELSE
                 MOVE 6 TO WS-TIPO
                 PERFORM 2635-TOT-DESC THRU 2635-FIM
                 IF AUX1-CS-RAMO-ATIVIDADE = 8
                    MOVE 9 TO WS-TIPO
                    PERFORM 2635-TOT-DESC THRU 2635-FIM
                 ELSE
                    MOVE 8 TO WS-TIPO
                    PERFORM 2635-TOT-DESC THRU 2635-FIM.

       2631-FIM. EXIT.

       2632-FIXA-IND.

           MOVE AUX1-CS-ESPECIE      TO WS-ESP.

           IF WS-ESP = ZEROS
              MOVE "BENEFICIO COM ESPECIE ZERADA" TO WS-PARAM1-ZDISPLAY
              MOVE AUX1-NU-NB                     TO WS-PARAM2-ZDISPLAY
              CALL "ZDISPLAY2 IN DTP/L1" USING       WS-PARAM1-ZDISPLAY,
                                                     WS-PARAM2-ZDISPLAY,
                                                     WS-INTEIRO-ZDISPLAY
              MOVE 01 TO WS-ESP
           ELSE
              IF WS-ESP > 99
                 MOVE 01 TO WS-ESP.

      *****    ALTERACAO DO CALCULO DA COMPET. DE AGREGACAO CONTABIL
           IF AUX1-DT-MOV-CREDITO > 20071231
              MOVE AUX1-DT-MOV-CREDITO TO WS-DT-AUX4-AAMMDD
           ELSE
              MOVE AUX1-DT-INI-VALIDADE TO WS-DT-AUX4-AAMMDD.
      *****        FIM ALTERACAO

           IF WS-DT-AUX4-COMPET < WS-COMP-SIS-INV
              MOVE WS-COMP-SIS-INV TO WS-DT-AUX4-COMPET.

           PERFORM 2632A-VARRE-TAB THRU 2632A-FIM
                   VARYING WS-COMP FROM 1 BY 1
                     UNTIL WS-COMP               > 5     OR
                           WS-TB-COMPET(WS-COMP) = ZEROS OR
                           WS-TB-COMPET(WS-COMP) = WS-DT-AUX4-COMPET

           IF PAR-SEAS = SPACES
              IF WS-IND > 5
                 MOVE "+ DE CINCO COMPET NO PROCESSAMENTO NB" TO
                       WS-PARAM1-ZDISPLAY
                 MOVE AUX1-NU-NB TO WS-PARAM2-ZDISPLAY
                 CALL "ZDISPLAY2 IN DTP/L1" USING
                                            WS-PARAM1-ZDISPLAY,
                                            WS-PARAM2-ZDISPLAY,
                                            WS-INTEIRO-ZDISPLAY
                 SET MYSELF(STATUS) TO -1.

           IF PAR-SEAS = "SEAS"
              IF WS-IND > 5
                 MOVE "+ DE CINCO COMPET NO PROCESSAMENTO NIT" TO
                       WS-PARAM1-ZDISPLAY
                 MOVE AUX1-ID-NIT TO WS-PARAM2-ZDISPLAY
                 CALL "ZDISPLAY2  IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                 SET MYSELF(STATUS) TO -1.

           IF WS-TB-COMPET (WS-COMP) = ZEROS
              MOVE WS-DT-AUX4-COMPET TO WS-TB-COMPET (WS-COMP).

           MOVE AUX1-ID-OL-MANUTENCAO TO WS-OL-MANUT.

           IF WS-UF < 02 OR > 28
              MOVE SPACES TO WS-MENSAGEM-ZDISPLAY
              STRING "NB : ",
                      AUX1-NU-NB,
                     " COM OL MANTENEDOR INVALIDO: ",
                      AUX1-ID-OL-MANUTENCAO
              DELIMITED BY SIZE
              INTO WS-MENSAGEM-ZDISPLAY
              CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
                                              WS-INTEIRO-ZDISPLAY
              MOVE 02 TO WS-UF.

           SUBTRACT 1 FROM WS-UF.

       2632-FIM. EXIT.

       2632A-VARRE-TAB.
       2632A-FIM. EXIT.
      /
       2633-CALC-IR.

           IF WS-TB-VL-RUBRICA-AUX (301) = ZEROS
              MOVE 100   TO WS-PERC-PREV
                            WS-PERC-PREV-2
           ELSE
              COMPUTE WS-PERC-PREV = 100 -
                     (WS-TB-VL-RUBRICA-AUX(301) / WS-TOT-BRUTO * 100)
              COMPUTE WS-PERC-PREV-2 = 100 -
                     (WS-VL-RUB-301 / WS-TOT-BRUTO-2 * 100).

           MOVE ZEROS TO WS-IR-PREV,WS-RUB-108-PREV
                         WS-IR-UNIAO,WS-RUB-108-UNIAO
                         WS-IR-PREV-2
                         WS-IR-UNIAO-2.

      ***  ALTERACAO PROVISORIA PARA IR DE ANISTIADOS
           IF AUX1-NU-NB = 0571793533 OR 0479083720 OR 0866418490 OR
                           0479009996 OR 0548577579 OR 0433200650 OR
             0879533617 OR 0684836173 OR 0635056313 OR 0796730156
              MOVE ZEROS TO WS-IR-PREV
           ELSE
      ***
             COMPUTE WS-IR-PREV =
              (WS-TB-VL-RUBRICA-AUX(201) + WS-TB-VL-RUBRICA-AUX(204) +
               WS-TB-VL-RUBRICA-AUX(205) + WS-TB-VL-RUBRICA-AUX(207) +
               WS-TB-VL-RUBRICA-AUX(209) + WS-TB-VL-RUBRICA-AUX(211))
               * WS-PERC-PREV / 100.

           COMPUTE WS-TOT-DESCIR-2 =
            (WS-TB-VL-RUBRICA-AUX(201) + WS-TB-VL-RUBRICA-AUX(204) +
             WS-TB-VL-RUBRICA-AUX(205) + WS-TB-VL-RUBRICA-AUX(207) +
             WS-TB-VL-RUBRICA-AUX(209) + WS-TB-VL-RUBRICA-AUX(211)).

           COMPUTE WS-IR-PREV-2 =
              (WS-TB-VL-RUBRICA-AUX(201) + WS-TB-VL-RUBRICA-AUX(204) +
               WS-TB-VL-RUBRICA-AUX(205) + WS-TB-VL-RUBRICA-AUX(207) +
               WS-TB-VL-RUBRICA-AUX(209) + WS-TB-VL-RUBRICA-AUX(211))
               * WS-PERC-PREV-2 / 100.

           IF WS-PERC-PREV NOT = 100,00000
      ***  ALTERACAO PROVISORIA PARA IR DE ANISTIADOS
              IF AUX1-NU-NB = 0571793533 OR 0479083720 OR 0866418490 OR
                              0479009996 OR 0548577579 OR 0433200650 OR
                0879533617 OR 0684836173 OR 0635056313 OR 0796730156
                 MOVE ZEROS TO WS-IR-UNIAO
              ELSE
      ***
                 COMPUTE WS-IR-UNIAO =
                (WS-TB-VL-RUBRICA-AUX(201) + WS-TB-VL-RUBRICA-AUX(204) +
                 WS-TB-VL-RUBRICA-AUX(205) + WS-TB-VL-RUBRICA-AUX(207) +
                 WS-TB-VL-RUBRICA-AUX(209) + WS-TB-VL-RUBRICA-AUX(211))
                 - WS-IR-PREV.

           IF WS-PERC-PREV-2 NOT = 100,00000
              COMPUTE WS-IR-UNIAO-2 =
                (WS-TB-VL-RUBRICA-AUX(201) + WS-TB-VL-RUBRICA-AUX(204) +
                 WS-TB-VL-RUBRICA-AUX(205) + WS-TB-VL-RUBRICA-AUX(207) +
                 WS-TB-VL-RUBRICA-AUX(209) + WS-TB-VL-RUBRICA-AUX(211))
                 - WS-IR-PREV-2.

          COMPUTE WS-RUB-108-PREV =
                                (WS-VL-RUB-108 * WS-PERC-PREV-2)/100.

          COMPUTE WS-RUB-108-UNIAO = (WS-VL-RUB-108 - WS-RUB-108-PREV).

       2633-FIM. EXIT.

       2634-TOT-ESP.

           IF IN-ACORDO-INTERNACIONAL
              ADD WS-TOT-BRUTO TO
                  WS-TB-TOT-BRUTO-ESP4(WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-BRUTO-ESP4(WS-COMP,    28, WS-ESP)
              ADD WS-TOT-DESC  TO
                  WS-TB-TOT-DESC-ESP4 (WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-DESC-ESP4 (WS-COMP,    28, WS-ESP)
              ADD 1            TO
                  WS-TB-TOT-ESP4 (WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-ESP4 (WS-COMP,    28, WS-ESP)
              MOVE 4 TO WS-TIPO
           ELSE
           IF AUX1-ID-BANCO = 998
              ADD WS-TOT-BRUTO TO
                  WS-TB-TOT-BRUTO-ESP2(WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-BRUTO-ESP2(WS-COMP,    28, WS-ESP)
              ADD WS-TOT-DESC  TO
                  WS-TB-TOT-DESC-ESP2 (WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-DESC-ESP2 (WS-COMP,    28, WS-ESP)
              ADD 1            TO
                  WS-TB-TOT-ESP2 (WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-ESP2 (WS-COMP,    28, WS-ESP)
              MOVE 2 TO WS-TIPO
           ELSE
           IF AUX1-ID-BANCO = 888
              ADD WS-TOT-BRUTO TO
                  WS-TB-TOT-BRUTO-ESP3(WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-BRUTO-ESP3(WS-COMP,    28, WS-ESP)
              ADD WS-TOT-DESC  TO
                  WS-TB-TOT-DESC-ESP3 (WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-DESC-ESP3 (WS-COMP,    28, WS-ESP)
              ADD 1            TO
                  WS-TB-TOT-ESP3 (WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-ESP3 (WS-COMP,    28, WS-ESP)
              MOVE 3 TO WS-TIPO
           ELSE
              ADD WS-TOT-BRUTO TO
                  WS-TB-TOT-BRUTO-ESP1(WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-BRUTO-ESP1(WS-COMP,    28, WS-ESP)
              ADD WS-TOT-DESC  TO
                  WS-TB-TOT-DESC-ESP1 (WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-DESC-ESP1 (WS-COMP,    28, WS-ESP)
              ADD 1            TO
                  WS-TB-TOT-ESP1 (WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-ESP1 (WS-COMP,    28, WS-ESP)
              MOVE 1 TO WS-TIPO.

           IF WS-TIPO = 1 OR 3 OR 4
              ADD WS-TOT-BRUTO TO
                  WS-TB-TOT-BRUTO-ESP5(WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-BRUTO-ESP5(WS-COMP,    28, WS-ESP)
              ADD WS-TOT-DESC  TO
                  WS-TB-TOT-DESC-ESP5 (WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-DESC-ESP5 (WS-COMP,    28, WS-ESP)
              ADD 1            TO
                  WS-TB-TOT-ESP5 (WS-COMP, WS-UF, WS-ESP)
                  WS-TB-TOT-ESP5 (WS-COMP,    28, WS-ESP).

           IF AUX1-ID-BANCO              = 998    AND
              (NOT IN-ACORDO-INTERNACIONAL)
              GO 2634-FIM.
           MOVE AUX1-CS-ESPECIE TO WS-ESPECIES-LOAS
           IF AUX1-CS-ESPECIE NOT = 89 AND 54 AND NOT ESPECIES-LOAS
              AND NOT ESPECIES-RMV
              COMPUTE WS-TB-TOT-BRUTO-ESP6(WS-COMP, WS-UF, WS-ESP) =
                      WS-TB-TOT-BRUTO-ESP6(WS-COMP, WS-UF, WS-ESP) +
                      WS-TOT-BRUTO - WS-TB-VL-RUBRICA-AUX(301)
              COMPUTE WS-TB-TOT-BRUTO-ESP6(WS-COMP,       28, WS-ESP) =
                      WS-TB-TOT-BRUTO-ESP6(WS-COMP,       28, WS-ESP) +
                      WS-TOT-BRUTO - WS-TB-VL-RUBRICA-AUX(301)
              COMPUTE WS-TB-TOT-DESC-ESP6(WS-COMP, WS-UF, WS-ESP) =
                      WS-TB-TOT-DESC-ESP6(WS-COMP, WS-UF, WS-ESP) +
                      WS-TOT-DESC - WS-IR-UNIAO
              COMPUTE WS-TB-TOT-DESC-ESP6(WS-COMP,       28, WS-ESP) =
                      WS-TB-TOT-DESC-ESP6(WS-COMP,       28, WS-ESP) +
                      WS-TOT-DESC - WS-IR-UNIAO
              IF AUX1-CS-RAMO-ATIVIDADE = 8
                 COMPUTE WS-TB-TOT-BRUTO-ESP9(WS-COMP, WS-UF, WS-ESP) =
                         WS-TB-TOT-BRUTO-ESP9(WS-COMP, WS-UF, WS-ESP) +
                         WS-TOT-BRUTO - WS-TB-VL-RUBRICA-AUX(301)
                 COMPUTE WS-TB-TOT-BRUTO-ESP9(WS-COMP,    28, WS-ESP) =
                         WS-TB-TOT-BRUTO-ESP9(WS-COMP,    28, WS-ESP) +
                         WS-TOT-BRUTO - WS-TB-VL-RUBRICA-AUX(301)
                 COMPUTE WS-TB-TOT-DESC-ESP9(WS-COMP, WS-UF, WS-ESP) =
                         WS-TB-TOT-DESC-ESP9(WS-COMP, WS-UF, WS-ESP) +
                         WS-TOT-DESC - WS-IR-UNIAO
                 COMPUTE WS-TB-TOT-DESC-ESP9(WS-COMP,    28, WS-ESP) =
                         WS-TB-TOT-DESC-ESP9(WS-COMP,    28, WS-ESP) +
                         WS-TOT-DESC - WS-IR-UNIAO
              ELSE
                 COMPUTE WS-TB-TOT-BRUTO-ESP8(WS-COMP, WS-UF, WS-ESP) =
                         WS-TB-TOT-BRUTO-ESP8(WS-COMP, WS-UF, WS-ESP) +
                         WS-TOT-BRUTO - WS-TB-VL-RUBRICA-AUX(301)
                 COMPUTE WS-TB-TOT-BRUTO-ESP8(WS-COMP,    28, WS-ESP) =
                         WS-TB-TOT-BRUTO-ESP8(WS-COMP,    28, WS-ESP) +
                         WS-TOT-BRUTO - WS-TB-VL-RUBRICA-AUX(301)
                 COMPUTE WS-TB-TOT-DESC-ESP8(WS-COMP, WS-UF, WS-ESP) =
                         WS-TB-TOT-DESC-ESP8(WS-COMP, WS-UF, WS-ESP) +
                         WS-TOT-DESC - WS-IR-UNIAO
                 COMPUTE WS-TB-TOT-DESC-ESP8(WS-COMP,    28, WS-ESP) =
                         WS-TB-TOT-DESC-ESP8(WS-COMP,    28, WS-ESP) +
                         WS-TOT-DESC - WS-IR-UNIAO.

           IF WS-TB-VL-RUBRICA-AUX (301) NOT = ZEROS
              ADD 1 TO WS-TB-TOT-ESP7(WS-COMP, WS-UF, WS-ESP)
                       WS-TB-TOT-ESP7(WS-COMP,    28, WS-ESP)
              ADD      WS-TB-VL-RUBRICA-AUX (301) TO
                       WS-TB-TOT-BRUTO-ESP7(WS-COMP, WS-UF, WS-ESP)
                       WS-TB-TOT-BRUTO-ESP7(WS-COMP,    28, WS-ESP)
              IF AUX1-CS-ESPECIE = 89 OR 54 OR ESPECIES-LOAS
                 OR ESPECIES-RMV
                 ADD   WS-TOT-DESC TO
                       WS-TB-TOT-DESC-ESP7(WS-COMP, WS-UF, WS-ESP)
                       WS-TB-TOT-DESC-ESP7(WS-COMP,    28, WS-ESP)
              ELSE
                 ADD   WS-IR-UNIAO TO
                       WS-TB-TOT-DESC-ESP7(WS-COMP, WS-UF, WS-ESP)
                       WS-TB-TOT-DESC-ESP7(WS-COMP,    28, WS-ESP).

           IF AUX1-CS-ESPECIE NOT = 89 AND 54 AND NOT ESPECIES-LOAS
              AND NOT ESPECIES-RMV
              IF WS-TB-COMPET(WS-COMP) < 200009
                 IF WS-TB-VL-RUBRICA-AUX (301) NOT = WS-TOT-BRUTO
                    ADD 1 TO WS-TB-TOT-ESP6(WS-COMP, WS-UF, WS-ESP)
                             WS-TB-TOT-ESP6(WS-COMP,    28, WS-ESP)
                    IF AUX1-CS-RAMO-ATIVIDADE = 8
                       ADD 1 TO WS-TB-TOT-ESP9(WS-COMP, WS-UF, WS-ESP)
                                WS-TB-TOT-ESP9(WS-COMP,    28, WS-ESP)
                    ELSE
                       ADD 1 TO WS-TB-TOT-ESP8(WS-COMP, WS-UF, WS-ESP)
                                WS-TB-TOT-ESP8(WS-COMP,    28, WS-ESP)
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF WS-TB-VL-RUBRICA-AUX (301) = ZEROS
                    ADD 1 TO WS-TB-TOT-ESP6(WS-COMP, WS-UF, WS-ESP)
                             WS-TB-TOT-ESP6(WS-COMP,    28, WS-ESP)
                    IF AUX1-CS-RAMO-ATIVIDADE = 8
                       ADD 1 TO WS-TB-TOT-ESP9(WS-COMP, WS-UF, WS-ESP)
                                WS-TB-TOT-ESP9(WS-COMP,    28, WS-ESP)
                    ELSE
                       ADD 1 TO WS-TB-TOT-ESP8(WS-COMP, WS-UF, WS-ESP)
                                WS-TB-TOT-ESP8(WS-COMP,    28, WS-ESP).

       2634-FIM. EXIT.
      /
       2635-TOT-DESC.

           ADD WS-IR-PREV TO WS-TOT-IR-PREV (WS-COMP, WS-UF, WS-TIPO).
           ADD WS-IR-PREV TO WS-TOT-IR-PREV (WS-COMP,    28, WS-TIPO).

           IF WS-TIPO = 6
              ADD WS-IR-UNIAO TO
                           WS-TOT-IR-UNIAO(WS-COMP, WS-UF, 7)
                           WS-TOT-IR-UNIAO(WS-COMP,    28, 7)
           ELSE
              IF WS-TIPO NOT = 8 AND 9
                 ADD WS-IR-UNIAO TO
                           WS-TOT-IR-UNIAO(WS-COMP, WS-UF, WS-TIPO)
                           WS-TOT-IR-UNIAO(WS-COMP,    28, WS-TIPO).

           ADD  WS-TB-VL-RUBRICA-AUX (206) TO
                WS-TOT-CONT-PREV (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONT-PREV (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (208) TO
                WS-TOT-CONT-PREV (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONT-PREV (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (212) TO
                WS-TOT-CONT-PREV (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONT-PREV (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (202) TO
                WS-TOT-PA        (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-PA        (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (210) TO
                WS-TOT-PA        (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-PA        (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (213) TO
                WS-TOT-PA        (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-PA        (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (203) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (214) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (215) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (216) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (217) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (219) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (220) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (221) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (222) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (223) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (224) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (225) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (226) TO
                WS-TOT-CONSIG    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CONSIG    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (104) TO
                WS-TOT-13-SAL    (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-13-SAL    (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (105) TO
                WS-TOT-SF        (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-SF        (WS-COMP,    28, WS-TIPO).
           ADD  WS-TB-VL-RUBRICA-AUX (121) TO
                WS-TOT-CPMF      (WS-COMP, WS-UF, WS-TIPO)
                WS-TOT-CPMF      (WS-COMP,    28, WS-TIPO).

           MOVE ZEROS TO WS-TOT-VL-CONS.

           PERFORM 2638-ADD-CONSIG THRU 2638-FIM
                   VARYING W77-IND-CONS FROM 1 BY 1
                     UNTIL W77-IND-CONS > 16.

           IF WS-TOT-VL-CONS < WS-TB-VL-RUBRICA-AUX(203) +
                               WS-TB-VL-RUBRICA-AUX(214)
              COMPUTE WS-TOT-VL-CONS =
                      WS-TB-VL-RUBRICA-AUX(203) +
                      WS-TB-VL-RUBRICA-AUX(214) -
                      WS-TOT-VL-CONS
              ADD WS-TOT-VL-CONS
               TO WS-TOT-COD-CONSIG
                  (WS-COMP, WS-UF, WS-TIPO, 7)
                   WS-TOT-COD-CONSIG
                  (WS-COMP,    28, WS-TIPO, 7).

           ADD  WS-TB-VL-RUBRICA-AUX (215) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 7)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 7).
           ADD  WS-TB-VL-RUBRICA-AUX (216) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 10)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 10).
           ADD  WS-TB-VL-RUBRICA-AUX (217) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 10)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 10).
           ADD  WS-TB-VL-RUBRICA-AUX (222) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 14)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 14).
           ADD  WS-TB-VL-RUBRICA-AUX (219) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 9)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 9).
           ADD  WS-TB-VL-RUBRICA-AUX (220) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 9)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 9).
           ADD  WS-TB-VL-RUBRICA-AUX (221) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 9)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 9).
           ADD  WS-TB-VL-RUBRICA-AUX (223) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 9)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 9).
           ADD  WS-TB-VL-RUBRICA-AUX (224) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 9)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 9).
           ADD  WS-TB-VL-RUBRICA-AUX (225) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 9)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 9).
           ADD  WS-TB-VL-RUBRICA-AUX (226) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 9)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 9).

           IF WS-RUB-NOVA NOT = ZEROS
              ADD WS-RUB-NOVA
               TO WS-TOT-COD-CONSIG
                  (WS-COMP, WS-UF, WS-TIPO, 7)
                   WS-TOT-COD-CONSIG
                  (WS-COMP,    28, WS-TIPO, 7).

      *****ALTERACAO P/ SEPARAR RUB. 218 (13o.PAGO COMTETENCIAS ANTERIO-
      *    RES). SUBTRAI RUB. 218 DA OCORRENCIA 7 (DEBITO COM INSS)  E
      *    SOMA NA OCORRENCIA 15 (EMPRESTIMO CEF), QUE NUNCA FOI USADA.
      *
      *    ESTA SUBTRACAO ESTA SENDO INIBIDA POIS A PARTIR DE 06/2008
      *    ESTE VALOR NAO ESTA SENDO SOMADO NA OCORR. 7.
      *
      *    SUBTRACT  WS-TB-VL-RUBRICA-AUX (218) FROM
      *              WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 7)
      *              WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 7).

           ADD  WS-TB-VL-RUBRICA-AUX (218) TO
                WS-TOT-COD-CONSIG   (WS-COMP, WS-UF, WS-TIPO, 15)
                WS-TOT-COD-CONSIG   (WS-COMP,    28, WS-TIPO, 15).

       2635-FIM.  EXIT.
      /
       2636-TOT-VL-BRUTO.

           COMPUTE WS-TOT-BRUTO = WS-TOT-BRUTO +
                   WS-TB-VL-RUBRICA-AUX(W77-IND-RUB).

       2636-FIM. EXIT.

       2637-TOT-VL-DESC.

            COMPUTE WS-TOT-DESC = WS-TOT-DESC +
                    WS-TB-VL-RUBRICA-AUX(W77-IND-RUB).

            IF W77-IND-RUB > 226
               ADD WS-TB-VL-RUBRICA-AUX(W77-IND-RUB) TO WS-RUB-NOVA.

       2637-FIM. EXIT.

       2638-ADD-CONSIG.

           IF W77-IND-CONS = 2 OR 3 OR 8 OR 9 OR 10 OR 14 OR 15
              GO TO 2638-FIM.
           IF W77-IND-CONS = 1
              IF WS-TB-VL-RUBRICA-AUX (904 + W77-IND-CONS) >
                 (WS-TB-VL-RUBRICA-AUX (203) +
                  WS-TB-VL-RUBRICA-AUX (214))
                 ADD WS-TB-VL-RUBRICA-AUX (203)
                     WS-TB-VL-RUBRICA-AUX (214)
                  TO WS-TOT-COD-CONSIG
                     (WS-COMP, WS-UF, WS-TIPO, W77-IND-CONS)
                      WS-TOT-COD-CONSIG
                     (WS-COMP,    28, WS-TIPO, W77-IND-CONS)
                      WS-TOT-VL-CONS
              ELSE
                 ADD WS-TB-VL-RUBRICA-AUX (904 + W77-IND-CONS)
                  TO WS-TOT-COD-CONSIG
                     (WS-COMP, WS-UF, WS-TIPO, W77-IND-CONS)
                      WS-TOT-COD-CONSIG
                     (WS-COMP,    28, WS-TIPO, W77-IND-CONS)
                      WS-TOT-VL-CONS
           ELSE
              IF (WS-TB-VL-RUBRICA-AUX (905 + W77-IND-CONS) +
                  WS-TOT-VL-CONS) >
                  (WS-TB-VL-RUBRICA-AUX (203) +
                   WS-TB-VL-RUBRICA-AUX (214))
                  COMPUTE WS-TB-VL-RUBRICA-AUX (905 + W77-IND-CONS) =
                          WS-TB-VL-RUBRICA-AUX (203) +
                          WS-TB-VL-RUBRICA-AUX (214) -
                          WS-TOT-VL-CONS
                  ADD WS-TB-VL-RUBRICA-AUX (905 + W77-IND-CONS)
                   TO WS-TOT-COD-CONSIG
                     (WS-COMP, WS-UF, WS-TIPO, W77-IND-CONS)
                      WS-TOT-COD-CONSIG
                     (WS-COMP,    28, WS-TIPO, W77-IND-CONS)
                      WS-TOT-VL-CONS
              ELSE
                  ADD WS-TB-VL-RUBRICA-AUX (905 + W77-IND-CONS)
                   TO WS-TOT-COD-CONSIG
                     (WS-COMP, WS-UF, WS-TIPO, W77-IND-CONS)
                      WS-TOT-COD-CONSIG
                     (WS-COMP,    28, WS-TIPO, W77-IND-CONS)
                      WS-TOT-VL-CONS.

       2638-FIM. EXIT.

       2639-MONTA-REG-IR.

           COMPUTE CPG075-VL-IR =
                (WS-TB-VL-RUBRICA-AUX(201) + WS-TB-VL-RUBRICA-AUX(204) +
                 WS-TB-VL-RUBRICA-AUX(205) + WS-TB-VL-RUBRICA-AUX(207) +
                 WS-TB-VL-RUBRICA-AUX(209) + WS-TB-VL-RUBRICA-AUX(211)).

           IF CPG075-VL-IR = 0
              GO TO 2639-FIM-MONTA-REG-IR.

      * COMO O IR SERA RECOLHIDO PELO SISPAGBEN SO DEVEM PERMANECER OS V
      * DE IR DE BENEFICIOS DE ACORDO INTERNACIONAL E EMPRESAS CONVENIAD
      * PARTIR DE 09/02/2015.
           MOVE AUX1-NU-NB              TO  CPG075-NU-NB.
           MOVE AUX1-DT-INI-PERIODO     TO  CPG075-DT-INI-PERIODO.
           MOVE AUX1-DT-FIM-PERIODO     TO  CPG075-DT-FIM-PERIODO.
           MOVE AUX1-CS-NATUR-CREDITO   TO  CPG075-CS-NATUR-CREDITO.
           MOVE AUX1-DT-INI-VALIDADE    TO  CPG075-DT-INI-VALID.
           COMPUTE CPG075-VL-BRUTO = WS-TOT-BRUTO + WS-BRUTO-RUB-105.

           IF WS-TB-VL-RUBRICA-AUX(204) > 0
              MOVE 88 TO  CPG075-TIPO-IR
           ELSE
              IF WS-TB-VL-RUBRICA-AUX(201) > 0
                 MOVE 87 TO  CPG075-TIPO-IR
              ELSE
                 PERFORM 2639A-ACESSA-IN-IR-EXT THRU
                         2639A-FIM-ACESSA-IN-IR-EXT.

           MOVE AUX1-CS-ORIGEM-CREDITO  TO  CPG075-CS-ORIGEM-CREDITO
           MOVE AUX1-DT-MOV-CREDITO     TO  CPG075-D2-MOV-CREDITO
           MOVE 0                       TO  CPG075-CS-OPERACAO.

      * COMO O IR SERA RECOLHIDO PELO SISPAGBEN SO DEVEM PERMANECER OS
      * VALORES DE IR DE BENEFICIOS DE ACORDO INTERNACIONAL E EMPRESAS
      * CONVENIADAS DE BENEFICIOS COM INICIO DE VALIDADE A PARTIR DE
      * 09/02/2015.

           IF (AUX1-DT-INI-VALIDADE < 20150209)
              WRITE REG-DSUBCPG075
           ELSE
              IF (AUX1-ID-BANCO = 998 OR 996)
                 WRITE REG-DSUBCPG075.

      * INCLUI ESSE ARQUIVO PARA GRAVAR O IR COMO ERA ANTES DA
      * ALTERACAO PARA O SISPAGBEN
           MOVE REG-DSUBCPG075 TO REG-DSUBCPG075AUX.
           WRITE REG-DSUBCPG075AUX.
           MOVE ZEROS TO REG-DSUBCPG075.
           MOVE ZEROS TO REG-DSUBCPG075AUX.

       2639-FIM-MONTA-REG-IR. EXIT.

       2639A-ACESSA-IN-IR-EXT.


           FIND ST-BENEF-NB AT NU-NB = AUX1-NU-NB
                ON EXCEPTION
                   IF DMSTATUS(NOTFOUND)
                      MOVE "ERRO FIND ST-BENEF-NB ROT. 2639"
                        TO  WS-PARAM1-ZDISPLAY
                      MOVE  AUX1-NU-NB TO  WS-PARAM2-ZDISPLAY
                      CALL "ZDISPLAY2 IN DTP/L1" USING
                                                WS-PARAM1-ZDISPLAY,
                                                WS-PARAM2-ZDISPLAY,
                                                WS-INTEIRO-ZDISPLAY.

      *                CALL SYSTEM DMTERMINATE.

            IF IN-IR-EXTERIOR OF DS-BENEFICIO
                MOVE 88 TO CPG075-TIPO-IR
            ELSE
                MOVE 87 TO CPG075-TIPO-IR.

       2639A-FIM-ACESSA-IN-IR-EXT. EXIT.
      /
       2639-II-GRAVA-EPU-REL.
      *---------------------*
      *
           IF AUX1-CS-UF < 02 OR > 28
              STRING "NB : " , AUX1-NU-NB ,
                     " COM UF INVALIDO: " , AUX1-CS-UF ,
                     " OL: " , AUX1-ID-OL-MANUTENCAO
              DELIMITED BY SIZE INTO WS-MENSAGEM-ZDISPLAY
              CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
                                              WS-INTEIRO-ZDISPLAY
              MOVE 02 TO AUX1-CS-UF.
      *
           IF AUX1-CS-TRAT-ECT
              ADD 1 TO WS-QT-EPU-REL ( 1 , 1 )
                       WS-QT-EPU-REL ( AUX1-CS-UF , 1 )
              ADD WS-VL-RUB-301-SEM-ALT
               TO WS-VL-EPU-REL      ( 1 , 1 )
                  WS-VL-EPU-REL      ( AUX1-CS-UF , 1 )
           ELSE
              IF AUX1-CS-TRAT-REDE
                 ADD 1 TO WS-QT-EPU-REL ( 1 , 2 )
                          WS-QT-EPU-REL ( AUX1-CS-UF , 2 )
                 ADD WS-VL-RUB-301-SEM-ALT
                  TO WS-VL-EPU-REL      ( 1 , 2 )
                     WS-VL-EPU-REL      ( AUX1-CS-UF , 2 )
              ELSE
                 IF AUX1-CS-TRAT-ESTATUTARIO
                    ADD 1 TO WS-QT-EPU-REL ( 1 , 3 )
                             WS-QT-EPU-REL ( AUX1-CS-UF , 3 )
                    ADD WS-VL-RUB-301-SEM-ALT
                     TO WS-VL-EPU-REL      ( 1 , 3 )
                        WS-VL-EPU-REL      ( AUX1-CS-UF , 3 )
                 ELSE
                    IF AUX1-CS-ESPECIE = 58 OR 59
                       ADD 1 TO WS-QT-EPU-REL ( 1 , 4 )
                                WS-QT-EPU-REL ( AUX1-CS-UF , 4 )
                       ADD WS-VL-RUB-301-SEM-ALT
                        TO WS-VL-EPU-REL      ( 1 , 4 )
                           WS-VL-EPU-REL      ( AUX1-CS-UF , 4 )
                    ELSE
                       IF AUX1-CS-ESPECIE = 56
                          ADD 1 TO WS-QT-EPU-REL ( 1 , 5 )
                                   WS-QT-EPU-REL ( AUX1-CS-UF , 5 )
                          ADD WS-VL-RUB-301-SEM-ALT
                           TO WS-VL-EPU-REL      ( 1 , 5 )
                              WS-VL-EPU-REL      ( AUX1-CS-UF , 5 )
                       ELSE
                          MOVE AUX1-CS-ESPECIE TO WS-ESPECIES-LOAS
                          IF ESPECIES-LOAS OR ESPECIES-RMV
                             OR AUX1-CS-ESPECIE = 60
                             PERFORM 2639-AC-LOAS THRU 2639-AC-LOAS-FIM
      *>>>>>                 ADD 1 TO WS-QT-EPU-REL ( 1 , 6 )
      *>>>>>                          WS-QT-EPU-REL ( AUX1-CS-UF , 6 )
      *>>>>>                 ADD WS-VL-RUB-301-SEM-ALT
      *>>>>>                  TO WS-VL-EPU-REL      ( 1 , 6 )
      *>>>>>                     WS-VL-EPU-REL      ( AUX1-CS-UF , 6 )
                          ELSE
                             IF AUX1-CS-ESPECIE = 89 AND
                                AUX1-CS-TRATAMENTO = 81
                                ADD 1 TO WS-QT-EPU-REL ( 1 , 7 )
                                         WS-QT-EPU-REL ( AUX1-CS-UF , 7)
                                ADD WS-VL-RUB-301-SEM-ALT
                                 TO WS-VL-EPU-REL      ( 1 , 7 )
                                    WS-VL-EPU-REL      ( AUX1-CS-UF , 7)
                             ELSE
                               IF AUX1-CS-ESPECIE = 96
                                  ADD 1 TO WS-QT-EPU-REL(1, 10)
                                           WS-QT-EPU-REL(AUX1-CS-UF, 10)
                                  ADD WS-VL-RUB-301-SEM-ALT
                                   TO WS-VL-EPU-REL     (1, 10)
                                      WS-VL-EPU-REL     (AUX1-CS-UF, 10)
                               ELSE
                                ADD 1 TO WS-REGS-DESP-EPU
                                ADD WS-VL-RUB-301-SEM-ALT
                                 TO WS-VL-DESP-EPU
                                IF WS-REGS-DESP-EPU < 10000
                                   MOVE SPACES TO WS-MENSAGEM-ZDISPLAY
                                STRING "NAO SE ENQUADRA CRITERIOS DO EPU
      -                        " NB: ",
                                AUX1-NU-NB," ESPE.: ",AUX1-CS-ESPECIE,
                                " TRATA.: "AUX1-CS-TRATAMENTO
                                DELIMITED BY SIZE
                                INTO WS-MENSAGEM-ZDISPLAY
                                CALL "ZDISPLAY IN DTP/L1" USING
                                      WS-MENSAGEM-ZDISPLAY,
                                      WS-INTEIRO-ZDISPLAY.
      *
       2639-II-FIM.  EXIT.
      *-----------*
       2639-AC-LOAS.
           IF AUX1-CS-ESPECIE = 87 OR 88
               ADD 1 TO WS-QT-EPU-REL ( 1 , 6 )
                        WS-QT-EPU-REL ( AUX1-CS-UF , 6 )
               ADD WS-VL-RUB-301-SEM-ALT
                TO WS-VL-EPU-REL      ( 1 , 6 )
                   WS-VL-EPU-REL      ( AUX1-CS-UF , 6 )
           ELSE
             IF AUX1-CS-ESPECIE = 11 OR 30
               ADD 1 TO WS-QT-EPU-REL ( 1 , 8 )
                        WS-QT-EPU-REL ( AUX1-CS-UF , 8 )
               ADD WS-VL-RUB-301-SEM-ALT
                TO WS-VL-EPU-REL      ( 1 , 8 )
                   WS-VL-EPU-REL      ( AUX1-CS-UF , 8 )
             ELSE
               IF AUX1-CS-ESPECIE = 60 AND AUX1-CS-TRATAMENTO = 82
                  ADD 1 TO WS-QT-EPU-REL ( 1 , 11 )
                           WS-QT-EPU-REL ( AUX1-CS-UF ,11)
                  IF WS-VL-RUB-301-SEM-ALT = ZEROS
                     MOVE AUX1-VL-LIQUIDO-CRED TO WS-VL-RUB-301-SEM-ALT
                     MOVE SPACES TO WS-MENSAGEM-ZDISPLAY
                     STRING "ZIKA SEM 301 - NB ",AUX1-NU-NB
                           DELIMITED BY SIZE
                           INTO WS-MENSAGEM-ZDISPLAY
                           CALL "ZDISPLAY IN DTP/L1" USING
                           WS-MENSAGEM-ZDISPLAY,
                           WS-INTEIRO-ZDISPLAY
                           ADD WS-VL-RUB-301-SEM-ALT
                           TO WS-VL-EPU-REL      ( 1 , 11 )
                             WS-VL-EPU-REL      ( AUX1-CS-UF ,11)
                  ELSE
                     ADD WS-VL-RUB-301-SEM-ALT
                      TO WS-VL-EPU-REL      ( 1 , 11 )
                         WS-VL-EPU-REL      ( AUX1-CS-UF ,11)

             ELSE
               ADD 1 TO WS-QT-EPU-REL ( 1 , 9 )
                        WS-QT-EPU-REL ( AUX1-CS-UF , 9 )
               ADD WS-VL-RUB-301-SEM-ALT
                TO WS-VL-EPU-REL      ( 1 , 9 )
                   WS-VL-EPU-REL      ( AUX1-CS-UF , 9 ).
       2639-AC-LOAS-FIM. EXIT.
      *
      /
       2640-MONTA-REG-REJ-CREDITOS.

            MOVE "FASE MONTA-REG-REJ-CREDITOS " TO W77-FASE.

            MOVE AUX1-NU-NB                  TO  CP10-NU-NB.
            MOVE AUX1-CS-NATUR-CREDITO       TO  CP10-CS-NATUR-CREDITO.
            MOVE AUX1-DT-FIM-PERIODO         TO  CP10-DT-FIM-PERIODO.
            MOVE AUX1-DT-INI-PERIODO         TO  CP10-DT-INI-PERIODO.
            MOVE AUX1-MEIO-PAGTO             TO  CP10-CS-MEIO-PAGTO.
            MOVE AUX1-DT-INI-VALIDADE        TO  CP10-DT-INI-VALIDADE.
            MOVE AUX1-DT-FIM-VALIDADE        TO  CP10-DT-FIM-VALIDADE.
            MOVE AUX1-CS-ORIGEM-CREDITO      TO  CP10-CS-ORIGEM-CREDITO.
            MOVE AUX1-DT-MOV-CREDITO         TO  CP10-DT-MOV-CREDITO.

            IF PAR-MOV-CREDITO  =  "CONC"      AND
               AUX1-SEQ-PROC    >  2           AND
               AUX1-DT-DDB      <  19930810
             COMPUTE AUX1-VL-LIQUIDO-CRED = AUX1-VL-LIQUIDO-CRED / 1000.

            MOVE AUX1-VL-LIQUIDO-CRED        TO  CP10-VL-LIQUIDO-CRED.
            MOVE AUX1-CS-ESPECIE             TO  CP10-CS-ESPECIE.
            MOVE AUX1-ID-BANCO               TO  CP10-ID-BANCO.
            MOVE AUX1-CS-UF                  TO  CP10-CS-UF.
            COMPUTE CP10-NU-SEQ-PROC = AUX1-SEQ-PROC
            MOVE AUX1-ID-NIT                 TO  CP10-ID-NIT.
            MOVE AUX1-NU-NIT-DV1             TO  CP10-NU-NIT-DV1.
            MOVE AUX-IN-NB-OK                TO   CP10-IN-NB-OK.
            MOVE AUX-IN-NATUR-CREDITO-OK TO   CP10-IN-NATUR-CREDITO-OK.
            MOVE AUX-IN-PERIODO-OK       TO   CP10-IN-PERIODO-OK.
            MOVE AUX-IN-VALIDADE-OK      TO   CP10-IN-VALIDADE-OK.
            MOVE AUX-IN-MEIO-PAGTO-OK    TO   CP10-IN-MEIO-PAGTO-OK.
            MOVE AUX-IN-BANCO-OK         TO   CP10-IN-BANCO-OK.
            MOVE AUX-IN-UF-OK            TO   CP10-IN-UF-OK.
            MOVE AUX-IN-VALOR-OK         TO   CP10-IN-VALOR-OK.
            MOVE AUX-IN-NIT-OK           TO   CP10-IN-NIT-OK.
            WRITE CP10-REG1.
            ADD 1 TO W77-GRAVADOS.

       2640-FIM. EXIT.

       2650-ATUAL-DATA-MOV-CREDITO.

            MOVE "FASE ATUAL-DATA-MOV-CREDITO " TO W77-FASE.

            IF PAR-SEAS = SPACES
               IF PAR-MOV-CREDITO = "CONC"
                  MOVE 1 TO AUX-ORIGEM
                            AUX1-SEQ-PROC
                  PERFORM 2651-ATUAL-DATA-LEIT-CONC THRU 2651-FIM
               ELSE
                  IF PAR-MOV-CREDITO ="MANU"
                  AND NAO-E-CREDITO-DE-CALAMIDADE
                     MOVE 2 TO AUX-ORIGEM
                     PERFORM 2652-ATUAL-DATA-LEIT-MANU THRU 2652-FIM.

       2650-FIM. EXIT.

       2651-ATUAL-DATA-LEIT-CONC.

            LOCK NEXT S2-CTRL-PROC AT D2-COMPET-PROC = AUX-COMP-CRED
                            AND CS-ORIGEM-CREDITO = AUX-ORIGEM
                            AND NU-SEQ-PROC = W77-NU-SEQ-CONCESSAO
                            ON EXCEPTION
                               IF DMSTATUS (NOTFOUND)
                                  GO TO 2651-FIM
                               ELSE
                                  MOVE "ERRO ROT 2651" TO
                                       WS-MENSAGEM-ZDISPLAY
                                  CALL "ZDISPLAY IN DTP/L1" USING
                                        WS-MENSAGEM-ZDISPLAY,
                                        WS-INTEIRO-ZDISPLAY
                                  CALL SYSTEM DMTERMINATE.

           MOVE WS-DATA-SIS-INV TO D2-PROC-CONPAG.

           BEGIN-TRANSACTION NO-AUDIT DS-RESTART OF BDBENCRED.

           PERFORM REN-DS-CTRL-PROC-CRED THRU
                   REN-DS-CTRL-PROC-CRED-EXIT.

      *GATILHO      DISPLAY "INIBIU STORE1 "
      * GO TO STORE1.
      *GATILHO      DISPLAY "INIBIU STORE1 "
      **
           STORE DS-CTRL-PROC-CRED
                 ON EXCEPTION
                 MOVE "ERRO STORE ROT 2651" TO WS-MENSAGEM-ZDISPLAY
                 CALL "ZDISPLAY IN DTP/L1" USING
                                         WS-MENSAGEM-ZDISPLAY,
                                         WS-INTEIRO-ZDISPLAY
                  CALL SYSTEM DMTERMINATE.
       STORE1.
           END-TRANSACTION NO-AUDIT DS-RESTART OF BDBENCRED.

      *      MOVE WS-DATA-SIS-INV TO D2-PROC-CONPAG.
      *      COMPUTE AUX1-SEQ-PROC = AUX1-SEQ-PROC + 1.
      *      GO 2651-ATUAL-DATA-LEIT-CONC.

       2651-FIM. EXIT.

       2652-ATUAL-DATA-LEIT-MANU.

           LOCK S2-CTRL-PROC AT D2-COMPET-PROC = AUX-COMP-CRED
                           AND CS-ORIGEM-CREDITO = AUX-ORIGEM
                           AND NU-SEQ-PROC = 1
                           ON EXCEPTION
                              IF DMSTATUS (NOTFOUND)
                                 MOVE "NOTFOUND CTRL-PROC ROT 2652" TO
                                       WS-MENSAGEM-ZDISPLAY
                                 CALL "ZDISPLAY IN DTP/L1" USING
                                       WS-MENSAGEM-ZDISPLAY,
                                       WS-INTEIRO-ZDISPLAY
                                 MOVE "DT COMPET PROC=" TO
                                       WS-PARAM1-ZDISPLAY
                                 MOVE AUX-COMP-CRED     TO
                                      WS-PARAM2-ZDISPLAY
                                 CALL "ZDISPLAY2 IN DTP/L1" USING
                                       WS-PARAM1-ZDISPLAY,
                                       WS-PARAM2-ZDISPLAY,
                                       WS-INTEIRO-ZDISPLAY
                                 MOVE "ORIGEM-CREDITO=" TO
                                       WS-PARAM1-ZDISPLAY
                                 MOVE AUX-ORIGEM TO WS-PARAM2-ZDISPLAY
                                 CALL "ZDISPLAY2 IN DTP/L1" USING
                                       WS-PARAM1-ZDISPLAY,
                                       WS-PARAM2-ZDISPLAY,
                                       WS-INTEIRO-ZDISPLAY
                                 MOVE "NU-SEQ-PROC=" TO
                                       WS-PARAM1-ZDISPLAY
                                 MOVE 1 TO WS-PARAM2-ZDISPLAY
                                 CALL "ZDISPLAY2 IN DTP/L1" USING
                                       WS-PARAM1-ZDISPLAY,
                                       WS-PARAM2-ZDISPLAY,
                                       WS-INTEIRO-ZDISPLAY
                                 GO TO 2652-FIM
                              ELSE
                                 MOVE "ERRO ROT 2652" TO
                                       WS-MENSAGEM-ZDISPLAY
                                 CALL "ZDISPLAY IN DTP/L1" USING
                                       WS-MENSAGEM-ZDISPLAY,
                                       WS-INTEIRO-ZDISPLAY
                                 CALL SYSTEM DMTERMINATE.

           MOVE WS-DATA-SIS-INV TO D2-PROC-CONPAG.

           BEGIN-TRANSACTION NO-AUDIT DS-RESTART OF BDBENCRED.

          PERFORM REN-DS-CTRL-PROC-CRED THRU REN-DS-CTRL-PROC-CRED-EXIT.

      *GATILHO     DISPLAY "INIBIU STORE2 "
      * GO TO STORE2.
      *GATILHO     DISPLAY "INIBIU STORE2 "

           STORE DS-CTRL-PROC-CRED
                 ON EXCEPTION
                 MOVE "ERRO STORE ROT 2652" TO WS-MENSAGEM-ZDISPLAY
                 CALL "ZDISPLAY IN DTP/L1" USING
                                           WS-MENSAGEM-ZDISPLAY,
                                           WS-INTEIRO-ZDISPLAY
                 CALL SYSTEM DMTERMINATE.
       STORE2.
           END-TRANSACTION NO-AUDIT DS-RESTART OF BDBENCRED.

       2652-FIM.
            EXIT.

       2653-ATUAL-PROCESSAMENTO-CALAM.

           BEGIN-TRANSACTION NO-AUDIT DS-RESTART OF BDBENCRED.

           SET ST-CALAMIDADE-ID  TO BEGINNING
           LOCK NEXT ST-CALAMIDADE-ID AT
                             ID-CALAMIDADE = WS-SISPG-COD-CALAMIDADE
             ON EXCEPTION
                IF DMSTATUS (NOTFOUND)
                   MOVE SPACES TO WS-PARAM1-ZDISPLAY
                   MOVE "CALAM NAO LOCALIZADA ROT. 2653"
                     TO  WS-PARAM1-ZDISPLAY
                   MOVE  WS-SISPG-COD-CALAMIDADE TO  WS-PARAM2-ZDISPLAY
                   CALL "ZDISPLAY2 IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                   CALL SYSTEM DMTERMINATE
                ELSE
                   MOVE SPACES TO WS-PARAM1-ZDISPLAY
                   MOVE "ERRO LOCK DS-CALAMIDADE ROT. 2653"
                     TO  WS-PARAM1-ZDISPLAY
                   MOVE  WS-SISPG-COD-CALAMIDADE TO  WS-PARAM2-ZDISPLAY
                   CALL "ZDISPLAY2 IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                   CALL SYSTEM DMTERMINATE.

           COMPUTE IN-PROCESSADA OF DS-CALAMIDADE = TRUE

           STORE DS-CALAMIDADE
              ON  EXCEPTION
                   MOVE SPACES TO WS-PARAM1-ZDISPLAY
                   MOVE "ERRO STORE DS-CALAMIDADE ROT. 2653"
                     TO  WS-PARAM1-ZDISPLAY
                   MOVE  WS-SISPG-COD-CALAMIDADE TO  WS-PARAM2-ZDISPLAY
                   CALL "ZDISPLAY2 IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                   CALL SYSTEM DMTERMINATE.

           END-TRANSACTION NO-AUDIT DS-RESTART OF BDBENCRED.

       2653-FIM.
            EXIT.


       2660-ROTINA-CHECKPOINT.

            MOVE "FASE ROTINA CHECKPOINT " TO W77-FASE.

            IF PAR-MOV-CREDITO = "ALTE"
               CLOSE BDCONPAGBEN
                     ON EXCEPTION
                        CALL SYSTEM DMTERMINATE.

            IF PAR-MOV-CREDITO NOT = "APAG"
               CLOSE BDBENCRED
                     ON EXCEPTION
                        CALL SYSTEM DMTERMINATE.

            IF PAR-MOV-CREDITO = "APAG" OR "ALTE"
               CLOSE BDSISBEN
                     ON EXCEPTION
                        CALL SYSTEM DMTERMINATE.

            IF PAR-SEAS = SPACES
               IF PAR-MOV-CREDITO = "CONC" OR "MANU" OR "CONV"
                  CLOSE BDSISBEN
                        ON EXCEPTION
                           CALL SYSTEM DMTERMINATE.

            CALL SYSTEM BREAKOUT.

            PERFORM 5000-CHECKPOINT.

            IF PAR-MOV-CREDITO = "ALTE"
               OPEN INQUIRY BDCONPAGBEN
                    ON EXCEPTION
                       CALL SYSTEM DMTERMINATE.

            IF PAR-MOV-CREDITO NOT = "APAG"
               OPEN UPDATE BDBENCRED
                    ON EXCEPTION
                       CALL SYSTEM DMTERMINATE.

            IF PAR-MOV-CREDITO = "APAG" OR "ALTE"
               OPEN INQUIRY BDSISBEN
                    ON EXCEPTION
                       CALL SYSTEM DMTERMINATE.

            IF PAR-SEAS = SPACES
               IF PAR-MOV-CREDITO = "CONC" OR "MANU" OR "CONV"
                  OPEN INQUIRY BDSISBEN
                       ON EXCEPTION
                          CALL SYSTEM DMTERMINATE.

            IF PAR-MOV-CREDITO = "APAG"
               SET ST-CALC-ACAO-JUD   TO BEGINNING
               PERFORM 2664-POSICIONA-CALC-ACAO-JUD THRU 2664-FIM.

            IF PAR-MOV-CREDITO = "ALTE"
               SET S2-CRED-ALTER-OL   TO BEGINNING
               PERFORM 2663-POSICIONA-MOV-CRED-ALTE THRU 2663-FIM.

            IF PAR-SEAS = "SEAS"
               SET ST-NIT-MOV-SEAS TO BEGINNING
               PERFORM 2665-POSICIONA-MOV-CRED-SEAS THRU 2665-FIM
            ELSE
               IF PAR-MOV-CREDITO = "MANU" OR "CONV"
                  IF E-CREDITO-DE-CALAMIDADE
                     SET ST-CRED-CALAM-NB  TO BEGINNING
                     PERFORM 2661B-POSICIONA-CRED-CALAM THRU 2661B-FIM
                  ELSE
                     SET ST-CRED-MANUT-NB  TO BEGINNING
                     PERFORM 2661-POSICIONA-MOV-CRED-MANU THRU 2661-FIM
               ELSE
                  IF PAR-MOV-CREDITO = "CONC"
                     SET ST-CRED-CONC-NB TO BEGINNING
                     PERFORM 2662-POSICIONA-MOV-CRED-CONC THRU 2662-FIM.

            MOVE 0 TO AUX-NU-CONT-CHECKPOINT.

       2660-FIM. EXIT.

       2661-POSICIONA-MOV-CRED-MANU.

            FIND FIRST ST-CRED-MANUT-NB AT
                       CS-ORIGEM-CREDITO OF DS-MOV-CRED-MANUT
                                         = AUX1-CS-ORIGEM-CREDITO AND
                       NU-SEQ-PROC       OF DS-MOV-CRED-MANUT
                                         = AUX1-SEQ-PROC          AND
                       NU-NB             OF DS-MOV-CRED-MANUT
                                         = AUX1-NU-NB             AND
                       IN-CRED-ATRASADO  OF DS-MOV-CRED-MANUT
                                         = AUX1-IN-CRED-ATRASADO
                 ON EXCEPTION
                    IF DMSTATUS (NOTFOUND)
                       MOVE "ERRO - NOTFOUND ROT 2661" TO
                             WS-MENSAGEM-ZDISPLAY
                       CALL "ZDISPLAY IN DTP/L1" USING
                                                 WS-MENSAGEM-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
                       CALL SYSTEM DMTERMINATE
                    ELSE
                       MOVE "ERRO FIND DS-MOV-CRED-MANUT" TO
                             WS-MENSAGEM-ZDISPLAY
                       CALL "ZDISPLAY IN DTP/L1" USING
                                                 WS-MENSAGEM-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
                       CALL SYSTEM DMTERMINATE.

            ADD 1 TO W77-LIDOS-MANUT WS-LIDOS-MANUT.

       2661-FIM. EXIT.

       2661B-POSICIONA-CRED-CALAM.

            FIND FIRST ST-CRED-CALAM-NB AT
                       ID-CALAMIDADE     OF DS-MOV-CRED-CALAM
                                         =  WS-SISPG-COD-CALAMIDADE AND
                       CS-ORIGEM-CREDITO OF DS-MOV-CRED-CALAM
                                         = AUX1-CS-ORIGEM-CREDITO   AND
                       NU-SEQ-PROC       OF DS-MOV-CRED-CALAM
                                         = AUX1-SEQ-PROC            AND
                       NU-NB             OF DS-MOV-CRED-CALAM
                                         = AUX1-NU-NB               AND
                       IN-CRED-ATRASADO  OF DS-MOV-CRED-CALAM
                                         = AUX1-IN-CRED-ATRASADO
                 ON EXCEPTION
                    IF DMSTATUS (NOTFOUND)
                       MOVE "ERRO - NOTFOUND ROT 2661B" TO
                             WS-MENSAGEM-ZDISPLAY
                       CALL "ZDISPLAY IN DTP/L1" USING
                                                 WS-MENSAGEM-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
                       CALL SYSTEM DMTERMINATE
                    ELSE
                       MOVE "ERRO FIND DS-MOV-CRED-CALAM" TO
                             WS-MENSAGEM-ZDISPLAY
                       CALL "ZDISPLAY IN DTP/L1" USING
                                                 WS-MENSAGEM-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
                       CALL SYSTEM DMTERMINATE.

            ADD 1 TO W77-LIDOS-MANUT WS-LIDOS-MANUT.

       2661B-FIM. EXIT.

       2662-POSICIONA-MOV-CRED-CONC.

            FIND FIRST ST-CRED-CONC-NB  AT
                       CS-ORIGEM-CREDITO OF DS-MOV-CRED-CONC
                                         = AUX1-CS-ORIGEM-CREDITO AND
                       NU-SEQ-PROC       OF DS-MOV-CRED-CONC
                                         = AUX1-SEQ-PROC          AND
                       NU-NB             OF DS-MOV-CRED-CONC
                                         = AUX1-NU-NB             AND
                       IN-CRED-ATRASADO  OF DS-MOV-CRED-CONC
                                         = AUX1-IN-CRED-ATRASADO
                 ON EXCEPTION
                    IF DMSTATUS (NOTFOUND)
                       MOVE "ERRO - NOTFOUND ROT 2662" TO
                             WS-MENSAGEM-ZDISPLAY
                       CALL "ZDISPLAY IN DTP/L1" USING
                             WS-MENSAGEM-ZDISPLAY,
                             WS-INTEIRO-ZDISPLAY
                       CALL SYSTEM DMTERMINATE
                    ELSE
                       MOVE "ERRO FIND DS-MOV-CRED-CONC" TO
                             WS-MENSAGEM-ZDISPLAY
                       CALL "ZDISPLAY IN DTP/L1" USING
                                 WS-MENSAGEM-ZDISPLAY,
                                 WS-INTEIRO-ZDISPLAY
                           CALL SYSTEM DMTERMINATE.

            ADD 1 TO W77-LIDOS-CONC WS-LIDOS-CONC.

       2662-FIM. EXIT.

       2663-POSICIONA-MOV-CRED-ALTE.

            FIND FIRST S2-CRED-ALTER-OL   AT
                       ID-OL-SOLIC-CRED  OF DS-MOV-CRED-ALTER
                                         = AUX1-ID-OL-MANUTENCAO  AND
                       D2-SOLIC-CREDITO  OF DS-MOV-CRED-ALTER
                                         = AUX1-DT-SOLIC-CREDITO  AND
                       NU-SEQ-SOLIC-CRED OF DS-MOV-CRED-ALTER
                                         = AUX1-NU-SEQ-SOLIC-CRED
                       ON EXCEPTION
                          IF DMSTATUS (NOTFOUND)
                             MOVE "ERRO - NOTFOUND ROT 2663" TO
                                   WS-MENSAGEM-ZDISPLAY
                             CALL "ZDISPLAY IN DTP/L1" USING
                                   WS-MENSAGEM-ZDISPLAY,
                                   WS-INTEIRO-ZDISPLAY
                             CALL SYSTEM DMTERMINATE
                          ELSE
                             MOVE "ERRO FIND DS-MOV-CRED-ALTER" TO
                                   WS-MENSAGEM-ZDISPLAY
                             CALL "ZDISPLAY IN DTP/L1" USING
                                   WS-MENSAGEM-ZDISPLAY,
                                   WS-INTEIRO-ZDISPLAY
                             CALL SYSTEM DMTERMINATE.

            ADD 1 TO W77-LIDOS-ALTE WS-LIDOS-ALTE.

       2663-FIM. EXIT.

       2664-POSICIONA-CALC-ACAO-JUD.

            FIND FIRST ST-CALC-ACAO-JUD   AT
                       CS-UF-OL-ACAO-JUD OF DS-CALC-ACAO-JUD
                                         = AUX1-CS-UF-OL-ACAO-JUD AND
                       CS-MUNICIPIO      OF DS-CALC-ACAO-JUD
                                         = AUX1-CS-MUNICIPIO      AND
                       NU-VARA-ACAO-JUD  OF DS-CALC-ACAO-JUD
                                         = AUX1-NU-VARA-ACAO-JUD  AND
                       NU-PROC-ACAO-JUD  OF DS-CALC-ACAO-JUD
                                         = AUX1-NU-PROC-ACAO-JUD  AND
                       NU-NB             OF DS-CALC-ACAO-JUD
                                         = AUX1-NU-NB             AND
                       CS-NATUR-CREDITO  OF DS-CALC-ACAO-JUD
                                         = AUX1-CS-NATUR-CREDITO
                 ON EXCEPTION
                    IF DMSTATUS (NOTFOUND)
                       MOVE "ERRO - NOTFOUND ROT 2664" TO
                             WS-MENSAGEM-ZDISPLAY
                       CALL "ZDISPLAY IN DTP/L1" USING
                             WS-MENSAGEM-ZDISPLAY,
                             WS-INTEIRO-ZDISPLAY
                       CALL SYSTEM DMTERMINATE
                    ELSE
                       MOVE "ERRO FIND DS-CALC-ACAO-JUD" TO
                             WS-MENSAGEM-ZDISPLAY
                       CALL "ZDISPLAY IN DTP/L1" USING
                                                 WS-MENSAGEM-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
                       CALL SYSTEM DMTERMINATE.

            ADD 1 TO W77-LIDOS-AP.

       2664-FIM. EXIT.

       2665-POSICIONA-MOV-CRED-SEAS.

            FIND FIRST ST-NIT-MOV-SEAS AT
                       ID-NIT           OF DS-MOV-CRED-SEAS
                                        = AUX1-ID-NIT         AND
                       D2-INI-PERIODO   OF DS-MOV-CRED-SEAS
                                        = AUX1-DT-INI-PERIODO AND
                       D2-FIM-PERIODO   OF DS-MOV-CRED-SEAS
                                        = AUX1-DT-FIM-PERIODO AND
                       CS-NATUR-CREDITO OF DS-MOV-CRED-SEAS
                                        = AUX1-CS-NATUR-CREDITO
                 ON EXCEPTION
                    IF DMSTATUS (NOTFOUND)
                       MOVE "ERRO - NOTFOUND ROT 2665" TO
                             WS-MENSAGEM-ZDISPLAY
                       CALL "ZDISPLAY IN DTP/L1" USING
                                                 WS-MENSAGEM-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
                       CALL SYSTEM DMTERMINATE
                    ELSE
                       MOVE "ERRO FIND DS-MOV-CRED-SEAS" TO
                             WS-MENSAGEM-ZDISPLAY
                       CALL "ZDISPLAY IN DTP/L1" USING
                                                 WS-MENSAGEM-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
                       CALL SYSTEM DMTERMINATE.

           ADD 1 TO W77-LIDOS-SEAS.

       2665-FIM. EXIT.

       2710-TIRA-RUBRICA-900-MOV-CONC.
      ****  AS RUBRICAS ACIMA DE 900 NAO DEVERAO SER PROCESSADAS
      ****  SENDO ASSIM ESTOU IGNORANDO-AS (A NIVEL DE PROCESSAMENTO)

           ADD 1 TO W77-IND-RUBRICAS

           IF (CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS) < 900)
           OR (CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS) = 905)
           OR (CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS) > 906
                                                           AND  < 924)
           OR (CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS) > 924
                                                           AND  < 930)
           OR (CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS) = 933)
              ADD 1 TO W77-IND-TAB-RUB
              MOVE CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS)
                              TO WS-TAB-CS-RUBR-ENTICA(W77-IND-TAB-RUB)
              MOVE VL-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS)
                              TO WS-TAB-VL-RUBRICA(W77-IND-TAB-RUB).

       2710-FIM. EXIT.

       2720-ALTERA-UWA-MOV-CONC.
      ***  A T E N C A O ... ESTA ALTERACAO DA UWA DO BANCO VISA EVITAR
      ***  QUE AS RUBRICAS ACIMA DE 900 SEJAM PROCESSADAS, ELAS NAO SE-
      ***  RAO ELIMINADAS DO DATASET

           ADD 1 TO W77-IND-RUBRICAS
           MOVE WS-TAB-CS-RUBR-ENTICA(W77-IND-RUBRICAS)
                TO CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS)
           MOVE WS-TAB-VL-RUBRICA(W77-IND-RUBRICAS)
                TO VL-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS).

       2720-EXIT. EXIT.

       2730-TIRA-RUBRICA-900-MOV-MANU.
      ****  AS RUBRICAS ACIMA DE 900 NAO DEVERAO SER PROCESSADAS
      ****  SENDO ASSIM ESTOU IGNORANDO-AS (A NIVEL DE PROCESSAMENTO)

           ADD 1 TO W77-IND-RUBRICAS

           IF (CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS) < 900)
           OR (CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS) = 905)
           OR (CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS) > 906
                                                            AND  < 924)
           OR (CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS) > 924
                                                            AND  < 930)
           OR (CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS) = 933)
              ADD 1 TO W77-IND-TAB-RUB
              MOVE CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS)
                              TO WS-TAB-CS-RUBR-ENTICA(W77-IND-TAB-RUB)
              MOVE VL-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS)
                                TO WS-TAB-VL-RUBRICA(W77-IND-TAB-RUB).

       2730-FIM. EXIT.

       2740-ALTERA-UWA-MOV-MANU.
      ***  A T E N C A O ... ESTA ALTERACAO DA UWA DO BANCO VISA EVITAR
      ***  QUE AS RUBRICAS ACIMA DE 900 SEJAM PROCESSADAS, ELAS NAO SE-
      ***  RAO ELIMINADAS DO DATASET

           ADD 1 TO W77-IND-RUBRICAS
           MOVE WS-TAB-CS-RUBR-ENTICA(W77-IND-RUBRICAS)
                TO CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS)
           MOVE WS-TAB-VL-RUBRICA(W77-IND-RUBRICAS)
                TO VL-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS).

       2740-EXIT. EXIT.

       2750-TIRA-RUBRICA-900-MOV-ALTE.
      ****  AS RUBRICAS ACIMA DE 900 NAO DEVERAO SER PROCESSADAS
      ****  SENDO ASSIM ESTOU IGNORANDO-AS (A NIVEL DE PROCESSAMENTO)

           ADD 1 TO W77-IND-RUBRICAS

           IF (CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS) < 900)
           OR (CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS) = 905)
           OR (CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS) > 906
                                                            AND  < 924)
           OR (CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS) > 924
                                                            AND  < 930)
           OR (CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS) = 933)
              ADD 1 TO W77-IND-TAB-RUB
              MOVE CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS)
                               TO WS-TAB-CS-RUBR-ENTICA(W77-IND-TAB-RUB)
              MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS)
                                TO WS-TAB-VL-RUBRICA(W77-IND-TAB-RUB).

       2750-FIM. EXIT.

       2760-ALTERA-UWA-MOV-ALTE.
      ***  A T E N C A O ... ESTA ALTERACAO DA UWA DO BANCO VISA EVITAR
      ***  QUE AS RUBRICAS ACIMA DE 900 SEJAM PROCESSADAS, ELAS NAO SE-
      ***  RAO ELIMINADAS DO DATASET

           ADD 1 TO W77-IND-RUBRICAS
           MOVE WS-TAB-CS-RUBR-ENTICA(W77-IND-RUBRICAS)
                TO CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS)
           MOVE WS-TAB-VL-RUBRICA(W77-IND-RUBRICAS)
                TO VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS).

       2760-EXIT. EXIT.

       2762-TIRA-RUBRICA-900-MOV-SEAS.
      ****  AS RUBRICAS ACIMA DE 900 NAO DEVERAO SER PROCESSADAS
      ****  SENDO ASSIM ESTOU IGNORANDO-AS (A NIVEL DE PROCESSAMENTO)

           ADD 1 TO W77-IND-RUBRICAS

           IF (CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS) < 900)
           OR (CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS) = 905)
           OR (CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS) > 906
                                                           AND  < 924)
           OR (CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS) > 924
                                                           AND  < 930)
           OR (CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS) = 933)
              ADD 1 TO W77-IND-TAB-RUB
              MOVE CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS)
                              TO WS-TAB-CS-RUBR-ENTICA(W77-IND-TAB-RUB)
              MOVE VL-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS)
                              TO WS-TAB-VL-RUBRICA(W77-IND-TAB-RUB).

       2762-FIM. EXIT.

       2764-ALTERA-UWA-MOV-SEAS.
      ***  A T E N C A O ... ESTA ALTERACAO DA UWA DO BANCO VISA EVITAR
      ***  QUE AS RUBRICAS ACIMA DE 900 SEJAM PROCESSADAS, ELAS NAO SE-
      ***  RAO ELIMINADAS DO DATASET

           ADD 1 TO W77-IND-RUBRICAS
           MOVE WS-TAB-CS-RUBR-ENTICA(W77-IND-RUBRICAS)
                TO CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS)
           MOVE WS-TAB-VL-RUBRICA(W77-IND-RUBRICAS)
                TO VL-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS).

       2764-EXIT. EXIT.

       2770-LIMPA-RUBRICAS-CONC.

            ADD 1 TO W77-IND-RUBRICAS
            MOVE LOW-VALUES
                 TO GR-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS).

       2770-EXIT. EXIT.

       2775-LIMPA-RUBRICAS-MANU.

            ADD 1 TO W77-IND-RUBRICAS
            MOVE LOW-VALUES
                 TO GR-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS).

       2775-EXIT. EXIT.

       2780-LIMPA-RUBRICAS-ALTE.

            ADD 1 TO W77-IND-RUBRICAS
            MOVE LOW-VALUES
              TO GR-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS).

       2780-EXIT. EXIT.

       2785-LIMPA-RUBRICAS-SEAS.

            ADD 1 TO W77-IND-RUBRICAS
            MOVE LOW-VALUES
              TO GR-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS).

       2785-EXIT.
           EXIT.
       2790-SALVA-RUB.

           IF CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS) = 301
              MOVE VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
              WS-VL-RUB-301-SEM-ALT WS-SISPG-VALOR-RUB301 LIN-RUB301.


      * SO TEM 47 RUBRICAS E ESTE INDEXADOR EH PARA OUTRA TABELA
           IF W77-IND-RUBRICAS < 43
            IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) > ZEROS
      ******   DISPLAY "NB=" NU-NB OF DS-MOV-CRED-CONC
      ******   DISPLAY "W77-IND-RUBRICAS=" W77-IND-RUBRICAS
               MOVE CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS)
               TO SISPG-CS-RUB (W77-IND-RUBRICAS)
                  SPGENT-CS-RUB (W77-IND-RUBRICAS).

            IF CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS) = 105
               MOVE VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                    WS-BRUTO-RUB-105
            ELSE
               MOVE VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                    WS-TB-VL-RUBRICA-AUX
                    (CS-RUBRICA OF DS-MOV-CRED-CONC(W77-IND-RUBRICAS)).

           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 104
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB104
                  SPGENT-VALOR-RUB104
                  MOVE 1 TO SISPG-MARCA-RUB104
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 105
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB105
                  SPGENT-VALOR-RUB105
                  MOVE 1 TO SISPG-MARCA-RUB105
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 107
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB107
                  SPGENT-VALOR-RUB107
                  MOVE 1 TO SISPG-MARCA-RUB107
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 108
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB108
                  SPGENT-VALOR-RUB108
                  MOVE 1 TO SISPG-MARCA-RUB108
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 112
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB112
                  SPGENT-VALOR-RUB112
                  MOVE 1 TO SISPG-MARCA-RUB112
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 136
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB136
                  SPGENT-VALOR-RUB136
                  MOVE 1 TO SISPG-MARCA-RUB136
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 154
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB154
                  SPGENT-VALOR-RUB154
                  MOVE 1 TO SISPG-MARCA-RUB154
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 160
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB160
                  SPGENT-VALOR-RUB160
                  MOVE 1 TO SISPG-MARCA-RUB160
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 161
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB161
                  SPGENT-VALOR-RUB161
                  MOVE 1 TO SISPG-MARCA-RUB161
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 162
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB162
                  SPGENT-VALOR-RUB162
                  MOVE 1 TO SISPG-MARCA-RUB162
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 201
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB201
                  SPGENT-VALOR-RUB201
                  MOVE 1 TO SISPG-MARCA-RUB201
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 202
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB202
                  SPGENT-VALOR-RUB202
                  MOVE 1 TO SISPG-MARCA-RUB202
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 203
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB203
                  SPGENT-VALOR-RUB203
                  MOVE 1 TO SISPG-MARCA-RUB203
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 204
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB204
                  SPGENT-VALOR-RUB204
                  MOVE 1 TO SISPG-MARCA-RUB204
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 205
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB205
                  SPGENT-VALOR-RUB205
                  MOVE 1 TO SISPG-MARCA-RUB205
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 206
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB206
                  SPGENT-VALOR-RUB206
                  MOVE 1 TO SISPG-MARCA-RUB206
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 207
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB207
                  SPGENT-VALOR-RUB207
                  MOVE 1 TO SISPG-MARCA-RUB207
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 208
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB208
                  SPGENT-VALOR-RUB208
                  MOVE 1 TO SISPG-MARCA-RUB208
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 209
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB209
                  SPGENT-VALOR-RUB209
                  MOVE 1 TO SISPG-MARCA-RUB209
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 210
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB210
                  SPGENT-VALOR-RUB210
                  MOVE 1 TO SISPG-MARCA-RUB210
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 214
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB214
                  SPGENT-VALOR-RUB214
                  MOVE 1 TO SISPG-MARCA-RUB214
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 215
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB215
                  SPGENT-VALOR-RUB215
                  MOVE 1 TO SISPG-MARCA-RUB215
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 216
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB216
                  SPGENT-VALOR-RUB216
                  MOVE 1 TO SISPG-MARCA-RUB216
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 217
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB217
                  SPGENT-VALOR-RUB217
                  MOVE 1 TO SISPG-MARCA-RUB217
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 218
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB218
                  SPGENT-VALOR-RUB218
                  MOVE 1 TO SISPG-MARCA-RUB218
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 219
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB219
                  SPGENT-VALOR-RUB219
                  TB-VL-RUB-ENT(01)
                  MOVE 1 TO SISPG-MARCA-RUB219
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 220
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB220
                  SPGENT-VALOR-RUB220
                  TB-VL-RUB-ENT(02)
                  MOVE 1 TO SISPG-MARCA-RUB220
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 221
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB221
                  SPGENT-VALOR-RUB221
                  TB-VL-RUB-ENT(03)
                  MOVE 1 TO SISPG-MARCA-RUB221
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 222
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB222
                  SPGENT-VALOR-RUB222
                  TB-VL-RUB-ENT(04)
                  MOVE 1 TO SISPG-MARCA-RUB222
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 223
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB223
                  SPGENT-VALOR-RUB223
                  TB-VL-RUB-ENT(05)
                  MOVE 1 TO SISPG-MARCA-RUB223
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 224
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB224
                  SPGENT-VALOR-RUB224
                  TB-VL-RUB-ENT(06)
                  MOVE 1 TO SISPG-MARCA-RUB224
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 225
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB225
                  SPGENT-VALOR-RUB225
                  TB-VL-RUB-ENT(07)
                  MOVE 1 TO SISPG-MARCA-RUB225
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 226
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB226
                  SPGENT-VALOR-RUB226
                  TB-VL-RUB-ENT(08)
                  MOVE 1 TO SISPG-MARCA-RUB226
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 227
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB227
                  SPGENT-VALOR-RUB227
                  MOVE 1 TO SISPG-MARCA-RUB227
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 228
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB228
                  SPGENT-VALOR-RUB228
                  TB-VL-RUB-ENT(09)
                  MOVE 1 TO SISPG-MARCA-RUB228
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 229
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB229
                  SPGENT-VALOR-RUB229
                  TB-VL-RUB-ENT(10)
                  MOVE 1 TO SISPG-MARCA-RUB229
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 230
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB230
                  SPGENT-VALOR-RUB230
                  TB-VL-RUB-ENT(11)
                  MOVE 1 TO SISPG-MARCA-RUB230
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 231
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB231
                  SPGENT-VALOR-RUB231
                  MOVE 1 TO SISPG-MARCA-RUB231
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 232
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB232
                  SPGENT-VALOR-RUB232
                  MOVE 1 TO SISPG-MARCA-RUB232
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 233
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB233
                  SPGENT-VALOR-RUB233
                  MOVE 1 TO SISPG-MARCA-RUB233
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 234
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB234
                  SPGENT-VALOR-RUB234
                  TB-VL-RUB-ENT(12)
                  MOVE 1 TO SISPG-MARCA-RUB234
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 236
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB236
                  SPGENT-VALOR-RUB236
                  MOVE 1 TO SISPG-MARCA-RUB236
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 237
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB237
                  SPGENT-VALOR-RUB237
                  MOVE 1 TO SISPG-MARCA-RUB237
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 238
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB238
                  SPGENT-VALOR-RUB238
                  MOVE 1 TO SISPG-MARCA-RUB238
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 239
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB239
                  SPGENT-VALOR-RUB239
                  TB-VL-RUB-ENT(13)
                  MOVE 1 TO SISPG-MARCA-RUB239
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 240
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB240
                  SPGENT-VALOR-RUB240
                  TB-VL-RUB-ENT(14)
                  MOVE 1 TO SISPG-MARCA-RUB240
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 241
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB241
                  SPGENT-VALOR-RUB241
                  TB-VL-RUB-ENT(15)
                  MOVE 1 TO SISPG-MARCA-RUB241
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 242
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB242
                  SISPG-VALOR-RUB242
                  TB-VL-RUB-ENT(16)
                  MOVE 1 TO SISPG-MARCA-RUB242
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 243
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB243
                  SISPG-VALOR-RUB243
                  TB-VL-RUB-ENT(17)
                  MOVE 1 TO SISPG-MARCA-RUB243
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 244
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB244
                  SISPG-VALOR-RUB244
                  TB-VL-RUB-ENT(18)
                  MOVE 1 TO SISPG-MARCA-RUB244
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 245
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB245
                  SISPG-VALOR-RUB245
                  TB-VL-RUB-ENT(19)
                  MOVE 1 TO SISPG-MARCA-RUB245
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 246
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB246
                  SISPG-VALOR-RUB246
                  TB-VL-RUB-ENT(20)
                  MOVE 1 TO SISPG-MARCA-RUB246
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 247
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB247
                  SISPG-VALOR-RUB247
                  TB-VL-RUB-ENT(21)
                  MOVE 1 TO SISPG-MARCA-RUB247
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 248
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB248
                  SISPG-VALOR-RUB248
                  TB-VL-RUB-ENT(22)
                  MOVE 1 TO SISPG-MARCA-RUB248
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 249
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB249
                  SISPG-VALOR-RUB249
                  TB-VL-RUB-ENT(23)
                  MOVE 1 TO SISPG-MARCA-RUB249
              ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 250
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB250
                  SISPG-VALOR-RUB250
                  TB-VL-RUB-ENT(24)
                  MOVE 1 TO SISPG-MARCA-RUB250
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 251
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB251
                  SISPG-VALOR-RUB251
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 252
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB252
                  SISPG-VALOR-RUB252
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 253
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB253
                  SISPG-VALOR-RUB253
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 254
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB254
                  SISPG-VALOR-RUB254
                  TB-VL-RUB-ENT(25)
                  MOVE 1 TO SISPG-MARCA-RUB254
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 255
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB255
                  SPGENT-VALOR-RUB255
                  MOVE 1 TO SISPG-MARCA-RUB255
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 256
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB256
                  SPGENT-VALOR-RUB256
                  TB-VL-RUB-ENT(26)
                  MOVE 1 TO SISPG-MARCA-RUB256
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 257
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB257
                  SPGENT-VALOR-RUB257
                  TB-VL-RUB-ENT(27)
                  MOVE 1 TO SISPG-MARCA-RUB257
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 258
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
      *            SISPG-VALOR-RUB258
                  SPGENT-VALOR-RUB258
                  TB-VL-RUB-ENT(28)
                  MOVE 1 TO SISPG-MARCA-RUB258
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 259
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB259
                  SPGENT-VALOR-RUB259
                  TB-VL-RUB-ENT(29)
                  MOVE 1 TO SISPG-MARCA-RUB259
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 260
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB260
                  SISPG-VALOR-RUB260
                  MOVE 1 TO SISPG-MARCA-RUB260
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 261
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB261
                  SISPG-VALOR-RUB261
                  MOVE 1 TO SISPG-MARCA-RUB261
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 262
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB262
                  SISPG-VALOR-RUB262
                  MOVE 1 TO SISPG-MARCA-RUB262
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 263
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB263
                  SISPG-VALOR-RUB263
                  MOVE 1 TO SISPG-MARCA-RUB263
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 264
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB264
                  SISPG-VALOR-RUB264
                  TB-VL-RUB-ENT(30)
                  MOVE 1 TO SISPG-MARCA-RUB264
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 265
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB265
                  SISPG-VALOR-RUB265
                  MOVE 1 TO SISPG-MARCA-RUB265
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 266
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB266
                  SISPG-VALOR-RUB266
                  TB-VL-RUB-ENT(31)
                  MOVE 1 TO SISPG-MARCA-RUB266
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 267
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB267
                  SISPG-VALOR-RUB267
                  TB-VL-RUB-ENT(32)
                  MOVE 1 TO SISPG-MARCA-RUB267
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 268
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB268
                  SISPG-VALOR-RUB268
                  MOVE 1 TO SISPG-MARCA-RUB268
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 269
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB269
                  SISPG-VALOR-RUB269
                  TB-VL-RUB-ENT(33)
                  MOVE 1 TO SISPG-MARCA-RUB269
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) =270
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB270
                  SISPG-VALOR-RUB270
                  TB-VL-RUB-ENT(34)
                  MOVE 1 TO SISPG-MARCA-RUB270
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 271
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB271
                  SISPG-VALOR-RUB271
                  TB-VL-RUB-ENT(35)
                  MOVE 1 TO SISPG-MARCA-RUB271
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 272
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB272
                  SISPG-VALOR-RUB272
                  TB-VL-RUB-ENT(36)
                  MOVE 1 TO SISPG-MARCA-RUB272
           ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 273
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB273
                  SISPG-VALOR-RUB273
                  TB-VL-RUB-ENT(37)
                  MOVE 1 TO SISPG-MARCA-RUB273
           ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 274
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB274
                  SISPG-VALOR-RUB274
                  TB-VL-RUB-ENT(38)
                  MOVE 1 TO SISPG-MARCA-RUB274
           ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 275
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
      *            SPGENT-VALOR-RUB275
                  SISPG-VALOR-RUB275
                  TB-VL-RUB-ENT(39)
                  MOVE 1 TO SISPG-MARCA-RUB275
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 301
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB301
                  SPGENT-VALOR-RUB301
                  MOVE 1 TO SISPG-MARCA-RUB301
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 905
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB905
                  SPGENT-VALOR-RUB905
                  MOVE 1 TO SISPG-MARCA-RUB905
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 909
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB909
                  SPGENT-VALOR-RUB909
                  MOVE 1 TO SISPG-MARCA-RUB909
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 910
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB910
                  SPGENT-VALOR-RUB910
                  MOVE 1 TO SISPG-MARCA-RUB910
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 911
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB911
                  SPGENT-VALOR-RUB911
                  MOVE 1 TO SISPG-MARCA-RUB911
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 912
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB912
                  SPGENT-VALOR-RUB912
                  MOVE 1 TO SISPG-MARCA-RUB912
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 916
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB916
                  SPGENT-VALOR-RUB916
                  MOVE 1 TO SISPG-MARCA-RUB916
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 917
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB917
                  SPGENT-VALOR-RUB917
                  MOVE 1 TO SISPG-MARCA-RUB917
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 921
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB921
                  SPGENT-VALOR-RUB921
                  MOVE 1 TO SISPG-MARCA-RUB921
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 925
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB925
                  SPGENT-VALOR-RUB925
                  MOVE 1 TO SISPG-MARCA-RUB925
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 926
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB926
                  SPGENT-VALOR-RUB926
                  MOVE 1 TO SISPG-MARCA-RUB926
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 927
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB927
                  SPGENT-VALOR-RUB927
                  MOVE 1 TO SISPG-MARCA-RUB927
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 928
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB928
                  SPGENT-VALOR-RUB928
                  MOVE 1 TO SISPG-MARCA-RUB928
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 929
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB929
                  SPGENT-VALOR-RUB929
                  MOVE 1 TO SISPG-MARCA-RUB929
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 933
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB933
                  SPGENT-VALOR-RUB933
                  MOVE 1 TO SISPG-MARCA-RUB933.

      *Em 28/12/2016 - Jose Fonseca.
      *DM.062989 - Retencao de IR para deposito em conta judicial
      *DM.064300 - Retencao de IR para deposito em conta judicial
      *Em 25/08/2017 - Jose Fonseca
      *DM.066820 - Retencao de IR para deposito em conta judicial.

           IF CS-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) = 201
              AND (NU-NB OF DS-MOV-CRED-CONC = 1180589847
              OR NU-NB OF DS-MOV-CRED-CONC = 1625337911
              OR NU-NB OF DS-MOV-CRED-CONC = 1594335777)
              COMPUTE SISPG-VALOR-RUB201 = SISPG-VALOR-RUB201 -
                      VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS)
              MOVE 0 TO SISPG-MARCA-RUB201.

       2790-FIM.
           EXIT.
       2792-SALVA-RUB.

           IF CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS) = 0
              GO TO 2792-FIM.

           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 301
              MOVE VL-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS) TO
              WS-VL-RUB-301-SEM-ALT WS-SISPG-VALOR-RUB301 LIN-RUB301.

      * SO TEM 47 RUBRICAS E ESTE INDEXADOR EH PARA OUTRA TABELA
           IF W77-IND-RUBRICAS < 31
            IF CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS) > ZEROS
               MOVE CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS)
               TO SISPG-CS-RUB (W77-IND-RUBRICAS)
                  SPGENT-CS-RUB (W77-IND-RUBRICAS).

            IF CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS) = 105
               MOVE VL-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS) TO
                    WS-BRUTO-RUB-105
            ELSE
               MOVE VL-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS) TO
                    WS-TB-VL-RUBRICA-AUX
                   (CS-RUBRICA OF DS-MOV-CRED-MANUT(W77-IND-RUBRICAS)).


           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 104
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB104
                  SPGENT-VALOR-RUB104
                  MOVE 1 TO SISPG-MARCA-RUB104
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 105
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB105
                  SPGENT-VALOR-RUB105
                  MOVE 1 TO SISPG-MARCA-RUB105
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 107
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB107
                  SISPG-VALOR-RUB107
                  MOVE 1 TO SISPG-MARCA-RUB107
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 108
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB108
                  SPGENT-VALOR-RUB108
                  MOVE 1 TO SISPG-MARCA-RUB108
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 112
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB112
                  SPGENT-VALOR-RUB112
                  MOVE 1 TO SISPG-MARCA-RUB112
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 136
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB136
                  SPGENT-VALOR-RUB136
                  MOVE 1 TO SISPG-MARCA-RUB136
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 154
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB154
                  SPGENT-VALOR-RUB154
                  MOVE 1 TO SISPG-MARCA-RUB154
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 160
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB160
                  SPGENT-VALOR-RUB160
                  MOVE 1 TO SISPG-MARCA-RUB160
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 161
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB161
                  SPGENT-VALOR-RUB161
                  MOVE 1 TO SISPG-MARCA-RUB161
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 162
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB162
                  SPGENT-VALOR-RUB162
                  MOVE 1 TO SISPG-MARCA-RUB162
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 201
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB201
                  SPGENT-VALOR-RUB201
                  MOVE 1 TO SISPG-MARCA-RUB201
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 202
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB202
                  SPGENT-VALOR-RUB202
                  MOVE 1 TO SISPG-MARCA-RUB202
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 203
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB203
                  SPGENT-VALOR-RUB203
                  MOVE 1 TO SISPG-MARCA-RUB203
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 204
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB204
                  SPGENT-VALOR-RUB204
                  MOVE 1 TO SISPG-MARCA-RUB204
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 205
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB205
                  SPGENT-VALOR-RUB205
                  MOVE 1 TO SISPG-MARCA-RUB205
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 206
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB206
                  SPGENT-VALOR-RUB206
                  MOVE 1 TO SISPG-MARCA-RUB206
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 207
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB207
                  SPGENT-VALOR-RUB207
                  MOVE 1 TO SISPG-MARCA-RUB207
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 208
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB208
                  SPGENT-VALOR-RUB208
                  MOVE 1 TO SISPG-MARCA-RUB208
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 209
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB209
                  SPGENT-VALOR-RUB209
                  MOVE 1 TO SISPG-MARCA-RUB209
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 210
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB210
                  SPGENT-VALOR-RUB210
                  MOVE 1 TO SISPG-MARCA-RUB210
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 214
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB214
                  SPGENT-VALOR-RUB214
                  MOVE 1 TO SISPG-MARCA-RUB214
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 215
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB215
                  SPGENT-VALOR-RUB215
                  MOVE 1 TO SISPG-MARCA-RUB215
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 216
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB216
                  SPGENT-VALOR-RUB216
                  MOVE 1 TO SISPG-MARCA-RUB216
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 217
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB217
                  SPGENT-VALOR-RUB217
                  MOVE 1 TO SISPG-MARCA-RUB217
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 218
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB218
                  SPGENT-VALOR-RUB218
                  MOVE 1 TO SISPG-MARCA-RUB218
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 219
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB219
                  SPGENT-VALOR-RUB219
                  TB-VL-RUB-ENT(01)
                  MOVE 1 TO SISPG-MARCA-RUB219
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 220
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB220
                  SPGENT-VALOR-RUB220
                  TB-VL-RUB-ENT(02)
                  MOVE 1 TO SISPG-MARCA-RUB220
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 221
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB221
                  SPGENT-VALOR-RUB221
                  TB-VL-RUB-ENT(03)
                  MOVE 1 TO SISPG-MARCA-RUB221
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 222
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB222
                  SPGENT-VALOR-RUB222
                  TB-VL-RUB-ENT(04)
                  MOVE 1 TO SISPG-MARCA-RUB222
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 223
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB223
                  SPGENT-VALOR-RUB223
                  TB-VL-RUB-ENT(05)
                  MOVE 1 TO SISPG-MARCA-RUB223
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 224
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB224
                  SPGENT-VALOR-RUB224
                  TB-VL-RUB-ENT(06)
                  MOVE 1 TO SISPG-MARCA-RUB224
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 225
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB225
                  SPGENT-VALOR-RUB225
                  TB-VL-RUB-ENT(07)
                  MOVE 1 TO SISPG-MARCA-RUB225
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 226
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB226
                  SPGENT-VALOR-RUB226
                  TB-VL-RUB-ENT(08)
                  MOVE 1 TO SISPG-MARCA-RUB226
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 227
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB227
                  SPGENT-VALOR-RUB227
                  MOVE 1 TO SISPG-MARCA-RUB227
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 228
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB228
                  SPGENT-VALOR-RUB228
                  TB-VL-RUB-ENT(09)
                  MOVE 1 TO SISPG-MARCA-RUB228
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 229
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB229
                  SPGENT-VALOR-RUB229
                  TB-VL-RUB-ENT(10)
                  MOVE 1 TO SISPG-MARCA-RUB229
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 230
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB230
                  SPGENT-VALOR-RUB230
                  TB-VL-RUB-ENT(11)
                  MOVE 1 TO SISPG-MARCA-RUB230
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 231
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB231
                  SPGENT-VALOR-RUB231
                  MOVE 1 TO SISPG-MARCA-RUB231
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 232
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB232
                  SPGENT-VALOR-RUB232
                  MOVE 1 TO SISPG-MARCA-RUB232
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 233
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB233
                  SPGENT-VALOR-RUB233
                  MOVE 1 TO SISPG-MARCA-RUB233
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 234
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB234
                  SPGENT-VALOR-RUB234
                  TB-VL-RUB-ENT(12)
                  MOVE 1 TO SISPG-MARCA-RUB234
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 236
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB236
                  SPGENT-VALOR-RUB236
                  MOVE 1 TO SISPG-MARCA-RUB236
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 237
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB237
                  SPGENT-VALOR-RUB237
                  MOVE 1 TO SISPG-MARCA-RUB237
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 238
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB238
                  SPGENT-VALOR-RUB238
                  MOVE 1 TO SISPG-MARCA-RUB238
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 239
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB239
                  SPGENT-VALOR-RUB239
                  TB-VL-RUB-ENT(13)
                  MOVE 1 TO SISPG-MARCA-RUB239
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 240
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB240
                  SPGENT-VALOR-RUB240
                  TB-VL-RUB-ENT(14)
                  MOVE 1 TO SISPG-MARCA-RUB240
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 241
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB241
                  SPGENT-VALOR-RUB241
                  TB-VL-RUB-ENT(15)
                  MOVE 1 TO SISPG-MARCA-RUB241
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 242
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB242
                  SISPG-VALOR-RUB242
                  TB-VL-RUB-ENT(16)
                  MOVE 1 TO SISPG-MARCA-RUB242
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 243
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB243
                  SISPG-VALOR-RUB243
                  TB-VL-RUB-ENT(17)
                  MOVE 1 TO SISPG-MARCA-RUB243
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 244
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB244
                  SISPG-VALOR-RUB244
                  TB-VL-RUB-ENT(18)
                  MOVE 1 TO SISPG-MARCA-RUB244
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 245
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB245
                  SISPG-VALOR-RUB245
                  TB-VL-RUB-ENT(19)
                  MOVE 1 TO SISPG-MARCA-RUB245
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 246
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB246
                  SISPG-VALOR-RUB246
                  TB-VL-RUB-ENT(20)
                  MOVE 1 TO SISPG-MARCA-RUB246
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 247
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB247
                  SISPG-VALOR-RUB247
                  TB-VL-RUB-ENT(21)
                  MOVE 1 TO SISPG-MARCA-RUB247
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 248
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB248
                  SPGENT-VALOR-RUB248
                  TB-VL-RUB-ENT(22)
                  MOVE 1 TO SISPG-MARCA-RUB248
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 249
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB249
                  SPGENT-VALOR-RUB249
                  TB-VL-RUB-ENT(23)
      *            DISPLAY "R249"SPGENT-VALOR-RUB249
                  MOVE 1 TO SISPG-MARCA-RUB249
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 250
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB250
                  SPGENT-VALOR-RUB250
                  TB-VL-RUB-ENT(24)
      *            DISPLAY "R250"SPGENT-VALOR-RUB250
                  MOVE 1 TO SISPG-MARCA-RUB250
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 251
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB251
                  SISPG-VALOR-RUB251
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 252
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB252
                  SISPG-VALOR-RUB252
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 253
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB253
                  SISPG-VALOR-RUB253
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 254
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB254
                  SISPG-VALOR-RUB254
                  TB-VL-RUB-ENT(25)
                  MOVE 1 TO SISPG-MARCA-RUB254
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 255
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB255
                  SPGENT-VALOR-RUB255
                  MOVE 1 TO SISPG-MARCA-RUB255
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 256
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB256
                  SPGENT-VALOR-RUB256
                  TB-VL-RUB-ENT(26)
                  MOVE 1 TO SISPG-MARCA-RUB256
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 257
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB257
                  SPGENT-VALOR-RUB257
                  TB-VL-RUB-ENT(27)
                  MOVE 1 TO SISPG-MARCA-RUB257
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 258
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB258
                  SPGENT-VALOR-RUB258
                  TB-VL-RUB-ENT(28)
                  MOVE 1 TO SISPG-MARCA-RUB258
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 259
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB259
                  SPGENT-VALOR-RUB259
                  TB-VL-RUB-ENT(29)
                  MOVE 1 TO SISPG-MARCA-RUB259
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 260
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB260
                  SISPG-VALOR-RUB260
                  MOVE 1 TO SISPG-MARCA-RUB260
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 261
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB261
                  SISPG-VALOR-RUB261
                  MOVE 1 TO SISPG-MARCA-RUB261
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 262
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB262
                  SISPG-VALOR-RUB262
                  MOVE 1 TO SISPG-MARCA-RUB262
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 263
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB263
                  SISPG-VALOR-RUB263
                  MOVE 1 TO SISPG-MARCA-RUB263
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 264
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB264
                  SISPG-VALOR-RUB264
                  TB-VL-RUB-ENT(30)
                  MOVE 1 TO SISPG-MARCA-RUB264
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 265
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB265
                  SISPG-VALOR-RUB265
                  MOVE 1 TO SISPG-MARCA-RUB265
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 266
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB266
                  SISPG-VALOR-RUB266
                  TB-VL-RUB-ENT(31)
                  MOVE 1 TO SISPG-MARCA-RUB266
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 267
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB267
                  SISPG-VALOR-RUB267
                  TB-VL-RUB-ENT(32)
                  MOVE 1 TO SISPG-MARCA-RUB267
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 268
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB268
                  SISPG-VALOR-RUB268
                  MOVE 1 TO SISPG-MARCA-RUB268
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 269
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB269
                  SISPG-VALOR-RUB269
                  TB-VL-RUB-ENT(33)
                  MOVE 1 TO SISPG-MARCA-RUB269
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) =270
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB270
                  SISPG-VALOR-RUB270
                  TB-VL-RUB-ENT(34)
                  MOVE 1 TO SISPG-MARCA-RUB270
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 271
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB271
                  SISPG-VALOR-RUB271
                  TB-VL-RUB-ENT(35)
                  MOVE 1 TO SISPG-MARCA-RUB271
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 272
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB272
                  SISPG-VALOR-RUB272
                  TB-VL-RUB-ENT(36)
                  MOVE 1 TO SISPG-MARCA-RUB272
           ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 273
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB273
                  SISPG-VALOR-RUB273
                  TB-VL-RUB-ENT(37)
                  MOVE 1 TO SISPG-MARCA-RUB273
           ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 274
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB274
                  SISPG-VALOR-RUB274
                  TB-VL-RUB-ENT(38)
                  MOVE 1 TO SISPG-MARCA-RUB274
           ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 275
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
      *            SPGENT-VALOR-RUB275
                  SISPG-VALOR-RUB275
                  TB-VL-RUB-ENT(39)
                  MOVE 1 TO SISPG-MARCA-RUB275
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 301
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB301
                  SPGENT-VALOR-RUB301
                  MOVE 1 TO SISPG-MARCA-RUB301
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 905
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB905
                  SPGENT-VALOR-RUB905
                  MOVE 1 TO SISPG-MARCA-RUB905
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 909
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB909
                  SPGENT-VALOR-RUB909
                  MOVE 1 TO SISPG-MARCA-RUB909
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 910
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB910
                  SPGENT-VALOR-RUB910
                  MOVE 1 TO SISPG-MARCA-RUB910
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 911
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB911
                  SPGENT-VALOR-RUB911
                  MOVE 1 TO SISPG-MARCA-RUB911
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 912
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB912
                  SPGENT-VALOR-RUB912
                  MOVE 1 TO SISPG-MARCA-RUB912
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 916
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB916
                  SPGENT-VALOR-RUB916
                  MOVE 1 TO SISPG-MARCA-RUB916
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 917
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB917
                  SPGENT-VALOR-RUB917
                  MOVE 1 TO SISPG-MARCA-RUB917
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 921
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB921
                  SPGENT-VALOR-RUB921
                  MOVE 1 TO SISPG-MARCA-RUB921
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 925
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB925
                  SPGENT-VALOR-RUB925
                  MOVE 1 TO SISPG-MARCA-RUB925
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 926
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB926
                  SPGENT-VALOR-RUB926
                  MOVE 1 TO SISPG-MARCA-RUB926
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 927
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB927
                  SPGENT-VALOR-RUB927
                  MOVE 1 TO SISPG-MARCA-RUB927
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 928
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB928
                  SPGENT-VALOR-RUB928
                  MOVE 1 TO SISPG-MARCA-RUB928
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 929
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB929
                  SPGENT-VALOR-RUB929
                  MOVE 1 TO SISPG-MARCA-RUB929
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 933
              ADD VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB933
                  SPGENT-VALOR-RUB933
                  MOVE 1 TO SISPG-MARCA-RUB933.
      *
      *Em 28/12/2016 - Jose Fonseca.
      *DM.062989 - Retencao de IR para deposito em conta judicial
      *DM.064300 - Retencao de IR para deposito em conta judicial
      *Em 25/08/2017 - Jose Fonseca
      *DM.066820 - Retencao de IR para deposito em conta judicial

           IF CS-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS) = 201
              AND (NU-NB OF DS-MOV-CRED-MANUT = 1180589847
              OR NU-NB OF DS-MOV-CRED-MANUT = 1625337911
              OR NU-NB OF DS-MOV-CRED-MANUT = 1594335777)
              COMPUTE SISPG-VALOR-RUB201 = SISPG-VALOR-RUB201 -
                      VL-RUBRICA OF DS-MOV-CRED-MANUT (W77-IND-RUBRICAS)
              MOVE 0 TO SISPG-MARCA-RUB201.

       2792-FIM.
           EXIT.

       2794-SALVA-RUB.

           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 301
              MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS) TO
              WS-VL-RUB-301-SEM-ALT WS-SISPG-VALOR-RUB301 LIN-RUB301.

      * SO TEM 47 RUBRICAS E ESTE INDEXADOR EH PARA OUTRA TABELA
           IF W77-IND-RUBRICAS < 43
            IF CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS) > ZEROS
               MOVE CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS)
               TO SISPG-CS-RUB (W77-IND-RUBRICAS)
                  SPGENT-CS-RUB (W77-IND-RUBRICAS).

            IF CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS) = 105
               MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS) TO
                    WS-BRUTO-RUB-105
            ELSE
               MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS) TO
                    WS-TB-VL-RUBRICA-AUX
                    (CS-RUBRICA OF DS-MOV-CRED-ALTER(W77-IND-RUBRICAS)).

           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 104
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB104
                  SPGENT-VALOR-RUB104
                  MOVE 1 TO SISPG-MARCA-RUB104
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 105
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB105
                  SPGENT-VALOR-RUB105
                  MOVE 1 TO SISPG-MARCA-RUB105
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 107
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB107
                  SPGENT-VALOR-RUB107
                  MOVE 1 TO SISPG-MARCA-RUB107
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 108
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB108
                  SPGENT-VALOR-RUB108
                  MOVE 1 TO SISPG-MARCA-RUB108
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 112
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB112
                  SPGENT-VALOR-RUB112
                  MOVE 1 TO SISPG-MARCA-RUB112
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 136
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB136
                  SPGENT-VALOR-RUB136
                  MOVE 1 TO SISPG-MARCA-RUB136
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 154
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB154
                  SPGENT-VALOR-RUB154
                  MOVE 1 TO SISPG-MARCA-RUB154
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 160
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB160
                  SPGENT-VALOR-RUB160
                  MOVE 1 TO SISPG-MARCA-RUB160
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 161
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB161
                  SPGENT-VALOR-RUB161
                  MOVE 1 TO SISPG-MARCA-RUB161
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 162
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB162
                  SPGENT-VALOR-RUB162
                  MOVE 1 TO SISPG-MARCA-RUB162
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 201
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB201
                  SPGENT-VALOR-RUB201
                  MOVE 1 TO SISPG-MARCA-RUB201
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 202
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB202
                  SPGENT-VALOR-RUB202
                  MOVE 1 TO SISPG-MARCA-RUB202
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 203
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB203
                  SPGENT-VALOR-RUB203
                  MOVE 1 TO SISPG-MARCA-RUB203
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 204
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB204
                  SPGENT-VALOR-RUB204
                  MOVE 1 TO SISPG-MARCA-RUB204
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 205
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB205
                  SPGENT-VALOR-RUB205
                  MOVE 1 TO SISPG-MARCA-RUB205
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 206
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB206
                  SPGENT-VALOR-RUB206
                  MOVE 1 TO SISPG-MARCA-RUB206
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 207
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB207
                  SPGENT-VALOR-RUB207
                  MOVE 1 TO SISPG-MARCA-RUB207
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 208
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB208
                  SPGENT-VALOR-RUB208
                  MOVE 1 TO SISPG-MARCA-RUB208
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 209
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB209
                  SPGENT-VALOR-RUB209
                  MOVE 1 TO SISPG-MARCA-RUB209
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 210
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB210
                  SPGENT-VALOR-RUB210
                  MOVE 1 TO SISPG-MARCA-RUB210
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 214
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB214
                  SPGENT-VALOR-RUB214
                  MOVE 1 TO SISPG-MARCA-RUB214
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 215
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB215
                  SPGENT-VALOR-RUB215
                  MOVE 1 TO SISPG-MARCA-RUB215
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 216
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB216
                  SPGENT-VALOR-RUB216
                  MOVE 1 TO SISPG-MARCA-RUB216
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 217
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB217
                  SPGENT-VALOR-RUB217
                  MOVE 1 TO SISPG-MARCA-RUB217
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 218
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB218
                  SPGENT-VALOR-RUB218
                  MOVE 1 TO SISPG-MARCA-RUB218
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 219
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB219
                  SPGENT-VALOR-RUB219
                  TB-VL-RUB-ENT(01)
                  MOVE 1 TO SISPG-MARCA-RUB219
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 220
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB220
                  SPGENT-VALOR-RUB220
                  TB-VL-RUB-ENT(02)
                  MOVE 1 TO SISPG-MARCA-RUB220
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 221
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB221
                  SPGENT-VALOR-RUB221
                  TB-VL-RUB-ENT(03)
                  MOVE 1 TO SISPG-MARCA-RUB221
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 222
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB222
                  SPGENT-VALOR-RUB222
                  TB-VL-RUB-ENT(04)
                  MOVE 1 TO SISPG-MARCA-RUB222
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 223
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB223
                  SPGENT-VALOR-RUB223
                  TB-VL-RUB-ENT(05)
                  MOVE 1 TO SISPG-MARCA-RUB223
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 224
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB224
                  SPGENT-VALOR-RUB224
                  TB-VL-RUB-ENT(06)
                  MOVE 1 TO SISPG-MARCA-RUB224
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 225
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB225
                  SPGENT-VALOR-RUB225
                  TB-VL-RUB-ENT(07)
                  MOVE 1 TO SISPG-MARCA-RUB225
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 226
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB226
                  SPGENT-VALOR-RUB226
                  TB-VL-RUB-ENT(08)
                  MOVE 1 TO SISPG-MARCA-RUB226
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 227
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB227
                  SPGENT-VALOR-RUB227
                  MOVE 1 TO SISPG-MARCA-RUB227
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 228
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB228
                  SPGENT-VALOR-RUB228
                  TB-VL-RUB-ENT(09)
                  MOVE 1 TO SISPG-MARCA-RUB228
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 229
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB229
                  SPGENT-VALOR-RUB229
                  TB-VL-RUB-ENT(10)
                  MOVE 1 TO SISPG-MARCA-RUB229
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 230
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB230
                  SPGENT-VALOR-RUB230
                  TB-VL-RUB-ENT(11)
                  MOVE 1 TO SISPG-MARCA-RUB230
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 231
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB231
                  SPGENT-VALOR-RUB231
                  MOVE 1 TO SISPG-MARCA-RUB231
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 232
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB232
                  SPGENT-VALOR-RUB232
                  MOVE 1 TO SISPG-MARCA-RUB232
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 233
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB233
                  SPGENT-VALOR-RUB233
                  MOVE 1 TO SISPG-MARCA-RUB233
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 234
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB234
                  SPGENT-VALOR-RUB234
                  TB-VL-RUB-ENT(12)
                  MOVE 1 TO SISPG-MARCA-RUB234
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 236
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB236
                  SPGENT-VALOR-RUB236
                  MOVE 1 TO SISPG-MARCA-RUB236
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 237
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB237
                  SPGENT-VALOR-RUB237
                  MOVE 1 TO SISPG-MARCA-RUB237
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 238
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB238
                  SPGENT-VALOR-RUB238
                  MOVE 1 TO SISPG-MARCA-RUB238
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 239
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB239
                  SPGENT-VALOR-RUB239
                  TB-VL-RUB-ENT(13)
                  MOVE 1 TO SISPG-MARCA-RUB239
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 240
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB240
                  SPGENT-VALOR-RUB240
                  TB-VL-RUB-ENT(14)
                  MOVE 1 TO SISPG-MARCA-RUB240
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 241
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB241
                  SPGENT-VALOR-RUB241
                  TB-VL-RUB-ENT(15)
                  MOVE 1 TO SISPG-MARCA-RUB241
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 242
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB242
                  SISPG-VALOR-RUB242
                  TB-VL-RUB-ENT(16)
                  MOVE 1 TO SISPG-MARCA-RUB242
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 243
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB243
                  SISPG-VALOR-RUB243
                  TB-VL-RUB-ENT(17)
                  MOVE 1 TO SISPG-MARCA-RUB243
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 244
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB244
                  SISPG-VALOR-RUB244
                  TB-VL-RUB-ENT(18)
                  MOVE 1 TO SISPG-MARCA-RUB244
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 245
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB245
                  SISPG-VALOR-RUB245
                  TB-VL-RUB-ENT(19)
                  MOVE 1 TO SISPG-MARCA-RUB245
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 246
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB246
                  SISPG-VALOR-RUB246
                  TB-VL-RUB-ENT(20)
                  MOVE 1 TO SISPG-MARCA-RUB246
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 247
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB247
                  SISPG-VALOR-RUB247
                  TB-VL-RUB-ENT(21)
                  MOVE 1 TO SISPG-MARCA-RUB247
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 248
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB248
                  SISPG-VALOR-RUB248
                  TB-VL-RUB-ENT(22)
                  MOVE 1 TO SISPG-MARCA-RUB248
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 249
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB249
                  SISPG-VALOR-RUB249
                  TB-VL-RUB-ENT(23)
                  MOVE 1 TO SISPG-MARCA-RUB249
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 250
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB250
                  SISPG-VALOR-RUB250
                  TB-VL-RUB-ENT(24)
                  MOVE 1 TO SISPG-MARCA-RUB250
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 251
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB251
                  SISPG-VALOR-RUB251
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 252
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB252
                  SISPG-VALOR-RUB252
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 253
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB253
                  SISPG-VALOR-RUB253
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 254
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB254
                  SISPG-VALOR-RUB254
                  TB-VL-RUB-ENT(25)
                  MOVE 1 TO SISPG-MARCA-RUB254
              ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 255
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB255
                  SPGENT-VALOR-RUB255
                  MOVE 1 TO SISPG-MARCA-RUB255
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 256
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB256
                  SPGENT-VALOR-RUB256
                  TB-VL-RUB-ENT(26)
                  MOVE 1 TO SISPG-MARCA-RUB256
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 257
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB257
                  SPGENT-VALOR-RUB257
                  TB-VL-RUB-ENT(27)
                  MOVE 1 TO SISPG-MARCA-RUB257
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 258
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB258
                  SPGENT-VALOR-RUB258
                  TB-VL-RUB-ENT(28)
                  MOVE 1 TO SISPG-MARCA-RUB258
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 259
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB259
                  SPGENT-VALOR-RUB259
                  TB-VL-RUB-ENT(29)
                  MOVE 1 TO SISPG-MARCA-RUB259
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 260
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB260
                  SISPG-VALOR-RUB260
                  MOVE 1 TO SISPG-MARCA-RUB260
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 261
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB261
                  SISPG-VALOR-RUB261
                  MOVE 1 TO SISPG-MARCA-RUB261
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 262
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB262
                  SISPG-VALOR-RUB262
                  MOVE 1 TO SISPG-MARCA-RUB262
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 263
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB263
                  SISPG-VALOR-RUB263
                  MOVE 1 TO SISPG-MARCA-RUB263
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 264
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB264
                  SISPG-VALOR-RUB264
                  TB-VL-RUB-ENT(30)
                  MOVE 1 TO SISPG-MARCA-RUB264
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 265
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB265
                  SISPG-VALOR-RUB265
                  MOVE 1 TO SISPG-MARCA-RUB265
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 266
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB266
                  SISPG-VALOR-RUB266
                  TB-VL-RUB-ENT(31)
                  MOVE 1 TO SISPG-MARCA-RUB266
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 267
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB267
                  SISPG-VALOR-RUB267
                  TB-VL-RUB-ENT(32)
                  MOVE 1 TO SISPG-MARCA-RUB267
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 268
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB268
                  SISPG-VALOR-RUB268
                  MOVE 1 TO SISPG-MARCA-RUB268
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 269
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB269
                  SISPG-VALOR-RUB269
                  TB-VL-RUB-ENT(33)
                  MOVE 1 TO SISPG-MARCA-RUB269
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) =270
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB270
                  SISPG-VALOR-RUB270
                  TB-VL-RUB-ENT(34)
                  MOVE 1 TO SISPG-MARCA-RUB270
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 271
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB271
                  SISPG-VALOR-RUB271
                  TB-VL-RUB-ENT(35)
                  MOVE 1 TO SISPG-MARCA-RUB271
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 272
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB272
                  SISPG-VALOR-RUB272
                  TB-VL-RUB-ENT(36)
                  MOVE 1 TO SISPG-MARCA-RUB272
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 273
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB273
                  SISPG-VALOR-RUB273
                  TB-VL-RUB-ENT(37)
                  MOVE 1 TO SISPG-MARCA-RUB273
           ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 274
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB274
                  SISPG-VALOR-RUB274
                  TB-VL-RUB-ENT(38)
                  MOVE 1 TO SISPG-MARCA-RUB274
           ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 275
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
      *            SPGENT-VALOR-RUB275
                  SISPG-VALOR-RUB275
                  TB-VL-RUB-ENT(39)
                  MOVE 1 TO SISPG-MARCA-RUB275
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 301
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB301
                  SPGENT-VALOR-RUB301
                  MOVE 1 TO SISPG-MARCA-RUB301
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 905
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB905
                  SPGENT-VALOR-RUB905
                  MOVE 1 TO SISPG-MARCA-RUB905
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 909
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB909
                  SPGENT-VALOR-RUB909
                  MOVE 1 TO SISPG-MARCA-RUB909
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 910
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB910
                  SPGENT-VALOR-RUB910
                  MOVE 1 TO SISPG-MARCA-RUB910
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 911
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB911
                  SPGENT-VALOR-RUB911
                  MOVE 1 TO SISPG-MARCA-RUB911
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 912
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB912
                  SPGENT-VALOR-RUB912
                  MOVE 1 TO SISPG-MARCA-RUB912
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 916
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB916
                  SPGENT-VALOR-RUB916
                  MOVE 1 TO SISPG-MARCA-RUB916
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 917
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB917
                  SPGENT-VALOR-RUB917
                  MOVE 1 TO SISPG-MARCA-RUB917
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 921
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB921
                  SPGENT-VALOR-RUB921
                  MOVE 1 TO SISPG-MARCA-RUB921
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 925
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB925
                  SPGENT-VALOR-RUB925
                  MOVE 1 TO SISPG-MARCA-RUB925
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 926
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB926
                  SPGENT-VALOR-RUB926
                  MOVE 1 TO SISPG-MARCA-RUB926
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 927
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB927
                  SPGENT-VALOR-RUB927
                  MOVE 1 TO SISPG-MARCA-RUB927
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 928
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB928
                  SPGENT-VALOR-RUB928
                  MOVE 1 TO SISPG-MARCA-RUB928
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 929
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB929
                  SPGENT-VALOR-RUB929
                  MOVE 1 TO SISPG-MARCA-RUB929
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 933
              ADD VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB933
                  SPGENT-VALOR-RUB933
                  MOVE 1 TO SISPG-MARCA-RUB933.
      *
      *Em 28/12/2016 - Jose Fonseca.
      *DM.062989 - Retencao de IR para deposito em conta judicial
      *DM.064300 - Retencao de IR para deposito em conta judicial.
      *Em 25/08/2017 - Jose Fonseca
      *DM.066820 - Retencao de IR para deposito em conta judicial.

           IF CS-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS) = 201
              AND (NU-NB OF DS-MOV-CRED-ALTER = 1180589847
              OR NU-NB OF DS-MOV-CRED-ALTER = 1625337911
              OR NU-NB OF DS-MOV-CRED-ALTER = 1594335777)
              COMPUTE SISPG-VALOR-RUB201 = SISPG-VALOR-RUB201 -
                      VL-RUBRICA OF DS-MOV-CRED-ALTER (W77-IND-RUBRICAS)
              MOVE 0 TO SISPG-MARCA-RUB201.

       2794-FIM. EXIT.

       2796-SALVA-RUB.

           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 301
              MOVE VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
              WS-VL-RUB-301-SEM-ALT WS-SISPG-VALOR-RUB301 LIN-RUB301.

      * SO TEM 47 RUBRICAS E ESTE INDEXADOR EH PARA OUTRA TABELA
           IF W77-IND-RUBRICAS < 43
            IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) > ZEROS
               MOVE CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS)
               TO SISPG-CS-RUB (W77-IND-RUBRICAS)
                  SPGENT-CS-RUB (W77-IND-RUBRICAS).

            IF CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS) = 105
               MOVE VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                    WS-BRUTO-RUB-105
            ELSE
               MOVE VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                    WS-TB-VL-RUBRICA-AUX
                    (CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS)).

           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 104
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB104
                  SPGENT-VALOR-RUB104
                  MOVE 1 TO SISPG-MARCA-RUB104
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 105
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB105
                  SPGENT-VALOR-RUB105
                  MOVE 1 TO SISPG-MARCA-RUB105
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS) = 107
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB107
                  SPGENT-VALOR-RUB107
                  MOVE 1 TO SISPG-MARCA-RUB107
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 108
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB108
                  SPGENT-VALOR-RUB108
                  MOVE 1 TO SISPG-MARCA-RUB108
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 112
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB112
                  SPGENT-VALOR-RUB112
                  MOVE 1 TO SISPG-MARCA-RUB112
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 136
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB136
                  SPGENT-VALOR-RUB136
                  MOVE 1 TO SISPG-MARCA-RUB136
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 154
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB154
                  SPGENT-VALOR-RUB154
                  MOVE 1 TO SISPG-MARCA-RUB154
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 160
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB160
                  SPGENT-VALOR-RUB160
                  MOVE 1 TO SISPG-MARCA-RUB160
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 161
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB161
                  SPGENT-VALOR-RUB161
                  MOVE 1 TO SISPG-MARCA-RUB161
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 162
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB162
                  SPGENT-VALOR-RUB162
                  MOVE 1 TO SISPG-MARCA-RUB162
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 201
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB201
                  SPGENT-VALOR-RUB201
                  MOVE 1 TO SISPG-MARCA-RUB201
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 202
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB202
                  SPGENT-VALOR-RUB202
                  MOVE 1 TO SISPG-MARCA-RUB202
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 203
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB203
                  SPGENT-VALOR-RUB203
                  MOVE 1 TO SISPG-MARCA-RUB203
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 204
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB204
                  SPGENT-VALOR-RUB204
                  MOVE 1 TO SISPG-MARCA-RUB204
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 205
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB205
                  SPGENT-VALOR-RUB205
                  MOVE 1 TO SISPG-MARCA-RUB205
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 206
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB206
                  SPGENT-VALOR-RUB206
                  MOVE 1 TO SISPG-MARCA-RUB206
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 207
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB207
                  SPGENT-VALOR-RUB207
                  MOVE 1 TO SISPG-MARCA-RUB207
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 208
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB208
                  SPGENT-VALOR-RUB208
                  MOVE 1 TO SISPG-MARCA-RUB208
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 209
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB209
                  SPGENT-VALOR-RUB209
                  MOVE 1 TO SISPG-MARCA-RUB209
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 210
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB210
                  SPGENT-VALOR-RUB210
                  MOVE 1 TO SISPG-MARCA-RUB210
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 214
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB214
                  SPGENT-VALOR-RUB214
                  MOVE 1 TO SISPG-MARCA-RUB214
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 215
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB215
                  SPGENT-VALOR-RUB215
                  MOVE 1 TO SISPG-MARCA-RUB215
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 216
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB216
                  SPGENT-VALOR-RUB216
                  MOVE 1 TO SISPG-MARCA-RUB216
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 217
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB217
                  SPGENT-VALOR-RUB217
                  MOVE 1 TO SISPG-MARCA-RUB217
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 218
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB218
                  SPGENT-VALOR-RUB218
                  MOVE 1 TO SISPG-MARCA-RUB218
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 219
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB219
                  SPGENT-VALOR-RUB219
                  TB-VL-RUB-ENT(01)
                  MOVE 1 TO SISPG-MARCA-RUB219
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 220
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB220
                  SPGENT-VALOR-RUB220
                  TB-VL-RUB-ENT(02)
                  MOVE 1 TO SISPG-MARCA-RUB220
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 221
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB221
                  SPGENT-VALOR-RUB221
                  TB-VL-RUB-ENT(03)
                  MOVE 1 TO SISPG-MARCA-RUB221
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 222
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB222
                  SPGENT-VALOR-RUB222
                  TB-VL-RUB-ENT(04)
                  MOVE 1 TO SISPG-MARCA-RUB222
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 223
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB223
                  SPGENT-VALOR-RUB223
                  TB-VL-RUB-ENT(05)
                  MOVE 1 TO SISPG-MARCA-RUB223
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 224
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB224
                  SPGENT-VALOR-RUB224
                  TB-VL-RUB-ENT(06)
                  MOVE 1 TO SISPG-MARCA-RUB224
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 225
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB225
                  SPGENT-VALOR-RUB225
                  TB-VL-RUB-ENT(07)
                  MOVE 1 TO SISPG-MARCA-RUB225
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 226
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB226
                  SPGENT-VALOR-RUB226
                  TB-VL-RUB-ENT(08)
                  MOVE 1 TO SISPG-MARCA-RUB226
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 227
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB227
                  SPGENT-VALOR-RUB227
                  MOVE 1 TO SISPG-MARCA-RUB227
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 228
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB228
                  SPGENT-VALOR-RUB228
                  TB-VL-RUB-ENT(09)
                  MOVE 1 TO SISPG-MARCA-RUB228
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 229
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB229
                  SPGENT-VALOR-RUB229
                  TB-VL-RUB-ENT(10)
                  MOVE 1 TO SISPG-MARCA-RUB229
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 230
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB230
                  SPGENT-VALOR-RUB230
                  TB-VL-RUB-ENT(11)
                  MOVE 1 TO SISPG-MARCA-RUB230
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 231
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB231
                  SPGENT-VALOR-RUB231
                  MOVE 1 TO SISPG-MARCA-RUB231
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 232
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB232
                  SPGENT-VALOR-RUB232
                  MOVE 1 TO SISPG-MARCA-RUB232
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 233
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB233
                  SPGENT-VALOR-RUB233
                  MOVE 1 TO SISPG-MARCA-RUB233
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 234
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB234
                  SPGENT-VALOR-RUB234
                  TB-VL-RUB-ENT(12)
                  MOVE 1 TO SISPG-MARCA-RUB234
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 236
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB236
                  SPGENT-VALOR-RUB236
                  MOVE 1 TO SISPG-MARCA-RUB236
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 237
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB237
                  SPGENT-VALOR-RUB237
                  MOVE 1 TO SISPG-MARCA-RUB237
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 238
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB238
                  SPGENT-VALOR-RUB238
                  MOVE 1 TO SISPG-MARCA-RUB238
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 239
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB239
                  SPGENT-VALOR-RUB239
                  TB-VL-RUB-ENT(13)
                  MOVE 1 TO SISPG-MARCA-RUB239
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 240
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB240
                  SPGENT-VALOR-RUB240
                  TB-VL-RUB-ENT(14)
                  MOVE 1 TO SISPG-MARCA-RUB240
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 241
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB241
                  SPGENT-VALOR-RUB241
                  TB-VL-RUB-ENT(15)
                  MOVE 1 TO SISPG-MARCA-RUB241
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 242
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB242
                  SISPG-VALOR-RUB242
                  TB-VL-RUB-ENT(16)
                  MOVE 1 TO SISPG-MARCA-RUB242
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 243
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB243
                  SISPG-VALOR-RUB243
                  TB-VL-RUB-ENT(17)
                  MOVE 1 TO SISPG-MARCA-RUB243
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 244
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB244
                  SISPG-VALOR-RUB244
                  TB-VL-RUB-ENT(18)
                  MOVE 1 TO SISPG-MARCA-RUB244
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 245
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB245
                  SISPG-VALOR-RUB245
                  TB-VL-RUB-ENT(19)
                  MOVE 1 TO SISPG-MARCA-RUB245
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 246
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB246
                  SISPG-VALOR-RUB246
                  TB-VL-RUB-ENT(20)
                  MOVE 1 TO SISPG-MARCA-RUB246
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 247
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB247
                  SISPG-VALOR-RUB247
                  TB-VL-RUB-ENT(21)
                  MOVE 1 TO SISPG-MARCA-RUB247
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 248
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB248
                  SISPG-VALOR-RUB248
                  TB-VL-RUB-ENT(22)
                  MOVE 1 TO SISPG-MARCA-RUB248
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS) = 249
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB249
                  SISPG-VALOR-RUB249
                  TB-VL-RUB-ENT(23)
                  MOVE 1 TO SISPG-MARCA-RUB249
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS(W77-IND-RUBRICAS) = 250
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB250
                  SISPG-VALOR-RUB250
                  TB-VL-RUB-ENT(24)
                  MOVE 1 TO SISPG-MARCA-RUB250
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 251
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB251
                  SISPG-VALOR-RUB251
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 252
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB252
                  SISPG-VALOR-RUB252
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 253
              ADD VL-RUBRICA OF DS-MOV-CRED-CONC (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB253
                  SISPG-VALOR-RUB253
                  MOVE 1 TO SISPG-MARCA-RUB251
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 254
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
      *            SPGENT-VALOR-RUB254
                  SISPG-VALOR-RUB254
                  TB-VL-RUB-ENT(25)
                  MOVE 1 TO SISPG-MARCA-RUB254
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 255
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB255
                  SPGENT-VALOR-RUB255
                  MOVE 1 TO SISPG-MARCA-RUB255
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 256
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB256
                  SPGENT-VALOR-RUB256
                  TB-VL-RUB-ENT(26)
                  MOVE 1 TO SISPG-MARCA-RUB256
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 257
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB257
                  SPGENT-VALOR-RUB257
                  TB-VL-RUB-ENT(27)
                  MOVE 1 TO SISPG-MARCA-RUB257
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 258
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB258
                  SPGENT-VALOR-RUB258
                  TB-VL-RUB-ENT(28)
                  MOVE 1 TO SISPG-MARCA-RUB258
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 259
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB259
                  SPGENT-VALOR-RUB259
                  TB-VL-RUB-ENT(29)
                  MOVE 1 TO SISPG-MARCA-RUB259
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 260
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB260
                  SISPG-VALOR-RUB260
                  MOVE 1 TO SISPG-MARCA-RUB260
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 261
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB261
                  SISPG-VALOR-RUB261
                  MOVE 1 TO SISPG-MARCA-RUB261
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 262
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB262
                  SISPG-VALOR-RUB262
                  MOVE 1 TO SISPG-MARCA-RUB262
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 263
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB263
                  SISPG-VALOR-RUB263
                  MOVE 1 TO SISPG-MARCA-RUB263
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 264
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB264
                  SISPG-VALOR-RUB264
                  TB-VL-RUB-ENT(30)
                  MOVE 1 TO SISPG-MARCA-RUB264
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 265
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB265
                  SISPG-VALOR-RUB265
                  MOVE 1 TO SISPG-MARCA-RUB265
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 266
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB266
                  SISPG-VALOR-RUB266
                  TB-VL-RUB-ENT(31)
                  MOVE 1 TO SISPG-MARCA-RUB266
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 267
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB267
                  SISPG-VALOR-RUB267
                  TB-VL-RUB-ENT(32)
                  MOVE 1 TO SISPG-MARCA-RUB267
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 268
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB268
                  SISPG-VALOR-RUB268
                  MOVE 1 TO SISPG-MARCA-RUB268
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 269
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB269
                  SISPG-VALOR-RUB269
                  TB-VL-RUB-ENT(33)
                  MOVE 1 TO SISPG-MARCA-RUB269
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) =270
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB270
                  SISPG-VALOR-RUB270
                  TB-VL-RUB-ENT(34)
                  MOVE 1 TO SISPG-MARCA-RUB270
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 271
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB271
                  SISPG-VALOR-RUB271
                  TB-VL-RUB-ENT(35)
                  MOVE 1 TO SISPG-MARCA-RUB271
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 272
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB272
                  SISPG-VALOR-RUB272
                  TB-VL-RUB-ENT(36)
                  MOVE 1 TO SISPG-MARCA-RUB272
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 273
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB273
                  SISPG-VALOR-RUB273
                  TB-VL-RUB-ENT(37)
                  MOVE 1 TO SISPG-MARCA-RUB273
           ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 274
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SPGENT-VALOR-RUB274
                  SISPG-VALOR-RUB274
                  TB-VL-RUB-ENT(38)
                  MOVE 1 TO SISPG-MARCA-RUB274
           ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 275
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
      *            SPGENT-VALOR-RUB275
                  SISPG-VALOR-RUB275
                  TB-VL-RUB-ENT(39)
                  MOVE 1 TO SISPG-MARCA-RUB275
            ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 301
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB301
                  SPGENT-VALOR-RUB301
                  MOVE 1 TO SISPG-MARCA-RUB301
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 905
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB905
                  SPGENT-VALOR-RUB905
                  MOVE 1 TO SISPG-MARCA-RUB905
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 909
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB909
                  SPGENT-VALOR-RUB909
                  MOVE 1 TO SISPG-MARCA-RUB909
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 910
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB910
                  SPGENT-VALOR-RUB910
                  MOVE 1 TO SISPG-MARCA-RUB910
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 911
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB911
                  SPGENT-VALOR-RUB911
                  MOVE 1 TO SISPG-MARCA-RUB911
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 912
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB912
                  SPGENT-VALOR-RUB912
                  MOVE 1 TO SISPG-MARCA-RUB912
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 916
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB916
                  SPGENT-VALOR-RUB916
                  MOVE 1 TO SISPG-MARCA-RUB916
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 917
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB917
                  SPGENT-VALOR-RUB917
                  MOVE 1 TO SISPG-MARCA-RUB917
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 921
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB921
                  SPGENT-VALOR-RUB921
                  MOVE 1 TO SISPG-MARCA-RUB921
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 925
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB925
                  SPGENT-VALOR-RUB925
                  MOVE 1 TO SISPG-MARCA-RUB925
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 926
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB926
                  SPGENT-VALOR-RUB926
                  MOVE 1 TO SISPG-MARCA-RUB926
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 927
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB927
                  SPGENT-VALOR-RUB927
                  MOVE 1 TO SISPG-MARCA-RUB927
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 928
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB928
                  SPGENT-VALOR-RUB928
                  MOVE 1 TO SISPG-MARCA-RUB928
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 929
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB929
                  SPGENT-VALOR-RUB929
                  MOVE 1 TO SISPG-MARCA-RUB929
             ELSE
           IF CS-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) = 933
              ADD VL-RUBRICA OF DS-MOV-CRED-SEAS (W77-IND-RUBRICAS) TO
                  SISPG-VALOR-RUB933
                  SPGENT-VALOR-RUB933
                  MOVE 1 TO SISPG-MARCA-RUB933.
      *

       2796-FIM.
      /
       3000-FINALIZACAO  SECTION.
      ******************************************************************
      *                                                                *
      *          DESCRICAO DA ROTINA DE FINALIZACAO (OPCIONAL)         *
      *                                                                *
      ******************************************************************
       3000-FINALIZAR.

           MOVE "FASE FINALIZAR " TO W77-FASE.

           IF PAR-MOV-CREDITO NOT = "APAG"
              CLOSE  DSUBCPG079 SAVE.

           IF   PAR-MOV-CREDITO        =   "APAG"
                CLOSE  DSUBCPG071 SAVE.

           IF   PAR-MOV-CREDITO        =   "APAG" OR "ALTE"
                CLOSE  BDSISBEN
                       ON EXCEPTION
                          MOVE "P/SUB/CPG/001-ERRO CLOSE BDSISBEN" TO
                                WS-MENSAGEM-ZDISPLAY
                          CALL "ZDISPLAY IN DTP/L1" USING
                                WS-MENSAGEM-ZDISPLAY,
                                WS-INTEIRO-ZDISPLAY
                          CALL SYSTEM DMTERMINATE.

           IF PAR-SEAS = SPACES
              IF PAR-MOV-CREDITO = "CONC" OR "MANU" OR "CONV"
                 CLOSE  BDSISBEN
                        ON EXCEPTION
                           MOVE "P/SUB/CPG/001-ERRO CLOSE BDSISBEN" TO
                                 WS-MENSAGEM-ZDISPLAY
                           CALL "ZDISPLAY IN DTP/L1" USING
                                 WS-MENSAGEM-ZDISPLAY,
                                 WS-INTEIRO-ZDISPLAY
                           CALL SYSTEM DMTERMINATE.

           IF PAR-MOV-CREDITO NOT = "APAG"
              CLOSE  BDBENCRED
                     ON EXCEPTION
                        MOVE "P/SUB/CPG/001-ERRO CLOSE BDBENCRED" TO
                              WS-MENSAGEM-ZDISPLAY
                        CALL "ZDISPLAY IN DTP/L1" USING
                              WS-MENSAGEM-ZDISPLAY,
                              WS-INTEIRO-ZDISPLAY
                        CALL SYSTEM DMTERMINATE.

           IF   PAR-MOV-CREDITO        =   "ALTE"
                CLOSE BDCONPAGBEN
                      ON EXCEPTION
                         MOVE "P/SUB/CPG/001-ERRO CLOSE BDCONPAGBEN" TO
                               WS-MENSAGEM-ZDISPLAY
                         CALL "ZDISPLAY IN DTP/L1" USING
                               WS-MENSAGEM-ZDISPLAY,
                               WS-INTEIRO-ZDISPLAY
                         CALL SYSTEM DMTERMINATE.

           IF PAR-SEAS = SPACES
      *        CLOSE DSUBCPG075 SAVE
              CLOSE DSUBCPG075AUX SAVE
              CLOSE DSUBCPG075 SAVE.

           IF PAR-MOV-CREDITO = "MANU" OR "CONV"
              CLOSE DSUBCPG081 SAVE.
      ***        CLOSE DSUBCPG081.

           CLOSE DSUBCPG001  SAVE.
      ***     CLOSE DSUBCPG001.
           CLOSE DSUBCPG003  SAVE.
      ***     CLOSE DSUBCPG003.
           CLOSE DSUBCPG078  SAVE.
      ***     CLOSE DSUBCPG078.


           IF   WS-FLAG-LISTA-RUB = 1
                CLOSE LSUBCPG058SISPG.

           PERFORM 6000-INCLUI-HEADER-SISPG  THRU
                   6000-INCLUI-HEADER-SISPG-EXIT
           PERFORM 7000-INCLUI-TRAILER-SISPG THRU
                   7000-INCLUI-TRAILER-SISPG-EX.

      *NOVO CLOSE DSUBCPG058SPGENT SAVE.
           CLOSE DSUBCPG058SPGENT SAVE.
           CLOSE DSUBCPG058SISPG  SAVE.

      *     CLOSE DSUBCPG058SPGCONSIG SAVE.

           IF PAR-SEAS = SPACES
              OPEN OUTPUT DSUBCPG072
                          DSUBCPG073
              PERFORM 4200-GRAVA-AGREG THRU 4200-FIM
              CLOSE DSUBCPG072 SAVE
      *        CLOSE DSUBCPG072
              CLOSE DSUBCPG073 SAVE.
      *        CLOSE DSUBCPG073.

           OPEN OUTPUT LSUBCPG001A.

           ACCEPT WS-DATA-SIS FROM DATE YYYYMMDD.
           MOVE WS-DD-SIS   TO WS-DT-AUX2-DIA
           MOVE WS-MM-SIS   TO WS-DT-AUX2-MES
           MOVE WS-AA-SIS   TO WS-DT-AUX2-ANO
           ACCEPT WS-HORA-SIS  FROM TIME.

           COMPUTE WS-DATA-FIM-LIB601 = (WS-DD-SIS * 1000000) +
                                        (WS-MM-SIS * 10000)   +
                                         WS-AA-SIS.

           MOVE WS-HH             TO WS-HORA-FIM-LIB601

           CALL "P/SUB/LIB/2K/601" USING  WS-DATA-INICIO-LIB601
                                       WS-HORA-INICIO-LIB601
                                       WS-DATA-FIM-LIB601
                                       WS-HORA-FIM-LIB601
                                       WS-INTERVALO-LIB601.

           PERFORM 3100-IMP-LSUBGER001   THRU 3100-FIM.

           CLOSE LSUBCPG001A.
      *
           IF PAR-SEAS = "SEAS"
              NEXT SENTENCE
           ELSE
              IF PAR-MOV-CREDITO = "CONC" OR "MANU" OR "CONV"
                 PERFORM 3300-EMITE-LSUBCPG001B THRU 3300-FIM.
      *
           MOVE 0 TO WS-OCORR-ERRO

           PERFORM 3200-VERIFICA-OCORRENCIA-ERRO THRU 3200-EXIT VARYING
                   W77-IND-TAB FROM 1 BY 1 UNTIL
                   W77-IND-TAB > 100 OR WS-OCORR-ERRO = 1

           IF WS-INCONS-CONSIG = 1
              MOVE "INCONSISTENCIA NB/EMPRESTIMO - PARE E VERIFIQUE" TO
                                              WS-MENSAGEM-ZDISPLAY
              CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
                                              WS-INTEIRO-ZDISPLAY.

      *    IF WS-OCORR-ERRO = 1 OR WS-INCONS-CONSIG = 1
      *       SET MYSELF (STATUS) TO -1.

      *     IF WS-INCONS-CONSIG = 1
      *        SET MYSELF (STATUS) TO -1.

       3000-FIM.  EXIT.

       3100-IMP-LSUBGER001.

            MOVE "FASE IMP-LSUBGER001 " TO W77-FASE.

            PERFORM 3115-IMP-CAB       THRU 3115-FIM.

            MOVE PAR-ESQUEMA        TO L001A-RESUP3-NRESQ.
            MOVE ZEROS TO  WS-QT-ACEITO

            IF WS-REG-LIDOS > 0
               COMPUTE WS-QT-ACEITO=((1 - (AUX-QT-ERRO /
                                             WS-REG-LIDOS)) * 100).

            MOVE WS-QT-ACEITO             TO L001A-RESUP3-QT-AC.
            MOVE WS-HORA-INICIO-LIB601 TO L001A-RESUP3-INICP.
            WRITE LSUB001-REG          FROM L001A-RESUP1 AFTER 3.
            WRITE LSUB001-REG          FROM L001A-RESUP2 AFTER 1.
            WRITE LSUB001-REG          FROM L001A-RESUP3 AFTER 2.
            MOVE WS-HORA-FIM-LIB601    TO L001A-RESUP4-FIMP.
            MOVE PAR-MOV-CREDITO       TO L001A-RESUP4-MOVCRED.
            WRITE LSUB001-REG          FROM L001A-RESUP4 AFTER 1.
            MOVE WS-INTERVALO-LIB601   TO L001A-RESUP5-ESTIMAT.

            IF PAR-SEAS = "SEAS"
               MOVE "SIM" TO L001A-RESUP5-SEAS
            ELSE
               MOVE "NAO" TO L001A-RESUP5-SEAS.

            WRITE LSUB001-REG          FROM L001A-RESUP5 AFTER 1.
            WRITE LSUB001-REG          FROM L001A-SUB    AFTER 4.

            IF PAR-MOV-CREDITO = "CONC"
               MOVE " CONCESSAO " TO L001A-REJ0-MOV
            ELSE
            IF PAR-MOV-CREDITO = "MANU"
               MOVE "MANUTENCAO " TO L001A-REJ0-MOV
            ELSE
               IF PAR-MOV-CREDITO = "ALTE"
                  MOVE "ALTERNATIVO" TO L001A-REJ0-MOV
               ELSE
                  IF PAR-MOV-CREDITO = "CONV"
                     MOVE "EMPR. CONV." TO L001A-REJ0-MOV
                  ELSE
                     MOVE "AUT. PAGTO" TO L001A-REJ0-MOV.

            WRITE LSUB001-REG          FROM L001A-REJ0   AFTER 2.
            MOVE AUX-QT-ERRO           TO L001A-REJ-REJEITADOS.
            WRITE LSUB001-REG          FROM L001A-REJ1   AFTER 2.
            MOVE AUX-QT-DELETADOS      TO L001A-REJ-DELETADOS.
            WRITE LSUB001-REG          FROM L001A-REJ1A AFTER  1
            MOVE AUX-QT-NU-SEQ-PROC    TO L001A-REJ-NU-SEQ-PROC-PAB.
            WRITE LSUB001-REG          FROM L001A-REJ1A2 AFTER  1
            WRITE LSUB001-REG          FROM L001A-REJ1A3 AFTER  1
            MOVE AUX-QT-MARCA-ERRO     TO L001A-REJ-MARCA-ERRO.
            WRITE LSUB001-REG          FROM L001A-REJ1B AFTER  1
            WRITE LSUB001-REG          FROM L001A-REJ2   AFTER 2.
            MOVE AUX-QT-ERRO-NB        TO L001A-REJ3-ERRO-NB.
            WRITE LSUB001-REG          FROM L001A-REJ3   AFTER 2.
            MOVE AUX-QT-ERRO-NATUR-CREDITO TO L001A-REJ4-ERRO-NAT.
            WRITE LSUB001-REG          FROM L001A-REJ4   AFTER 1.
            MOVE AUX-QT-ERRO-PAGTO     TO L001A-REJ5-ERRO-MEIO.
            WRITE LSUB001-REG          FROM L001A-REJ5   AFTER 1.
            MOVE AUX-QT-ERRO-PERIODO   TO L001A-REJ6-ERRO-PER.
            WRITE LSUB001-REG          FROM L001A-REJ6   AFTER 1.
            MOVE AUX-QT-ERRO-VALIDADE  TO L001A-REJ7-ERRO-VAL.
            WRITE LSUB001-REG          FROM L001A-REJ7   AFTER 1.
            MOVE AUX-QT-ERRO-BANCO     TO L001A-REJ8-ERRO-BANCO.
            WRITE LSUB001-REG          FROM L001A-REJ8   AFTER 1.
            MOVE AUX-QT-ERRO-UF        TO L001A-REJ9-ERRO-UF.
            WRITE LSUB001-REG          FROM L001A-REJ9   AFTER 1.
            MOVE AUX-QT-ERRO-VALOR     TO L001A-REJ10-ERRO-VALOR
            WRITE LSUB001-REG          FROM L001A-REJ10  AFTER 1.
            MOVE AUX-QT-ERRO-NIT       TO L001A-REJ11-ERRO-NIT
            WRITE LSUB001-REG          FROM L001A-REJ11  AFTER 1.
            WRITE LSUB001-REG          FROM L001A-CRED1  AFTER 5.
            WRITE LSUB001-REG          FROM L001A-CRED2  AFTER 1.
            WRITE LSUB001-REG          FROM L001A-CRED3  AFTER 1.
            WRITE LSUB001-REG          FROM L001A-CRED4  AFTER 1.
            WRITE LSUB001-REG          FROM L001A-CRED6  AFTER 2.
            WRITE LSUB001-REG          FROM L001A-CRED7  AFTER 1.
            ADD 39                     TO WS-CONT-LIN.
            MOVE 0                     TO W77-IND-TAB.

            PERFORM 3110-IMP-TAB-CREDPROC THRU  3110-FIM.

            MOVE  SPACES TO VALOR-DET-R.
            MOVE "REGS LOAS DEFICIENTE (87) GRAVS." TO TEXTO-DET.
            MOVE  WS-REGS-058-LOAS-DEF              TO QTDE-DET.
            WRITE LSUB001-REG  FROM LINHA-DET       AFTER 2.
      *
            MOVE "REGS LOAS DEFICIENTE (88) GRAVS." TO TEXTO-DET.
            MOVE  WS-REGS-058-LOAS-IDOSO            TO QTDE-DET.
            WRITE LSUB001-REG  FROM LINHA-DET       AFTER 1.
      *
            MOVE "REGS RMV DEFICIENTE (11,30) GRAVS." TO TEXTO-DET.
            MOVE  WS-REGS-058-RMV-DEF              TO QTDE-DET.
            WRITE LSUB001-REG  FROM LINHA-DET       AFTER 2.
      *
            MOVE "REGS RMV IDOSO (12,40) GRAVS." TO TEXTO-DET.
            MOVE  WS-REGS-058-RMV-ID                 TO QTDE-DET.
            WRITE LSUB001-REG  FROM LINHA-DET       AFTER 1.
      *
            MOVE "REGS SO EPU GRAVS.              " TO TEXTO-DET.
            MOVE  WS-REGS-058-SO-EPU                TO QTDE-DET.
            WRITE LSUB001-REG  FROM LINHA-DET       AFTER 1.
      *
            MOVE "REGS SO PREVIDENCIARIO GRAVS.   " TO TEXTO-DET.
            MOVE  WS-REGS-058-SO-PREV               TO QTDE-DET.
            WRITE LSUB001-REG  FROM LINHA-DET       AFTER 1.
      *
            MOVE "REGS MISTO GRAVS.               " TO TEXTO-DET.
            MOVE  WS-REGS-058-MISTO                 TO QTDE-DET.
            WRITE LSUB001-REG  FROM LINHA-DET       AFTER 1.
      *
            MOVE "REGS MISTO SO EPU GRAVS (301 > LIQ)" TO TEXTO-DET.
            MOVE  WS-REGS-058-MISTO-SO-EPU             TO QTDE-DET.
            WRITE LSUB001-REG  FROM LINHA-DET          AFTER 1.
      *
            MOVE "REGS COM SALARIO FAMILIA           " TO TEXTO-DET.
            MOVE  WS-REGS-SAL-FAMILIA                  TO QTDE-DET.
            WRITE LSUB001-REG  FROM LINHA-DET          AFTER 1.

            ADD 1 TO W77-IMPRESSOS.

       3100-FIM. EXIT.

       3110-IMP-TAB-CREDPROC.

            ADD 1 TO W77-IND-TAB

            IF W77-IND-TAB > 100
               GO TO 3110-FIM.

            IF TCP-LIDOS-QT  (W77-IND-TAB) = ZEROS
               GO TO 3110-IMP-TAB-CREDPROC.

            IF WS-CONT-LIN > 60
               PERFORM 3115-IMP-CAB THRU 3115-FIM.

            MOVE TCP-DATA(W77-IND-TAB)      TO WS-DT-AUX2-AAMMDD
            MOVE WS-DT-AUX2-ANO             TO WS-DT-AUX1-ANO
            MOVE WS-DT-AUX2-MES             TO WS-DT-AUX1-MES
            MOVE WS-DT-AUX2-DIA             TO WS-DT-AUX1-DIA
            MOVE WS-DT-AUX1-DDMMAA          TO L001A-CRED8-DTAMOV
            MOVE TCP-LIDOS-QT(W77-IND-TAB)  TO L001A-CRED8-LIDOS
            MOVE TCP-ACEITOS-QT(W77-IND-TAB) TO L001A-CRED8-ACEITOS
            MOVE TCP-REJ-QT(W77-IND-TAB)    TO L001A-CRED8-REJEITADOS
            MOVE TCP-LIDOS-VR(W77-IND-TAB)  TO L001A-CRED9-LIDOSVR
            MOVE TCP-ACEITOS-VR(W77-IND-TAB) TO L001A-CRED9-ACEITOSVR
            MOVE TCP-REJ-VR (W77-IND-TAB) TO L001A-CRED9-REJEITADOSVR

            IF TCP-LIDOS-QT(W77-IND-TAB)=(TCP-ACEITOS-QT (W77-IND-TAB) +
                                        TCP-REJ-QT   (W77-IND-TAB)) AND
               TCP-LIDOS-VR(W77-IND-TAB)=(TCP-ACEITOS-VR (W77-IND-TAB) +
                                        TCP-REJ-VR   (W77-IND-TAB))
               MOVE "OK  " TO L001A-CRED9-MENSAGEM
            ELSE
               MOVE "ERRO" TO L001A-CRED9-MENSAGEM.

            WRITE LSUB001-REG FROM L001A-CRED8 AFTER 1.
            WRITE LSUB001-REG FROM L001A-CRED9 AFTER 1.
            ADD 2 TO WS-CONT-LIN.
            GO TO 3110-IMP-TAB-CREDPROC.

       3110-FIM. EXIT.

       3115-IMP-CAB.

            MOVE WS-DT-AUX3-DDMMAA     TO L001A-CAB1-DATA.
            MOVE WS-HH                 TO WS-HH-INT.
            MOVE WS-HH-MM              TO L001A-CAB2-HORA.

            ADD 1                      TO WS-CONT-PAG.
            MOVE WS-CONT-PAG           TO L001A-CAB3-PAG.
            WRITE LSUB001-REG          FROM L001A-SUB AFTER  CHANNEL 1.
            WRITE LSUB001-REG          FROM L001A-CAB1   AFTER 1.
            WRITE LSUB001-REG          FROM L001A-CAB2   AFTER 1.
            WRITE LSUB001-REG          FROM L001A-CAB3   AFTER 1.
            WRITE LSUB001-REG          FROM L001A-SUB    AFTER 1.
            MOVE 5                     TO WS-CONT-LIN.

            IF WS-CONT-PAG > 1
               WRITE LSUB001-REG    FROM L001A-CRED1  AFTER 2
               WRITE LSUB001-REG    FROM L001A-CRED2  AFTER 1
               WRITE LSUB001-REG    FROM L001A-CRED3  AFTER 1
               WRITE LSUB001-REG    FROM L001A-CRED4  AFTER 1
               WRITE LSUB001-REG    FROM L001A-CRED6  AFTER 2
               WRITE LSUB001-REG    FROM L001A-CRED7  AFTER 1
               ADD  8               TO WS-CONT-LIN.

      3115-FIM. EXIT.
      *
       3200-VERIFICA-OCORRENCIA-ERRO.

           IF PAR-MOV-CREDITO = "ALTE"  AND
              TCP-DATA (W77-IND-TAB) < 19941121
              GO TO 3200-EXIT
           ELSE
             IF TCP-REJ-VR (W77-IND-TAB) > ZERO
                MOVE "HOUVE ERRO NO PROCESSAMENTO - PARE E VERIFIQUE"
                  TO WS-MENSAGEM-ZDISPLAY
                CALL "ZDISPLAY IN DTP/L1" USING WS-MENSAGEM-ZDISPLAY,
                                                WS-INTEIRO-ZDISPLAY
                MOVE 1 TO WS-OCORR-ERRO.

       3200-EXIT.
            EXIT.
      *
       3300-EMITE-LSUBCPG001B.
      *----------------------*

           OPEN OUTPUT LSUBCPG001B.
      *
           MOVE 0  TO WS-CONT-PAG.

           PERFORM 3310-EMITE-CPG001B-UF
              THRU 3310-FIM
           VARYING WS-UF FROM 2 BY 1
             UNTIL WS-UF > 28.
      *
           MOVE 1 TO WS-UF.

           PERFORM 3310-EMITE-CPG001B-UF
              THRU 3310-FIM.
      *
           MOVE SPACES TO VALOR-DET-R.
           MOVE "QTDE  DE CREDS.COM 301 NAO ENQUADR."   TO TEXTO-DET.
           MOVE  WS-REGS-DESP-EPU                       TO QTDE-DET.
           WRITE LSUB001B-REG  FROM LINHA-DET AFTER 3.
      *
           MOVE SPACES TO QTDE-DET-R.
           MOVE "VALOR DE CREDS.COM 301 NAO ENQUADR."   TO TEXTO-DET.
           MOVE  WS-VL-DESP-EPU                         TO VALOR-DET.
           WRITE LSUB001B-REG  FROM LINHA-DET AFTER 1.
      *
           CLOSE LSUBCPG001B.
           IF PAR-MOV-CREDITO = "MANU" OR "CONV"
              CLOSE DSUBDNT001 SAVE.

           PERFORM 3320-IMP-ENT-CLASSES
              THRU 3320-FIM.

           CLOSE LSUBCPG058SISPG.
      *
       3300-FIM.  EXIT.
      *--------*
      *
      /
       3310-EMITE-CPG001B-UF.
      *---------------------*
      *
      *    CABECALHO
      *
           ADD 1                      TO WS-CONT-PAG.
           MOVE WS-CONT-PAG           TO L001B-CAB-PAG.
           WRITE LSUB001B-REG         FROM L001A-SUB AFTER  CHANNEL 1.
           WRITE LSUB001B-REG         FROM L001A-CAB1   AFTER 1.
           WRITE LSUB001B-REG         FROM L001A-CAB2   AFTER 1.
           WRITE LSUB001B-REG         FROM L001B-CAB4   AFTER 1.
           WRITE LSUB001B-REG         FROM L001A-SUB    AFTER 1.

           IF WS-UF = 1
              MOVE "BR" TO NM-SIGLA-UF-CAB5
              ADD 1 TO WS-IND-DNT
           ELSE
              MOVE  WS-TB-SIGL-UF  ( WS-UF - 1 ) TO  NM-SIGLA-UF-CAB5.

           WRITE LSUB001B-REG         FROM L001B-CAB5   AFTER 2.
           WRITE LSUB001B-REG         FROM L001B-CAB6   AFTER 3.
           WRITE LSUB001B-REG         FROM L001B-CAB7   AFTER 1.
      *
      *    DETALHE
      *
           MOVE ZEROS TO WS-QT-AUX
                         WS-VL-AUX.
           MOVE 1 TO WS-IND-DNT.
           MOVE WS-QTD-PAB-TOT TO WS-QTD-DNT(3)
           MOVE WS-VL-PAB-TOT  TO WS-VL-DNT(3).

           PERFORM 3315-IMP-VALORES-DET-001B
              THRU 3315-FIM
           VARYING WS-IND FROM 1 BY 1
             UNTIL WS-IND > 11.

           WRITE   LSUB001B-REG FROM L001B-CAB7 AFTER 1.
           MOVE    "TOTAL"      TO DESC-DET-001B.
           MOVE     WS-QT-AUX   TO QT-DET-001B.
           MOVE     WS-VL-AUX   TO VL-DET-001B.
           WRITE   LSUB001B-REG FROM L001B-DET  AFTER 1.
           WRITE   LSUB001B-REG FROM L001B-CAB7 AFTER 1.

           IF (ATTRIBUTE RESIDENT OF DSUBDNT001 = VALUE TRUE)
             IF (PAR-MOV-CREDITO = "MANU" OR "CONV") AND (WS-UF = 1)
                PERFORM 3318-GRAVA-ARQ-DNT THRU 3318-FIM-GRAVA-ARQ-DNT
                   VARYING WS-IND-DNT FROM 1 BY 1
                   UNTIL WS-IND-DNT > 12
              COMPUTE COD-DESCRICAO = WS-IND-DNT
              COMPUTE QTD-CREDITOS = WS-QT-AUX + WS-QT-AUX-LIDO
              COMPUTE VL-CREDITOS   = WS-VL-AUX + WS-VL-AUX-LIDO
              WRITE REG-DNT
              ADD 1 TO WS-GRAVADOS-DNT
             ELSE
               NEXT SENTENCE
           ELSE
             STRING
             "DSUBDNT001 NAO RESIDENTE: "WS-TITLE-DSUBDNT001
             DELIMITED BY SIZE INTO   WS-MENSAGEM-ZDISPLAY
             CALL "ZDISPLAY IN DTP/L1" USING
                                                WS-MENSAGEM-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY.

      *
       3310-FIM.  EXIT.
      *--------*
      *
      /
      *-------------------------*
       3315-IMP-VALORES-DET-001B.
      *-------------------------*
      *
           MOVE  WS-DESC-EPU-TAB   ( WS-IND ) TO DESC-DET-001B.

           MOVE  WS-QT-EPU-REL ( WS-UF WS-IND )
             TO  QT-DET-001B.
           ADD   WS-QT-EPU-REL ( WS-UF WS-IND ) TO WS-QT-AUX.
           MOVE  WS-VL-EPU-REL ( WS-UF WS-IND )
             TO  VL-DET-001B.
           ADD   WS-VL-EPU-REL ( WS-UF WS-IND ) TO WS-VL-AUX.

      *Jose Fonseca - DM.096405 - INSS EM NUMEROS
           IF WS-UF = 1
              IF WS-IND = 8 OR 9
                 COMPUTE  WS-QTD-DNT(9) = WS-QTD-DNT(9) +
                                         WS-QT-EPU-REL ( WS-UF WS-IND )
                 COMPUTE WS-VL-DNT(9) = WS-VL-DNT(9) +
                                        WS-VL-EPU-REL ( WS-UF WS-IND )
              ELSE
                 MOVE WS-QT-EPU-REL ( WS-UF WS-IND ) TO
                                            WS-QTD-DNT(WS-IND-DNT + 3)
                 MOVE WS-VL-EPU-REL ( WS-UF WS-IND ) TO
                                            WS-VL-DNT(WS-IND-DNT + 3)
                 ADD 1 TO WS-IND-DNT.
      *Fim DM.096405.

           WRITE LSUB001B-REG  FROM L001B-DET   AFTER 1.
      *
       3315-FIM.  EXIT.
      *--------*
      *
       3318-GRAVA-ARQ-DNT.
      *-------------------

           COMPUTE COD-DESCRICAO = WS-IND-DNT
           COMPUTE QTD-CREDITOS = WS-QTD-DNT(WS-IND-DNT) +
                                            WS-QTD-DNT-LIDO(WS-IND-DNT)
           COMPUTE VL-CREDITOS = WS-VL-DNT(WS-IND-DNT) +
                                            WS-VL-DNT-LIDO(WS-IND-DNT)
           WRITE REG-DNT
           ADD 1 TO WS-GRAVADOS-DNT.

       3318-FIM-GRAVA-ARQ-DNT. EXIT.
      *----------------------------
      /
      *--------------------------------------------------
       3319-LER-DNT.
      *--------------------------------------------------

           READ DSUBDNT001
               AT END
               GO TO 3319-FIM-LER-DNT.
           IF  WS-IND-DNT < 13
               MOVE QTD-CREDITOS  TO WS-QTD-DNT-LIDO(WS-IND-DNT)
               MOVE VL-CREDITOS   TO WS-VL-DNT-LIDO(WS-IND-DNT).

      *-------------------------------------------------
       3319-FIM-LER-DNT. EXIT.
      *-------------------------------------------------
      *----------------------------
      /
       3320-IMP-ENT-CLASSES.
      *----------------------

           OPEN OUTPUT LSUBCPG058SISPG.

           MOVE WS-DT-AUX3-DDMMAA            TO LSPG1A-CAB1-DATA.
           MOVE WS-HH-MM                     TO LSPG1A-CAB2-HORA.
           MOVE 0001                         TO LSPG1A-CAB3-PAG.
           MOVE PAR-MOV-CREDITO              TO LSPG1A-CAB3-MOV.
      *     WRITE LSUBSISPG-REG         FROM  LSPG1A-SPACES.

           WRITE LSUBSISPG-REG         FROM LSPG1A-SUB     AFTER 1.

           WRITE LSUBSISPG-REG         FROM LSPG1A-CAB1    AFTER 1.

           WRITE LSUBSISPG-REG         FROM LSPG1A-CAB2    AFTER 1.

           WRITE LSUBSISPG-REG         FROM LSPG1A-CAB3    AFTER 1.

           WRITE LSUBSISPG-REG         FROM LSPG1A-SUB     AFTER 1.

           WRITE LSUBSISPG-REG         FROM LSPG-CAB6   AFTER 3.
           WRITE LSUBSISPG-REG         FROM LSPG-CAB7   AFTER 1.
      *
      *    DETALHE
      *
           MOVE ZEROS TO WS-QT-AUX
                         WS-VL-AUX.

           PERFORM 3320-IMP-VALORES-DET-SPG
              THRU 3320-FIM-IMP
           VARYING WS-IND FROM 1 BY 1
             UNTIL WS-IND > 39.

           WRITE   LSUBSISPG-REG FROM LSPG-CAB7 AFTER 1.

       3320-FIM. EXIT.
      *------------------------------
       3320-IMP-VALORES-DET-SPG.
      *------------------------------

           MOVE  TAB-NM-ENTIDADE(WS-IND)     TO DESC-DET-SPG.

           MOVE TAB-CS-RUBR-ENT(WS-IND)          TO CS-RUBR-ENT

           MOVE  TB-QT-TOTAL-ENT(WS-IND)     TO QT-DET-SPG.

           MOVE  TB-VL-TOTAL-RUB-ENT(WS-IND) TO  VL-DET-SPG.

           WRITE LSUBSISPG-REG  FROM LSPG-DET   AFTER 1.

      *------------------------------
       3320-FIM-IMP.
       EXIT.
      *------------------------------

       4000-ENVIA-OFIS.

           MOVE "ENVIANDO MSG VIA OFIS" TO W77-FASE.
           MOVE PAR-ESQUEMA             TO WS-OFIS-ESQUEMA.
           MOVE 1                       TO L001A-CAB3-PAG.

           WRITE REG-WOFIS            FROM L001A-SUB
           WRITE REG-WOFIS            FROM L001A-CAB1
           WRITE REG-WOFIS            FROM L001A-CAB2
           WRITE REG-WOFIS            FROM L001A-CAB3
           WRITE REG-WOFIS            FROM L001A-SUB
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-RESUP1
           WRITE REG-WOFIS            FROM L001A-RESUP2
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-RESUP3
           WRITE REG-WOFIS            FROM L001A-RESUP4
           WRITE REG-WOFIS            FROM L001A-RESUP5
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-SUB
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-REJ0
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-REJ1
           WRITE REG-WOFIS            FROM L001A-REJ1A
           WRITE REG-WOFIS            FROM L001A-REJ1A2
           WRITE REG-WOFIS            FROM L001A-REJ1A3
           WRITE REG-WOFIS            FROM L001A-REJ1B
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-REJ2
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-REJ3
           WRITE REG-WOFIS            FROM L001A-REJ4
           WRITE REG-WOFIS            FROM L001A-REJ5
           WRITE REG-WOFIS            FROM L001A-REJ6
           WRITE REG-WOFIS            FROM L001A-REJ7
           WRITE REG-WOFIS            FROM L001A-REJ8
           WRITE REG-WOFIS            FROM L001A-REJ9
           WRITE REG-WOFIS            FROM L001A-REJ10
           WRITE REG-WOFIS            FROM L001A-REJ11
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-CRED1
           WRITE REG-WOFIS            FROM L001A-CRED2
           WRITE REG-WOFIS            FROM L001A-CRED3
           WRITE REG-WOFIS            FROM L001A-CRED4
           WRITE REG-WOFIS            FROM L001A-SPACES
           WRITE REG-WOFIS            FROM L001A-CRED6
           WRITE REG-WOFIS            FROM L001A-CRED7

           MOVE ZEROS                 TO W77-IND-TAB.

           PERFORM 4100-IMP-TAB-PARA-OFIS THRU  4100-FIM.

           MOVE  SPACES TO REG-WOFIS.
           WRITE REG-WOFIS.

           MOVE  SPACES TO VALOR-DET-R.
           MOVE "REGS LOAS DEFICIENTE (87) GRAVS." TO TEXTO-DET.
           MOVE  WS-REGS-058-LOAS-DEF              TO QTDE-DET.
           WRITE REG-WOFIS    FROM LINHA-DET.
      *
           MOVE "REGS LOAS DEFICIENTE (88) GRAVS." TO TEXTO-DET.
           MOVE  WS-REGS-058-LOAS-IDOSO            TO QTDE-DET.
           WRITE REG-WOFIS    FROM LINHA-DET.
      *
           MOVE "REGS SO EPU GRAVS.              " TO TEXTO-DET.
           MOVE  WS-REGS-058-SO-EPU                TO QTDE-DET.
           WRITE REG-WOFIS    FROM LINHA-DET.
      *
           MOVE "REGS SO PREVIDENCIARIO GRAVS.   " TO TEXTO-DET.
           MOVE  WS-REGS-058-SO-PREV               TO QTDE-DET.
           WRITE REG-WOFIS    FROM LINHA-DET.
      *
           MOVE "REGS MISTO GRAVS.               " TO TEXTO-DET.
           MOVE  WS-REGS-058-MISTO                 TO QTDE-DET.
           WRITE REG-WOFIS    FROM LINHA-DET.
      *
           MOVE "REGS MISTO SO EPU GRAVS (301 > LIQ)" TO TEXTO-DET.
           MOVE  WS-REGS-058-MISTO-SO-EPU             TO QTDE-DET.
           WRITE REG-WOFIS FROM LINHA-DET.
      *
           CLOSE WOFIS SAVE.
       4000-FIM.
            EXIT.
      *
       4100-IMP-TAB-PARA-OFIS.

            ADD 1 TO W77-IND-TAB

            IF W77-IND-TAB > 100
               GO TO 4100-FIM.

            IF TCP-LIDOS-QT  (W77-IND-TAB) = ZEROS
               GO TO 4100-IMP-TAB-PARA-OFIS.

            MOVE TCP-DATA(W77-IND-TAB)      TO WS-DT-AUX2-AAMMDD
            MOVE WS-DT-AUX2-ANO             TO WS-DT-AUX1-ANO
            MOVE WS-DT-AUX2-MES             TO WS-DT-AUX1-MES
            MOVE WS-DT-AUX2-DIA             TO WS-DT-AUX1-DIA
            MOVE WS-DT-AUX1-DDMMAA          TO L001A-CRED8-DTAMOV
            MOVE TCP-LIDOS-QT(W77-IND-TAB)  TO L001A-CRED8-LIDOS
            MOVE TCP-ACEITOS-QT(W77-IND-TAB) TO L001A-CRED8-ACEITOS
            MOVE TCP-REJ-QT(W77-IND-TAB)    TO L001A-CRED8-REJEITADOS
            MOVE TCP-LIDOS-VR(W77-IND-TAB)  TO L001A-CRED9-LIDOSVR
            MOVE TCP-ACEITOS-VR(W77-IND-TAB) TO L001A-CRED9-ACEITOSVR
            MOVE TCP-REJ-VR (W77-IND-TAB) TO L001A-CRED9-REJEITADOSVR

            IF TCP-LIDOS-QT(W77-IND-TAB)=(TCP-ACEITOS-QT (W77-IND-TAB) +
                                        TCP-REJ-QT   (W77-IND-TAB)) AND
               TCP-LIDOS-VR(W77-IND-TAB)=(TCP-ACEITOS-VR (W77-IND-TAB) +
                                        TCP-REJ-VR   (W77-IND-TAB))
               MOVE "OK  " TO L001A-CRED9-MENSAGEM
            ELSE
               MOVE "ERRO" TO L001A-CRED9-MENSAGEM.

            WRITE REG-WOFIS   FROM L001A-CRED8
            WRITE REG-WOFIS   FROM L001A-CRED9
            GO TO 4100-IMP-TAB-PARA-OFIS.

       4100-FIM. EXIT.
      /
       4200-GRAVA-AGREG.

            PERFORM 4210A-TOT-T5-DESC THRU 4210A-FIM
                    VARYING WS-COMP FROM 1 BY 1
                      UNTIL WS-COMP  > 5
                    AFTER WS-UF FROM 1 BY 1
                      UNTIL WS-UF > 28
                    AFTER WS-TIPO FROM 1 BY 1
                      UNTIL WS-TIPO > 4.

            PERFORM 4210-GRAVA-TP1 THRU 4210-FIM
                    VARYING WS-COMP FROM 1 BY 1
                      UNTIL WS-COMP  > 5
                    AFTER WS-UF FROM 1 BY 1
                      UNTIL WS-UF > 28
                    AFTER WS-ESP   FROM 1 BY 1
                      UNTIL WS-ESP   > 99.

            PERFORM 4211-GRAVA-TP2 THRU 4211-FIM
                    VARYING WS-COMP FROM 1 BY 1
                      UNTIL WS-COMP  > 5
                    AFTER WS-UF FROM 1 BY 1
                      UNTIL WS-UF > 28
                    AFTER WS-ESP   FROM 1 BY 1
                      UNTIL WS-ESP   > 99.

           PERFORM 4212-GRAVA-TP3 THRU 4212-FIM
                   VARYING WS-COMP FROM 1 BY 1
                     UNTIL WS-COMP  > 5
                   AFTER WS-UF FROM 1 BY 1
                     UNTIL WS-UF > 28
                   AFTER WS-ESP   FROM 1 BY 1
                     UNTIL WS-ESP   > 99.

           PERFORM 4213-GRAVA-TP4 THRU 4213-FIM
                   VARYING WS-COMP FROM 1 BY 1
                     UNTIL WS-COMP  > 5
                   AFTER WS-UF FROM 1 BY 1
                     UNTIL WS-UF > 28
                   AFTER WS-ESP   FROM 1 BY 1
                     UNTIL WS-ESP   > 99.

           PERFORM 4214-GRAVA-TP5 THRU 4214-FIM
                   VARYING WS-COMP FROM 1 BY 1
                     UNTIL WS-COMP  > 5
                   AFTER WS-UF FROM 1 BY 1
                     UNTIL WS-UF > 28
                   AFTER WS-ESP   FROM 1 BY 1
                     UNTIL WS-ESP   > 99.

           PERFORM 4215-GRAVA-TP6 THRU 4215-FIM
                   VARYING WS-COMP FROM 1 BY 1
                     UNTIL WS-COMP  > 5
                   AFTER WS-UF FROM 1 BY 1
                     UNTIL WS-UF > 28
                   AFTER WS-ESP   FROM 1 BY 1
                     UNTIL WS-ESP   > 99.

           PERFORM 4216-GRAVA-TP7 THRU 4216-FIM
                   VARYING WS-COMP FROM 1 BY 1
                     UNTIL WS-COMP  > 5
                   AFTER WS-UF FROM 1 BY 1
                     UNTIL WS-UF > 28
                   AFTER WS-ESP   FROM 1 BY 1
                     UNTIL WS-ESP   > 99.

           PERFORM 4216B-GRAVA-TP8 THRU 4216B-FIM
                   VARYING WS-COMP FROM 1 BY 1
                     UNTIL WS-COMP  > 5
                   AFTER WS-UF FROM 1 BY 1
                     UNTIL WS-UF > 28
                   AFTER WS-ESP   FROM 1 BY 1
                     UNTIL WS-ESP   > 99.

           PERFORM 4216C-GRAVA-TP9 THRU 4216C-FIM
                   VARYING WS-COMP FROM 1 BY 1
                     UNTIL WS-COMP  > 5
                   AFTER WS-UF FROM 1 BY 1
                     UNTIL WS-UF > 28
                   AFTER WS-ESP   FROM 1 BY 1
                     UNTIL WS-ESP   > 99.

           PERFORM 4217-GRAVA-TP0 THRU 4217-FIM
                   VARYING WS-COMP FROM 1 BY 1
                     UNTIL WS-COMP  > 5
                   AFTER WS-UF FROM 1 BY 1
                     UNTIL WS-UF > 28
                   AFTER WS-ESP   FROM 1 BY 1
                     UNTIL WS-ESP   > 99.

           PERFORM 4218-GRAVA-EST THRU 4218-FIM
                   VARYING WS-COMP FROM 1 BY 1
                     UNTIL WS-COMP  > 5
                   AFTER WS-UF FROM 1 BY 1
                     UNTIL WS-UF > 28
                   AFTER WS-TIPO FROM 1 BY 1
                     UNTIL WS-TIPO  > 10.

       4200-FIM. EXIT.
      /
       4210A-TOT-T5-DESC.

           IF WS-TIPO = 1 OR 3 OR 4
              ADD WS-TOT-IR-PREV  (WS-COMP, WS-UF, WS-TIPO) TO
                  WS-TOT-IR-PREV  (WS-COMP, WS-UF,       5)
              ADD WS-TOT-IR-UNIAO (WS-COMP, WS-UF, WS-TIPO) TO
                  WS-TOT-IR-UNIAO (WS-COMP, WS-UF,       5)
              ADD WS-TOT-CONT-PREV(WS-COMP, WS-UF, WS-TIPO) TO
                  WS-TOT-CONT-PREV(WS-COMP, WS-UF,       5)
              ADD WS-TOT-PA       (WS-COMP, WS-UF, WS-TIPO) TO
                  WS-TOT-PA       (WS-COMP, WS-UF,       5)
              ADD WS-TOT-13-SAL   (WS-COMP, WS-UF, WS-TIPO) TO
                  WS-TOT-13-SAL   (WS-COMP, WS-UF,       5)
              ADD WS-TOT-SF       (WS-COMP, WS-UF, WS-TIPO) TO
                  WS-TOT-SF       (WS-COMP, WS-UF,       5)
              ADD WS-TOT-CPMF     (WS-COMP, WS-UF, WS-TIPO) TO
                  WS-TOT-CPMF     (WS-COMP, WS-UF,       5)
              PERFORM 4210B-TOTBR-CONSIG THRU 4210B-FIM
                      VARYING WS-ESP FROM 1 BY 1
                        UNTIL WS-ESP > 16.

           ADD WS-TOT-IR-PREV  (WS-COMP, WS-UF, WS-TIPO) TO
               WS-TOT-IR-PREV  (WS-COMP, WS-UF,      10).
           ADD WS-TOT-IR-UNIAO (WS-COMP, WS-UF, WS-TIPO) TO
               WS-TOT-IR-UNIAO (WS-COMP, WS-UF,      10).
           ADD WS-TOT-CONT-PREV(WS-COMP, WS-UF, WS-TIPO) TO
               WS-TOT-CONT-PREV(WS-COMP, WS-UF,      10).
           ADD WS-TOT-PA       (WS-COMP, WS-UF, WS-TIPO) TO
               WS-TOT-PA       (WS-COMP, WS-UF,      10).
           ADD WS-TOT-13-SAL   (WS-COMP, WS-UF, WS-TIPO) TO
               WS-TOT-13-SAL   (WS-COMP, WS-UF,      10).
           ADD WS-TOT-SF       (WS-COMP, WS-UF, WS-TIPO) TO
               WS-TOT-SF       (WS-COMP, WS-UF,      10).
           ADD WS-TOT-CPMF     (WS-COMP, WS-UF, WS-TIPO) TO
               WS-TOT-CPMF     (WS-COMP, WS-UF,      10).

           PERFORM 4210C-TOTBR-CONSIG THRU 4210C-FIM
                   VARYING WS-ESP FROM 1 BY 1
                     UNTIL WS-ESP > 16.

       4210A-FIM. EXIT.

       4210B-TOTBR-CONSIG.

           IF WS-TOT-COD-CONSIG
              (WS-COMP, WS-UF, WS-TIPO, WS-ESP) NOT = ZEROS
              ADD WS-TOT-COD-CONSIG
                  (WS-COMP, WS-UF, WS-TIPO, WS-ESP) TO
                  WS-TOT-COD-CONSIG
                  (WS-COMP, WS-UF,       5, WS-ESP).

       4210B-FIM. EXIT.

       4210C-TOTBR-CONSIG.

           IF WS-TOT-COD-CONSIG
              (WS-COMP, WS-UF, WS-TIPO, WS-ESP) NOT = ZEROS
              ADD WS-TOT-COD-CONSIG
                  (WS-COMP, WS-UF, WS-TIPO, WS-ESP) TO
                  WS-TOT-COD-CONSIG
                  (WS-COMP, WS-UF,      10, WS-ESP).

       4210C-FIM. EXIT.
      /
       4210-GRAVA-TP1.

           IF WS-TB-TOT-BRUTO-ESP1 (WS-COMP, WS-UF, WS-ESP) NOT = 0
           OR WS-TB-TOT-DESC-ESP1  (WS-COMP, WS-UF, WS-ESP) NOT = 0
              MOVE WS-TB-COMPET (WS-COMP)       TO CPG072-COMPET
              MOVE 1                            TO CPG072-TIPO
              MOVE WS-TB-SIGL-UF(WS-UF)         TO CPG072-SIGL-UF
              MOVE WS-ESP                       TO CPG072-ESPECIE
              MOVE WS-TB-TOT-BRUTO-ESP1 (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-BRUTO
              MOVE WS-TB-TOT-DESC-ESP1  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-DESC
              MOVE WS-TB-TOT-ESP1       (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-TOT-BENEF
              ADD  1                            TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM
              ADD  WS-TB-TOT-BRUTO-ESP1 (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-BRUTO-ESP1 (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-DESC-ESP1  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-DESC-ESP1  (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-ESP1  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-ESP1  (WS-COMP, WS-UF).

           IF WS-ESP = 99 AND
              (WS-TB-TOT-GER-BRUTO-ESP1 (WS-COMP, WS-UF) NOT = 0 OR
               WS-TB-TOT-GER-DESC-ESP1  (WS-COMP, WS-UF) NOT = 0)
              MOVE WS-TB-COMPET (WS-COMP)  TO CPG072-COMPET
              MOVE ZEROS  TO CPG072-ESPECIE
              MOVE WS-TB-TOT-GER-BRUTO-ESP1 (WS-COMP, WS-UF) TO
                             CPG072-BRUTO
              MOVE WS-TB-TOT-GER-DESC-ESP1  (WS-COMP, WS-UF) TO
                             CPG072-DESC
              MOVE WS-TB-TOT-GER-ESP1  (WS-COMP, WS-UF) TO
                             CPG072-TOT-BENEF
              ADD 1       TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM.

       4210-FIM. EXIT.

       4211-GRAVA-TP2.

           IF WS-TB-TOT-BRUTO-ESP2 (WS-COMP, WS-UF, WS-ESP) NOT = 0
           OR WS-TB-TOT-DESC-ESP2  (WS-COMP, WS-UF, WS-ESP) NOT = 0
              MOVE WS-TB-COMPET (WS-COMP)       TO CPG072-COMPET
              MOVE 2                            TO CPG072-TIPO
              MOVE WS-TB-SIGL-UF(WS-UF)         TO CPG072-SIGL-UF
              MOVE WS-ESP                       TO CPG072-ESPECIE
              MOVE WS-TB-TOT-BRUTO-ESP2 (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-BRUTO
              MOVE WS-TB-TOT-DESC-ESP2  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-DESC
              MOVE WS-TB-TOT-ESP2  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-TOT-BENEF
              ADD  1                            TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM
              ADD  WS-TB-TOT-BRUTO-ESP2 (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-BRUTO-ESP1 (WS-COMP, WS-UF, WS-ESP)
                   WS-TB-TOT-GER-BRUTO-ESP2 (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-DESC-ESP2  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-DESC-ESP1  (WS-COMP, WS-UF, WS-ESP)
                   WS-TB-TOT-GER-DESC-ESP2  (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-ESP2  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-ESP1  (WS-COMP, WS-UF, WS-ESP)
                   WS-TB-TOT-GER-ESP2  (WS-COMP, WS-UF).

           IF WS-ESP = 99 AND
              (WS-TB-TOT-GER-BRUTO-ESP2 (WS-COMP, WS-UF) NOT = 0 OR
               WS-TB-TOT-GER-DESC-ESP2  (WS-COMP, WS-UF) NOT = 0)
              MOVE WS-TB-COMPET (WS-COMP) TO CPG072-COMPET
              MOVE ZEROS  TO CPG072-ESPECIE
              MOVE WS-TB-TOT-GER-BRUTO-ESP2 (WS-COMP, WS-UF) TO
                             CPG072-BRUTO
              MOVE WS-TB-TOT-GER-DESC-ESP2  (WS-COMP, WS-UF) TO
                             CPG072-DESC
              MOVE WS-TB-TOT-GER-ESP2(WS-COMP, WS-UF) TO
                             CPG072-TOT-BENEF
              ADD 1       TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM.

       4211-FIM. EXIT.
      /
       4212-GRAVA-TP3.

           IF WS-TB-TOT-BRUTO-ESP3 (WS-COMP, WS-UF, WS-ESP) NOT = 0
           OR WS-TB-TOT-DESC-ESP3  (WS-COMP, WS-UF, WS-ESP) NOT = 0
              MOVE WS-TB-COMPET (WS-COMP)       TO CPG072-COMPET
              MOVE 3                            TO CPG072-TIPO
              MOVE WS-TB-SIGL-UF(WS-UF)         TO CPG072-SIGL-UF
              MOVE WS-ESP                       TO CPG072-ESPECIE
              MOVE WS-TB-TOT-BRUTO-ESP3 (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-BRUTO
              MOVE WS-TB-TOT-DESC-ESP3  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-DESC
              MOVE WS-TB-TOT-ESP3  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-TOT-BENEF
              ADD  1                            TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM
              ADD  WS-TB-TOT-BRUTO-ESP3 (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-BRUTO-ESP1 (WS-COMP, WS-UF, WS-ESP)
                   WS-TB-TOT-GER-BRUTO-ESP3 (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-DESC-ESP3  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-DESC-ESP1  (WS-COMP, WS-UF, WS-ESP)
                   WS-TB-TOT-GER-DESC-ESP3  (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-ESP3  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-ESP1  (WS-COMP, WS-UF, WS-ESP)
                   WS-TB-TOT-GER-ESP3  (WS-COMP, WS-UF).

           IF WS-ESP = 99 AND
              (WS-TB-TOT-GER-BRUTO-ESP3 (WS-COMP, WS-UF) NOT = 0 OR
               WS-TB-TOT-GER-DESC-ESP3  (WS-COMP, WS-UF) NOT = 0)
              MOVE WS-TB-COMPET (WS-COMP) TO CPG072-COMPET
              MOVE ZEROS  TO CPG072-ESPECIE
              MOVE WS-TB-TOT-GER-BRUTO-ESP3 (WS-COMP, WS-UF) TO
                             CPG072-BRUTO
              MOVE WS-TB-TOT-GER-DESC-ESP3  (WS-COMP, WS-UF) TO
                             CPG072-DESC
              MOVE WS-TB-TOT-GER-ESP3  (WS-COMP, WS-UF) TO
                             CPG072-TOT-BENEF
              ADD 1       TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM.

       4212-FIM. EXIT.

       4213-GRAVA-TP4.

           IF WS-TB-TOT-BRUTO-ESP4 (WS-COMP, WS-UF, WS-ESP) NOT = 0
           OR WS-TB-TOT-DESC-ESP4  (WS-COMP, WS-UF, WS-ESP) NOT = 0
              MOVE WS-TB-COMPET (WS-COMP)       TO CPG072-COMPET
              MOVE 4                            TO CPG072-TIPO
              MOVE WS-TB-SIGL-UF(WS-UF)         TO CPG072-SIGL-UF
              MOVE WS-ESP                       TO CPG072-ESPECIE
              MOVE WS-TB-TOT-BRUTO-ESP4 (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-BRUTO
              MOVE WS-TB-TOT-DESC-ESP4  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-DESC
              MOVE WS-TB-TOT-ESP4  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-TOT-BENEF
              ADD  1                            TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM
              ADD  WS-TB-TOT-BRUTO-ESP4 (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-BRUTO-ESP1 (WS-COMP, WS-UF, WS-ESP)
                   WS-TB-TOT-GER-BRUTO-ESP4 (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-DESC-ESP4  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-DESC-ESP1 (WS-COMP, WS-UF, WS-ESP)
                   WS-TB-TOT-GER-DESC-ESP4  (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-ESP4  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-ESP1 (WS-COMP, WS-UF, WS-ESP)
                   WS-TB-TOT-GER-ESP4  (WS-COMP, WS-UF).

           IF WS-ESP = 99 AND
              (WS-TB-TOT-GER-BRUTO-ESP4 (WS-COMP, WS-UF) NOT = 0 OR
               WS-TB-TOT-GER-DESC-ESP4  (WS-COMP, WS-UF) NOT = 0)
              MOVE WS-TB-COMPET (WS-COMP) TO CPG072-COMPET
              MOVE ZEROS  TO CPG072-ESPECIE
              MOVE WS-TB-TOT-GER-BRUTO-ESP4 (WS-COMP, WS-UF) TO
                             CPG072-BRUTO
              MOVE WS-TB-TOT-GER-DESC-ESP4  (WS-COMP, WS-UF) TO
                             CPG072-DESC
              MOVE WS-TB-TOT-GER-ESP4  (WS-COMP, WS-UF) TO
                             CPG072-TOT-BENEF
              ADD 1       TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM.

       4213-FIM. EXIT.
      /
       4214-GRAVA-TP5.

           IF WS-TB-TOT-BRUTO-ESP5 (WS-COMP, WS-UF, WS-ESP) NOT = 0
           OR WS-TB-TOT-DESC-ESP5  (WS-COMP, WS-UF, WS-ESP) NOT = 0
              MOVE WS-TB-COMPET (WS-COMP)       TO CPG072-COMPET
              MOVE 5                            TO CPG072-TIPO
              MOVE WS-TB-SIGL-UF(WS-UF)         TO CPG072-SIGL-UF
              MOVE WS-ESP                       TO CPG072-ESPECIE
              MOVE WS-TB-TOT-BRUTO-ESP5 (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-BRUTO
              MOVE WS-TB-TOT-DESC-ESP5  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-DESC
              MOVE WS-TB-TOT-ESP5  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-TOT-BENEF
              ADD  1                            TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM
              ADD  WS-TB-TOT-BRUTO-ESP5 (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-BRUTO-ESP5 (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-DESC-ESP5  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-DESC-ESP5  (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-ESP5  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-ESP5  (WS-COMP, WS-UF).

           IF WS-ESP = 99 AND
              (WS-TB-TOT-GER-BRUTO-ESP5 (WS-COMP, WS-UF) NOT = 0 OR
               WS-TB-TOT-GER-DESC-ESP5  (WS-COMP, WS-UF) NOT = 0)
              MOVE WS-TB-COMPET (WS-COMP) TO CPG072-COMPET
              MOVE ZEROS  TO CPG072-ESPECIE
              MOVE WS-TB-TOT-GER-BRUTO-ESP5 (WS-COMP, WS-UF) TO
                             CPG072-BRUTO
              MOVE WS-TB-TOT-GER-DESC-ESP5  (WS-COMP, WS-UF) TO
                             CPG072-DESC
              MOVE WS-TB-TOT-GER-ESP5  (WS-COMP, WS-UF) TO
                             CPG072-TOT-BENEF
              ADD 1       TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM.

       4214-FIM. EXIT.

       4215-GRAVA-TP6.

           IF WS-TB-TOT-BRUTO-ESP6 (WS-COMP, WS-UF, WS-ESP) NOT = 0
           OR WS-TB-TOT-DESC-ESP6  (WS-COMP, WS-UF, WS-ESP) NOT = 0
              MOVE WS-TB-COMPET (WS-COMP)       TO CPG072-COMPET
              MOVE 6                            TO CPG072-TIPO
              MOVE WS-TB-SIGL-UF(WS-UF)         TO CPG072-SIGL-UF
              MOVE WS-ESP                       TO CPG072-ESPECIE
              MOVE WS-TB-TOT-BRUTO-ESP6 (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-BRUTO
              MOVE WS-TB-TOT-DESC-ESP6  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-DESC
              MOVE WS-TB-TOT-ESP6  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-TOT-BENEF
              ADD  1                            TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM
              ADD  WS-TB-TOT-BRUTO-ESP6 (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-BRUTO-ESP6 (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-DESC-ESP6  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-DESC-ESP6  (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-ESP6  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-ESP6  (WS-COMP, WS-UF).

           IF WS-ESP = 99 AND
              (WS-TB-TOT-GER-BRUTO-ESP6 (WS-COMP, WS-UF) NOT = 0 OR
               WS-TB-TOT-GER-DESC-ESP6  (WS-COMP, WS-UF) NOT = 0)
              MOVE WS-TB-COMPET (WS-COMP) TO CPG072-COMPET
              MOVE ZEROS  TO CPG072-ESPECIE
              MOVE WS-TB-TOT-GER-BRUTO-ESP6 (WS-COMP, WS-UF) TO
                             CPG072-BRUTO
              MOVE WS-TB-TOT-GER-DESC-ESP6  (WS-COMP, WS-UF) TO
                             CPG072-DESC
              MOVE WS-TB-TOT-GER-ESP6  (WS-COMP, WS-UF) TO
                             CPG072-TOT-BENEF
              ADD 1       TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM.

       4215-FIM. EXIT.
      /
       4216-GRAVA-TP7.

           IF WS-TB-TOT-BRUTO-ESP7 (WS-COMP, WS-UF, WS-ESP) NOT = 0
           OR WS-TB-TOT-DESC-ESP7  (WS-COMP, WS-UF, WS-ESP) NOT = 0
              MOVE WS-TB-COMPET (WS-COMP)       TO CPG072-COMPET
              MOVE 7                            TO CPG072-TIPO
              MOVE WS-TB-SIGL-UF(WS-UF)         TO CPG072-SIGL-UF
              MOVE WS-ESP                       TO CPG072-ESPECIE
              MOVE WS-TB-TOT-BRUTO-ESP7 (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-BRUTO
              MOVE WS-TB-TOT-DESC-ESP7  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-DESC
              MOVE WS-TB-TOT-ESP7  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-TOT-BENEF
              ADD  1                            TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM
              ADD  WS-TB-TOT-BRUTO-ESP7 (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-BRUTO-ESP7 (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-DESC-ESP7  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-DESC-ESP7  (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-ESP7  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-ESP7  (WS-COMP, WS-UF).

           IF WS-ESP = 99 AND
              (WS-TB-TOT-GER-BRUTO-ESP7 (WS-COMP, WS-UF) NOT = 0 OR
               WS-TB-TOT-GER-DESC-ESP7  (WS-COMP, WS-UF) NOT = 0)
              MOVE WS-TB-COMPET (WS-COMP) TO CPG072-COMPET
              MOVE ZEROS  TO CPG072-ESPECIE
              MOVE WS-TB-TOT-GER-BRUTO-ESP7 (WS-COMP, WS-UF) TO
                             CPG072-BRUTO
              MOVE WS-TB-TOT-GER-DESC-ESP7  (WS-COMP, WS-UF) TO
                             CPG072-DESC
              MOVE WS-TB-TOT-GER-ESP7  (WS-COMP, WS-UF) TO
                             CPG072-TOT-BENEF
              ADD 1       TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM.

       4216-FIM. EXIT.

       4216B-GRAVA-TP8.

           IF WS-TB-TOT-BRUTO-ESP8 (WS-COMP, WS-UF, WS-ESP) NOT = 0
           OR WS-TB-TOT-DESC-ESP8  (WS-COMP, WS-UF, WS-ESP) NOT = 0
              MOVE WS-TB-COMPET (WS-COMP)       TO CPG072-COMPET
              MOVE 8                            TO CPG072-TIPO
              MOVE WS-TB-SIGL-UF(WS-UF)         TO CPG072-SIGL-UF
              MOVE WS-ESP                       TO CPG072-ESPECIE
              MOVE WS-TB-TOT-BRUTO-ESP8 (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-BRUTO
              MOVE WS-TB-TOT-DESC-ESP8  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-DESC
              MOVE WS-TB-TOT-ESP8  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-TOT-BENEF
              ADD  1                            TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM
              ADD  WS-TB-TOT-BRUTO-ESP8 (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-BRUTO-ESP8 (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-DESC-ESP8  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-DESC-ESP8  (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-ESP8  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-ESP8  (WS-COMP, WS-UF).

           IF WS-ESP = 99 AND
              (WS-TB-TOT-GER-BRUTO-ESP8 (WS-COMP, WS-UF) NOT = 0 OR
               WS-TB-TOT-GER-DESC-ESP8  (WS-COMP, WS-UF) NOT = 0)
              MOVE WS-TB-COMPET (WS-COMP) TO CPG072-COMPET
              MOVE ZEROS  TO CPG072-ESPECIE
              MOVE WS-TB-TOT-GER-BRUTO-ESP8 (WS-COMP, WS-UF) TO
                             CPG072-BRUTO
              MOVE WS-TB-TOT-GER-DESC-ESP8  (WS-COMP, WS-UF) TO
                             CPG072-DESC
              MOVE WS-TB-TOT-GER-ESP8  (WS-COMP, WS-UF) TO
                             CPG072-TOT-BENEF
              ADD 1       TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM.

       4216B-FIM. EXIT.
      /
       4216C-GRAVA-TP9.

           IF WS-TB-TOT-BRUTO-ESP9 (WS-COMP, WS-UF, WS-ESP) NOT = 0
           OR WS-TB-TOT-DESC-ESP9  (WS-COMP, WS-UF, WS-ESP) NOT = 0
              MOVE WS-TB-COMPET (WS-COMP)       TO CPG072-COMPET
              MOVE 9                            TO CPG072-TIPO
              MOVE WS-TB-SIGL-UF(WS-UF)         TO CPG072-SIGL-UF
              MOVE WS-ESP                       TO CPG072-ESPECIE
              MOVE WS-TB-TOT-BRUTO-ESP9 (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-BRUTO
              MOVE WS-TB-TOT-DESC-ESP9  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-DESC
              MOVE WS-TB-TOT-ESP9  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-TOT-BENEF
              ADD  1                            TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM
              ADD  WS-TB-TOT-BRUTO-ESP9 (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-BRUTO-ESP9 (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-DESC-ESP9  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-DESC-ESP9  (WS-COMP, WS-UF)
              ADD  WS-TB-TOT-ESP9  (WS-COMP, WS-UF, WS-ESP)
               TO  WS-TB-TOT-GER-ESP9  (WS-COMP, WS-UF).

           IF WS-ESP = 99 AND
              (WS-TB-TOT-GER-BRUTO-ESP9 (WS-COMP, WS-UF) NOT = 0 OR
               WS-TB-TOT-GER-DESC-ESP9  (WS-COMP, WS-UF) NOT = 0)
              MOVE WS-TB-COMPET (WS-COMP) TO CPG072-COMPET
              MOVE ZEROS  TO CPG072-ESPECIE
              MOVE WS-TB-TOT-GER-BRUTO-ESP9 (WS-COMP, WS-UF) TO
                             CPG072-BRUTO
              MOVE WS-TB-TOT-GER-DESC-ESP9  (WS-COMP, WS-UF) TO
                             CPG072-DESC
              MOVE WS-TB-TOT-GER-ESP9  (WS-COMP, WS-UF) TO
                             CPG072-TOT-BENEF
              ADD 1       TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM.

       4216C-FIM. EXIT.
      /
       4217-GRAVA-TP0.

           IF WS-TB-TOT-BRUTO-ESP1 (WS-COMP, WS-UF, WS-ESP) NOT = 0
           OR WS-TB-TOT-DESC-ESP1  (WS-COMP, WS-UF, WS-ESP) NOT = 0
              MOVE WS-TB-COMPET (WS-COMP)       TO CPG072-COMPET
              MOVE ZEROS                        TO CPG072-TIPO
              MOVE WS-TB-SIGL-UF(WS-UF)         TO CPG072-SIGL-UF
              MOVE WS-ESP                       TO CPG072-ESPECIE
              MOVE WS-TB-TOT-BRUTO-ESP1 (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-BRUTO
              MOVE WS-TB-TOT-DESC-ESP1  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-DESC
              MOVE WS-TB-TOT-ESP1  (WS-COMP, WS-UF, WS-ESP)
                                                TO CPG072-TOT-BENEF
              ADD  1                            TO W77-GR-CPG072
              PERFORM 4220-GRAVA-CPG072 THRU 4220-FIM.

       4217-FIM. EXIT.

       4218-GRAVA-EST.

           IF (WS-TB-TOT-ESP-EST-DIV-TP
                 (WS-COMP, WS-UF, WS-TIPO) NOT = ZEROS) OR
              (WS-TB-TOT-ESP-EST-DIV-TP2
                 (WS-COMP, WS-UF, WS-TIPO) NOT = ZEROS)
              MOVE WS-TB-COMPET (WS-COMP)       TO CPG073-COMPET
              MOVE WS-TB-SIGL-UF(WS-UF)         TO CPG073-SIGL-UF
              MOVE WS-TIPO-1                    TO CPG073-TIPO
              MOVE WS-TOT-IR-PREV  (WS-COMP, WS-UF, WS-TIPO) TO
                                                          CPG073-IR
              MOVE WS-TOT-IR-UNIAO (WS-COMP, WS-UF, WS-TIPO) TO
                                                          CPG073-IR-UNI
              MOVE WS-TOT-CONT-PREV(WS-COMP, WS-UF, WS-TIPO) TO
                                                          CPG073-C-PREV
              MOVE WS-TOT-PA       (WS-COMP, WS-UF, WS-TIPO) TO
                                                          CPG073-PA
              MOVE WS-TOT-13-SAL   (WS-COMP, WS-UF, WS-TIPO) TO
                                                          CPG073-13-SAL
              MOVE WS-TOT-SF       (WS-COMP, WS-UF, WS-TIPO) TO
                                                          CPG073-SF
              MOVE WS-TOT-CONSIG   (WS-COMP, WS-UF, WS-TIPO) TO
                                                          CPG073-CONSIG
              MOVE WS-TOT-CPMF     (WS-COMP, WS-UF, WS-TIPO) TO
                                                          CPG073-CPMF
              PERFORM 4219-MOV-CONSIG THRU 4219-FIM
                      VARYING WS-ESP FROM 1 BY 1
                        UNTIL WS-ESP > 16
              ADD  1  TO W77-GR-CPG073
              IF WS-TIPO = 10
                 MOVE ZEROS TO CPG073-TIPO
                 PERFORM 4221-GRAVA-CPG073 THRU 4221-FIM
              ELSE
                 PERFORM 4221-GRAVA-CPG073 THRU 4221-FIM.

       4218-FIM. EXIT.

       4219-MOV-CONSIG.

           MOVE WS-TOT-COD-CONSIG(WS-COMP, WS-UF, WS-TIPO, WS-ESP)
             TO CPG073-TP-CONSIG(WS-ESP).

       4219-FIM. EXIT.

       4220-GRAVA-CPG072.

           WRITE REG-DSUBCPG072.

           MOVE WS-TB-COMPET(WS-COMP) TO WS-COMP-13.
           MOVE 13                    TO WS-COMP-13-MES.
           MOVE WS-COMP-13            TO CPG072-COMPET.

           WRITE REG-DSUBCPG072.

       4220-FIM. EXIT.

       4221-GRAVA-CPG073.

           WRITE REG-DSUBCPG073.

           MOVE WS-TB-COMPET(WS-COMP) TO WS-COMP-13.
           MOVE 13                    TO WS-COMP-13-MES.
           MOVE WS-COMP-13            TO CPG073-COMPET.

           WRITE REG-DSUBCPG073.

       4221-FIM. EXIT.

           COPY "*B/DTP/PAD/01/CHECKPOINT/PD2 ON PKBDSGF".

       4000-DISPLAY.

                MOVE WS-MSG-LIDOS  TO W77-MSG
                CALL "ZDISPLAY IN DTP/L1" USING W77-MSG,
                                                W77-STATUS-BACKUP-CKP.

       4000-FIM-DISPLAY. EXIT.

      *-----------------------------------*
       4500-VERIFICA-SAL-FAM.
      *-----------------------------------*

      *-----------------------------------*
       4500-FIM-VERIFICA-SAL-FAM. EXIT.
      *-----------------------------------*

      *-----------------------------------*
       VERIFICA-RUBR-IR.
      *-----------------------------------*


      *-----------------------------------*
       FIM-VERIFICA-RUBR-IR.
      *-----------------------------------*

      *-----------------------------------*
       VERIFICA.
      *-----------------------------------*


      *-----------------------------------*
       FIM-VERIFICA. EXIT.
      *-----------------------------------*



      *---------------------------------------------*
       4600-VERIFICA-URB-RURAL.
      *---------------------------------------------*


      *Verifica se eh urbano ou rural.
           IF SISPG-CS-ORIG-ORC NOT EQUAL 1
              MOVE 0 TO  WS-URBANO-RURAL
           ELSE
              IF AUX1-RURAL
                 MOVE 2 TO WS-URBANO-RURAL
              ELSE
                 MOVE 1 TO WS-URBANO-RURAL.

      *---------------------------------------------*
       4600-FIM-VERIFICA-URB-RURAL.
           EXIT.
      *---------------------------------------------*
      *-----------------------------------------------------*
       5000-VERIFICA-FIM-VAL.
      *-----------------------------------------------------*

      *     IF CP01-DT-FIM-VALIDADE NOT GREATER THAN 20150227
      *        WRITE DSUBCPG050-REG.


      *-----------------------------------------------------*
       5000-FIM-VERIFICA-VAL.
        EXIT.
      *-----------------------------------------------------*
      *-----------------------------------------------------*
       5000-BUSCA-CALAM.
      *-----------------------------------------------------*

           MOVE 1 TO WS-ACHOU-CALAM.

           FIND ST-BENEF-CALAM-NB AT NU-NB = SISPG-NB
                ON EXCEPTION
                   IF DMSTATUS(NOTFOUND)
                   MOVE ZEROS TO WS-ACHOU-CALAM
                   MOVE SPACES TO WS-PARAM1-ZDISPLAY
                   STRING "NAO ACHOU DS-BENEF-CALAM, NB: ", SISPG-NB
                   DELIMITED BY SIZE   INTO WS-PARAM1-ZDISPLAY
                   CALL "ZDISPLAY IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                              WS-INTEIRO-ZDISPLAY
                   ELSE
                      CALL SYSTEM DMTERMINATE.

           IF ACHOU-CALAM
              MOVE ID-CALAMIDADE OF DS-BENEF-CALAM
                                            TO WS-SISPG-COD-CALAMIDADE
           ELSE
              MOVE 99999 TO WS-SISPG-COD-CALAMIDADE.

      *------------------------------------------------------*
       5000-FIM-BUSCA-CALAM.
      *------------------------------------------------------*

      *------------------------------------------------------*
       6000-INCLUI-HEADER-SISPG.
      **** DISPLAY "HEADER-ORIG-CRED="    SISPG-ORIG-CRED-H.
           MOVE CP01-DT-FIM-VALIDADE   TO SISPG-DT-FIM-VALIDADE-H
                                         SPGENT-DT-FIM-VALIDADE-H.

           IF E-CREDITO-DE-CALAMIDADE
              MOVE 5                     TO SISPG-ORIG-CRED-H
                                            SPGENT-ORIG-CRED-H
              MOVE ID-CALAMIDADE OF DS-CALAMIDADE
                                         TO SISPG-ID-CALAMIDADE-H
                                            SPGENT-ID-CALAMIDADE-H
              MOVE NU-PORTARIA   OF DS-CALAMIDADE
                                         TO SISPG-NUM-PORTARIA-H
                                            SPGENT-NUM-PORTARIA-H
              MOVE DT-PORTARIA   OF DS-CALAMIDADE
                                         TO SISPG-DT-PORTARIA-H
                                            SPGENT-DT-PORTARIA-H
              MOVE CP01-DT-INI-VALIDADE  TO SISPG-DT-INI-VALIDADE-H
                                            SPGENT-DT-INI-VALIDADE-H
           ELSE
              MOVE WS-SISPG-ORIG-CRED-H   TO SISPG-ORIG-CRED-H
                                             SPGENT-ORIG-CRED-H
              MOVE ZEROS                 TO SISPG-ID-CALAMIDADE-H
                                            SISPG-NUM-PORTARIA-H
                                            SISPG-DT-PORTARIA-H
                                            SISPG-DT-INI-VALIDADE-H
                                            SPGENT-ID-CALAMIDADE-H
                                            SPGENT-NUM-PORTARIA-H
                                            SPGENT-DT-PORTARIA-H
                                            SPGENT-DT-INI-VALIDADE-H.

           MOVE 1                              TO SISPG-NU-SEQ-ARQ-H
                                                  SPGENT-NU-SEQ-ARQ-H.
           MOVE 0                              TO SISPG-ID-ULT-ARQ-H
                                                  SPGENT-ID-ULT-ARQ-H.
      *     PERFORM 9900-MOVE-TAB-RUB-HEADER THRU
      *             9900-MOVE-TAB-RUB-HEADER-EXIT.

           PERFORM 9900-MOVE-RUB-HEADER THRU
                   9900-MOVE-RUB-HEADER-EXIT
                   VARYING IX FROM 1 BY 1
                   UNTIL IX > 96.

           WRITE REG-058-SISPG FROM DSUBCPG058-REG-SISPG-HEADER.

           WRITE REG-058-SPGENT FROM DSUBCPG058-REG-SPGENT-HEADER.

           ADD 1 TO WS-GRAV-SISPG.
           ADD 1 TO WS-SISPG-QTD-REG-T.
      **** DISPLAY "H=" DSUBCPG058-REG-SISPG-HEADER.
      **** DISPLAY "R=" SISPG-LISTA-RUBR-H-R.
           MOVE SPACES TO WS-MSG
                STRING "LISTA TODAS RUBRICAS "
                SISPG-LISTA-RUBR-H-R
                DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1"
                      USING WS-MSG
                            WS-STATUS-BACKUP.
      *OBS SO EH USADO QUANDO FOR CALAMIDADE. (FUTURO).
       6000-INCLUI-HEADER-SISPG-EXIT.
           EXIT.
       7000-INCLUI-TRAILER-SISPG.

           ADD 1 TO WS-SISPG-QTD-REG-T.
           MOVE WS-SISPG-QTD-REG-T    TO SISPG-QTD-REG-T
                                         SPGENT-QTD-REG-T.
           MOVE WS-SISPG-TOT-VL-LIQ-T TO SISPG-TOT-VL-LIQ-T
                                         SPGENT-TOT-VL-LIQ-T.

           WRITE REG-058-SISPG FROM DSUBCPG058-REG-SISPG-T.

           WRITE REG-058-SPGENT FROM DSUBCPG058-REG-SPGENT-T.

           ADD 1 TO WS-GRAV-SISPG.
      **** DISPLAY "REG-TARILER=" REG-058-SISPG.
       7000-INCLUI-TRAILER-SISPG-EX.
           EXIT.
       7000-LIMPA-RUB.

           IF IX < 96
              MOVE ZEROS TO SISPG-CS-RUB (IX)
                            SPGENT-CS-RUB(IX)
           ELSE
              MOVE ZEROS TO SISPG-CS-RUB (IX).

       7000-LIMPA-RUB-EXIT.
           EXIT.
       7000-LIMPA-ENT.

           MOVE ZEROS TO TB-VL-RUB-ENT(IX).

       7000-LIMPA-ENT-EXIT.
            EXIT.

       8000-BUSCA-CONSIG-EXC.
      *-----------------------

           FIND NEXT SS-CONSIG-EMPREST AT
                NU-NB           = NU-NB OF DS-MOV-CRED-MANUT
                ON EXCEPTION
                IF DMSTATUS(NOTFOUND)
                   MOVE 1 TO WS-FIM-CONSIG
                   GO TO 8000-FIM-BUSCA-CONSIG-EXC
                ELSE
                CALL SYSTEM DMTERMINATE.

           IF NOT (IN-EXC-CONSIG-APS OR IN-EXC-CONSIG-BAN)
              GO TO 8000-FIM-BUSCA-CONSIG-EXC.

           IF (D2-FIM-DESCONTO OF DS-CONSIGNACAO > ZEROS
                   AND D2-FIM-DESCONTO OF DS-CONSIGNACAO <
                   (D2-COMPET-CREDITO OF DS-MOV-CRED-MANUT + 1))
              GO TO 8000-FIM-BUSCA-CONSIG-EXC.

           MOVE D2-INI-DESCONTO OF DS-CONSIGNACAO TO WS-CONS-INICIO.

           COMPUTE WS-NU-PARCELA-CONSIG
                        =  (WS-AA-SIS - WS-CONS-INI-ANO)
                           * 12 + (WS-MM-SIS -
                                   WS-CONS-INI-MES)     + 1.

           IF  IN-EXC-CONSIG-APS
               MOVE 01 TO WS-IN-EXC-CONSIG
           ELSE
              IF  IN-EXC-CONSIG-BAN
                  MOVE 02 TO WS-IN-EXC-CONSIG
              ELSE
                  MOVE 00 TO WS-IN-EXC-CONSIG.

           MOVE ALL @00@ TO REG-DSUBCPG081
           MOVE CP01-DT-MOV-CREDITO    TO CPG081-DT-MOV-CREDITO
           MOVE CP01-NU-NB             TO CPG081-NU-NB
           MOVE CP01-CS-NATUR-CREDITO  TO CPG081-CS-NATUR-CREDITO
           MOVE CP01-DT-FIM-PERIODO    TO CPG081-DT-FIM-PERIODO
           MOVE CP01-DT-INI-PERIODO    TO CPG081-DT-INI-PERIODO
           MOVE CP01-ID-NIT            TO CPG081-ID-NIT
           MOVE CP01-SEQ-REG           TO CPG081-SEQ-REG
           MOVE ID-EMPRESTIMO     OF DS-CONSIGNACAO
                                       TO CPG081-ID-EMPRESTIMO
           MOVE VL-CONSIGNACAO    OF DS-CONSIGNACAO
                                       TO CPG081-VL-PARCELA-CONSIG
           MOVE WS-NU-PARCELA-CONSIG
                                       TO CPG081-NU-PARCELA-CONSIG
           MOVE QT-DESCONTO OF DS-CONSIGNACAO
                                       TO CPG081-QT-TOT-PAR-CONSIG
           MOVE WS-IN-EXC-CONSIG       TO CPG081-CS-NAO-DESCONTO

           MOVE ID-BANCO  OF DS-CONSIGNACAO
                                       TO CPG081-ID-BANCO-EMPREST
           MOVE D2-INI-DESCONTO   OF DS-CONSIGNACAO
                                       TO CPG081-D2-INI-DESCONTO
           MOVE CS-CONSIGNACAO    OF DS-CONSIGNACAO
                                       TO CPG081-CS-CONSIGNACAO

           MOVE ID-BANCO-SISPAG OF DS-CONSIGNACAO TO CPG081-ID-BCO-GLOSA

           ADD 1 TO WS-CONT-CONSIG

           WRITE REG-DSUBCPG081.

       8000-FIM-BUSCA-CONSIG-EXC. EXIT.
      *------------------------------

       9010-ABRE-DSUBCPG058SPG.
      *-----------------------*

           IF  WS-E-CRED-CALAMIDADE = "SIM"
              MOVE "CALA" TO PAR-MOV-CREDITO.

           MOVE PAR-MOV-CREDITO TO WS-ROTINA
                                   WS-ROTINA-R274.
           CHANGE  ATTRIBUTE TITLE OF  DSUBCPG058SISPG
                                   TO  WS-TITLE-DSUBCPG058R275.
           CHANGE  ATTRIBUTE TITLE OF DSUBCPG058SPGENT
                                   TO  WS-TITLE-DSUBCPG058R274.
           CHANGE  ATTRIBUTE SECURITYTYPE OF DSUBCPG058SISPG TO PUBLIC.
           CHANGE  ATTRIBUTE SECURITYTYPE OF DSUBCPG058SPGENT TO PUBLIC.
           OPEN    OUTPUT    DSUBCPG058SISPG.
           OPEN    OUTPUT    DSUBCPG058SPGENT.

           IF  WS-E-CRED-CALAMIDADE = "SIM"
               MOVE "MANU" TO PAR-MOV-CREDITO.

       9010-FIM.
      *---------*

       9550-LIMPA-RUB-H.

           IF IX < 96
              MOVE ZEROS TO SPGENT-CS-RUBRICA-H ( IX )
                            SISPG-CS-RUBRICA-H ( IX )
           ELSE
               MOVE ZEROS TO SISPG-CS-RUBRICA-H ( IX ).

       9550-LIMPA-RUB-H-EXIT.
           EXIT.
       9900-MOVE-TAB-RUB-HEADER.
      *****DISPLAY SISPG-MARCA-GRUPO-RUBRICAS.
      * A Partir de junho de 2013 o INSS pediu para que se colocasse no
      * header do registro arquivo D/SUB/CPG/058/SISPG a tabela de
      * rubricas que serao usadas no arquivo e nao mais so as que vem
      * do dataset que da origem ao arquivo.
      * por isso nao testar mais a marca de rubrica e mover todas.

      *--* IF SISPG-MARCA-RUB104 = 1
              ADD 1 TO IND-TB2
              MOVE 104 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB105 = 1
              ADD 1 TO IND-TB2
              MOVE 105 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB107 = 1
              ADD 1 TO IND-TB2
              MOVE 107 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB108 = 1
              ADD 1 TO IND-TB2
              MOVE 108 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB112 = 1
              ADD 1 TO IND-TB2
              MOVE 112 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB136 = 1
              ADD 1 TO IND-TB2
              MOVE 136 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB154 = 1
              ADD 1 TO IND-TB2
              MOVE 154 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB160 = 1
              ADD 1 TO IND-TB2
              MOVE 160 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB160 = 1
              ADD 1 TO IND-TB2
              MOVE 161 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB160 = 1
              ADD 1 TO IND-TB2
              MOVE 162 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB201 = 1
              ADD 1 TO IND-TB2
              MOVE 201 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB202 = 1
              ADD 1 TO IND-TB2
              MOVE 202 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB203 = 1
              ADD 1 TO IND-TB2
              MOVE 203 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB204 = 1
              ADD 1 TO IND-TB2
              MOVE 204 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB205 = 1
              ADD 1 TO IND-TB2
              MOVE 205 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB206 = 1
              ADD 1 TO IND-TB2
              MOVE 206 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB207 = 1
              ADD 1 TO IND-TB2
              MOVE 207 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB208 = 1
              ADD 1 TO IND-TB2
              MOVE 208 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB209 = 1
              ADD 1 TO IND-TB2
              MOVE 209 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB210 = 1
              ADD 1 TO IND-TB2
              MOVE 210 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB214 = 1
              ADD 1 TO IND-TB2
              MOVE 214 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB215 = 1
              ADD 1 TO IND-TB2
              MOVE 215 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB216 = 1
              ADD 1 TO IND-TB2
              MOVE 216 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB217 = 1
              ADD 1 TO IND-TB2
              MOVE 217 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB218 = 1
              ADD 1 TO IND-TB2
              MOVE 218 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB219 = 1
              ADD 1 TO IND-TB2
              MOVE 219 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB220 = 1
              ADD 1 TO IND-TB2
              MOVE 220 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB221 = 1
              ADD 1 TO IND-TB2
              MOVE 221 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB222 = 1
              ADD 1 TO IND-TB2
              MOVE 222 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB223 = 1
              ADD 1 TO IND-TB2
              MOVE 223 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB224 = 1
              ADD 1 TO IND-TB2
              MOVE 224 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB225 = 1
              ADD 1 TO IND-TB2
              MOVE 225 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB226 = 1
              ADD 1 TO IND-TB2
              MOVE 226 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB227 = 1
              ADD 1 TO IND-TB2
              MOVE 227 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB228 = 1
              ADD 1 TO IND-TB2
              MOVE 228 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB229 = 1
              ADD 1 TO IND-TB2
              MOVE 229 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB230 = 1
              ADD 1 TO IND-TB2
              MOVE 230 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB231 = 1
              ADD 1 TO IND-TB2
              MOVE 231 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB232 = 1
              ADD 1 TO IND-TB2
              MOVE 232 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB233 = 1
              ADD 1 TO IND-TB2
              MOVE 233 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB234 = 1
              ADD 1 TO IND-TB2
              MOVE 234 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB236 = 1
              ADD 1 TO IND-TB2
              MOVE 236 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB237 = 1
              ADD 1 TO IND-TB2
              MOVE 237 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB238 = 1
              ADD 1 TO IND-TB2
              MOVE 238 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB239 = 1
              ADD 1 TO IND-TB2
              MOVE 239 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB240 = 1
              ADD 1 TO IND-TB2
              MOVE 240 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB241 = 1
              ADD 1 TO IND-TB2
              MOVE 241 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).
      *--* IF SISPG-MARCA-RUB242 = 1
              ADD 1 TO IND-TB2
              MOVE 242 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).
      *--* IF SISPG-MARCA-RUB243 = 1
              ADD 1 TO IND-TB2
              MOVE 243 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).
      *--* IF SISPG-MARCA-RUB244 = 1
              ADD 1 TO IND-TB2
              MOVE 244 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).
      *--* IF SISPG-MARCA-RUB245 = 1
              ADD 1 TO IND-TB2
              MOVE 245 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).
      *--* IF SISPG-MARCA-RUB246 = 1
              ADD 1 TO IND-TB2
              MOVE 246 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).
      *--* IF SISPG-MARCA-RUB247 = 1
              ADD 1 TO IND-TB2
              MOVE 247 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).
      *--* IF SISPG-MARCA-RUB248 = 1
              ADD 1 TO IND-TB2
              MOVE 248 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB249 = 1
              ADD 1 TO IND-TB2
              MOVE 249 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB251 = 1
              ADD 1 TO IND-TB2
              MOVE 250 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 251 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 252 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 253 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB254 = 1
              ADD 1 TO IND-TB2
              MOVE 254 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB255 = 1
              ADD 1 TO IND-TB2
              MOVE 255 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB256 = 1
              ADD 1 TO IND-TB2
              MOVE 256 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB257 = 1
              ADD 1 TO IND-TB2
              MOVE 257 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB258 = 1
              ADD 1 TO IND-TB2
              MOVE 258 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 259 TO SISPG-CS-RUBRICA-H( IND-TB2  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 260 TO SISPG-CS-RUBRICA-H( IND-TB2  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 261 TO SISPG-CS-RUBRICA-H( IND-TB2  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 262 TO SISPG-CS-RUBRICA-H( IND-TB2  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 263 TO SISPG-CS-RUBRICA-H( IND-TB2  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 264 TO SISPG-CS-RUBRICA-H( IND-TB2  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 265 TO SISPG-CS-RUBRICA-H( IND-TB2  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 266 TO SISPG-CS-RUBRICA-H( IND-TB2  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 267 TO SISPG-CS-RUBRICA-H( IND-TB2  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 268 TO SISPG-CS-RUBRICA-H( IND-TB2  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 269 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 270 TO SISPG-CS-RUBRICA-H( IND-TB2  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 271 TO SISPG-CS-RUBRICA-H( IND-TB2   ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 272 TO SISPG-CS-RUBRICA-H( IND-TB2   ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 273 TO SISPG-CS-RUBRICA-H( IND-TB2   ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 274 TO SISPG-CS-RUBRICA-H( IND-TB2   ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB2
              MOVE 275 TO SISPG-CS-RUBRICA-H( IND-TB2   ).

      *--* IF SISPG-MARCA-RUB301 = 1
              ADD 1 TO IND-TB2
              MOVE 301 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB905 = 1
              ADD 1 TO IND-TB2
              MOVE 905 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB909 = 1
              ADD 1 TO IND-TB2
              MOVE 909 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB910 = 1
              ADD 1 TO IND-TB2
              MOVE 910 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB911 = 1
              ADD 1 TO IND-TB2
              MOVE 911 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB912 = 1
              ADD 1 TO IND-TB2
              MOVE 912 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB916 = 1
              ADD 1 TO IND-TB2
              MOVE 916 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB917 = 1
              ADD 1 TO IND-TB2
              MOVE 917 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB921 = 1
              ADD 1 TO IND-TB2
              MOVE 921 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB925 = 1
              ADD 1 TO IND-TB2
              MOVE 925 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB926 = 1
              ADD 1 TO IND-TB2
              MOVE 926 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB927 = 1
              ADD 1 TO IND-TB2
              MOVE 927 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB928 = 1
              ADD 1 TO IND-TB2
              MOVE 928 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB929 = 1
              ADD 1 TO IND-TB2
              MOVE 929 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).

      *--* IF SISPG-MARCA-RUB933 = 1
              ADD 1 TO IND-TB2
              MOVE 933 TO SISPG-CS-RUBRICA-H ( IND-TB2 ).


              ADD 1 TO IND-TB-ENT
              MOVE 104 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB105 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 105 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB107 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 107 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB108 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 108 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB112 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 112 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB136 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 136 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB154 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 154 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB160 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 160 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB160 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 161 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB160 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 162 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB201 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 201 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB202 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 202 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB203 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 203 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB204 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 204 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB205 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 205 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB206 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 206 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB207 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 207 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB208 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 208 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB209 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 209 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB210 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 210 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB214 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 214 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB215 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 215 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB216 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 216 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB217 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 217 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB218 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 218 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB219 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 219 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB220 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 220 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB221 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 221 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB222 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 222 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB223 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 223 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB224 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 224 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB225 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 225 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB226 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 226 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB227 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 227 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB228 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 228 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB229 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 229 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB230 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 230 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB231 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 231 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB232 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 232 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB233 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 233 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB234 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 234 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB236 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 236 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB237 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 237 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB238 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 238 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB239 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 239 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB240 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 240 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB241 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 241 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SISPG-MARCA-RUB242 = 1
      *        ADD 1 TO IND-TB2
              ADD 1 TO IND-TB-ENT
              MOVE 242 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SISPG-MARCA-RUB243 = 1
      *        ADD 1 TO IND-TB2
              ADD 1 TO IND-TB-ENT
              MOVE 243 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SISPG-MARCA-RUB244 = 1
      *        ADD 1 TO IND-TB2
              ADD 1 TO IND-TB-ENT
              MOVE 244 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SISPG-MARCA-RUB245 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 245 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SISPG-MARCA-RUB246 = 1
      *        ADD 1 TO IND-TB2
              ADD 1 TO IND-TB-ENT
              MOVE 246 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SISPG-MARCA-RUB247 = 1
      *        ADD 1 TO IND-TB2
              ADD 1 TO IND-TB-ENT
              MOVE 247 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SISPG-MARCA-RUB248 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 248 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB249 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 249 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 250 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 251 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 252 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 253 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 254 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 255 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 256 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 257 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 258 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 259 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 260 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 261 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 262 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 263 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 264 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 265 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 266 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).
      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 267 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 268 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 269 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 270 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 271 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 272 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 273 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SISPG-MARCA-RUB250 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 274 TO SPGENT-CS-RUBRICA-H( IND-TB-ENT  ).

      *--* IF SPGENT-MARCA-RUB301 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 301 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB905 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 905 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB909 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 909 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB910 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 910 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB911 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 911 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB912 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 912 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB916 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 916 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB917 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 917 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB921 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 921 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB925 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 925 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB926 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 926 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB927 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 927 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB928 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 928 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB929 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 929 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

      *--* IF SPGENT-MARCA-RUB933 = 1
              ADD 1 TO IND-TB-ENT
              MOVE 933 TO SPGENT-CS-RUBRICA-H ( IND-TB-ENT ).

       9900-MOVE-TAB-RUB-HEADER-EXIT.
           EXIT.
       ZZZZ-LISTA-RUBR-CONC.
           ADD 1 TO IND-TB.
           IF IND-TB < 7
              MOVE SPACES TO BRANCO-I (IND-TB)
              MOVE CS-RUBRICA OF DS-MOV-CRED-CONC (IX)
              TO CS-RUB-I (IND-TB)
              MOVE VL-RUBRICA OF DS-MOV-CRED-CONC (IX)
              TO VL-RUB-I (IND-TB)
           ELSE
            IF IND-TB = 7
               WRITE LSUBSISPG-REG FROM LIN-SISPG AFTER 1
               MOVE SPACES TO LIN-SISPG
               MOVE SPACES TO BRANCO-I (1)
               MOVE CS-RUBRICA OF DS-MOV-CRED-CONC (IX)
               TO CS-RUB-I (1)
               MOVE VL-RUBRICA OF DS-MOV-CRED-CONC (IX)
               TO VL-RUB-I (1)
               MOVE 1 TO IND-TB.
       ZZZZ-LISTA-RUBR-CONC-EXIT.
           EXIT.
       ZZZZ-LISTA-RUBR-MANU.
           ADD 1 TO IND-TB.
           IF IND-TB < 7
              MOVE SPACES TO BRANCO-I (IND-TB)
              MOVE CS-RUBRICA OF DS-MOV-CRED-MANUT (IX)
              TO CS-RUB-I (IND-TB)
              MOVE VL-RUBRICA OF DS-MOV-CRED-MANUT (IX)
              TO VL-RUB-I (IND-TB)
           ELSE
            IF IND-TB = 7
               WRITE LSUBSISPG-REG FROM LIN-SISPG AFTER 1
               MOVE SPACES TO LIN-SISPG
               MOVE SPACES TO BRANCO-I (1)
               MOVE CS-RUBRICA OF DS-MOV-CRED-MANUT (IX)
               TO CS-RUB-I (1)
               MOVE VL-RUBRICA OF DS-MOV-CRED-MANUT (IX)
               TO VL-RUB-I (1)
               MOVE 1 TO IND-TB.
       ZZZZ-LISTA-RUBR-MANU-EXIT.
           EXIT.
       ZZZZ-LISTA-RUBR-ALTER.
           ADD 1 TO IND-TB.
           IF IND-TB < 7
              MOVE SPACES TO BRANCO-I (IND-TB)
              MOVE CS-RUBRICA OF DS-MOV-CRED-ALTER (IX)
              TO CS-RUB-I (IND-TB)
              MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER (IX)
              TO VL-RUB-I (IND-TB)
           ELSE
            IF IND-TB = 7
               WRITE LSUBSISPG-REG FROM LIN-SISPG AFTER 1
               MOVE SPACES TO LIN-SISPG
               MOVE SPACES TO BRANCO-I (1)
               MOVE CS-RUBRICA OF DS-MOV-CRED-ALTER (IX)
               TO CS-RUB-I (1)
               MOVE VL-RUBRICA OF DS-MOV-CRED-ALTER (IX)
               TO VL-RUB-I (1)
               MOVE 1 TO IND-TB.
       ZZZZ-LISTA-RUBR-ALTER-EXIT.
           EXIT.
       ROT-CALC-PERC-RUBR.

           MOVE TB-SISPG-RUBR ( TB-IND-SISPG )
             TO WS-TB-SISPG-RUBR-ORIGINAL.

           COMPUTE WS-TB-SISPG-RUBR-PREV =
              TB-SISPG-RUBR ( TB-IND-SISPG ) * WS-SISPG-PERC-PREV / 100.

           IF WS-EPU-PREV = "EPU "
              COMPUTE TB-SISPG-RUBR ( TB-IND-SISPG ) =
                   WS-TB-SISPG-RUBR-ORIGINAL - WS-TB-SISPG-RUBR-PREV.

           IF WS-EPU-PREV = "PREV"
              MOVE WS-TB-SISPG-RUBR-PREV TO
                      TB-SISPG-RUBR ( TB-IND-SISPG ).

       ROT-CALC-PERC-RUBR-EXIT.
           EXIT.
       9960-CAL-PERC-RUB-EPU-PREV.

           COMPUTE WS-SISPG-VALOR-LIQUIDO-PREV  =
                 ( WS-SISPG-VALOR-LIQUIDO * WS-SISPG-PERC-PREV / 100 ).
           COMPUTE WS-SISPG-VALOR-BRUTO-PREV    =
                 ( WS-SISPG-VALOR-BRUTO   * WS-SISPG-PERC-PREV / 100 ).
           COMPUTE SISPG-VALOR-LIQUIDO = WS-SISPG-VALOR-LIQUIDO -
                                         WS-SISPG-VALOR-LIQUIDO-PREV.
           MOVE SISPG-VALOR-LIQUIDO TO SPGENT-VALOR-LIQUIDO.
           COMPUTE SISPG-VALOR-BRUTO   = WS-SISPG-VALOR-BRUTO -
                                         WS-SISPG-VALOR-BRUTO-PREV.
           MOVE SISPG-VALOR-BRUTO   TO SPGENT-VALOR-BRUTO.
       9960-SEGUNDO-RUB301.
           IF  WS-EPU-PREV = "PREV"
               MOVE  WS-SISPG-VALOR-LIQUIDO-PREV TO SISPG-VALOR-LIQUIDO
                                                   SPGENT-VALOR-LIQUIDO
               MOVE  WS-SISPG-VALOR-BRUTO-PREV TO SISPG-VALOR-BRUTO
                                                  SPGENT-VALOR-BRUTO.



       9961-PULA-EPU.
      ***  MOVE SPACES TO WS-MSG
      ***  MOVE ALL "-"                                  TO  WS-MSG
      ***  CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
      ***                                 WS-STATUS-BACKUP.
      ***
      ***  MOVE SPACES TO WS-MSG
      ***  STRING "NB DO " WS-EPU-PREV "="
      ***    AUX1-NU-NB  " LIQ-ANT=" WS-SISPG-VALOR-LIQUIDO
      ***                " LIQ-APO=" SISPG-VALOR-LIQUIDO
      ***               DELIMITED BY SIZE INTO WS-MSG
      ***       CALL "ZDISPLAY IN DTP/L1"
      ***             USING WS-MSG
      ***                   WS-STATUS-BACKUP.
      ***
      ***  MOVE SPACES TO WS-MSG
      ***  STRING "NB DO " WS-EPU-PREV "="
      ***    AUX1-NU-NB  " BRU-ANT=" WS-SISPG-VALOR-BRUTO
      ***                " BRU-APO=" SISPG-VALOR-BRUTO
      ***               DELIMITED BY SIZE INTO WS-MSG
      ***       CALL "ZDISPLAY IN DTP/L1"
      ***             USING WS-MSG
      ***                   WS-STATUS-BACKUP.
      ***

      ***  MOVE SPACES TO WS-MSG
      ***  STRING "NB DE RUB301 > 0 - CALC PERC " WS-EPU-PREV "="
      ***    AUX1-NU-NB DELIMITED BY SIZE INTO WS-MSG
      ***       CALL "ZDISPLAY IN DTP/L1"
      ***             USING WS-MSG
      ***                   WS-STATUS-BACKUP.

           IF  SISPG-VALOR-GRUPO-RUBR-GUARDA > ZEROS
      ***      MOVE SPACES TO WS-MSG
      ***      STRING "NB DE RUB301 > 0 E VALORES RUBRICAS > ZERO="
      ***        AUX1-NU-NB " PERC-PREV=" WS-PERC-PREV
      ***                   DELIMITED BY SIZE INTO WS-MSG
      ***           CALL "ZDISPLAY IN DTP/L1"
      ***                 USING WS-MSG
      ***                       WS-STATUS-BACKUP
             NEXT SENTENCE
             ELSE

      ***      MOVE SPACES TO WS-MSG
      ***      STRING "NB DE RUB301 > 0 VALORES ZERADOS PERC APLICADO SO
      ***             " VALOR LIQ E BRUTO "
      ***        AUX1-NU-NB DELIMITED BY SIZE INTO WS-MSG
      ***           CALL "ZDISPLAY IN DTP/L1"
      ***                 USING WS-MSG
      ***                       WS-STATUS-BACKUP
               GO TO 9960-CAL-PERC-RUB-EPU-PREV-EX.

           MOVE SISPG-VALOR-GRUPO-RUBR-GUARDA TO
                TAB-SISPG-GR-RUB.

      ***   IF AUX1-NU-NB = 760216215
      ***      MOVE SPACES TO WS-MSG
      ***      STRING "NB DO " WS-EPU-PREV "="
      ***        AUX1-NU-NB
      ***        "ANTES SISPG-VALOR=" SISPG-VALOR-GRUPO-RUBR-GUARDA
      ***         "ANTES TAB-SISPG-VALOR=" TAB-SISPG-GR-RUB
      ***         "ANTES PERC-EPU=" WS-SISPG-PERC-EPU
      ***         "PERC-PREV=" WS-SISPG-PERC-PREV
      ***                   DELIMITED BY SIZE INTO WS-MSG
      ***           CALL "ZDISPLAY IN DTP/L1"
      ***                 USING WS-MSG
      ***                       WS-STATUS-BACKUP.

           PERFORM  ROT-CALC-PERC-RUBR THRU
                    ROT-CALC-PERC-RUBR-EXIT
              VARYING TB-IND-SISPG FROM 1 BY 1
                UNTIL TB-IND-SISPG > 66.

           MOVE TAB-SISPG-GR-RUB TO
                SISPG-VALOR-GRUPO-RUBRICAS
                SPGENT-VALOR-GRUPO-RUBRICAS.

      ***   IF AUX1-NU-NB = 760216215
      ***      MOVE SPACES TO WS-MSG
      ***      STRING "NB DO " WS-EPU-PREV "="
      ***        AUX1-NU-NB
      ***        "APOS SISPG-VALOR=" SISPG-VALOR-GRUPO-RUBR-GUARDA
      ***         "APOS TAB-SISPG-VALOR=" TAB-SISPG-GR-RUB
      ***         "APOS PERC-EPU=" WS-SISPG-PERC-EPU
      ***         "PERC-PREV=" WS-SISPG-PERC-PREV
      ***                   DELIMITED BY SIZE INTO WS-MSG
      ***           CALL "ZDISPLAY IN DTP/L1"
      ***                 USING WS-MSG
      ***                       WS-STATUS-BACKUP.
      ***


       9960-CAL-PERC-RUB-EPU-PREV-EX.
           EXIT.

       9980-PEGA-COD-CALAM.

           FIND NEXT ST-CALAMIDADE-ID
             ON EXCEPTION
                IF DMSTATUS (NOTFOUND)
                   MOVE SPACES TO WS-PARAM1-ZDISPLAY
                   MOVE "CALAM PROCESSADA OU NAO LOCALIZADA ROT. 9980"
                     TO  WS-PARAM1-ZDISPLAY
                   MOVE  AUX1-NU-NB TO  WS-PARAM2-ZDISPLAY
                   CALL "ZDISPLAY2 IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                   CALL SYSTEM DMTERMINATE
                ELSE
                   MOVE SPACES TO WS-PARAM1-ZDISPLAY
                   MOVE "ERRO FIND DS-CALAMIDADE ROT. 9980"
                     TO  WS-PARAM1-ZDISPLAY
                   MOVE  AUX1-NU-NB TO  WS-PARAM2-ZDISPLAY
                   CALL "ZDISPLAY2 IN DTP/L1" USING
                                             WS-PARAM1-ZDISPLAY,
                                             WS-PARAM2-ZDISPLAY,
                                             WS-INTEIRO-ZDISPLAY
                   CALL SYSTEM DMTERMINATE.

           IF IN-PROCESSADA OF DS-CALAMIDADE
              GO TO 9980-PEGA-COD-CALAM.

      *  FIND NEXT ST-BENEF-CALAM-NB AT NU-NB OF DS-BENEF-CALAM =
      *             AUX1-NU-NB
      *          ON EXCEPTION
      *             IF DMSTATUS(NOTFOUND)
      *                MOVE SPACES TO WS-PARAM1-ZDISPLAY
      *                MOVE "ERRO FIND ST-BENEF-CALAM ROT. 9980"
      *                  TO  WS-PARAM1-ZDISPLAY
      *                MOVE  AUX1-NU-NB TO  WS-PARAM2-ZDISPLAY
      *                CALL "ZDISPLAY2 IN DTP/L1" USING
      *                                          WS-PARAM1-ZDISPLAY,
      *                                          WS-PARAM2-ZDISPLAY,
      *                                          WS-INTEIRO-ZDISPLAY
      *                GO TO  9980-PEGA-COD-CALAM-EXIT
      *             ELSE
      *                CALL SYSTEM DMTERMINATE.

         MOVE ID-CALAMIDADE OF DS-CALAMIDADE TO WS-SISPG-COD-CALAMIDADE
                                                AUX1-ID-CALAMIDADE.

       9980-PEGA-COD-CALAM-EXIT. EXIT.

      *-----------------------------------
       9990-VERIFICA-ENT-CLASSE.
      *-----------------------------------

         IF TB-VL-RUB-ENT(WS-IND-ENT) > 0
            MOVE ZEROS TO SPGENT-QUANT-CREDITOS
            MOVE ZEROS TO SPGENT-VALOR-BRUTO
            MOVE ZEROS TO SPGENT-VALOR-LIQUIDO
            MOVE ZEROS TO SISPG-QUANT-CREDITOS
            MOVE ZEROS TO SISPG-VALOR-BRUTO
            MOVE ZEROS TO SISPG-VALOR-LIQUIDO
            PERFORM 10000-ZERA-VALORES THRU
                    10000-FIM-ZERA-VALORES
                   VARYING IX FROM 1 BY 1
                   UNTIL IX > 96
            PERFORM 10010-CRIA-ENT-CLASSE THRU
                    10010-FIM-CRIA-ENT-CLASSE
         ELSE
            MOVE ZEROS TO SPGENT-ENT-CLASS
            MOVE ZEROS TO SPGENT-QTD-ENT-CLASS
            MOVE ZEROS TO SISPG-ENT-CLASS
            MOVE ZEROS TO SISPG-QTD-ENT-CLASS.

      *-----------------------------------
        9990-FIM-VERIFICA-ENT-CLASSE. EXIT.
      *-----------------------------------
      *-----------------------------------------------
       10000-ZERA-VALORES.
      *-----------------------------------------------

           IF IX < 96
              MOVE ZEROS TO SPGENT-VL-RUBRICA(IX)
              MOVE ZEROS TO SISPG-VL-RUBRICA(IX)
           ELSE
              MOVE ZEROS TO SISPG-VL-RUBRICA(IX).

      *-----------------------------------------------
       10000-FIM-ZERA-VALORES. EXIT.
      *-----------------------------------------------
      *------------------------------
       10010-CRIA-ENT-CLASSE.
      *------------------------------

           IF WS-IND-ENT < 39
              MOVE  TB-VL-RUB-ENT(WS-IND-ENT) TO
                    SPGENT-VL-RUBRICA(TAB-IND-RUBR(WS-IND-ENT))
                    SISPG-VL-RUBRICA(TAB-IND-RUBR(WS-IND-ENT))
           ELSE
                MOVE  TB-VL-RUB-ENT(WS-IND-ENT) TO
                 SISPG-VL-RUBRICA(TAB-IND-RUBR(WS-IND-ENT)).
      *     ELSE
      *        DISPLAY "TB250"TB-VL-RUB-ENT(WS-IND-ENT)
      *        MOVE TB-VL-RUB-ENT(WS-IND-ENT)
      *             TO SPGENT-VL-RUBRICA(TAB-IND-RUBR(WS-IND-ENT)).

           IF WS-IND-ENT < 39
              MOVE TAB-CS-RUBR-ENT(WS-IND-ENT) TO SPGENT-ENT-CLASS
              MOVE TAB-CS-RUBR-ENT(WS-IND-ENT) TO SISPG-ENT-CLASS
           ELSE
              MOVE TAB-CS-RUBR-ENT(WS-IND-ENT) TO SISPG-ENT-CLASS.

           ADD 1 TO TB-QT-TOTAL-ENT(WS-IND-ENT).

           ADD TB-VL-RUB-ENT(WS-IND-ENT) TO
               TB-VL-TOTAL-RUB-ENT(WS-IND-ENT).

           IF WS-IND-ENT < 39
              MOVE 1 TO SPGENT-QTD-ENT-CLASS
              MOVE 1 TO SISPG-QTD-ENT-CLASS
           ELSE
              MOVE 1 TO SISPG-QTD-ENT-CLASS.

           IF WS-IND-ENT < 39
              WRITE REG-058-SPGENT FROM DSUBCPG058-REG-SPGENT
              WRITE REG-058-SISPG  FROM DSUBCPG058-REG-SISPG
           ELSE
              WRITE REG-058-SISPG FROM DSUBCPG058-REG-SISPG.
           ADD 1 TO WS-SISPG-QTD-REG-T.
           ADD 1 TO WS-GRAV-ENTIDADES.

      *---------------------------------
        10010-FIM-CRIA-ENT-CLASSE. EXIT.
      *---------------------------------
      *------------------------------------
       11000-LIMPA-RUB-ENT.
      *------------------------------------

           MOVE ZEROS TO TB-VL-RUB-ENT(IX).

      *------------------------------------
       11000-LIMPA-RUB-ENT-EXIT. EXIT.
      *------------------------------------

      *------------------------------------
       11500-MOSTRA-CONSIG.
      *------------------------------------
           IF WS-CONTA-NAO-CONSIG < 10000
                MOVE ID-EMPRESTIMO OF DS-MOV-CRED-MANUT(W77-IND) TO
                                                     AUX1-ID-EMPRESTIMO
                MOVE SPACES TO WS-MENSAGEM-ZDISPLAY
                   STRING "NFD SS-CONSIG-EMPREST - NB: ",
                           AUX1-NU-NB," ID-EMP.: ",AUX1-ID-EMPRESTIMO
                           DELIMITED BY SIZE
                           INTO WS-MENSAGEM-ZDISPLAY
                           CALL "ZDISPLAY IN DTP/L1" USING
                           WS-MENSAGEM-ZDISPLAY,
                           WS-INTEIRO-ZDISPLAY.
      *-----------------------------------------
       11500-FIM-MOSTRA-CONSIG.
       EXIT.
      *-----------------------------------------
      *-----------------------------------------------------------
       9900-MOVE-RUB-HEADER.
      *-----------------------------------------------------------

           MOVE TAB-CS-RUBR(IX) TO SISPG-CS-RUBRICA-H(IX).
           IF IX < 81
              MOVE TAB-CS-RUBR(IX) TO SPGENT-CS-RUBRICA-H(IX).
           IF IX > 81
              MOVE TAB-CS-RUBR(IX) TO SPGENT-CS-RUBRICA-H(IX - 1).
      *-----------------------------------------------------------
       9900-MOVE-RUB-HEADER-EXIT. EXIT.
      *-----------------------------------------------------------


       UNISYS-2K SECTION.
      ********** UNISYS Y2K - Linha alterada em 24/06/1999 12:27:38
           COPY "*B/SUB/REN/DS-CTRL-PROC-CRED ON PKBDSGF".

       FIM-DO-PROGRAMA.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%   REFERENCIAS CRUZADAS DO FONTE     %%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      * LIBRARY - B/SUB/REN/DS-CTRL-PROC-CRED CICLO = 0001VERSAO =   04

      * LIBRARY - B/DTP/PAD/01/CHECKPOINT/PD2 CICLO = 0001VERSAO =   11

      * LIBRARY - B/DTP/PAD/01/CHECKPOINT/PD1 CICLO = 0001VERSAO =   01

      * LIBRARY - B/DTP/PAD/01/CHECKPOINT/WS  CICLO = 0001VERSAO =   06

      * LIBRARY - B/SUB/CPG/2K/042            CICLO = 0001VERSAO =   33

      * LIBRARY - B/SUB/CPG/SPG/RUB           CICLO = 0001VERSAO =   11

      * LIBRARY - B/SUB/CPG/058/SPG/R274      CICLO = 0001VERSAO =   01

      * LIBRARY - B/SUB/CPG/058/SPG/R275      CICLO = 0001VERSAO =   01

      * LIBRARY - B/SUB/CPG/2K/477            CICLO = 0001VERSAO =   02

      * LIBRARY - B/SUB/CPG/2K/450            CICLO = 0001VERSAO =   29

      * LIBRARY - B/SUB/CPG/2K/430            CICLO = 0001VERSAO =   02

      * LIBRARY - B/SUB/CPG/2K/410            CICLO = 0001VERSAO =   04

      * LIBRARY - B/SUB/CPG/2K/401            CICLO = 0001VERSAO =   35

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
