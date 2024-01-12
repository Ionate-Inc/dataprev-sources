      *
      * UNISYS Y2K - TEAM 2000
      *
      $SET LIST
      $SET ERRORLIMIT = 10
      $SET FEDLEVEL = 5
      *$SET TADS
      *========================*
       IDENTIFICATION  DIVISION.
      *========================*
       PROGRAM-ID.      PSUBCPG052SPG.
       DATE-WRITTEN.    15/10/2012.
       DATE-COMPILED.
      ******************************************************************
      *  OBJETIVO: GERAR UM ARQUIVO COM QTDE/VALOR DE CREDITOS EMITI-  *
      *            DOS POR ESTADO/BANCO/MEIO-PAGTO/ORIGEM-CREDITO/     *
      *            DT-INI-VALIDADE/INDICADOR LOAS.                     *
      *                                                                *
      *  HOSTNAME PRODUCAO: DTPRJCV3                                   *
      *  USERCODE PRODUCAO: PDBATBEN                                   *
      *  DIVISAO:           DDCOSUP                                    *
      *  GRAU DIFICULDADE:                                             *
      ******************************************************************
      *  ALTERACAO EM MARCO/08: INCLUSAO DA DT-PROVISAO E DA QUEBRA    *
      *                         POR CS-AGENCIA-PIONEIRA                *
      ******************************************************************
      *  ALTERACAO EM MARCO/09: DUPLICACAO DO ARQUIVO DSUBCPG052 PARA  *
      *                         O SISTEMA SCF (ARRECADACAO), COM A NO- *
      *                         MENCLATURA PADRAO DE ARQS A SEREM      *
      *                         TRANSFERIDOS. O DATASET DS-CTRL-SEQ-TP-*
      *                         CR (BDBENCRED) VAI PASSAR A SER ATUALI-*
      *                         ZADO COM O ULTIMO SEQUENCIAL.          *
      ******************************************************************
      /
      *====================*
       ENVIRONMENT DIVISION.
      *====================*
      *
        CONFIGURATION SECTION.
      *----------------------*
        SOURCE-COMPUTER.   A16.
        OBJECT-COMPUTER.   A18.
        SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
      *
        INPUT-OUTPUT SECTION.
      *---------------------*
        FILE-CONTROL.
           SELECT DSUBCPG051SPG     ASSIGN TO DISK.
           SELECT DSUBCPG052SPG     ASSIGN TO DISK.
           SELECT WOFIS          ASSIGN TO DISK.
           SELECT LSUBCPG052A    ASSIGN TO PRINTER.
      /
      *=============*
       DATA DIVISION.
      *=============*
        FILE SECTION.
      *-------------*
      *
        FD DSUBCPG051SPG
           RECORD CONTAINS  1800 CHARACTERS
           BLOCK  CONTAINS    10 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 26000
           VALUE OF TITLE IS
           "D/SUB/CPG/051/SPG/R274 ON PKDADOS."
           SECURITYTYPE PUBLIC.
      *NOVO LAYOUT REGISTRO DO DSUBCPG058SISPG.
      *     COPY "B/SUB/CPG/051/SPG/ENT  ON PKPRG".
           COPY "*B/SUB/CPG/051/SPG/R274 ON PKBDSGF".
      *    COPY "B/SUB/CPG/051/SPG  ON PKPRG".

        FD DSUBCPG052SPG
           RECORD CONTAINS  1800 CHARACTERS
           BLOCK  CONTAINS    10 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 36000
           VALUE OF TITLE IS "D/SUB/CPG/052/SPG/R274 ON PKDADOS."
           SECURITYTYPE PUBLIC.
      *NOVO LAYOUT REGISTRO DO DSUBCPG058SISPG.
      *     COPY "B/SUB/CPG/051/SPG/ENTIDADES  ON PKPRG".
      *     COPY "*B/SUB/CPG/051/SPG/CNF ON PKBDSGF".
           COPY "*B/SUB/CPG/051/SPG/R274 ON PKBDSGF".
      *     COPY "B/SUB/CPG/051/SPG  ON PKPRG".

       FD WOFIS
          RECORD CONTAINS 80 CHARACTERS
          BLOCK  CONTAINS 10 RECORDS
          VALUE  OF TITLE IS WS-OFIS-TITLE.
       01 REG-WOFIS               PIC X(80).
      *
        FD LSUBCPG052A.
        01 LINHA052A              PIC X(133).
      *
      /
       DATA-BASE SECTION.
      *******************
      *
       DB BDBENCRED.
      * DB BDBENCRED = BDBENCREDTR.
       01 DS-CTRL-SEQ-TP-CR.
       01 DS-RESTART.
      *
      /
        WORKING-STORAGE SECTION.
      *------------------------*
      *
           COPY "*B/DTP/PAD/01/CHECKPOINT/WS ON PKBDSGF".

       77  WS-MSG-LIDOS                  PIC X(132).
       77  W77-MSG                       PIC X(132).
      *
       77 WS-EOF                         PIC X(03) VALUE "NAO".
      $SET MAP
       77 WS-REGS-LIDOS                  PIC 9(08) VALUE ZEROS BINARY.
       77 WS-REGS-CHECKPOINT             PIC 9(08) VALUE ZEROS BINARY.
       77 WS-REGS-GRAVADOS               PIC 9(08) VALUE ZEROS BINARY.
       77 WS-REGS-GRAVADOS-SEAS          PIC 9(08) VALUE ZEROS BINARY.
      $RESET MAP
       77 W77-ESQUEMA                    PIC 9(05) BINARY.
       77 WS-PAR-MOV-CREDITO             PIC X(04).
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:51
      *77 W77-DATA                       PIC 9(06) BINARY.
       77 W77-DATA                       PIC 9(08) BINARY.
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:51
      *77 W77-DATA-FIM                   PIC 9(06) BINARY.
       77 W77-DATA-FIM                   PIC 9(08) BINARY.
       77 W77-HORA                       PIC 9(06) BINARY.
       77 W77-HORA-FIM                   PIC 9(06) BINARY.
       77 WS-ESTIMATIVA                  PIC 9(06).
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:51
      *77 WS-DATA-INICIO-PROC            PIC 9(06).
       77 WS-DATA-INICIO-PROC            PIC 9(08).
       01 WS-HORA-INICIO-PROC            PIC 9(06).
       01 WS-HORA-INI-R REDEFINES WS-HORA-INICIO-PROC.
          03 WS-HHHMM-PROC                  PIC 9(04).
          03 FILLER                         PIC 9(02).
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:51
      *77 WS-DATA-FIM-PROC               PIC 9(06).
       77 WS-DATA-FIM-PROC               PIC 9(08).
       77 WS-HORA-FIM-PROC               PIC 9(06).
       77 CONT-PAG                       PIC 9(06).
       77 WS-CONTA-LINHAS0521            PIC 9(02) VALUE 60.
       77 WS-QUAL-ARQUIVO                PIC 9(01) VALUE 9.
       77 WS-IN-SEAS-ANT                 PIC 9(01) VALUE 0.
       77 WS-CS-UF-ANT                   PIC 9(02).
       77 WS-ID-BANCO-ANT                PIC 9(03).
       77 WS-CS-AGENCIA-PIONEIRA-ANT     PIC 9(01).
       77 WS-CS-MEIO-PAGTO-ANT           PIC 9(02).
       77 WS-CS-ORIGEM-CREDITO-ANT       PIC 9(01).
       77 WS-DT-INI-VALIDADE-ANT         PIC 9(08).
       77 WS-DT-PROVISAO-ANT             PIC 9(08).
       77 WS-CS-ESP-LOAS-ANT             PIC 9(01).
       77 WS-DT-PROC-CREDITO-ANT         PIC 9(08).
       77 WS-QTDE-AUX                    PIC 9(08).
       77 WS-VALOR-AUX                   PIC 9(15)V99.
       77 WS-QTDE-IR-AUX                 PIC 9(08).
       77 WS-VALOR-IR-AUX                PIC 9(15)V99.
       77 WS-QTDE-TOTAL                  PIC 9(08).
       77 WS-QTDE-TOTAL-SEAS             PIC 9(08).
       77 WS-VALOR-TOTAL                 PIC 9(15)V99.
       77 WS-VALOR-TOTAL-SEAS            PIC 9(15)V99.
       77 W77-OFIS-CLASSE                PIC 9(11)  BINARY.
       77 W77-OFIS-PRAZO                 PIC 9(11)  BINARY VALUE 365.
       77 W77-OFIS-RETORNO               PIC S9(11) BINARY.
       77 WS-PARAM1-ZDISPLAY             PIC X(75).
       77 WS-PARAM2-ZDISPLAY             PIC X(15).
       77 WS-INTEIRO-ZDISPLAY            PIC 9(01)  BINARY VALUE 0.
       77 WS-CS-ROT-GER-CRED             PIC 9(02).
      *
      *Novas variaveis usadas no Pregao da folha.

       77 WS-ID-PREGAO-ANT               PIC 9(02).
       77 WS-ID-LOTE-ANT                 PIC 9(02).
       77 WS-CS-ORIG-ORC-ANT             PIC 9(01).
       77 WS-CS-REDE-ECT-ANT             PIC 9(01).
       77 WS-CS-URBANO-RURAL-ANT         PIC 9(01).
       77 WS-GR-ESPECIES-ANT             PIC 9(02).
       77 WS-ID-SAL-FAMILIA-ANT          PIC 9(01).
      *
      *
       01 WS-OFIS-TITLE             PIC X(030) VA
          "W/SUB/CPG/052/OFIS ON PKDADOS.".
       01 WS-OFIS-REMETENTE         PIC X(012) VA "RCTRLSUB".
       01 WS-OFIS-LISTA             PIC X(015) VA "SUBCPG051SPG".
       01 WS-OFIS-GAVETA            PIC X(012) VA "ZERO".
       01 WS-OFIS-PASTA             PIC X(012) VA "CPG".
       01 WS-OFIS-ASSUNTO.
          03 FILLER                 PIC X(004) VA "ESQ".
          03 WS-OFIS-ESQUEMA        PIC 9(005).
          03 FILLER                 PIC X(015) VA "-LSUBCPG052SPGA".
          03 FILLER                 PIC X(019) VA SPACES.
      *
       01 WS-DT-AAAAMMDD                 PIC 9(08).
       01 WS-DT-AAAAMMDD-R REDEFINES WS-DT-AAAAMMDD.
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:51
      *   03 WS-DT-ANO19                 PIC 9(02).
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:51
      *   03 WS-DT-AAMMDD                PIC 9(06).
          03 WS-DT-AAMMDD                PIC 9(08).
      *
      *--------------------------------------------------------------
      *
       01  WS-DATA-ACCEPT.
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:51
      *    03 WS-DATA-AC             PIC 9(10) TYPE IS NUMERIC-DATE.
           03 WS-DATA-AC             PIC 9(08) .
           03 WS-DATA-AC-R REDEFINES WS-DATA-AC.
      ********** UNISYS Y2K - Linha incluida em 28/04/1999 16:45:51
               05 WS-ANO-AC          PIC 9(04).
               05 WS-MES-AC          PIC 9(02).
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:51
      *        05 FILLER             PIC X(01).
               05 WS-DIA-AC          PIC 9(02).
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:51
      *        05 FILLER             PIC X(01).
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:51
      *        05 WS-ANO-AC          PIC 9(04).
      *
       01  TABELA-DIA                       PIC  X(24)  VALUE
           "312831303130313130313031".
       01  TABELA-DIA-R  REDEFINES  TABELA-DIA.
           03  TB-DIA                       PIC  9(02)  OCCURS 12 TIMES.

       01 WS-TITLE-DSUBCPG052.
          03 WS-NOME-ROTINA-SPG       PIC X(14).
          03 WS-DATA-TITLE.
      *       05 WS-ANO-TITLE          PIC 9(04).
             05 WS-MES-TITLE          PIC 9(02).
             05 WS-DIA-TITLE          PIC 9(02).
             05 FILLER                PIC X(01) VALUE "/".
             05 WS-HHHMM-TITLE        PIC 9(04).
          03 FILLER                   PIC X(09) VALUE " ON SORT.".

       01 WS-TITLE-DSUBCPG052-CALA.
          03 WS-NOME-ROTINA-SPG-CALA  PIC X(14).
          03 WS-ID-CALAMIDADE         PIC 9(03).
          03 FILLER                   PIC X(09) VALUE " ON SORT.".
      *
       01 WS-TITLE-DSUBCPG052-MAN.
          03 WS-NOME-ROTINA-SPG-MAN   PIC X(14).
          03 WS-DATA-TITLE-M.
             05 WS-ANO-TITLE-M        PIC 9(04).
             05 WS-MES-TITLE-M        PIC 9(02).
      *    03 FILLER                   PIC X(02) VALUE "/2".
          03 FILLER                   PIC X(12) VALUE " ON SORT.".

       01 WS-TITLE-DSUBCPG052SCF.
          03 WS-NOME-ROTINA-SCF       PIC X(09).
          03 FILLER                   PIC X(15) VALUE "/MMMMMMMM/I997/".
          03 WS-DATA-TITLE-SCF.
             05 WS-ANO-TITLE-SCF      PIC 9(04).
             05 WS-MES-TITLE-SCF      PIC 9(02).
             05 WS-DIA-TITLE-SCF      PIC 9(02).
          03 WS-DATA-TITLE-SCF-R REDEFINES WS-DATA-TITLE-SCF PIC 9(08).
          03 FILLER                   PIC X(01) VALUE "/".
          03 WS-HORA-TITLE-SCF        PIC 9(06).
          03 FILLER                   PIC X(01) VALUE "/".
          03 FILLER                   PIC X(01) VALUE "D".
          03 WS-SEQUENCIAL-TITLE-SCF  PIC 9(07).
          03 FILLER                   PIC X(09) VALUE " ON PACK.".
      *
      /
        01 CAB-01.
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:52
      *    03 FILLER                PIC X(18) VALUE "MPS/DATAPREV".
           03 FILLER                PIC X(16) VALUE "MPS/DATAPREV".
           03 FILLER                PIC X(48) VALUE
                   "I N S S - INSTITUTO NACIONAL DO SEGURO SOCIAL".
           03 FILLER                PIC X(06) VALUE "DATA: ".
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:52
      *    03 CAB01-DATA            PIC 99/09/09.
           03 CAB01-DATA            PIC 99/09/0999.
      *
        01 CAB-02.
           03 FILLER              PIC X(25) VALUE "P/SUB/CPG/052/R274".
           03 FILLER                PIC X(40) VALUE
              "SISTEMA  UNICO  DE  BENEFICIO".
           03 FILLER                PIC X(06) VALUE "HORA: ".
           03 CAB02-HORA            PIC 99I:99I:99.
      *
        01 CAB-031.
           03 FILLER                PIC X(15) VALUE "LSUBCPG052UNI.A".
           03 FILLER                PIC X(02) VALUE SPACES.
           03 FILLER                PIC X(52) VALUE
              "***    GERACAO DE ARQ. REDUZIDO PARA O DARDO    ***".
           03 FILLER                PIC X(07) VALUE "PAG.: ".
           03 CAB031-PAGINA         PIC ZZZ.ZZ9.
      *
        01 CAB-031A.
           03 FILLER                PIC X(80) VALUE ALL "=".
      *
        01 CAB-041.
           03 FILLER                PIC X(34) VALUE
                                "TEMPO DE PROCESSAMENTO (HH:MM:SS):".
           03 FILLER                PIC X(18) VALUE
                                "       PARAMETROS:".
      *
        01 CAB-041A.
           03 FILLER                PIC X(35) VALUE ALL "=".
           03 FILLER                PIC X(18) VALUE
                                "   ===============".
      *
        01 CAB-052.
           03 FILLER                PIC X(27) VALUE
                                       "INICIO DO PROCESSAMENTO  : ".
           03 CAB05-HORA-INI        PIC 99I:99I:99.
           03 FILLER                PIC X(13) VALUE
                                       "   NUM.ESQ.: ".
           03 CAB05-NUM-ESQ         PIC 9(05).
      *
        01 CAB-061.
           03 FILLER                PIC X(27) VALUE
                                       "TERMINO DO PROCESSAMENTO : ".
           03 CAB06-HORA-FIM        PIC 99I:99I:99.
           03 FILLER                PIC X(13) VALUE  " PARAM PROC: ".
           03 CAB06-PARAM           PIC X(04).
      *
        01 CAB-071.
           03 FILLER                PIC X(27) VALUE
                                      "ESTIMATIVA DE TEMPO GASTO: ".
           03 CAB07-TEMPO-GASTO     PIC 99I:99I:99.
      *
        01 DET1.
           03 TEXTO-DET1            PIC X(46).
           03 FILLER                PIC X(02) VALUE ": ".
           03 QT-DET1               PIC ZZ.ZZZ.ZZ9.
           03 QT-DET1-R REDEFINES QT-DET1 PIC X(10).
           03 VL-DET1               PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
           03 VL-DET1-R REDEFINES VL-DET1 PIC X(22).
      *
      /
      *=================================================================
       PROCEDURE DIVISION
            USING  W77-ESQUEMA WS-PAR-MOV-CREDITO
                                     W77-OFIS-CLASSE.
      *=================================================================
      *
       DECLARATIVES.
      *
       DE-INTERRUPT SECTION.
           USE INTERRUPT PROCEDURE.
      *
       DE-INTERROMPE.
      *
           MOVE SPACES TO  WS-PARAM2-ZDISPLAY   WS-PARAM1-ZDISPLAY
           MOVE "LIDOS   :"       TO WS-PARAM1-ZDISPLAY.
           MOVE  WS-REGS-LIDOS TO WS-PARAM2-ZDISPLAY.
           CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                            WS-PARAM2-ZDISPLAY,
                                            WS-INTEIRO-ZDISPLAY.
           MOVE SPACES TO  WS-PARAM2-ZDISPLAY   WS-PARAM1-ZDISPLAY
           MOVE "GRAVADOS:"       TO WS-PARAM1-ZDISPLAY.
           MOVE  WS-REGS-GRAVADOS TO WS-PARAM2-ZDISPLAY.
           MOVE  1                TO WS-INTEIRO-ZDISPLAY.
           CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                            WS-PARAM2-ZDISPLAY,
                                            WS-INTEIRO-ZDISPLAY.
           MOVE 0 TO WS-INTEIRO-ZDISPLAY.
      *
       END DECLARATIVES.
      *
      /
      *****************************************************************
       PRINCIPAL SECTION.
      *-----------------*
       INICIO.
      *
      *    MOVE 1      TO W77-ESQUEMA.
      *    MOVE "MANU" TO  WS-PAR-MOV-CREDITO.
      *    MOVE 305    TO  W77-OFIS-CLASSE.

           COPY "*B/DTP/PAD/01/CHECKPOINT/PD1 ON PKBDSGF".
      *
           ATTACH DE-INTERRUPT TO ATTRIBUTE EXCEPTIONEVENT OF MYSELF.
      *
           PERFORM  1000-ABERTURA
              THRU  1000-FIM.
      *
           PERFORM 9000-LE-CPG051
              THRU 9000-FIM.

           PERFORM 9010-ABRE-DSUBCPG052SPG
              THRU 9010-FIM
      *
           PERFORM 10000-GRAVA-HEADER
               THRU 10000-FIM.
      *
           PERFORM 9000-LE-CPG051
              THRU 9000-FIM.
      *
           PERFORM 2000-LOOP
              THRU 2000-FIM
             UNTIL WS-EOF EQUAL "SIM".
      *
           PERFORM 11000-GRAVA-TRAILER
               THRU 11000-FIM.
      *
           PERFORM 3000-EMITIR-RELATORIO
              THRU 3000-FIM.
      *
           PERFORM 4000-FINALIZA
              THRU 4000-FIM.
      *
           STOP  RUN.
      *
      /
       1000-ABERTURA.
      *-------------*
      *
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:52
           CALL "ZDATA4 IN DTP/L1" USING W77-DATA.
           MOVE W77-DATA    TO  WS-DATA-INICIO-PROC.
      *
           CALL "ZHORA IN DTP/L1" GIVING W77-HORA.
           MOVE W77-HORA  TO  WS-HORA-INICIO-PROC.
      *
           SET DSUBCPG051SPG(FILETYPE) TO 7.
           OPEN INPUT  DSUBCPG051SPG.
      *     MOVE   WS-PAR-MOV-CREDITO TO   WS-ROTINA-TITLE.
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:52
           ACCEPT WS-DATA-AC         FROM DATE YYYYMMDD.
           MOVE   WS-ANO-AC          TO   WS-ANO-TITLE-M.
           MOVE   WS-MES-AC          TO   WS-MES-TITLE WS-MES-TITLE-M.
           MOVE   WS-DIA-AC          TO   WS-DIA-TITLE.
           MOVE   WS-HHHMM-PROC      TO   WS-HHHMM-TITLE.
      *GATILHO
      *     IF WS-DIA-TITLE = 01
      *        MOVE TB-DIA (WS-MES-AC) TO WS-DIA-TITLE
      *        COMPUTE WS-MES-TITLE = WS-MES-TITLE - 1
      *        IF WS-MES-TITLE = ZEROS
      *           MOVE 12 TO WS-MES-TITLE.
      *
      *     IF WS-DIA-TITLE NOT EQUAL 01
      *        COMPUTE WS-DIA-TITLE = WS-DIA-TITLE - 01.
      *
           OPEN UPDATE BDBENCRED.
      *
           IF WS-PAR-MOV-CREDITO = "MANU"
              IF WS-IN-SEAS-ANT EQUAL 0
              MOVE "FSUBCPG50" TO WS-NOME-ROTINA-SCF
              MOVE "D/SUB/SPG/950/" TO WS-NOME-ROTINA-SPG-MAN
              MOVE  36         TO WS-CS-ROT-GER-CRED
              ELSE
              MOVE "FCASCPG31" TO WS-NOME-ROTINA-SCF
              MOVE  18         TO WS-CS-ROT-GER-CRED

           ELSE
              IF WS-PAR-MOV-CREDITO = "CONC"
                 IF WS-IN-SEAS-ANT EQUAL 0
                 MOVE "FSUBCPG51" TO WS-NOME-ROTINA-SCF
                 MOVE "D/SUB/SPG/951/" TO WS-NOME-ROTINA-SPG
                 MOVE  12         TO WS-CS-ROT-GER-CRED
                 ELSE
                 MOVE "FCASCPG32" TO WS-NOME-ROTINA-SCF
                 MOVE  19         TO WS-CS-ROT-GER-CRED

              ELSE
                 IF WS-PAR-MOV-CREDITO = "ALTE"
                    IF WS-IN-SEAS-ANT EQUAL 0
                    MOVE "FSUBCPG54" TO WS-NOME-ROTINA-SCF
                    MOVE "D/SUB/SPG/954/" TO WS-NOME-ROTINA-SPG
                    MOVE  16         TO WS-CS-ROT-GER-CRED
                    ELSE
                    MOVE "FCASCPG33" TO WS-NOME-ROTINA-SCF
                    MOVE  20         TO WS-CS-ROT-GER-CRED
                 ELSE
                    IF WS-PAR-MOV-CREDITO = "EMPR"
      ************     IF WS-IN-SEAS-ANT EQUAL 0
                          MOVE "FSUBCPG55" TO WS-NOME-ROTINA-SCF
                          MOVE "D/SUB/SPG/955/" TO WS-NOME-ROTINA-SPG
                          MOVE  25       TO WS-CS-ROT-GER-CRED
                    ELSE
                       IF WS-PAR-MOV-CREDITO = "GLOS"
      **************      IF WS-IN-SEAS-ANT EQUAL 0
                          MOVE "FSUBCPG56" TO WS-NOME-ROTINA-SCF
                          MOVE "D/SUB/SPG/956/" TO WS-NOME-ROTINA-SPG
                          MOVE  26         TO WS-CS-ROT-GER-CRED
                       ELSE
                          IF WS-PAR-MOV-CREDITO = "ESPE"
      *****************      IF WS-IN-SEAS-ANT EQUAL 0
                             MOVE "FSUBCPG53" TO WS-NOME-ROTINA-SCF
                             MOVE "D/SUB/SPG/953/" TO WS-NOME-ROTINA-SPG
                             MOVE  28         TO WS-CS-ROT-GER-CRED
                          ELSE
      *Em 25/05/2019, pagar atrasados de Talidomida - DM.080880.
                           IF WS-PAR-MOV-CREDITO = "TALI"
                             MOVE "FSUBCPG53" TO WS-NOME-ROTINA-SCF
                             MOVE "D/SUB/SPG/953/" TO WS-NOME-ROTINA-SPG
                             ADD 1 TO WS-HHHMM-TITLE
                             MOVE  28         TO WS-CS-ROT-GER-CRED
                           ELSE
                              IF WS-PAR-MOV-CREDITO = "CALA"
                                MOVE "D/SUB/SPG/958/"
                                             TO WS-NOME-ROTINA-SPG-CALA
                                MOVE  35     TO WS-CS-ROT-GER-CRED
                              ELSE
                                MOVE "PARAMETRO INVALIDO" TO
                                              WS-PARAM1-ZDISPLAY
                                MOVE  WS-PAR-MOV-CREDITO  TO
                                              WS-PARAM2-ZDISPLAY
                                CALL "ZDISPLAY2 IN DTP/L1" USING
                                                     WS-PARAM1-ZDISPLAY,
                                                     WS-PARAM2-ZDISPLAY,
                                                     WS-INTEIRO-ZDISPLAY
                          SET MYSELF (STATUS) TO -1.
      *

           BEGIN-TRANSACTION NO-AUDIT DS-RESTART.

           LOCK ST-CTRL-SEQ-TP-CR AT
                CS-ROT-GER-CRED = WS-CS-ROT-GER-CRED AND
                ID-BANCO        = 0
                ON EXCEPTION
                   IF DMSTATUS (NOTFOUND)
                      CREATE DS-CTRL-SEQ-TP-CR
                      MOVE WS-CS-ROT-GER-CRED TO CS-ROT-GER-CRED
                      MOVE 0                  TO ID-BANCO
errors*                                          NU-ULT-SEQ-TP
                   ELSE
                      MOVE "ERRO LOCK ST-CTRL-SEQ-TP-CR."
                        TO  WS-PARAM1-ZDISPLAY
                      MOVE  WS-CS-ROT-GER-CRED
                        TO  WS-PARAM2-ZDISPLAY
                      CALL "ZDISPLAY2 IN DTP/L1" USING
                                                 WS-PARAM1-ZDISPLAY,
                                                 WS-PARAM2-ZDISPLAY,
                                                 WS-INTEIRO-ZDISPLAY
                      CALL SYSTEM DMTERMINATE.
      *
           MOVE    WS-DATA-TITLE TO WS-DATA-TITLE-SCF.
           MOVE    W77-HORA      TO WS-HORA-TITLE-SCF.

errors*    COMPUTE WS-SEQUENCIAL-TITLE-SCF = NU-ULT-SEQ-TP + 1.

      ***     IF WS-PAR-MOV-CREDITO = "MANU"
      ***        CHANGE  ATTRIBUTE TITLE OF DSUBCPG052SPG
      ***                                    TO  WS-TITLE-DSUBCPG052-MAN
      ***     ELSE
      ***        CHANGE  ATTRIBUTE TITLE OF DSUBCPG052SPG
      ***                                     TO  WS-TITLE-DSUBCPG052.

      ***     CHANGE  ATTRIBUTE SECURITYTYPE OF DSUBCPG052SPG TO PUBLIC.
      ***     OPEN    OUTPUT    DSUBCPG052SPG.

       1000-FIM. EXIT.
      *---------------*
      *
      /
       2000-LOOP.
      *---------*
      *
           WRITE DSUBCPG051-REG-SPG51 OF DSUBCPG052SPG
                        FROM DSUBCPG051-REG-SPG51 OF DSUBCPG051SPG.
           ADD SPG51-VALOR-LIQUIDO OF DSUBCPG051SPG TO WS-VALOR-TOTAL.
           ADD 1 TO WS-REGS-GRAVADOS.
           ADD 1 TO   WS-QTDE-TOTAL.
           PERFORM 9000-LE-CPG051
              THRU 9000-FIM.

      *
       2000-FIM.
      *---------*
      *
      /
       2900-PAB-SAI.
      *-------------*

           MOVE SPACES TO  WS-PARAM2-ZDISPLAY   WS-PARAM1-ZDISPLAY
           MOVE "DESPREZO PAB: "    TO WS-PARAM2-ZDISPLAY.
           MOVE DSUBCPG051-REG-SPG51 OF DSUBCPG051SPG
                                                 TO WS-PARAM1-ZDISPLAY.
           CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                            WS-PARAM2-ZDISPLAY,
                                            WS-INTEIRO-ZDISPLAY.
       2900-PAB-SAIU. EXIT.
      *---------------------*
      *
      /
       3000-EMITIR-RELATORIO.
      *---------------------*
      *
           OPEN OUTPUT LSUBCPG052A.
           OPEN OUTPUT WOFIS.
      *
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:53
      *    CALL  "ZDATA  IN DTP/L1" USING W77-DATA.
           CALL  "ZDATA4  IN DTP/L1" USING W77-DATA.
      *
           CALL  "ZHORA  IN DTP/L1" GIVING W77-HORA.
      *
           MOVE W77-DATA      TO  WS-DATA-FIM-PROC.
           MOVE W77-HORA      TO  WS-HORA-FIM-PROC.
      *
      ********** UNISYS Y2K - Linha alterada em 28/04/1999 16:45:53
      *     CALL "P/SUB/LIB/601" USING  WS-DATA-INICIO-PROC
            CALL "P/SUB/LIB/2K/601" USING  WS-DATA-INICIO-PROC
                                        WS-HORA-INICIO-PROC
                                        WS-DATA-FIM-PROC
                                        WS-HORA-FIM-PROC
                                        WS-ESTIMATIVA.
      *
           MOVE   SPACES                 TO  LINHA052A.
           MOVE   W77-DATA               TO  CAB01-DATA.
           MOVE   W77-HORA               TO  CAB02-HORA.
           MOVE     1                    TO  CONT-PAG.
           MOVE   CONT-PAG               TO  CAB031-PAGINA.
           MOVE   WS-HORA-INICIO-PROC    TO  CAB05-HORA-INI.
           MOVE   WS-HORA-FIM-PROC       TO  CAB06-HORA-FIM.
           MOVE   WS-ESTIMATIVA          TO  CAB07-TEMPO-GASTO.
           MOVE   W77-ESQUEMA            TO  CAB05-NUM-ESQ
                                             WS-OFIS-ESQUEMA.
           MOVE   WS-PAR-MOV-CREDITO     TO  CAB06-PARAM.
      *
           WRITE  LINHA052A   FROM  CAB-01   AFTER PAGE.
           WRITE  LINHA052A   FROM  CAB-02   AFTER  2.
           WRITE  LINHA052A   FROM  CAB-031  AFTER  2.
           WRITE  LINHA052A   FROM  CAB-031A AFTER  1.
           WRITE  LINHA052A   FROM  CAB-041  AFTER  2.
           WRITE  LINHA052A   FROM  CAB-041A AFTER  1.
           WRITE  LINHA052A   FROM  CAB-052  AFTER  2.
           WRITE  LINHA052A   FROM  CAB-061  AFTER  1.
           WRITE  LINHA052A   FROM  CAB-071  AFTER  1.
           WRITE  LINHA052A   FROM  CAB-031A AFTER  1.
      *
           WRITE  REG-WOFIS   FROM  CAB-01.
           WRITE  REG-WOFIS   FROM  CAB-02.
           WRITE  REG-WOFIS   FROM  CAB-031.
           WRITE  REG-WOFIS   FROM  CAB-031A.
           WRITE  REG-WOFIS   FROM  CAB-041.
           WRITE  REG-WOFIS   FROM  CAB-041A.
           WRITE  REG-WOFIS   FROM  CAB-052.
           WRITE  REG-WOFIS   FROM  CAB-061.
           WRITE  REG-WOFIS   FROM  CAB-071.
           WRITE  REG-WOFIS   FROM  CAB-031A.
      *
      *    LINHAS DE DETALHE
      *
           MOVE   SPACES TO VL-DET1-R.
           MOVE  "REGS LIDOS SEM DESPREZO MEIO PAGTO = 4" TO TEXTO-DET1.
           MOVE  WS-REGS-LIDOS                           TO QT-DET1.
           WRITE LINHA052A FROM DET1                        AFTER 2.
           WRITE REG-WOFIS FROM DET1.
      *
           MOVE  "REGS GRAVADOS NO DSUBCPG052          " TO TEXTO-DET1.
           MOVE   WS-REGS-GRAVADOS                       TO QT-DET1.
           WRITE  LINHA052A FROM DET1                       AFTER 1.
           WRITE REG-WOFIS FROM DET1.
      *
           MOVE  "TOTAL DE BENEFICIOS                  " TO TEXTO-DET1.
           MOVE  WS-QTDE-TOTAL                           TO QT-DET1.
           WRITE LINHA052A FROM DET1                        AFTER 2.
           WRITE REG-WOFIS FROM DET1.
      *
           MOVE   SPACES TO QT-DET1-R.
           MOVE  "TOTAL DE VALOR LIQUIDO               " TO TEXTO-DET1.
           MOVE   WS-VALOR-TOTAL                         TO VL-DET1.
           WRITE  LINHA052A FROM DET1                       AFTER 1.
           WRITE REG-WOFIS FROM DET1.
      *-----------------------------------------------------------------
           MOVE   SPACES TO QT-DET1-R VL-DET1-R.
           MOVE  "REGS GRAVADOS NO DSUBCPG052 (SEAS)   " TO TEXTO-DET1.
           MOVE   WS-REGS-GRAVADOS-SEAS                  TO QT-DET1.
           WRITE  LINHA052A FROM DET1                       AFTER 1.
           WRITE REG-WOFIS FROM DET1.
      *
           MOVE  "TOTAL DE BENEFICIOS         (SEAS)   " TO TEXTO-DET1.
           MOVE  WS-QTDE-TOTAL-SEAS                      TO QT-DET1.
           WRITE LINHA052A FROM DET1                        AFTER 2.
           WRITE REG-WOFIS FROM DET1.
      *
           MOVE   SPACES TO QT-DET1-R.
           MOVE  "TOTAL DE VALOR LIQUIDO      (SEAS)   " TO TEXTO-DET1.
           MOVE   WS-VALOR-TOTAL-SEAS                    TO VL-DET1.
           WRITE  LINHA052A FROM DET1                       AFTER 1.
           WRITE REG-WOFIS FROM DET1.
           CLOSE LSUBCPG052A.
      *
           CLOSE WOFIS SAVE.

       3000-FIM. EXIT.
      *--------*
      *
      /
       4000-FINALIZA.
      *-------------*
      *
          CLOSE DSUBCPG051SPG.
      *
      *    CLOSE DSUBCPG052SPG      SAVE.

       4000-FIM-2.

      * Comentado em 26/05/2019 - Anderson Brito sob orienta??o JCM
      *    CLOSE DSUBCPG052SPG   SAVE.
      *
      * Incluso em 26/05/2019 - Anderson Brito sob orienta??o JCM
          IF WS-REGS-GRAVADOS > 0
             CLOSE DSUBCPG052SPG   SAVE
          ELSE
             CLOSE DSUBCPG052SPG.
      * Fim inclus?o em 26/05/2019 - Anderson Brito sob orienta??o JCM

          OPEN INPUT DSUBCPG052SPG.

          CLOSE DSUBCPG052SPG RELEASE.
      *
errors*   MOVE  WS-SEQUENCIAL-TITLE-SCF TO NU-ULT-SEQ-TP.
errors*   MOVE  WS-DATA-TITLE-SCF-R     TO DT-PROC-SEQ-TP.
      *    STORE DS-CTRL-SEQ-TP-CR.
          END-TRANSACTION NO-AUDIT DS-RESTART.

       4000-FINALIZOU-2. EXIT.
       4000-CLOSE.
      *
          CLOSE BDBENCRED.
      *
       4000-FIM. EXIT.
      *---------------*
      *
           COPY "*B/DTP/PAD/01/CHECKPOINT/PD2 ON PKBDSGF".

       4000-DISPLAY.
      *-------------*

          MOVE WS-MSG-LIDOS  TO W77-MSG
          CALL "ZDISPLAY IN DTP/L1" USING W77-MSG,
                                       W77-STATUS-BACKUP-CKP.

       4000-FIM-DISPLAY. EXIT.
      *-----------------------*

       9000-LE-CPG051.
      *---------------*

           READ DSUBCPG051SPG
                AT END
                   MOVE "SIM" TO WS-EOF
                   GO TO 9000-FIM.

           IF SPG51-ID-REG-T OF DSUBCPG051SPG GREATER 2
              MOVE "SIM" TO WS-EOF
              ADD 1 TO WS-REGS-LIDOS
              GO TO 9000-FIM.

           ADD 1 TO WS-REGS-LIDOS.

           IF SPG51-CS-MEIO-PAGTO OF DSUBCPG051SPG = 4
              IF SPG51-VALOR-LIQUIDO OF DSUBCPG051SPG = ZEROS
                 AND SPG51-QTD-ENT-CLASS OF DSUBCPG051SPG = ZEROS
                 PERFORM 2900-PAB-SAI THRU 2900-PAB-SAIU
                 GO TO 9000-LE-CPG051
              ELSE
                 IF SPG51-ID-BANCO OF DSUBCPG051SPG NOT = 001
                    PERFORM 2900-PAB-SAI THRU 2900-PAB-SAIU.



       9000-FIM.
      *---------*
      *
       9010-ABRE-DSUBCPG052SPG.
      *-----------------------*

      *     IF WS-PAR-MOV-CREDITO = "MANU"
      *        CHANGE  ATTRIBUTE TITLE OF DSUBCPG052SPG
      *                                     TO  WS-TITLE-DSUBCPG052-MAN
      *     ELSE
      *        IF WS-PAR-MOV-CREDITO = "CALA"
      *           MOVE SPG51-ID-CALAMIDADE-H OF DSUBCPG051SPG
      *                                     TO WS-ID-CALAMIDADE
      *           CHANGE  ATTRIBUTE TITLE OF DSUBCPG052SPG
      *                                    TO  WS-TITLE-DSUBCPG052-CALA
      *        ELSE
      *           CHANGE  ATTRIBUTE TITLE OF DSUBCPG052SPG
      *                                     TO  WS-TITLE-DSUBCPG052.

           CHANGE  ATTRIBUTE SECURITYTYPE OF DSUBCPG052SPG TO PUBLIC.
           OPEN    OUTPUT    DSUBCPG052SPG.

       9010-FIM.
      *---------*

       10000-GRAVA-HEADER.
      *--------------------*

           MOVE DSUBCPG051-REG-SPG51-HEADER OF DSUBCPG051SPG
               TO  DSUBCPG051-REG-SPG51-HEADER OF DSUBCPG052SPG.

           IF WS-PAR-MOV-CREDITO = "CALA"
               MOVE 5 TO SPG51-ORIGEM-CREDITO OF DSUBCPG052SPG.

           WRITE DSUBCPG051-REG-SPG51-HEADER OF DSUBCPG052SPG.

      *
       10000-FIM.
        EXIT.
      *---------------*

       11000-GRAVA-TRAILER.
      *--------------------*

           MOVE ALL "00" TO DSUBCPG051-REG-SPG51-TRAILER
                                                      OF DSUBCPG052SPG.
           MOVE 9                   TO  SPG51-ID-REG-T OF DSUBCPG052SPG.
           MOVE WS-VALOR-TOTAL  TO  SPG51-TOT-VL-LIQ-T OF DSUBCPG052SPG.
           MOVE WS-REGS-GRAVADOS   TO  SPG51-QTD-REG-T OF DSUBCPG052SPG.
           WRITE DSUBCPG051-REG-SPG51-TRAILER OF DSUBCPG052SPG.

       11000-FIM.
        EXIT.
      *---------------*

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%   REFERENCIAS CRUZADAS DO FONTE     %%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      * LIBRARY - B/DTP/PAD/01/CHECKPOINT/PD2 CICLO = 0001VERSAO =   11

      * LIBRARY - B/DTP/PAD/01/CHECKPOINT/PD1 CICLO = 0001VERSAO =   01

      * LIBRARY - B/DTP/PAD/01/CHECKPOINT/WS  CICLO = 0001VERSAO =   06

      * LIBRARY - B/SUB/CPG/051/SPG/R274      CICLO = 0001VERSAO =   00

      * LIBRARY - B/SUB/CPG/051/SPG/R274      CICLO = 0001VERSAO =   00

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
