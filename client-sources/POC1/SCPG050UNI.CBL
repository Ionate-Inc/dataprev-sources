      *
      * UNISYS Y2K - TEAM 2000
      *
      $SET LIST
      $SET ERRORLIMIT = 10
      $SET FEDLEVEL = 5
      *********$SET TADS
      *========================*
       IDENTIFICATION  DIVISION.
      *========================*
errors PROGRAM-ID.      PSUBCPG050SPGUNI.
       DATE-WRITTEN.    15/10/2012.
       DATE-COMPILED.
      ******************************************************************
      *  OBJETIVO: GERAR UM ARQUIVO COM QTDE/VALOR DE CREDITOS EMITI-  *
      *            DOS POR OP/MEIO-PAGTO/ORIGEM-CREDITO/DT-INI-VALIDA- *
      *            DE/INDICADOR LOAS/ID-PAIS.                          *
      *                                                                *
      *  HOSTNAME PRODUCAO: DTPRJCV3                                   *
      *  USERCODE PRODUCAO: PDBATBEN                                   *
      *  DIVISAO:           DDCOSUP                                    *
      *  GRAU DIFICULDADE:                                             *
      ******************************************************************
      * ALTERACAO EM FEV/MAR 1998 - INCLUSAO DA DATA DE PROVISIONAMENTO*
      *     MARKID "PROVIS"         SERAH GRAVADO UM NOVO ARQUIVO      *
      *                             QUE EH UMA COPIA DO DSUBCPG050,    *
      *                             TENDO INCLUIDO NELE A DATA DE PRO- *
      *                             VISIONAMENTO (OPTOU-SE POR GRAVAR  *
      *                             MAIS UM ARQUIVO PARA NAO MEXER EM  *
      *                             TODA A ROTINA JAH EXISTENTE).      *
      *                             NO INICIO DO PROGRAMA, QUANDO ESTI-*
      *                             VER SENDO PROCESSADA A CONCESSAO,  *
      *                             SERA MONTADA UMA TABELA COM A MENOR*
      *                             DATA DE INICIO DE VALIDADE POR     *
      *                             BANCO. (ESTA DATA SERA UTILIZADA NO*
      *                             CALCULO DA DATA DE PROVISIONAMENTO)*
      *                             ATENCAO: EM 26/03/98, POR SOLICITA-*
      *                             CAO DO INSS, A DATA DE PROVISAO PA-*
      *                             RA AGENC. PIONEIRAS EH 4 DIAS ANTES*
      *                             (AO INVES DE 3)                    *
      ******************************************************************
      * ALTERACOES EM AGOSTO/2000 - MARKID EPU-SCF                     *
      *                             . NAO SERAH MAIS GERADO O ARQUIVO  *
      *                             DSUBCPG050 (QUE NAO EH USADO EM    *
      *                             ROTINA ALGUMA)                     *
      *                             . O ARQUIVO DE ENTRADA NAO EH MAIS *
      *                             O DSUBCPG058, MAS O DSUBCPG058, QUE*
      *                             TEM O LAYOUT DO DSUBCPG050, SENDO  *
      *                             QUE OS CAMPOS MICRORREGIAO E MUNI- *
      *                             CIPIO ESTAO ZERADOS E SERAO COMPLE-*
      *                             TADOS NESTE PROGRAMA.              *
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
           SELECT DSUBCPG058     ASSIGN TO DISK.
           SELECT DSUBCPG051     ASSIGN TO DISK.
           SELECT DSUBCPG058SPG  ASSIGN TO DISK.
           SELECT DSUBCPG051SPG  ASSIGN TO DISK.
           SELECT WOFIS          ASSIGN TO DISK.
           SELECT WOFIS2         ASSIGN TO DISK.
           SELECT LSUBCPG050A    ASSIGN TO PRINTER.
      /
      *=============*
       DATA DIVISION.
      *=============*
        FILE SECTION.
      *-------------*
      *
       FD DSUBCPG058
          RECORD CONTAINS  105 CHARACTERS
          BLOCK  CONTAINS  180 RECORDS
          VA OF  TITLE "D/SUB/CPG/058/SPG/SORT ON PKDADOS.".
      *
              COPY "*B/SUB/CPG/050/SUL      ON PKBDSGF".
      *   COPY "B/SUB/CPG/050/JCM     ON PKPRG".
        FD DSUBCPG058SPG
           RECORD CONTAINS  1800 CHARACTERS
           BLOCK  CONTAINS    10 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 40000
           VALUE OF TITLE IS
            "D/SUB/CPG/058/SPG/R274/ESPE/SORT ON PKDADOS."
           SECURITYTYPE PUBLIC.

      *NOVO LAYOUT REGISTRO DO DSUBCPG058SISPG.
      ***      COPY "*B/SUB/CPG/058/SISPG  ON PKBDSGF".
            COPY "*B/SUB/CPG/058/SPG/R274 ON PKBDSGF".
      *     COPY "B/SUB/CPG/058/SPG/ENTCLASS  ON PKPRG".
      *      COPY "B/SUB/CPG/058/SPG/R255 ON PKPRG".
      *
       FD DSUBCPG051
          RECORD CONTAINS  120 CHARACTERS
          BLOCK  CONTAINS  150 RECORDS
          VA OF  TITLE "D/SUB/CPG/051/NOVO ON PKDADOS."
          SECURITYTYPE IS PUBLIC.
              COPY "*B/SUB/CPG/051/SUL  ON PKBDSGF".
      *** COPY "B/SUB/CPG/051/JCM ON PKPRG".


        FD DSUBCPG051SPG
           RECORD CONTAINS  1800 CHARACTERS
           BLOCK  CONTAINS    10 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 26000
           VALUE OF TITLE IS "D/SUB/CPG/051/SPG/R274 ON PKDADOS."
           SECURITYTYPE PUBLIC.

      *   COPY "B/SUB/CPG/051/SPG/ENTIDADES  ON PKPRG".
          COPY "*B/SUB/CPG/051/SPG/R274 ON PKBDSGF".
      *     COPY "B/SUB/CPG/051/SPG/R255 ON PKPRG".
      *
       FD WOFIS
          RECORD CONTAINS 80 CHARACTERS
          BLOCK  CONTAINS 10 RECORDS
          VALUE  OF TITLE IS WS-OFIS-TITLE.
       01 REG-WOFIS               PIC X(80).
      *
       FD WOFIS2
          RECORD CONTAINS 80 CHARACTERS
          BLOCK  CONTAINS 10 RECORDS
          VALUE  OF TITLE IS WS-OFIS-TITLE2.
       01 REG-WOFIS2              PIC X(80).
      *
        FD LSUBCPG050A.
        01 LINHA050A              PIC X(133).
      *
      /
        WORKING-STORAGE SECTION.
      *------------------------*
      *
       77 WS-CP50-ID-BANCO               PIC 9(03) VA 0.
       77 WS-CP50-ID-ORGAO-PAGADOR       PIC 9(06) VA 0.
       77 WS-CP50-DT-INI-VALIDADE        PIC 9(08) VA 0.


       77 WS-EOF                         PIC X(03) VALUE "NAO".
      $SET MAP
       77 WS-REGS-LIDOS                  PIC 9(08) VALUE ZEROS BINARY.
       77 WS-MSG-LIDOS                     PIC X(132).
       77 W77-MSG                          PIC X(132).
       77 WS-REGS-CHECKPOINT             PIC 9(08) VALUE ZEROS BINARY.
       77 WS-REGS-GRAVADOS               PIC 9(08) VALUE ZEROS BINARY.
       77 WS-REGS-GRAVADOS-051           PIC 9(08) VALUE ZEROS BINARY.
       77 WS-REGS-DESP-BANCO-0           PIC 9(08) VALUE ZEROS BINARY.
       77 WS-OPS-ERRO-TB0007             PIC 9(08) VALUE ZEROS BINARY.
       77 WS-OPS-ERRO-TB0023             PIC 9(08) VALUE ZEROS BINARY.
       77 WS-MRS-ERRO-TB0049             PIC 9(08) VALUE ZEROS BINARY.
      $RESET MAP
       77 WS-PARAM1-ZDISPLAY             PIC X(80).
       77 WS-PARAM2-ZDISPLAY             PIC X(80).
       77 WS-INTEIRO-ZDISPLAY            PIC 9(01) BINARY VALUE 0.
       77 WS-MSG                         PIC X(150).
       77 WS-STATUS-BACKUP               PIC 9(11) BINARY.
       77 W77-ESQUEMA                    PIC 9(05) BINARY.
       77 WS-PAR-MOV-CREDITO             PIC X(04).
       77 WS-PAR-PSTC                    PIC X(02).
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *77 W77-DATA                       PIC 9(06) BINARY.
       77 W77-DATA                       PIC 9(08) BINARY.
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *77 W77-DATA-FIM                   PIC 9(06) BINARY.
       77 W77-DATA-FIM                   PIC 9(08) BINARY.
       77 W77-HORA                       PIC 9(06) BINARY.
       77 W77-HORA-FIM                   PIC 9(06) BINARY.
       77 WS-ESTIMATIVA                  PIC 9(06).
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *77 WS-DATA-INICIO-PROC            PIC 9(06).
       77 WS-DATA-INICIO-PROC            PIC 9(08).
       77 WS-HORA-INICIO-PROC            PIC 9(06).
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *77 WS-DATA-FIM-PROC               PIC 9(06).
       77 WS-DATA-FIM-PROC               PIC 9(08).
       77 WS-HORA-FIM-PROC               PIC 9(06).
       77 CONT-PAG                       PIC 9(06).
       77 WS-CONTA-LINHAS0501            PIC 9(02) VALUE 60.
       77 WS-IN-SEAS-ANT                 PIC 9(01).
       77 WS-IN-PION-ANT                 PIC 9(01).
       77 WS-ID-OP-ANT                   PIC 9(06).
       77 WS-BANCO-ANT                   PIC 9(03).
       77 WS-CS-MEIO-PAGTO-ANT           PIC 9(02).
       77 WS-CS-ORIGEM-CREDITO-ANT       PIC 9(01).
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *77 WS-DT-INI-VALIDADE-ANT         PIC 9(06).
       77 WS-DT-INI-VALIDADE-ANT         PIC 9(08).
       77 WS-CS-ESP-LOAS-ANT             PIC 9(01).
       77 WS-QTDE-AUX                    PIC 9(08).
       77 WS-VALOR-AUX                   PIC 9(15)V99.
       77 WS-VL-ENT-AUX                  PIC 9(15)V99.
       77 WS-QTDE-TOTAL                  PIC 9(08).
       77 WS-QTDE-ENT-AUX                PIC 9(06).
       77 WS-VALOR-TOTAL                 PIC 9(15)V99.
       77 WS-VALOR-IR-AUX                PIC 9(15)V99.
       77 WS-QTDE-IR-AUX                 PIC 9(08).
       77 WS-ID-BANCO-TAB                PIC 9(03).
       77 WS-ID-MICRO-REG-TAB            PIC 9(06).
       77 WS-CS-MUNIC-SINPAS-TAB         PIC 9(05).
       77 W77-OFIS-CLASSE                PIC 9(11)  BINARY.
       77 W77-OFIS-PRAZO                 PIC 9(11)  BINARY VALUE 365.
       77 W77-OFIS-RETORNO               PIC S9(11) BINARY.
       77 W77-OFIS-CLASSE2               PIC 9(11)  BINARY.
       77 W77-OFIS-PRAZO2                PIC 9(11)  BINARY VALUE 365.
       77 W77-OFIS-RETORNO2              PIC S9(11) BINARY.
       77 WS-TESTE-CANCELAR-PROGRAMA     PIC  9(01).

      *Variaveis do SISPAGBEN.
       77 WS-CALAMIDADE-ANT              PIC 9(03) VALUE ZEROS.
       77 WS-ADIANT-13SAL-ANT            PIC 9(01) VALUE ZEROS.
       77 WS-SPGENT-ID-REG-ANT            PIC 9(01) VALUE ZEROS.
       77 WS-VALOR-BRUTO-AUX             PIC 9(15)V99.
       77 WS-IND                         PIC 9(02) VALUE ZEROS.

       01  WS-TABELA-RUBRICAS.
           03 WS-TAB-RUBRICAS OCCURS 95 TIMES.
              05  WS-TAB-VL-RUBRICA     PIC 9(015)V99.
              05 FILLER                 PIC 9(001).


      *
      *Variaveis usadas para o pregao.

       77 WS-ID-PREGAO-ANT               PIC 9(02) BINARY.
       77 WS-ID-LOTE-ANT                 PIC 9(02) BINARY.
       77 WS-CS-ORIG-ORCAM-ANT           PIC 9(01) BINARY.
       77 WS-CS-REDE-ECT-ANT             PIC 9(01) BINARY.
       77 WS-CS-URBANO-RURAL-ANT         PIC 9(01) BINARY.
       77 WS-CS-ESPECIE-ANT              PIC 9(02) BINARY.
       77 WS-ID-SAL-FAMILIA-ANT          PIC 9(01) BINARY.
       77 WS-ENT-CLASS-ANT               PIC 9(02) BINARY.

      *Paises do Mercosul

       77 WS-ID-PAIS-ANT                 PIC 9(03) BINARY.

      *
       77 WS-TIPO-MICRO-REG              PIC 9(01).
      ****88 WS-AGENCIA-PIONEIRA         VALUE 6.
      *   88 WS-AGENCIA-PIONEIRA         VALUE 1.
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *77 WS-DT-PROVIS                   PIC 9(06).
       77 WS-DT-PROVIS                   PIC 9(08).
      *
       01 WS-OFIS-TITLE             PIC X(030) VA
          "W/SUB/CPG/050/OFIS ON PKDADOS.".
       01 WS-OFIS-REMETENTE         PIC X(012) VA "RCTRLSUB".
       01 WS-OFIS-LISTA             PIC X(012) VA "SUBCPG050SPG".
       01 WS-OFIS-GAVETA            PIC X(012) VA "ZERO".
       01 WS-OFIS-PASTA             PIC X(012) VA "CPG".
       01 WS-OFIS-ASSUNTO.
          03 FILLER                 PIC X(004) VA "ESQ".
          03 WS-OFIS-ESQUEMA        PIC 9(005).
          03 FILLER                 PIC X(015) VA "-LSUBCPG050SPGA".
          03 FILLER                 PIC X(019) VA SPACES.
      *
       01 WS-OFIS-TITLE2            PIC X(030) VA
          "W/SUB/CPG/050/OFI2 ON PKDADOS.".
       01 WS-OFIS-REMETENTE2        PIC X(012) VA "RCTRLSUB".
       01 WS-OFIS-LISTA2            PIC X(012) VA "SUBCPG050B".
       01 WS-OFIS-GAVETA2           PIC X(012) VA "ZERO".
       01 WS-OFIS-PASTA2            PIC X(012) VA "CPG".
       01 WS-OFIS-ASSUNTO2.
          03 FILLER                 PIC X(004) VA "ESQ".
          03 WS-OFIS-ESQUEMA2       PIC 9(005).
          03 FILLER                 PIC X(015) VA "-LSUBCPG050SPGB".
          03 FILLER                 PIC X(019) VA SPACES.
      *
       01 WS-DT-AAAAMMDDV                PIC 9(08).
       01 WS-DT-AAAAMMDDV-R REDEFINES WS-DT-AAAAMMDDV.
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *   03 WS-DT-ANO19V                PIC 9(02).
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *   03 WS-DT-AAMMDDV               PIC 9(06).
          03 WS-DT-AAMMDDV               PIC 9(08).
      *
       01 WS-DATA-ACCEPT.
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *   03 WS-DATA-AC             PIC 9(10) TYPE IS NUMERIC-DATE.
          03 WS-DATA-AC             PIC 9(08).
          03 WS-DATA-AC-R REDEFINES WS-DATA-AC.
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *      05 WS-MES-AC           PIC 9(02).
             05 WS-ANO-AC           PIC 9(04).
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *      05 FILLER              PIC X(01).
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *      05 WS-DIA-AC           PIC 9(02).
             05 WS-MES-AC           PIC 9(02).
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *      05 FILLER              PIC X(01).
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *      05 WS-ANO-AC           PIC 9(04).
             05 WS-DIA-AC           PIC 9(02).
      *
       01 WS-DT-PROC                PIC 9(08).
       01 WS-DT-PROC-R REDEFINES WS-DT-PROC.
          03 WS-ANO-DT-PROC         PIC 9(04).
          03 WS-MES-DT-PROC         PIC 9(02).
          03 WS-DIA-DT-PROC         PIC 9(02).
      *
       01 WS-TABELA-BANCOS-DT.
          03 WS-DT-INI-VALIDADE-BANCO  OCCURS 999 TIMES PIC 9(08).
      *
      *--------------------------------------------------------------
      *
      /
        01 CAB-01.
           03 FILLER                PIC X(18) VALUE "MPS/DATAPREV".
           03 FILLER                PIC X(46) VALUE
                   "I N S S - INSTITUTO NACIONAL DO SEGURO SOCIAL".
           03 FILLER                PIC X(06) VALUE "DATA: ".
           03 CAB01-DATA            PIC 99/99/9999.
      *
        01 CAB-02.
           03 FILLER                PIC X(29) VALUE
           "P/SUB/CPG/050/R274".
           03 FILLER                PIC X(40) VALUE
              "SISTEMA  UNICO  DE  BENEFICIO".
           03 FILLER                PIC X(06) VALUE "HORA: ".
           03 CAB02-HORA            PIC 99I:99I:99.
      *
        01 CAB-031.
           03 FILLER                PIC X(15) VALUE "LSUBCPG050UNI.A".
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
        01 CAB-050.
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
       01  DET-OFIS2.
           03 FILLER                PIC X(37) VALUE
              "A DATA DE PROCESSAMENTO DA CONCESSAO ".
           03 NUM-CONC-OFIS2        PIC X(02).
           03 FILLER                PIC X(05) VALUE " EH: ".
           03 WS-DT-PROC-OFIS-DIA   PIC 99.
           03 FILLER                PIC X(01) VALUE "/".
           03 WS-DT-PROC-OFIS-MES   PIC 99.
           03 FILLER                PIC X(01) VALUE "/".
           03 WS-DT-PROC-OFIS-ANO   PIC 9(04).
      *
           COPY "*B/SDC/LBD/SETTB00233 ON PKBDSGF".
      ***  COPY "*B/SDC/LBD/SETTB00494 ON PKBDSGF".
      ***  COPY "*B/SDC/LBD/SETTB00073 ON PKBDSGF".
           COPY "*B/DTP/PAD/01/CHECKPOINT/WS ON PKBDSGF".
      *
      /
      *=================================================================
       PROCEDURE DIVISION
             USING  W77-ESQUEMA WS-PAR-MOV-CREDITO
                                      WS-PAR-PSTC W77-OFIS-CLASSE.
      *=================================================================
      *
       DECLARATIVES.
      *
       DE-INTERRUPT SECTION.
           USE INTERRUPT PROCEDURE.
      *
       DE-INTERROMPE.
      *
      *****DISPLAY "LIDOS: " WS-REGS-LIDOS " GRAVS: " WS-REGS-GRAVADOS.
      *
       END DECLARATIVES.
      *
      /
      *****************************************************************
       PRINCIPAL SECTION.
      *-----------------*
       INICIO.
      *
      *   MOVE 12345  TO W77-ESQUEMA.
      *   MOVE "MANU" TO WS-PAR-MOV-CREDITO.
      *   MOVE "M06"  TO WS-PAR-PSTC.
      *   MOVE 309    TO W77-OFIS-CLASSE.
           COPY "*B/DTP/PAD/01/CHECKPOINT/PD1 ON PKBDSGF".

           ATTACH DE-INTERRUPT TO ATTRIBUTE EXCEPTIONEVENT OF MYSELF.
      *
           SET DSUBCPG058SPG(FILETYPE) TO 7.

           IF WS-PAR-MOV-CREDITO = "CONC"
              OPEN INPUT DSUBCPG058SPG
              PERFORM 9000-LE-CPG058
                 THRU 9000-FIM
              PERFORM 0500-MONTA-TABELA
                 THRU 0500-FIM
                UNTIL WS-EOF = "SIM"
              CLOSE   DSUBCPG058SPG RELEASE
              MOVE    "NAO"  TO WS-EOF.
      *
           PERFORM  1000-ABERTURA
              THRU  1000-FIM.
      *
           PERFORM 9000-LE-CPG058
              THRU 9000-FIM.

           PERFORM 10000-GRAVA-HEADER
               THRU 10000-FIM.
      *
           PERFORM 9000-LE-CPG058
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
           IF WS-TESTE-CANCELAR-PROGRAMA = 1
              MOVE "ATENCAO !!! MEIOS DE PAGTO INVALIDOS "
                TO  WS-PARAM1-ZDISPLAY
              CALL "ZDISPLAY IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                              WS-INTEIRO-ZDISPLAY
              SET MYSELF (STATUS) TO -1.
      *
           STOP  RUN.
      *
      /
       0500-MONTA-TABELA.
      *-----------------*
      *
          IF SPGENT-ID-BANCO = 0
             ADD 1 TO WS-REGS-DESP-BANCO-0
             GO 0500-LE-PROXIMO.
      *
          IF WS-DT-INI-VALIDADE-BANCO ( SPGENT-ID-BANCO ) = 0
             MOVE SPGENT-DT-INI-VALIDADE  TO
             WS-DT-INI-VALIDADE-BANCO ( SPGENT-ID-BANCO )
          ELSE
             IF WS-DT-INI-VALIDADE-BANCO ( SPGENT-ID-BANCO ) >
                SPGENT-DT-INI-VALIDADE
                MOVE SPGENT-DT-INI-VALIDADE  TO
                WS-DT-INI-VALIDADE-BANCO ( SPGENT-ID-BANCO ).
      *
       0500-LE-PROXIMO.
      *---------------*
      *
           PERFORM 9000-LE-CPG058
              THRU 9000-FIM.
      *
       0500-FIM.  EXIT.
      *--------*
      *
      /
       1000-ABERTURA.
      *-------------*
      *
           SET DSUBCPG058SPG(FILETYPE) TO 7.
           OPEN INPUT  DSUBCPG058SPG.
           CHANGE ATTRIBUTE SECURITYTYPE OF DSUBCPG051SPG TO PUBLIC.
            OPEN OUTPUT DSUBCPG051SPG.
      *
      *      CALL "LAYOUT IN DTP/LBD/TAB" USING   FONTE-LAYOUT-TB00233,
      *                                           CODE-LAYOUT-TB00233
      *                                  GIVING       RETORNO-TB00233
           IF RETORNO-TB00233 = 2
              MOVE "ERRO ABERTURA TB00233" TO WS-PARAM1-ZDISPLAY
              CALL "ZDISPLAY IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                              WS-INTEIRO-ZDISPLAY
              SET MYSELF(STATUS) TO -1.

      *
           CALL "ZDATA4 IN DTP/L1" GIVING W77-DATA.
           MOVE W77-DATA    TO  WS-DATA-INICIO-PROC.
      *
           CALL "ZHORA IN DTP/L1" GIVING W77-HORA.
      *
           MOVE W77-HORA  TO  WS-HORA-INICIO-PROC.
      *
           ACCEPT WS-DATA-AC FROM DATE YYYYMMDD.
           MOVE   WS-ANO-AC  TO   WS-ANO-DT-PROC.
           MOVE   WS-MES-AC  TO   WS-MES-DT-PROC.
           MOVE   WS-DIA-AC  TO   WS-DIA-DT-PROC.

       1000-FIM. EXIT.
      *--------*
      /
       2000-LOOP.
      *----------*

           IF  SPGENT-ID-REG-H NOT EQUAL 2
               PERFORM 9000-LE-CPG058
                  THRU 9000-FIM.
           MOVE SPGENT-ID-REG-H TO WS-SPGENT-ID-REG-ANT.
           MOVE SPGENT-IN-CRED-PIONEIRA TO WS-IN-PION-ANT.
           PERFORM 2200-LOOP-PIONEIRA
               THRU 2200-FIM
             UNTIL WS-EOF EQUAL "SIM"
                  OR SPGENT-ID-REG-H
                  NOT EQUAL WS-SPGENT-ID-REG-ANT
                  OR SPGENT-IN-CRED-PIONEIRA
                  NOT EQUAL WS-IN-PION-ANT.

      *
       2000-FIM. EXIT.
      *--------*

      /

       2200-LOOP-PIONEIRA.
      *-------------------*
      *
           MOVE SPGENT-ID-BANCO TO WS-BANCO-ANT.

           PERFORM 2250-LOOP-ID-BANCO
              THRU 2250-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT.

      *
       2200-FIM. EXIT.
      *---------------*
      /
       2250-LOOP-ID-BANCO.
      *-------------------*
      *
           MOVE SPGENT-CS-MEIO-PAGTO TO WS-CS-MEIO-PAGTO-ANT.

           PERFORM 2300-LOOP-MEIO-PAGTO
              THRU 2300-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SPGENT-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT.
      *
       2250-FIM. EXIT.
      *--------*
      /
       2300-LOOP-MEIO-PAGTO.
      *--------------------*
      *
           MOVE SPGENT-DT-INI-VALIDADE TO WS-DT-INI-VALIDADE-ANT.

           PERFORM 2420-LOOP-DT-INI-VALIDADE
              THRU 2420-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SPGENT-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SPGENT-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT.
      *
       2300-FIM. EXIT.
      *---------------*
      /
       2420-LOOP-DT-INI-VALIDADE.
      *-------------------------*
           MOVE SPGENT-CS-ORIG-ORC TO  WS-CS-ORIG-ORCAM-ANT.

           PERFORM 2430-LOOP-CS-ORIG-ORCAM
              THRU 2430-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SPGENT-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SPGENT-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SPGENT-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT.
      *
       2420-FIM. EXIT.
      *---------------*
      /
       2430-LOOP-CS-ORIG-ORCAM.
      *-------------------------*

           MOVE SPGENT-IN-CRED-EPU TO  WS-CS-REDE-ECT-ANT.

           PERFORM 2440-LOOP-CS-REDE-ECT
              THRU 2440-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SPGENT-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SPGENT-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SPGENT-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SPGENT-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT.
      *
       2430-FIM. EXIT.
      *---------------*
      /
       2440-LOOP-CS-REDE-ECT.
      *-------------------------*

           MOVE SPGENT-CS-URBANO-RURAL TO  WS-CS-URBANO-RURAL-ANT.

           PERFORM 2450-LOOP-CS-URBANO-RURAL
              THRU 2450-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SPGENT-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SPGENT-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SPGENT-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SPGENT-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SPGENT-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT.

      *
       2440-FIM. EXIT.
      *---------------*
      /
       2450-LOOP-CS-URBANO-RURAL.
      *-------------------------*

           MOVE SPGENT-CS-ESPECIE TO  WS-CS-ESPECIE-ANT.

           PERFORM 2460-LOOP-CS-ESPECIE
              THRU 2460-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SPGENT-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SPGENT-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SPGENT-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SPGENT-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SPGENT-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SPGENT-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT.

      *
       2450-FIM. EXIT.
      *--------*
      /
      *
       2460-LOOP-CS-ESPECIE.
      *-------------------------*

           MOVE SPGENT-ID-PAIS TO  WS-ID-PAIS-ANT.
      *
           PERFORM 2550-LOOP-ID-PAIS
              THRU 2550-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SPGENT-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SPGENT-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SPGENT-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SPGENT-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SPGENT-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SPGENT-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT
                   OR SPGENT-ID-PAIS
                   NOT EQUAL WS-ID-PAIS-ANT.

       2460-FIM. EXIT.
      *---------------*
      /
       2550-LOOP-ID-PAIS.
      *-------------------------*

           MOVE SPGENT-COD-CALAMIDADE TO  WS-CALAMIDADE-ANT.
      *
           PERFORM 2600-LOOP-CALAMIDADE
              THRU 2600-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SPGENT-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SPGENT-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SPGENT-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SPGENT-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SPGENT-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SPGENT-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT
                   OR SPGENT-ID-PAIS
                   NOT EQUAL WS-ID-PAIS-ANT
                   OR SPGENT-COD-CALAMIDADE
                   NOT EQUAL WS-CALAMIDADE-ANT.

       2550-FIM. EXIT.
      *---------------*
      /
      *
       2600-LOOP-CALAMIDADE.
      *-------------------------*

           MOVE SPGENT-IN-ADIANTE-13-SAL TO  WS-ADIANT-13SAL-ANT.
      *
           PERFORM 2610-LOOP-13-SAL
              THRU 2610-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SPGENT-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SPGENT-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SPGENT-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SPGENT-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SPGENT-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SPGENT-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT
                   OR SPGENT-ID-PAIS
                   NOT EQUAL WS-ID-PAIS-ANT
                   OR SPGENT-COD-CALAMIDADE
                   NOT EQUAL WS-CALAMIDADE-ANT
                   OR SPGENT-IN-ADIANTE-13-SAL
                   NOT EQUAL WS-ADIANT-13SAL-ANT.

       2600-FIM. EXIT.
      *---------------*

       2610-LOOP-13-SAL.
      *----------------------*
      *
           MOVE SPGENT-ENT-CLASS TO WS-ENT-CLASS-ANT.

           PERFORM 2620-LOOP-ENTIDADES
              THRU 2620-FIM-ENT
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SPGENT-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SPGENT-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SPGENT-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SPGENT-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SPGENT-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SPGENT-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT
                   OR SPGENT-ID-PAIS
                   NOT EQUAL WS-ID-PAIS-ANT
                   OR SPGENT-COD-CALAMIDADE
                   NOT EQUAL WS-CALAMIDADE-ANT
                   OR SPGENT-IN-ADIANTE-13-SAL
                   NOT EQUAL WS-ADIANT-13SAL-ANT
                   OR SPGENT-ENT-CLASS
                   NOT EQUAL WS-ENT-CLASS-ANT.
      *

      *
       2610-FIM. EXIT.
      *--------*

       2620-LOOP-ENTIDADES.
      *----------------------*

           MOVE 0 TO WS-DT-PROVIS.

           MOVE ZEROS            TO WS-QTDE-AUX WS-QTDE-IR-AUX
                                  WS-VALOR-BRUTO-AUX   WS-VALOR-AUX
                                  WS-QTDE-ENT-AUX      WS-VL-ENT-AUX.

           PERFORM 6000-ZERA-RUBRICAS THRU 6000-FIM
                   VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 95.
      *
           PERFORM 2620-LOOP-ESP-LOAS
              THRU 2620-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SPGENT-ID-REG-H
                   NOT EQUAL WS-SPGENT-ID-REG-ANT
                   OR SPGENT-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SPGENT-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SPGENT-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SPGENT-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SPGENT-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SPGENT-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SPGENT-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SPGENT-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT
                   OR SPGENT-ID-PAIS
                   NOT EQUAL WS-ID-PAIS-ANT
                   OR SPGENT-COD-CALAMIDADE
                   NOT EQUAL WS-CALAMIDADE-ANT
                   OR SPGENT-IN-ADIANTE-13-SAL
                   NOT EQUAL WS-ADIANT-13SAL-ANT
                   OR SPGENT-ENT-CLASS
                   NOT EQUAL WS-ENT-CLASS-ANT.
      *
           PERFORM 2630-GRAVAR-SAIDAS
              THRU 2630-FIM.
      *
       2620-FIM-ENT.
           EXIT.
      *--------------*


       2620-LOOP-ESP-LOAS.
      *------------------*
      *
           ADD  1 TO WS-REGS-LIDOS
                     WS-REGS-CHECKPOINT.
      *

           IF WS-REGS-CHECKPOINT > 1000000
              DETACH DE-INTERRUPT
              PERFORM 5000-CHECKPOINT
              ATTACH DE-INTERRUPT TO ATTRIBUTE EXCEPTIONEVENT OF MYSELF
              MOVE 0 TO WS-REGS-CHECKPOINT.

           ADD  SPGENT-QUANT-CREDITOS      TO WS-QTDE-AUX
                                           WS-QTDE-TOTAL.
           ADD  SPGENT-VALOR-LIQUIDO TO WS-VALOR-AUX
                                           WS-VALOR-TOTAL.

           ADD  SPGENT-VALOR-BRUTO   TO  WS-VALOR-BRUTO-AUX.

           ADD SPGENT-QTD-ENT-CLASS TO WS-QTDE-ENT-AUX.

           PERFORM 7000-SOMA-RUBRICAS THRU 7000-FIM
                    VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 95.

      *
           PERFORM 9000-LE-CPG058
              THRU 9000-FIM.
      *
       2620-FIM.  EXIT.
      *--------*
      *
      /
       2630-GRAVAR-SAIDAS.
      *------------------*
      *
      *   GRAVANDO O DSUBCPG051 => DARAH ORIGEM AO COFRE2
      *
           IF WS-BANCO-ANT = 998 OR 996
              GO 2630-FIM.
      *
      *     MOVE  ALL ZEROS                TO DSUBCPG051-REG-SPG51.


      **Moves DETALHE.

           MOVE  WS-SPGENT-ID-REG-ANT      TO SPG51-ID-REG.
           MOVE  WS-BANCO-ANT             TO SPG51-ID-BANCO.
           MOVE  WS-CS-MEIO-PAGTO-ANT     TO SPG51-CS-MEIO-PAGTO.
           MOVE  WS-IN-PION-ANT           TO SPG51-IN-CRED-PIONEIRA.
           MOVE  WS-DT-INI-VALIDADE-ANT   TO WS-DT-AAMMDDV.
           MOVE  WS-DT-AAAAMMDDV          TO SPG51-DT-INI-VALIDADE.

           IF    WS-DT-PROVIS NOT = 0
                 MOVE  WS-DT-PROVIS       TO WS-DT-AAMMDDV
                 MOVE  WS-DT-AAAAMMDDV    TO CP51-N-DT-PROVISAO.

           MOVE  WS-CS-ORIG-ORCAM-ANT     TO SPG51-CS-ORIG-ORC.
           MOVE  WS-CS-REDE-ECT-ANT       TO SPG51-IN-CRED-EPU.
           MOVE  WS-CS-URBANO-RURAL-ANT   TO SPG51-CS-URBANO-RURAL.
           MOVE  WS-CS-ESPECIE-ANT        TO SPG51-CS-ESPECIE.
           MOVE  WS-ID-PAIS-ANT           TO SPG51-ID-PAIS.
           MOVE  WS-CALAMIDADE-ANT        TO SPG51-COD-CALAMIDADE.
           MOVE  WS-ADIANT-13SAL-ANT      TO SPG51-IN-ADIANTE-13-SAL.
           MOVE  WS-ENT-CLASS-ANT         TO SPG51-ENT-CLASS.
           MOVE  WS-QTDE-AUX              TO SPG51-QUANT-CREDITOS.
           MOVE  WS-VALOR-AUX             TO SPG51-VALOR-LIQUIDO.
           MOVE  WS-VALOR-BRUTO-AUX       TO SPG51-VALOR-BRUTO.
           MOVE  WS-QTDE-ENT-AUX          TO SPG51-QTD-ENT-CLASS.

           PERFORM 8000-MOVE-RUBRICAS THRU 8000-FIM
                   VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 95.

           WRITE DSUBCPG051-REG-SPG51.


           ADD   1 TO WS-REGS-GRAVADOS-051.
      *
       2630-FIM. EXIT.
      *--------*
      *
      /
       3000-EMITIR-RELATORIO.
      *---------------------*
      *
           OPEN OUTPUT LSUBCPG050A.
           OPEN OUTPUT WOFIS.
      *
           CALL  "ZDATA4  IN DTP/L1" GIVING W77-DATA.
      *
           CALL  "ZHORA  IN DTP/L1" GIVING W77-HORA.
      *
           MOVE W77-DATA      TO  WS-DATA-FIM-PROC.
           MOVE W77-HORA      TO  WS-HORA-FIM-PROC.
      *
      ********** UNISYS Y2K - Linha alterada em 27/04/1999 15:47:14
      *     CALL "P/SUB/LIB/601" USING  WS-DATA-INICIO-PROC
            CALL "P/SUB/LIB/2K/601" USING  WS-DATA-INICIO-PROC
                                        WS-HORA-INICIO-PROC
                                        WS-DATA-FIM-PROC
                                        WS-HORA-FIM-PROC
                                        WS-ESTIMATIVA.
      *
           MOVE   SPACES                 TO  LINHA050A.
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
           WRITE  LINHA050A   FROM  CAB-01   AFTER PAGE.
           WRITE  LINHA050A   FROM  CAB-02   AFTER  2.
           WRITE  LINHA050A   FROM  CAB-031  AFTER  2.
           WRITE  LINHA050A   FROM  CAB-031A AFTER  1.
           WRITE  LINHA050A   FROM  CAB-041  AFTER  2.
           WRITE  LINHA050A   FROM  CAB-041A AFTER  1.
           WRITE  LINHA050A   FROM  CAB-050  AFTER  2.
           WRITE  LINHA050A   FROM  CAB-061  AFTER  1.
           WRITE  LINHA050A   FROM  CAB-071  AFTER  1.
           WRITE  LINHA050A   FROM  CAB-031A AFTER  1.
      *
           WRITE  REG-WOFIS   FROM  CAB-01.
           WRITE  REG-WOFIS   FROM  CAB-02.
           WRITE  REG-WOFIS   FROM  CAB-031.
           WRITE  REG-WOFIS   FROM  CAB-031A.
           WRITE  REG-WOFIS   FROM  CAB-041.
           WRITE  REG-WOFIS   FROM  CAB-041A.
           WRITE  REG-WOFIS   FROM  CAB-050.
           WRITE  REG-WOFIS   FROM  CAB-061.
           WRITE  REG-WOFIS   FROM  CAB-071.
           WRITE  REG-WOFIS   FROM  CAB-031A.
      *
      *    LINHAS DE DETALHE
      *
           MOVE   SPACES TO VL-DET1-R.
           MOVE  "REGS LIDOS DSUBCPG058                " TO TEXTO-DET1.
           MOVE  WS-REGS-LIDOS                           TO QT-DET1.
           WRITE LINHA050A FROM DET1                        AFTER 2.
           WRITE REG-WOFIS FROM DET1.
      *
           MOVE  "REGS GRAVADOS NO DSUBCPG051          " TO TEXTO-DET1.
           MOVE   WS-REGS-GRAVADOS-051                   TO QT-DET1.
           WRITE  LINHA050A FROM DET1                       AFTER 1.
           WRITE REG-WOFIS FROM DET1.
      *
           MOVE  "TOTAL DE BENEFICIOS                  " TO TEXTO-DET1.
           MOVE  WS-QTDE-TOTAL                           TO QT-DET1.
           WRITE LINHA050A FROM DET1                        AFTER 2.
           WRITE REG-WOFIS FROM DET1.
      *
           MOVE   SPACES TO QT-DET1-R.
           MOVE  "TOTAL DE VALOR LIQUIDO               " TO TEXTO-DET1.
           MOVE   WS-VALOR-TOTAL                         TO VL-DET1.
           WRITE  LINHA050A FROM DET1                       AFTER 1.
           WRITE REG-WOFIS FROM DET1.
      *
           MOVE    SPACES TO VL-DET1-R.
           MOVE   "PESQS NA TB0023 COM ERRO            " TO TEXTO-DET1.
           MOVE   WS-OPS-ERRO-TB0023                     TO QT-DET1.
           WRITE  LINHA050A FROM DET1                       AFTER 2.
           WRITE REG-WOFIS FROM DET1.
      *
           MOVE   "PESQS NA TB0049 COM ERRO            " TO TEXTO-DET1.
           MOVE   WS-MRS-ERRO-TB0049                     TO QT-DET1.
           WRITE  LINHA050A FROM DET1                       AFTER 2.
           WRITE REG-WOFIS FROM DET1.
      *
           CLOSE LSUBCPG050A.
           CLOSE WOFIS SAVE.
      *
           IF WS-PAR-MOV-CREDITO NOT = "CONC"
              GO 3000-FIM.
      *
      *    ENVIA OFIS COM A DATA DE PROCESSAMENTO DA CONCESSAO.
      *
           OPEN OUTPUT WOFIS2.
           WRITE  REG-WOFIS2  FROM  CAB-01.
           WRITE  REG-WOFIS2  FROM  CAB-02.
           WRITE  REG-WOFIS2  FROM  CAB-031.
           WRITE  REG-WOFIS2  FROM  CAB-031A.
           WRITE  REG-WOFIS2  FROM  CAB-041.
           WRITE  REG-WOFIS2  FROM  CAB-041A.
           WRITE  REG-WOFIS2  FROM  CAB-050.
           WRITE  REG-WOFIS2  FROM  CAB-061.
           WRITE  REG-WOFIS2  FROM  CAB-071.
           WRITE  REG-WOFIS2  FROM  CAB-031A.
      *
           MOVE   WS-PAR-PSTC    TO NUM-CONC-OFIS2.
           MOVE   WS-ANO-DT-PROC TO WS-DT-PROC-OFIS-ANO.
           MOVE   WS-MES-DT-PROC TO WS-DT-PROC-OFIS-MES.
           MOVE   WS-DIA-DT-PROC TO WS-DT-PROC-OFIS-DIA.
           WRITE  REG-WOFIS2  FROM  DET-OFIS2.
           CLOSE  WOFIS2 SAVE.
       3000-FIM. EXIT.
      *
       4000-FINALIZA.
          CLOSE DSUBCPG058SPG.
      *
          CLOSE DSUBCPG051SPG SAVE.
       4000-FIM. EXIT.
      *
       9000-LE-CPG058.

           READ DSUBCPG058SPG
                AT END
                    MOVE "SIM" TO WS-EOF.
      *
           IF SPGENT-ID-REG-H OF DSUBCPG058SPG EQUAL 1
              ADD 1 TO WS-REGS-LIDOS
              GO TO 9000-FIM.

           IF SPGENT-ID-REG-T OF DSUBCPG058SPG GREATER 2
              MOVE "SIM" TO WS-EOF
              ADD 1 TO WS-REGS-LIDOS
              GO TO 9000-FIM.

           IF WS-EOF NOT = "SIM"
              IF ( SPGENT-CS-MEIO-PAGTO = 3  ) AND
                 ( SPGENT-ID-BANCO NOT = 998 ) AND
                 ( SPGENT-ID-BANCO NOT = 996 ) AND
                 ( WS-PAR-MOV-CREDITO NOT = "EMPR" ) AND
                 ( WS-PAR-MOV-CREDITO NOT = "GLOS" )
                 MOVE SPGENT-ID-BANCO TO WS-CP50-ID-BANCO
                 MOVE CP50-N-ID-ORGAO-PAGADOR
                                           TO WS-CP50-ID-ORGAO-PAGADOR
                 MOVE SPGENT-DT-INI-VALIDADE
                                          TO WS-CP50-DT-INI-VALIDADE
                 MOVE SPACES TO                    WS-MSG
                 STRING "NB COM MEIO PAGTO INV.REG LIDOS: "
                        "ID-BANCO: " WS-CP50-ID-BANCO
                        " ORGAO-PAGADOR: "WS-CP50-ID-ORGAO-PAGADOR
                        " DT-INI-VALIDADE: "WS-CP50-DT-INI-VALIDADE
                 DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                               WS-STATUS-BACKUP
      *                                             WS-INTEIRO-ZDISPLAY
                 MOVE SPACES TO                    WS-MSG
      *           STRING WS-CP50-ID-BANCO "/"
      *                  WS-CP50-ID-ORGAO-PAGADOR "/"
      *                  WS-CP50-DT-INI-VALIDADE
      *           DELIMITED BY SIZE INTO WS-MSG
      *          CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
      *                                         WS-STATUS-BACKUP
      *                                            WS-INTEIRO-ZDISPLAY
                 MOVE 0 TO WS-TESTE-CANCELAR-PROGRAMA
                 GO 9000-LE-CPG058.

       9000-FIM. EXIT.
      *
           COPY "*B/DTP/PAD/01/CHECKPOINT/PD2 ON PKBDSGF".
      *
       4000-DISPLAY.
           MOVE WS-MSG-LIDOS TO W77-MSG
           CALL "ZDISPLAY IN DTP/L1" USING W77-MSG,
                                           W77-STATUS-BACKUP-CKP.
       4000-FIM-DISPLAY. EXIT.
      *
      /
       2100-PESQ-DADOS-OP.
      *------------------*

           MOVE WS-ID-OP-ANT TO TB00233-CHAVE.
           CALL "SETTB00233 IN DTP/LBD/TAB" USING REGISTRO-TB00233,
                                               CODE-LAYOUT-TB00233
                                            GIVING RETORNO-TB00233.
      *
           IF RETORNO-TB00233 = 0
              MOVE CS-MUNI-SINPAS OF REGISTRO-TB00233-R
                                  TO WS-CS-MUNIC-SINPAS-TAB
           ELSE
              MOVE 01000          TO WS-CS-MUNIC-SINPAS-TAB
              MOVE "OP NAO ENCONTRADO NA TB0023: " TO WS-PARAM1-ZDISPLAY
              MOVE  WS-ID-OP-ANT                   TO WS-PARAM2-ZDISPLAY
              CALL "ZDISPLAY2 IN DTP/L1" USING WS-PARAM1-ZDISPLAY,
                                               WS-PARAM2-ZDISPLAY,
                                               WS-INTEIRO-ZDISPLAY
              ADD 1 TO WS-OPS-ERRO-TB0023.

       2100-FIM.  EXIT.
      *--------*
      *
      /
       6000-ZERA-RUBRICAS.
      *--------------------*

           MOVE ZEROS TO WS-TAB-VL-RUBRICA (WS-IND).

       6000-FIM.
           EXIT.
      *---------------*

       7000-SOMA-RUBRICAS.
      *--------------------*

           ADD SPGENT-VL-RUBRICA(WS-IND) TO WS-TAB-VL-RUBRICA (WS-IND).

       7000-FIM.
           EXIT.
      *--------------*

       8000-MOVE-RUBRICAS.
      *--------------------*

           MOVE WS-TAB-VL-RUBRICA (WS-IND) TO SPG51-VL-RUBRICA(WS-IND).

       8000-FIM.
           EXIT.
      *--------------*

       10000-GRAVA-HEADER.
      *--------------------*

            MOVE ALL "00" TO DSUBCPG051-REG-SPG51.
      **Moves HEADER.

            MOVE  SPGENT-ID-REG-H           TO SPG51-ID-REG-H
            MOVE  SPGENT-DT-PROC-H          TO SPG51-DT-PROC-H
            MOVE  SPGENT-ID-CALAMIDADE-H    TO SPG51-ID-CALAMIDADE-H
            MOVE  SPGENT-NUM-PORTARIA-H     TO SPG51-NUM-PORTARIA-H
            MOVE  SPGENT-DT-PORTARIA-H      TO SPG51-DT-PORTARIA-H
            MOVE  SPGENT-DT-INI-VALIDADE-H  TO SPG51-DT-INI-VALIDADE-H
            MOVE  SPGENT-DT-FIM-VALIDADE-H  TO SPG51-DT-FIM-VALIDADE-H
            MOVE  SPGENT-NU-SEQ-ARQ-H       TO SPG51-NU-SEQ-ARQ-H
            MOVE  SPGENT-ID-ULT-ARQ-H       TO SPG51-ID-ULT-ARQ-H
            MOVE  SPGENT-LISTA-RUBR-H       TO SPG51-LISTA-RUBR-H
      *     MOVE "10410510811213615420120220320420520620720820921021421
      *-"16217218219220221222223224225226227228229230231232233234301905
      *-"9910911912916917921925926927928929933" TO SPG51-LISTA-RUBR-H.

      *     MOVE  SPGENT-ORIG-CRED-H        TO SPG51-ORIGEM-CREDITO.

            IF WS-PAR-MOV-CREDITO = "CONC"
               MOVE 1 TO SPG51-ORIGEM-CREDITO
            ELSE
               IF WS-PAR-MOV-CREDITO = "MANU" OR "EMPR" OR "GLOS"
                  MOVE 2 TO SPG51-ORIGEM-CREDITO
               ELSE
                  IF WS-PAR-MOV-CREDITO = "ALTE"
                     MOVE 3 TO SPG51-ORIGEM-CREDITO
                  ELSE
                     IF WS-PAR-MOV-CREDITO = "ESPE"
                        MOVE 4 TO SPG51-ORIGEM-CREDITO
                     ELSE
                        IF WS-PAR-MOV-CREDITO = "CALA"
                           MOVE 5 TO SPG51-ORIGEM-CREDITO
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

           WRITE DSUBCPG051-REG-SPG51-HEADER.
           ADD   1 TO WS-REGS-GRAVADOS-051.

      *
       10000-FIM.
           EXIT.
      *---------------*
      *
       11000-GRAVA-TRAILER.
      *--------------------*

           MOVE ALL "00" TO DSUBCPG051-REG-SPG51-TRAILER.
           MOVE 9                       TO  SPG51-ID-REG-T.
           MOVE SPGENT-TOT-VL-LIQ-T      TO  SPG51-TOT-VL-LIQ-T.
           ADD   1 TO WS-REGS-GRAVADOS-051.
           MOVE WS-REGS-GRAVADOS-051    TO  SPG51-QTD-REG-T
           WRITE DSUBCPG051-REG-SPG51-TRAILER.

       11000-FIM.
           EXIT.
      *---------------*

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%   REFERENCIAS CRUZADAS DO FONTE     %%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      * LIBRARY - B/DTP/PAD/01/CHECKPOINT/PD2 CICLO = 0001VERSAO =   11

      * LIBRARY - B/DTP/PAD/01/CHECKPOINT/PD1 CICLO = 0001VERSAO =   01

      * LIBRARY - B/DTP/PAD/01/CHECKPOINT/WS  CICLO = 0001VERSAO =   06

      * LIBRARY - B/SDC/LBD/SETTB00233        CICLO = 0001VERSAO =   10

      * LIBRARY - B/SUB/CPG/051/SPG/R274      CICLO = 0001VERSAO =   00

      * LIBRARY - B/SUB/CPG/051/SUL           CICLO = 0001VERSAO =   00

      * LIBRARY - B/SUB/CPG/058/SPG/R274      CICLO = 0001VERSAO =   01

      * LIBRARY - B/SUB/CPG/050/SUL           CICLO = 0001VERSAO =   00

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
