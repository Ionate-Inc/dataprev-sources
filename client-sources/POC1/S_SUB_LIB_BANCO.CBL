      *$$SET LIST
      *$$SET ERRORLIMIT = 9
      *$$SET FEDLEVEL   = 5
      *$SET SHARING = PRIVATE
      *$SET TADS
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCO.
       AUTHOR. RENATO VIANA www tutorialdocumenta com br.
       DATE-WRITTEN. 25/07/2000.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANCO ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD  BANCO.


       01  REG-BANCO               PIC X(132).

       WORKING-STORAGE SECTION.
      *

       77  WS-HOJEB                    PIC  9(8) BINARY.
       77  WS-TIMEB                    PIC  9(6) BINARY.
       77  WS-ERRO                     PIC 9.
       77  WS-INDEX                    PIC 9.
       77  WS-HOJE                     PIC 99/99/9999.
       77  WS-TIME                     PIC 99I:99I:99.

       01  WS-BANCOPIC9COMP            PIC 9(03) COMP.
       01  WS-PERGUNTAPICX25           PIC X(12).
       01  WS-RESPOSTAPICX25           PIC X(25).
       01  WS-REDEFINE1 REDEFINES WS-RESPOSTAPICX25.
           03 WS-BOOLEANPIC9X          PIC 9.
           03 FILLER                   PIC X(24).
       01  WS-REDEFINE2 REDEFINES WS-RESPOSTAPICX25.
           03 WS-LABELFITA             PIC X(05).
           03 WS-LABELFITAMATRIZ       PIC X(03).
           03 WS-LABELFITAPONTO        PIC X.
           03 FILLER                   PIC X(16).
       01  WS-REDEFINE3 REDEFINES WS-RESPOSTAPICX25.
           03 WS-BPI                   PIC 9(05).
           03 FILLER                   PIC X(20).

       01  WS-TABELA-BANCOS.
           03 WS-001-BANCO     PIC 9(03) VA 001.
           03 WS-001-NOME      PIC X(25) VA "BANCO DO BRASIL          ".
           03 WS-001-FAIXARQUIVO  PIC 9(01) VA 1.
           03 WS-001-BPI          PIC 9(05) VA 38000.
           03 WS-001-LAYOUT       PIC 9(01) VA 1.
           03 WS-001-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-001-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-001-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-001-TP-CONCESSAO PIC 9(01) VA 1.
           03 WS-001-TP-MACICA    PIC 9(01) VA 1.
           03 WS-001-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-001-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-001-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-001-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-003-BANCO     PIC 9(03) VA 003.
           03 WS-003-NOME      PIC X(25) VA "AMAZONIA                 ".
           03 WS-003-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-003-BPI          PIC 9(05) VA 38000.
           03 WS-003-LAYOUT       PIC 9(01) VA 1.
           03 WS-003-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-003-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-003-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-003-TP-CONCESSAO PIC 9(01) VA 3.
           03 WS-003-TP-MACICA    PIC 9(01) VA 3.
           03 WS-003-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-003-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-003-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-003-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-004-BANCO     PIC 9(03) VA 004.
           03 WS-004-NOME      PIC X(25) VA "NORDESTE DO BRASIL ".
           03 WS-004-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-004-BPI          PIC 9(05) VA 38000.
           03 WS-004-LAYOUT       PIC 9(01) VA 1.
           03 WS-004-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-004-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-004-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-004-TP-CONCESSAO PIC 9(01) VA 3.
           03 WS-004-TP-MACICA    PIC 9(01) VA 3.
           03 WS-004-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-004-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-004-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-004-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-008-BANCO     PIC 9(03) VA 008.
           03 WS-008-NOME      PIC X(25) VA "MERIDIONAL DO BRASIL ".
           03 WS-008-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-008-BPI          PIC 9(05) VA 38000.
           03 WS-008-LAYOUT       PIC 9(01) VA 1.
           03 WS-008-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-008-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-008-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-008-TP-CONCESSAO PIC 9(01) VA 4.
           03 WS-008-TP-MACICA    PIC 9(01) VA 4.
           03 WS-008-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-008-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-008-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-008-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-020-BANCO     PIC 9(03) VA 020.
           03 WS-020-NOME      PIC X(25) VA "PRODUBAN                 ".
           03 WS-020-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-020-BPI          PIC 9(05) VA 01600.
           03 WS-020-LAYOUT       PIC 9(01) VA 0.
           03 WS-020-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-020-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-020-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-020-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-020-TP-MACICA    PIC 9(01) VA 0.
           03 WS-020-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-020-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-020-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-020-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-021-BANCO     PIC 9(03) VA 021.
           03 WS-021-NOME      PIC X(25) VA "BANESTES                 ".
           03 WS-021-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-021-BPI          PIC 9(05) VA 06250.
           03 WS-021-LAYOUT       PIC 9(01) VA 1.
           03 WS-021-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-021-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-021-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-021-TP-CONCESSAO PIC 9(01) VA 2.
           03 WS-021-TP-MACICA    PIC 9(01) VA 2.
           03 WS-021-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-021-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-021-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-021-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-022-BANCO     PIC 9(03) VA 022.
           03 WS-022-NOME      PIC X(25) VA "CREDIREAL                ".
           03 WS-022-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-022-BPI          PIC 9(05) VA 06250.
           03 WS-022-LAYOUT       PIC 9(01) VA 0.
           03 WS-022-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-022-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-022-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-022-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-022-TP-MACICA    PIC 9(01) VA 0.
           03 WS-022-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-022-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-022-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-022-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-024-BANCO     PIC 9(03) VA 024.
           03 WS-024-NOME      PIC X(25) VA "BANDEPE                  ".
           03 WS-024-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-024-BPI          PIC 9(05) VA 38000.
           03 WS-024-LAYOUT       PIC 9(01) VA 1.
           03 WS-024-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-024-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-024-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-024-TP-CONCESSAO PIC 9(01) VA 4.
           03 WS-024-TP-MACICA    PIC 9(01) VA 4.
           03 WS-024-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-024-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-024-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-024-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-026-BANCO     PIC 9(03) VA 026.
           03 WS-026-NOME      PIC X(25) VA "BANACRE                  ".
           03 WS-026-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-026-BPI          PIC 9(05) VA 06250.
           03 WS-026-LAYOUT       PIC 9(01) VA 0.
           03 WS-026-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-026-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-026-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-026-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-026-TP-MACICA    PIC 9(01) VA 0.
           03 WS-026-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-026-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-026-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-026-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-027-BANCO     PIC 9(03) VA 027.
           03 WS-027-NOME      PIC X(25) VA "B E S C ".
           03 WS-027-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-027-BPI          PIC 9(05) VA 38000.
           03 WS-027-LAYOUT       PIC 9(01) VA 1.
           03 WS-027-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-027-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-027-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-027-TP-CONCESSAO PIC 9(01) VA 2.
           03 WS-027-TP-MACICA    PIC 9(01) VA 2.
           03 WS-027-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-027-TP-POSSUI    PIC 9(01) VA 1.
      *    03 WS-027-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-027-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-027-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-028-BANCO     PIC 9(03) VA 028.
           03 WS-028-NOME      PIC X(25) VA "ESTADO DA BAHIA ".
           03 WS-028-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-028-BPI          PIC 9(05) VA 06250.
           03 WS-028-LAYOUT       PIC 9(01) VA 1.
           03 WS-028-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-028-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-028-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-028-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-028-TP-MACICA    PIC 9(01) VA 0.
           03 WS-028-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-028-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-028-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-028-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-029-BANCO     PIC 9(03) VA 029.
           03 WS-029-NOME      PIC X(25) VA "B A N E R J ".
           03 WS-029-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-029-BPI          PIC 9(05) VA 38000.
           03 WS-029-LAYOUT       PIC 9(01) VA 1.
           03 WS-029-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-029-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-029-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-029-TP-CONCESSAO PIC 9(01) VA 4.
           03 WS-029-TP-MACICA    PIC 9(01) VA 4.
           03 WS-029-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-029-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-029-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-029-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-031-BANCO     PIC 9(03) VA 031.
           03 WS-031-NOME      PIC X(25) VA "B E G ".
           03 WS-031-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-031-BPI          PIC 9(05) VA 38000.
           03 WS-031-LAYOUT       PIC 9(01) VA 1.
           03 WS-031-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-031-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-031-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-031-TP-CONCESSAO PIC 9(01) VA 3.
           03 WS-031-TP-MACICA    PIC 9(01) VA 3.
           03 WS-031-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-031-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-031-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-031-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-032-BANCO     PIC 9(03) VA 032.
           03 WS-032-NOME      PIC X(25) VA "ESTADO DE MATO GROSSO ".
           03 WS-032-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-032-BPI          PIC 9(05) VA 06250.
           03 WS-032-LAYOUT       PIC 9(01) VA 0.
           03 WS-032-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-032-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-032-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-032-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-032-TP-MACICA    PIC 9(01) VA 0.
           03 WS-032-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-032-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-032-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-032-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-033-BANCO     PIC 9(03) VA 033.
           03 WS-033-NOME      PIC X(25) VA "BANESPA                  ".
           03 WS-033-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-033-BPI          PIC 9(05) VA 38000.
           03 WS-033-LAYOUT       PIC 9(01) VA 1.
           03 WS-033-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-033-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-033-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-033-TP-CONCESSAO PIC 9(01) VA 4.
           03 WS-033-TP-MACICA    PIC 9(01) VA 4.
           03 WS-033-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-033-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-033-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-033-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-034-BANCO     PIC 9(03) VA 034.
           03 WS-034-NOME      PIC X(25) VA "ESTADO DO AMAZONAS ".
           03 WS-034-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-034-BPI          PIC 9(05) VA 38000.
           03 WS-034-LAYOUT       PIC 9(01) VA 1.
           03 WS-034-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-034-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-034-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-034-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-034-TP-MACICA    PIC 9(01) VA 0.
           03 WS-034-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-034-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-034-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-034-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-035-BANCO     PIC 9(03) VA 035.
           03 WS-035-NOME      PIC X(25) VA "B E C ".
           03 WS-035-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-035-BPI          PIC 9(05) VA 38000.
           03 WS-035-LAYOUT       PIC 9(01) VA 1.
           03 WS-035-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-035-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-035-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-035-TP-CONCESSAO PIC 9(01) VA 3.
           03 WS-035-TP-MACICA    PIC 9(01) VA 0.
           03 WS-035-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-035-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-035-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-035-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-036-BANCO     PIC 9(03) VA 036.
           03 WS-036-NOME      PIC X(25) VA "B E M ".
           03 WS-036-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-036-BPI          PIC 9(05) VA 38000.
           03 WS-036-LAYOUT       PIC 9(01) VA 1.
           03 WS-036-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-036-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-036-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-036-TP-CONCESSAO PIC 9(01) VA 3.
           03 WS-036-TP-MACICA    PIC 9(01) VA 3.
           03 WS-036-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-036-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-036-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-036-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-037-BANCO     PIC 9(03) VA 037.
           03 WS-037-NOME      PIC X(25) VA "ESTADO DO PARA ".
           03 WS-037-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-037-BPI          PIC 9(05) VA 38000.
           03 WS-037-LAYOUT       PIC 9(01) VA 1.
           03 WS-037-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-037-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-037-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-037-TP-CONCESSAO PIC 9(01) VA 3.
           03 WS-037-TP-MACICA    PIC 9(01) VA 3.
           03 WS-037-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-037-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-037-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-037-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-038-BANCO     PIC 9(03) VA 038.
           03 WS-038-NOME      PIC X(25) VA "BANESTADO                ".
           03 WS-038-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-038-BPI          PIC 9(05) VA 38000.
           03 WS-038-LAYOUT       PIC 9(01) VA 1.
           03 WS-038-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-038-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-038-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-038-TP-CONCESSAO PIC 9(01) VA 2.
           03 WS-038-TP-MACICA    PIC 9(01) VA 2.
           03 WS-038-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-038-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-038-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-038-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-039-BANCO     PIC 9(03) VA 039.
           03 WS-039-NOME      PIC X(25) VA "ESTADO DO PIAUI ".
           03 WS-039-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-039-BPI          PIC 9(05) VA 38000.
           03 WS-039-LAYOUT       PIC 9(01) VA 1.
           03 WS-039-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-039-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-039-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-039-TP-CONCESSAO PIC 9(01) VA 3.
           03 WS-039-TP-MACICA    PIC 9(01) VA 3.
           03 WS-039-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-039-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-039-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-039-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-041-BANCO     PIC 9(03) VA 041.
           03 WS-041-NOME      PIC X(25) VA "BANRISUL                 ".
           03 WS-041-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-041-BPI          PIC 9(05) VA 38000.
           03 WS-041-LAYOUT       PIC 9(01) VA 1.
           03 WS-041-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-041-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-041-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-041-TP-CONCESSAO PIC 9(01) VA 2.
           03 WS-041-TP-MACICA    PIC 9(01) VA 2.
           03 WS-041-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-041-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-041-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-041-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-047-BANCO     PIC 9(03) VA 047.
           03 WS-047-NOME      PIC X(25) VA "ESTADO DE SERGIPE ".
           03 WS-047-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-047-BPI          PIC 9(05) VA 38000.
           03 WS-047-LAYOUT       PIC 9(01) VA 1.
           03 WS-047-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-047-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-047-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-047-TP-CONCESSAO PIC 9(01) VA 3.
           03 WS-047-TP-MACICA    PIC 9(01) VA 3.
           03 WS-047-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-047-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-047-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-047-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-048-BANCO     PIC 9(03) VA 048.
           03 WS-048-NOME      PIC X(25) VA "BEMGE                    ".
           03 WS-048-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-048-BPI          PIC 9(05) VA 38000.
           03 WS-048-LAYOUT       PIC 9(01) VA 1.
           03 WS-048-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-048-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-048-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-048-TP-CONCESSAO PIC 9(01) VA 4.
           03 WS-048-TP-MACICA    PIC 9(01) VA 4.
           03 WS-048-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-048-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-048-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-048-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-059-BANCO     PIC 9(03) VA 059.
           03 WS-059-NOME      PIC X(25) VA "ESTADO DE RONDONIA ".
           03 WS-059-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-059-BPI          PIC 9(05) VA 06250.
           03 WS-059-LAYOUT       PIC 9(01) VA 0.
           03 WS-059-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-059-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-059-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-059-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-059-TP-MACICA    PIC 9(01) VA 0.
           03 WS-059-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-059-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-059-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-059-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-069-BANCO     PIC 9(03) VA 069.
           03 WS-069-NOME      PIC X(25) VA "BANCO CREFISA S/A        ".
           03 WS-069-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-069-BPI          PIC 9(05) VA 38000.
           03 WS-069-LAYOUT       PIC 9(01) VA 1.
           03 WS-069-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-069-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-069-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-069-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-069-TP-MACICA    PIC 9(01) VA 6.
           03 WS-069-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-069-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-069-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-069-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-070-BANCO     PIC 9(03) VA 070.
           03 WS-070-NOME      PIC X(25) VA "BANCO DE BRASILIA ".
           03 WS-070-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-070-BPI          PIC 9(05) VA 38000.
           03 WS-070-LAYOUT       PIC 9(01) VA 1.
           03 WS-070-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-070-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-070-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-070-TP-CONCESSAO PIC 9(01) VA 3.
           03 WS-070-TP-MACICA    PIC 9(01) VA 3.
           03 WS-070-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-070-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-070-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-070-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-077-BANCO     PIC 9(03) VA 077.
           03 WS-077-NOME      PIC X(25) VA "BANCO INTER S/A          ".
           03 WS-077-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-077-BPI          PIC 9(05) VA 38000.
           03 WS-077-LAYOUT       PIC 9(01) VA 1.
           03 WS-077-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-077-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-077-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-077-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-077-TP-MACICA    PIC 9(01) VA 6.
           03 WS-077-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-077-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-077-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-077-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-081-BANCO     PIC 9(03) VA 081.
           03 WS-081-NOME      PIC X(25) VA "BANCO SEGURO S/A         ".
           03 WS-081-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-081-BPI          PIC 9(05) VA 38000.
           03 WS-081-LAYOUT       PIC 9(01) VA 1.
           03 WS-081-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-081-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-081-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-081-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-081-TP-MACICA    PIC 9(01) VA 6.
           03 WS-081-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-081-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-081-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-081-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-104-BANCO     PIC 9(03) VA 104.
           03 WS-104-NOME      PIC X(25) VA "CAIXA ECONOMICA FEDERAL ".
           03 WS-104-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-104-BPI          PIC 9(05) VA 38000.
           03 WS-104-LAYOUT       PIC 9(01) VA 1.
           03 WS-104-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-104-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-104-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-104-TP-CONCESSAO PIC 9(01) VA 4.
           03 WS-104-TP-MACICA    PIC 9(01) VA 4.
           03 WS-104-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-104-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-104-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-104-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-121-BANCO     PIC 9(03) VA 121.
           03 WS-121-NOME      PIC X(25) VA "BANCO AGIBANK S/A        ".
           03 WS-121-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-121-BPI          PIC 9(05) VA 38000.
           03 WS-121-LAYOUT       PIC 9(01) VA 1.
           03 WS-121-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-121-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-121-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-121-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-121-TP-MACICA    PIC 9(01) VA 6.
           03 WS-121-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-121-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-121-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-121-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-151-BANCO     PIC 9(03) VA 151.
           03 WS-151-NOME      PIC X(25) VA "NOSSACAIXA               ".
           03 WS-151-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-151-BPI          PIC 9(05) VA 38000.
           03 WS-151-LAYOUT       PIC 9(01) VA 1.
           03 WS-151-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-151-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-151-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-151-TP-CONCESSAO PIC 9(01) VA 4.
           03 WS-151-TP-MACICA    PIC 9(01) VA 4.
           03 WS-151-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-151-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-151-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-151-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-153-BANCO     PIC 9(03) VA 153.
           03 WS-153-NOME      PIC X(25) VA "CEERS                    ".
           03 WS-153-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-153-BPI          PIC 9(05) VA 38000.
           03 WS-153-LAYOUT       PIC 9(01) VA 0.
           03 WS-153-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-153-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-153-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-153-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-153-TP-MACICA    PIC 9(01) VA 0.
           03 WS-153-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-153-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-153-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-153-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-212-BANCO     PIC 9(03) VA 212.
           03 WS-212-NOME      PIC X(25) VA "BANCO ORIGINAL S/A       ".
           03 WS-212-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-212-BPI          PIC 9(05) VA 38000.
           03 WS-212-LAYOUT       PIC 9(01) VA 1.
           03 WS-212-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-212-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-212-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-212-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-212-TP-MACICA    PIC 9(01) VA 6.
           03 WS-212-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-212-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-212-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-212-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-215-BANCO     PIC 9(03) VA 215.
           03 WS-215-NOME      PIC X(25) VA "AMERICA DO SUL ".
           03 WS-215-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-215-BPI          PIC 9(05) VA 38000.
           03 WS-215-LAYOUT       PIC 9(01) VA 0.
           03 WS-215-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-215-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-215-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-215-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-215-TP-MACICA    PIC 9(01) VA 0.
           03 WS-215-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-215-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-215-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-215-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-230-BANCO     PIC 9(03) VA 230.
           03 WS-230-NOME      PIC X(25) VA "BANDEIRANTES             ".
           03 WS-230-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-230-BPI          PIC 9(05) VA 38000.
           03 WS-230-LAYOUT       PIC 9(01) VA 0.
           03 WS-230-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-230-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-230-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-230-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-230-TP-MACICA    PIC 9(01) VA 0.
           03 WS-230-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-230-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-230-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-230-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-231-BANCO     PIC 9(03) VA 231.
           03 WS-231-NOME      PIC X(25) VA "BOAVISTA                 ".
           03 WS-231-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-231-BPI          PIC 9(05) VA 06250.
           03 WS-231-LAYOUT       PIC 9(01) VA 0.
           03 WS-231-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-231-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-231-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-231-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-231-TP-MACICA    PIC 9(01) VA 0.
           03 WS-231-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-231-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-231-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-231-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-237-BANCO     PIC 9(03) VA 237.
           03 WS-237-NOME      PIC X(25) VA "BRADESCO                 ".
           03 WS-237-FAIXARQUIVO  PIC 9(01) VA 5.
           03 WS-237-BPI          PIC 9(05) VA 38000.
           03 WS-237-LAYOUT       PIC 9(01) VA 1.
           03 WS-237-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-237-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-237-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-237-TP-CONCESSAO PIC 9(01) VA 5.
           03 WS-237-TP-MACICA    PIC 9(01) VA 5.
           03 WS-237-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-237-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-237-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-237-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-244-BANCO     PIC 9(03) VA 244.
           03 WS-244-NOME      PIC X(25) VA "CIDADE                   ".
           03 WS-244-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-244-BPI          PIC 9(05) VA 38000.
           03 WS-244-LAYOUT       PIC 9(01) VA 0.
           03 WS-244-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-244-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-244-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-244-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-244-TP-MACICA    PIC 9(01) VA 0.
           03 WS-244-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-244-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-244-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-244-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
      *    03 WS-275-BANCO     PIC 9(03) VA 275.
      *    03 WS-275-NOME      PIC X(25) VA "REAL                     ".
      *    03 WS-275-FAIXARQUIVO  PIC 9(01) VA 4.
      *    03 WS-275-BPI          PIC 9(05) VA 38000.
      *    03 WS-275-LAYOUT       PIC 9(01) VA 0.
      *    03 WS-275-TP-NPARTES   PIC 9(01) VA 0.
      *    03 WS-275-TP-IRREGULAR PIC 9(01) VA 0.
      *    03 WS-275-TP-BLOQUEIO  PIC 9(01) VA 0.
      *    03 WS-275-TP-CONCESSAO PIC 9(01) VA 0.
      *    03 WS-275-TP-MACICA    PIC 9(01) VA 0.
      *    03 WS-275-TP-RETORNO   PIC 9(01) VA 0.
      *    03 WS-275-TP-POSSUI    PIC 9(01) VA 0.
      ******************************************************************
           03 WS-282-BANCO     PIC 9(03) VA 282.
           03 WS-282-NOME      PIC X(25) VA "BRASILEIRO COMERCIAL ".
           03 WS-282-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-282-BPI          PIC 9(05) VA 06250.
           03 WS-282-LAYOUT       PIC 9(01) VA 0.
           03 WS-282-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-282-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-282-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-282-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-282-TP-MACICA    PIC 9(01) VA 0.
           03 WS-282-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-282-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-282-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-282-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-291-BANCO     PIC 9(03) VA 291.
           03 WS-291-NOME      PIC X(25) VA "CREDITO NACIONAL ".
           03 WS-291-FAIXARQUIVO  PIC 9(01) VA 5.
           03 WS-291-BPI          PIC 9(05) VA 38000.
           03 WS-291-LAYOUT       PIC 9(01) VA 1.
           03 WS-291-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-291-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-291-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-291-TP-CONCESSAO PIC 9(01) VA 5.
           03 WS-291-TP-MACICA    PIC 9(01) VA 5.
           03 WS-291-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-291-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-291-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-291-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-302-BANCO     PIC 9(03) VA 302.
           03 WS-302-NOME      PIC X(25) VA "PROGRESSO                ".
           03 WS-302-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-302-BPI          PIC 9(05) VA 38000.
           03 WS-302-LAYOUT       PIC 9(01) VA 0.
           03 WS-302-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-302-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-302-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-302-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-302-TP-MACICA    PIC 9(01) VA 0.
           03 WS-302-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-302-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-302-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-302-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-308-BANCO     PIC 9(03) VA 308.
           03 WS-308-NOME      PIC X(25) VA "COMERCIAL BANCESA ".
           03 WS-308-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-308-BPI          PIC 9(05) VA 06250.
           03 WS-308-LAYOUT       PIC 9(01) VA 0.
           03 WS-308-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-308-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-308-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-308-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-308-TP-MACICA    PIC 9(01) VA 0.
           03 WS-308-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-308-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-308-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-308-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-318-BANCO     PIC 9(03) VA 318.
           03 WS-318-NOME      PIC X(25) VA "BMG                    ".
           03 WS-318-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-318-BPI          PIC 9(05) VA 38000.
           03 WS-318-LAYOUT       PIC 9(01) VA 1.
           03 WS-318-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-318-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-318-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-318-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-318-TP-MACICA    PIC 9(01) VA 6.
           03 WS-318-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-318-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-318-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-318-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-334-BANCO     PIC 9(03) VA 334.
           03 WS-334-NOME      PIC X(25) VA "ECONOMICO                ".
           03 WS-334-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-334-BPI          PIC 9(05) VA 06250.
           03 WS-334-LAYOUT       PIC 9(01) VA 0.
           03 WS-334-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-334-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-334-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-334-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-334-TP-MACICA    PIC 9(01) VA 0.
           03 WS-334-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-334-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-334-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-334-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-336-BANCO     PIC 9(03) VA 336.
           03 WS-336-NOME      PIC X(25) VA "BANCO C6 S/A             ".
           03 WS-336-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-336-BPI          PIC 9(05) VA 38000.
           03 WS-336-LAYOUT       PIC 9(01) VA 1.
           03 WS-336-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-336-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-336-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-336-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-336-TP-MACICA    PIC 9(01) VA 6.
           03 WS-336-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-336-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-336-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-336-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-341-BANCO     PIC 9(03) VA 341.
           03 WS-341-NOME      PIC X(25) VA "ITAU                     ".
           03 WS-341-FAIXARQUIVO  PIC 9(01) VA 5.
           03 WS-341-BPI          PIC 9(05) VA 38000.
           03 WS-341-LAYOUT       PIC 9(01) VA 1.
           03 WS-341-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-341-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-341-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-341-TP-CONCESSAO PIC 9(01) VA 5.
           03 WS-341-TP-MACICA    PIC 9(01) VA 5.
           03 WS-341-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-341-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-341-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-341-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-344-BANCO     PIC 9(03) VA 344.
           03 WS-344-NOME      PIC X(25) VA "MERCANTIL DE PERNAMBUCO ".
           03 WS-344-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-344-BPI          PIC 9(05) VA 06250.
           03 WS-344-LAYOUT       PIC 9(01) VA 0.
           03 WS-344-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-344-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-344-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-344-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-344-TP-MACICA    PIC 9(01) VA 0.
           03 WS-344-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-344-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-344-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-344-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-346-BANCO     PIC 9(03) VA 346.
           03 WS-346-NOME      PIC X(25) VA "FRANCES E BRASILEIRO ".
           03 WS-346-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-346-BPI          PIC 9(05) VA 06250.
           03 WS-346-LAYOUT       PIC 9(01) VA 0.
           03 WS-346-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-346-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-346-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-346-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-346-TP-MACICA    PIC 9(01) VA 0.
           03 WS-346-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-346-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-346-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-346-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-347-BANCO     PIC 9(03) VA 347.
           03 WS-347-NOME      PIC X(25) VA "SUDAMERIS DO BRASIL ".
           03 WS-347-FAIXARQUIVO  PIC 9(01) VA 4.
           03 WS-347-BPI          PIC 9(05) VA 06250.
           03 WS-347-LAYOUT       PIC 9(01) VA 0.
           03 WS-347-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-347-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-347-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-347-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-347-TP-MACICA    PIC 9(01) VA 0.
           03 WS-347-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-347-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-347-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-347-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-353-BANCO     PIC 9(03) VA 353.
           03 WS-353-NOME      PIC X(25) VA "SANTANDER BRASIL S/A ".
           03 WS-353-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-353-BPI          PIC 9(05) VA 38000.
           03 WS-353-LAYOUT       PIC 9(01) VA 1.
           03 WS-353-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-353-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-353-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-353-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-353-TP-MACICA    PIC 9(01) VA 6.
           03 WS-353-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-353-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-353-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-353-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-356-BANCO     PIC 9(03) VA 356.
           03 WS-356-NOME      PIC X(25) VA "ABN-AMRO BANK ".
           03 WS-356-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-356-BPI          PIC 9(05) VA 06250.
           03 WS-356-LAYOUT       PIC 9(01) VA 1.
           03 WS-356-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-356-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-356-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-356-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-356-TP-MACICA    PIC 9(01) VA 6.
           03 WS-356-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-356-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-356-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-356-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-388-BANCO     PIC 9(03) VA 388.
           03 WS-388-NOME      PIC X(25) VA "MERCANTIL DE DESCONTOS ".
           03 WS-388-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-388-BPI          PIC 9(05) VA 06250.
           03 WS-388-LAYOUT       PIC 9(01) VA 0.
           03 WS-388-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-388-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-388-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-388-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-388-TP-MACICA    PIC 9(01) VA 0.
           03 WS-388-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-388-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-388-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-388-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-389-BANCO     PIC 9(03) VA 389.
           03 WS-389-NOME      PIC X(25) VA "MERCANTIL DE BRASIL ".
           03 WS-389-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-389-BPI          PIC 9(05) VA 38000.
           03 WS-389-LAYOUT       PIC 9(01) VA 1.
           03 WS-389-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-389-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-389-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-389-TP-CONCESSAO PIC 9(01) VA 2.
           03 WS-389-TP-MACICA    PIC 9(01) VA 2.
           03 WS-389-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-389-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-389-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-389-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-392-BANCO     PIC 9(03) VA 392.
           03 WS-392-NOME      PIC X(25) VA "MERCANTIL DE SAO PAULO ".
           03 WS-392-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-392-BPI          PIC 9(05) VA 06250.
           03 WS-392-LAYOUT       PIC 9(01) VA 1.
           03 WS-392-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-392-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-392-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-392-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-392-TP-MACICA    PIC 9(01) VA 6.
           03 WS-392-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-392-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-392-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-392-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-394-BANCO     PIC 9(03) VA 394.
           03 WS-394-NOME      PIC X(25) VA "MERCANTIL DE CREDITO ".
           03 WS-394-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-394-BPI          PIC 9(05) VA 06250.
           03 WS-394-LAYOUT       PIC 9(01) VA 0.
           03 WS-394-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-394-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-394-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-394-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-394-TP-MACICA    PIC 9(01) VA 0.
           03 WS-394-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-394-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-394-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-394-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-399-BANCO     PIC 9(03) VA 399.
           03 WS-399-NOME      PIC X(25) VA "BAMERINDUS DO BRASIL ".
           03 WS-399-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-399-BPI          PIC 9(05) VA 38000.
           03 WS-399-LAYOUT       PIC 9(01) VA 1.
           03 WS-399-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-399-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-399-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-399-TP-CONCESSAO PIC 9(01) VA 2.
           03 WS-399-TP-MACICA    PIC 9(01) VA 2.
           03 WS-399-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-399-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-399-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-399-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-409-BANCO     PIC 9(03) VA 409.
           03 WS-409-NOME      PIC X(25) VA "UNIBANCO                 ".
           03 WS-409-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-409-BPI          PIC 9(05) VA 38000.
           03 WS-409-LAYOUT       PIC 9(01) VA 1.
           03 WS-409-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-409-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-409-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-409-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-409-TP-MACICA    PIC 9(01) VA 6.
           03 WS-409-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-409-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-409-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-409-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-415-BANCO     PIC 9(03) VA 415.
           03 WS-415-NOME      PIC X(25) VA "NACIONAL                 ".
           03 WS-415-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-415-BPI          PIC 9(05) VA 06250.
           03 WS-415-LAYOUT       PIC 9(01) VA 0.
           03 WS-415-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-415-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-415-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-415-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-415-TP-MACICA    PIC 9(01) VA 0.
           03 WS-415-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-415-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-415-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-415-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-420-BANCO     PIC 9(03) VA 420.
           03 WS-420-NOME      PIC X(25) VA "BANORTE                  ".
           03 WS-420-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-420-BPI          PIC 9(05) VA 01600.
           03 WS-420-LAYOUT       PIC 9(01) VA 0.
           03 WS-420-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-420-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-420-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-420-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-420-TP-MACICA    PIC 9(01) VA 0.
           03 WS-420-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-420-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-420-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-420-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-422-BANCO     PIC 9(03) VA 422.
           03 WS-422-NOME      PIC X(25) VA "SAFRA                    ".
           03 WS-422-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-422-BPI          PIC 9(05) VA 38000.
           03 WS-422-LAYOUT       PIC 9(01) VA 1.
           03 WS-422-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-422-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-422-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-422-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-422-TP-MACICA    PIC 9(01) VA 6.
           03 WS-422-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-422-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-422-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-422-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-424-BANCO     PIC 9(03) VA 424.
           03 WS-424-NOME      PIC X(25) VA "NOROESTE                 ".
           03 WS-424-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-424-BPI          PIC 9(05) VA 38000.
           03 WS-424-LAYOUT       PIC 9(01) VA 0.
           03 WS-424-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-424-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-424-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-424-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-424-TP-MACICA    PIC 9(01) VA 0.
           03 WS-424-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-424-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-424-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-424-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-453-BANCO     PIC 9(03) VA 453.
           03 WS-453-NOME      PIC X(25) VA "RURAL                    ".
           03 WS-453-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-453-BPI          PIC 9(05) VA 38000.
           03 WS-453-LAYOUT       PIC 9(01) VA 1.
           03 WS-453-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-453-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-453-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-453-TP-CONCESSAO PIC 9(01) VA 2.
           03 WS-453-TP-MACICA    PIC 9(01) VA 2.
           03 WS-453-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-453-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-453-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-453-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-466-BANCO     PIC 9(03) VA 466.
           03 WS-466-NOME      PIC X(25) VA "MITSUBISH BRASILEIRO ".
           03 WS-466-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-466-BPI          PIC 9(05) VA 06250.
           03 WS-466-LAYOUT       PIC 9(01) VA 0.
           03 WS-466-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-466-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-466-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-466-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-466-TP-MACICA    PIC 9(01) VA 0.
           03 WS-466-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-466-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-466-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-466-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-483-BANCO     PIC 9(03) VA 483.
           03 WS-483-NOME      PIC X(25) VA "AGRIMISA                 ".
           03 WS-483-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-483-BPI          PIC 9(05) VA 01600.
           03 WS-483-LAYOUT       PIC 9(01) VA 0.
           03 WS-483-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-483-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-483-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-483-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-483-TP-MACICA    PIC 9(01) VA 0.
           03 WS-483-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-483-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-483-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-483-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-623-BANCO     PIC 9(03) VA 623.
           03 WS-623-NOME      PIC X(25) VA "BANCO PAN S/A            ".
           03 WS-623-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-623-BPI          PIC 9(05) VA 38000.
           03 WS-623-LAYOUT       PIC 9(01) VA 1.
           03 WS-623-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-623-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-623-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-623-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-623-TP-MACICA    PIC 9(01) VA 6.
           03 WS-623-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-623-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-623-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-623-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-641-BANCO     PIC 9(03) VA 641.
           03 WS-641-NOME      PIC X(25) VA "EXCEL ECONOMICO ".
           03 WS-641-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-641-BPI          PIC 9(05) VA 06250.
           03 WS-641-LAYOUT       PIC 9(01) VA 1.
           03 WS-641-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-641-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-641-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-641-TP-CONCESSAO PIC 9(01) VA 3.
           03 WS-641-TP-MACICA    PIC 9(01) VA 3.
           03 WS-641-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-641-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-641-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-641-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-645-BANCO     PIC 9(03) VA 645.
           03 WS-645-NOME      PIC X(25) VA "ESTADO DE RORAIMA ".
           03 WS-645-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-645-BPI          PIC 9(05) VA 06250.
           03 WS-645-LAYOUT       PIC 9(01) VA 0.
           03 WS-645-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-645-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-645-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-645-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-645-TP-MACICA    PIC 9(01) VA 0.
           03 WS-645-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-645-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-645-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-645-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-707-BANCO     PIC 9(03) VA 707.
           03 WS-707-NOME      PIC X(25) VA "BANCO DAYCOVAL S/A       ".
           03 WS-707-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-707-BPI          PIC 9(05) VA 38000.
           03 WS-707-LAYOUT       PIC 9(01) VA 1.
           03 WS-707-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-707-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-707-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-707-TP-CONCESSAO PIC 9(01) VA 6.
           03 WS-707-TP-MACICA    PIC 9(01) VA 6.
           03 WS-707-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-707-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-707-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-707-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-748-BANCO     PIC 9(03) VA 748.
           03 WS-748-NOME      PIC X(25) VA "BANSICREDI               ".
           03 WS-748-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-748-BPI          PIC 9(05) VA 06250.
           03 WS-748-LAYOUT       PIC 9(01) VA 1.
           03 WS-748-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-748-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-748-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-748-TP-CONCESSAO PIC 9(01) VA 2.
           03 WS-748-TP-MACICA    PIC 9(01) VA 2.
           03 WS-748-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-748-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-748-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-748-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-756-BANCO     PIC 9(03) VA 756.
           03 WS-756-NOME      PIC X(25) VA "BANCOOB                  ".
           03 WS-756-FAIXARQUIVO  PIC 9(01) VA 3.
           03 WS-756-BPI          PIC 9(05) VA 01600.
           03 WS-756-LAYOUT       PIC 9(01) VA 1.
           03 WS-756-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-756-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-756-TP-BLOQUEIO  PIC 9(01) VA 1.
           03 WS-756-TP-CONCESSAO PIC 9(01) VA 3.
           03 WS-756-TP-MACICA    PIC 9(01) VA 3.
           03 WS-756-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-756-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-756-TP-ESPECIAL  PIC 9(01) VA 1.
           03 WS-756-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-888-BANCO     PIC 9(03) VA 888.
           03 WS-888-NOME      PIC X(25) VA "CORREIOS                 ".
           03 WS-888-FAIXARQUIVO  PIC 9(01) VA 2.
           03 WS-888-BPI          PIC 9(05) VA 38000.
           03 WS-888-LAYOUT       PIC 9(01) VA 0.
           03 WS-888-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-888-TP-IRREGULAR PIC 9(01) VA 1.
           03 WS-888-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-888-TP-CONCESSAO PIC 9(01) VA 2.
           03 WS-888-TP-MACICA    PIC 9(01) VA 2.
           03 WS-888-TP-RETORNO   PIC 9(01) VA 1.
           03 WS-888-TP-POSSUI    PIC 9(01) VA 1.
           03 WS-888-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-888-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-953-BANCO     PIC 9(03) VA 953.
           03 WS-953-NOME      PIC X(25) VA "SICREDI - SISTEMA C.COOP ".
           03 WS-953-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-953-BPI          PIC 9(05) VA 06250.
           03 WS-953-LAYOUT       PIC 9(01) VA 0.
           03 WS-953-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-953-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-953-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-953-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-953-TP-MACICA    PIC 9(01) VA 0.
           03 WS-953-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-953-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-953-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-953-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************
           03 WS-998-BANCO     PIC 9(03) VA 998.
           03 WS-998-NOME      PIC X(25) VA "EMPRESAS  CONVENENTES ".
           03 WS-998-FAIXARQUIVO  PIC 9(01) VA 6.
           03 WS-998-BPI          PIC 9(05) VA 06250.
           03 WS-998-LAYOUT       PIC 9(01) VA 0.
           03 WS-998-TP-NPARTES   PIC 9(01) VA 0.
           03 WS-998-TP-IRREGULAR PIC 9(01) VA 0.
           03 WS-998-TP-BLOQUEIO  PIC 9(01) VA 0.
           03 WS-998-TP-CONCESSAO PIC 9(01) VA 0.
           03 WS-998-TP-MACICA    PIC 9(01) VA 0.
           03 WS-998-TP-RETORNO   PIC 9(01) VA 0.
           03 WS-998-TP-POSSUI    PIC 9(01) VA 0.
           03 WS-998-TP-ESPECIAL  PIC 9(01) VA 0.
           03 WS-998-TP-IRREGDIA  PIC 9(01) VA 0.
      ******************************************************************

       01  WS-BANCOS-TABELAS REDEFINES WS-TABELA-BANCOS.
errors*  02 WS-IND PIC 99.
errors*  02 WS-INDEND PIC 99.       
          02 WS-BANCOS-OC OCCURS 74 INDEXED BY WS-IND WS-INDEND.
           03 WS-NNN-BANCO     PIC 9(03).
           03 WS-NNN-NOME      PIC X(25).
           03 WS-NNN-FAIXARQUIVO  PIC 9(01).
           03 WS-NNN-BPI          PIC 9(05).
           03 WS-NNN-LAYOUT       PIC 9(01).
           03 WS-NNN-TP-NPARTES   PIC 9(01).
           03 WS-NNN-TP-IRREGULAR PIC 9(01).
           03 WS-NNN-TP-BLOQUEIO  PIC 9(01).
           03 WS-NNN-TP-CONCESSAO PIC 9(01).
           03 WS-NNN-TP-MACICA    PIC 9(01).
           03 WS-NNN-TP-RETORNO   PIC 9(01).
           03 WS-NNN-TP-POSSUI    PIC 9(01).
           03 WS-NNN-TP-ESPECIAL  PIC 9(01).
           03 WS-NNN-TP-IRREGDIA  PIC 9(01).

           77  WS-ESPACO          PIC 9(11) BINARY VA 1.
       01  WS-ONDE.
           03 FILLER              PIC X(13)
           VA "RELATORIO EM:".
           03 WS-TITLE-BANCO  PIC X(60).

       01  WS-PAGINA              PIC 9.

       01  WS-LINHAS              PIC 9(02) VA 37.

       01  WS-LINHA1.
           03 FILLER PIC X(04) VA ":BCO".
           03 FILLER PIC X(26) VA ":NOME".
           03 FILLER PIC X(06) VA ":FAIXA".
           03 FILLER PIC X(06) VA ":BPI".
           03 FILLER PIC X(07) VA ":LAYOUT".
           03 FILLER PIC X(08) VA ":NPARTES".
           03 FILLER PIC X(10) VA ":IRREGULAR".
           03 FILLER PIC X(09) VA ":BLOQUEIO".
           03 FILLER PIC X(10) VA ":CONCESSAO".
           03 FILLER PIC X(07) VA ":MACICA".
           03 FILLER PIC X(08) VA ":RETORNO".
           03 FILLER PIC X(07) VA ":POSSUI".
           03 FILLER PIC X(09) VA ":ESPECIAL".
           03 FILLER PIC X(09) VA ":IRREGDIA".

       01  WS-LINHA2.
           03 WSL1BCO PIC X(01) VA ":".
           03 WSL2BCO PIC X(03) VA "BCO".
           03 WSL1NOM PIC X(01) VA ":".
           03 WSL2NOM PIC X(25) VA "NOME".
           03 WSL1FAI PIC X(01) VA ":".
           03 WSL2FAI PIC BBZBB.
           03 WSL1BPI PIC X(01) VA ":".
           03 WSL2BPI PIC X(05) VA "BPI".
           03 WSL1LAY PIC X(01) VA ":".
           03 WSL2LAY PIC BBZBBB.
           03 WSL1NPA PIC X(01) VA ":".
           03 WSL2NPA PIC BBZBBBB.
           03 WSL1IRR PIC X(01) VA ":".
           03 WSL2IRR PIC BBBZBBBBB.
           03 WSL1BLO PIC X(01) VA ":".
           03 WSL2BLO PIC BBZBBBBB.
           03 WSL1CON PIC X(01) VA ":".
           03 WSL2CON PIC BBZBBBBBB.
           03 WSL1MAC PIC X(01) VA ":".
           03 WSL2MAC PIC BZBBBB.
           03 WSL1RET PIC X(01) VA ":".
           03 WSL2RET PIC BBZBBBB.
           03 WSL1POS PIC X(01) VA ":".
           03 WSL2POS PIC BZBBBB.
           03 WSL1ESP PIC X(01) VA ":".
           03 WSL2ESP PIC BBBZBBBB.
           03 WSL1IRD PIC X(01) VA ":".
           03 WSL2IRD PIC BBBZBBBB.

       PROCEDURE DIVISION USING WS-BANCOPIC9COMP
                                WS-PERGUNTAPICX25
                                WS-RESPOSTAPICX25.
       100-INICIO.

           IF WS-HOJEB EQUAL ZEROS
           CALL "ZDATA4 IN DTP/L1" GIVING WS-HOJEB
           MOVE WS-HOJEB TO WS-HOJE
           CALL "ZHORA IN DTP/L1" GIVING WS-TIMEB
           MOVE WS-TIMEB TO WS-TIME
           PERFORM 900-FIM-DA-TABELA THRU 900-FIM-DA-TABELA-FIM
           SET WS-IND TO 0

           PERFORM 400-RELATORIO-CRITICA THRU
                   400-RELATORIO-CRITICA-FIM
           UNTIL WS-IND EQUAL   WS-INDEND

           IF WS-ERRO EQUAL 1

           OPEN OUTPUT BANCO

           MOVE ATTRIBUTE TITLE OF BANCO TO WS-TITLE-BANCO

           CALL "ZWRITE IN DTP/L1" USING WS-ESPACO WS-ONDE

           SET WS-IND TO 0
           PERFORM 400-RELATORIO-CRITICA THRU
                   400-RELATOU
           UNTIL WS-IND EQUAL WS-INDEND
           MOVE "TABELA INTERNA COM (*)ERRO(S) *****" TO REG-BANCO
           WRITE REG-BANCO BEFORE 1

           CLOSE BANCO SAVE

           SET MYSELF (STATUS) TO -1.

           IF WS-PERGUNTAPICX25 EQUAL "NOME"
           MOVE SPACES TO WS-RESPOSTAPICX25
           ELSE
           MOVE ALL "9" TO WS-RESPOSTAPICX25.
           IF WS-PERGUNTAPICX25 NOT EQUAL "HELP"
           SET WS-IND TO 1
           SEARCH WS-BANCOS-OC AT END GO 200-ERRO


           WHEN WS-NNN-BANCO (WS-IND) = WS-BANCOPIC9COMP NEXT SENTENCE.

           IF WS-PERGUNTAPICX25 EQUAL "NOME"
           PERFORM 300-NOME
           ELSE

           IF WS-PERGUNTAPICX25 EQUAL "FAIXARQUIVO"
           PERFORM 310-FAIXARQ
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "BPI"
           PERFORM 320-BPI
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "LAYOUT"
           PERFORM 325-LAYOUT
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "TP-ESPECIAL"
           PERFORM 327-TP-ESPECIAL
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "TP-IRREGDIA"
           PERFORM 329-TP-IRREGDIA
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "TP-NPARTES"
           PERFORM 330-TP-PARTES
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "TP-IRREGULAR"
           PERFORM 340-TP-IRREG
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "TP-BLOQUEIO"
           PERFORM 350-TP-BLOQ
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "TP-CONCESSAO"
           PERFORM 360-TP-CONC
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "TP-MACICA"
           PERFORM 370-TP-MACICA
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "TP-RETORNO"
           PERFORM 375-TP-RETORNO
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "TP-POSSUI"
           PERFORM 380-TP-POSSUI
           ELSE
           IF WS-PERGUNTAPICX25 EQUAL "LABELFITA"
           PERFORM 390-LABELFITA
           ELSE
           OPEN OUTPUT BANCO

           MOVE ATTRIBUTE TITLE OF BANCO TO WS-TITLE-BANCO

           CALL "ZWRITE IN DTP/L1" USING WS-ESPACO WS-ONDE

           SET WS-IND TO 0
           PERFORM 400-RELATORIO-CRITICA THRU
                   400-RELATOU
           UNTIL WS-IND EQUAL WS-INDEND

           MOVE "FIM DO RELATORIO REQUISITADO ----" TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           CLOSE BANCO SAVE.

       200-ERRO.
           EXIT PROGRAM.

       300-NOME.
           MOVE WS-NNN-NOME (WS-IND) TO WS-RESPOSTAPICX25 WSL2NOM.

       310-FAIXARQ.
           MOVE WS-NNN-FAIXARQUIVO (WS-IND) TO WS-BOOLEANPIC9X WSL2FAI.

       320-BPI.

           MOVE WS-NNN-BPI (WS-IND) TO WS-BPI WSL2BPI.

       325-LAYOUT.
           MOVE WS-NNN-LAYOUT (WS-IND) TO WS-BOOLEANPIC9X WSL2LAY.

       327-TP-ESPECIAL.
           MOVE WS-NNN-TP-ESPECIAL (WS-IND) TO WS-BOOLEANPIC9X WSL2ESP.

       329-TP-IRREGDIA.
           MOVE WS-NNN-TP-IRREGDIA (WS-IND) TO WS-BOOLEANPIC9X WSL2IRD.

       330-TP-PARTES.
           MOVE WS-NNN-TP-NPARTES (WS-IND) TO WS-BOOLEANPIC9X WSL2NPA.

       340-TP-IRREG.
           MOVE WS-NNN-TP-IRREGULAR (WS-IND) TO WS-BOOLEANPIC9X WSL2IRR.

       350-TP-BLOQ.
           MOVE WS-NNN-TP-BLOQUEIO (WS-IND) TO WS-BOOLEANPIC9X WSL2BLO.

       360-TP-CONC.
           MOVE WS-NNN-TP-CONCESSAO (WS-IND) TO WS-BOOLEANPIC9X WSL2CON.

       370-TP-MACICA.
           MOVE WS-NNN-TP-MACICA (WS-IND) TO WS-BOOLEANPIC9X WSL2MAC.

       375-TP-RETORNO.
           MOVE WS-NNN-TP-RETORNO (WS-IND) TO WS-BOOLEANPIC9X WSL2RET.

       380-TP-POSSUI.
           MOVE WS-NNN-TP-POSSUI (WS-IND) TO WS-BOOLEANPIC9X WSL2POS.

       380-TP-POSSUI-RELATORIO. EXIT.

       390-LABELFITA.
           MOVE "FBEMM" TO WS-LABELFITA.
           MOVE WS-BANCOPIC9COMP TO WS-LABELFITAMATRIZ.
           MOVE "." TO WS-LABELFITAPONTO.

       400-RELATORIO.
           MOVE SPACES TO REG-BANCO.
           WRITE REG-BANCO BEFORE CHANNEL 1
           ADD 1 TO WS-PAGINA
           STRING "PAG. " WS-PAGINA " DATE " WS-HOJE " TIME " WS-TIME
           DELIMITED BY SIZE INTO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE ATTRIBUTE NAME OF MYSELF TO REG-BANCO.
           WRITE REG-BANCO BEFORE 1.
           MOVE "O programa acima identificado chamou indiretamente a su
      -    "brotina P/SUB/LIB/BANCO."
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE "Este relatorio foi impresso por requisicao (:)normal ou
      -    " devido a (*)erro de consistencia na tabela interna da subro
      -    "tina acima" TO REG-BANCO.
           WRITE REG-BANCO BEFORE 1
           MOVE ALL "-" TO REG-BANCO
           WRITE REG-BANCO BEFORE 1.


           MOVE "A subrotina chamada pelas demais abaixo: TODAS no forma
      -    "to CALL 'P/SUB/LIB/BANCO/xxx' USING BANCO      RESPOSTA"
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE "
      -    "                                    9(3) COMP  X(25)"
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE "            Caso o BANCO NAO EXISTA na tabela a RESPOST
      -    "A sera preenchida com NOVES a menos que esteja solicitando o
      -    " NOME." TO REG-BANCO
           WRITE REG-BANCO BEFORE 2
           MOVE "P/SUB/LIB/BANCO/NOME     responde o NOME DO BANCO (ou E
      -    "SPACOS caso o BANCO nao exista)"
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE "P/SUB/LIB/BANCO/LABELFITA    sempre responde FBEMM???.
      -    " onde ??? corresponde ao BANCO seguido do ponto (.)"
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE "P/SUB/LIB/BANCO/BPI responde 01600,06250,38000 ou 99999
      -    " caso gerar em disco onde independe CONCESSAO ou MACICA."
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE "P/SUB/LIB/BANCO/LAYOUT responde 0 (antigo) ou 1 (NIT)"
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE "P/SUB/LIB/BANCO/FAIXARQUIVO responde de 1 ate 6"
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 2
           MOVE "P/SUB/LIB/BANCO/TPNPARTES  responde 0,2,4,6 ou 8 (DIVIS
      -    "AO ARQUIVO TP MACICA)"
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE "P/SUB/LIB/BANCO/TPx,onde x=IRREGULAR,BLOQUEIO,CONCESSAO
      -    ",MACICA,RETORNO,ESPECIAL,IRREGDIA respondem 0 ou <*> (NAO ou
      -    " SIM TP/BANCO)."
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE "                      <*> diferente de zero (1 ou a FAI
      -    "XARQUIVO nos casos CONCESSAO/MACICA pois compoem o ROTULO TP
      -    ".)"
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE "P/SUB/LIB/BANCO/TPPOSSUI      responde 0 (nao) ou 1 (si
      -    "m) EM FUNCAO DOS .../TPx (so responde nao possui tp se todos
      -    " envio = nao)"
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 2
           MOVE "P/SUB/LIB/BANCO/HELP       emite este relatorio indepen
      -    "dente das informacoes nos campos USING"
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE ALL "-" TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE "Tabela Interna da subrotina P/SUB/LIB/BANCO"
           TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           MOVE ALL "-" TO REG-BANCO
           WRITE REG-BANCO BEFORE 1
           WRITE REG-BANCO FROM WS-LINHA1 BEFORE 1.

       400-RELATORIO-CABECALHO-FIM. EXIT.

       400-RELATORIO-CRITICA.

           SET WS-IND UP BY 1.
           MOVE ALL ":" TO WS-LINHA2
           MOVE WS-NNN-BANCO (WS-IND) TO WSL2BCO

           PERFORM 300-NOME THRU 380-TP-POSSUI-RELATORIO

           IF WS-NNN-BANCO (WS-IND) LESS 1 OR GREATER 998
           MOVE "*" TO WSL1BCO.

           IF WS-NNN-NOME (WS-IND) EQUAL SPACES
           MOVE "*" TO WSL1NOM.

           IF WS-NNN-FAIXARQUIVO (WS-IND) LESS 1 OR GREATER 6
           MOVE "*" TO WSL1FAI.

           IF WS-NNN-BPI (WS-IND) NOT EQUAL 01600 AND 06250 AND 38000
           AND 99999
           MOVE "*" TO WSL1BPI.
           IF WS-NNN-BPI (WS-IND) EQUAL 99999
           IF WS-NNN-TP-POSSUI (WS-IND) NOT EQUAL 0
           MOVE "*" TO WSL1BPI.

           IF WS-NNN-LAYOUT (WS-IND) NOT EQUAL 0 AND 1
           MOVE "*" TO WSL1LAY.

           IF WS-NNN-TP-ESPECIAL (WS-IND) NOT EQUAL 0 AND 1
           MOVE "*" TO WSL1ESP.
           IF WS-NNN-TP-ESPECIAL (WS-IND) EQUAL 1
           IF WS-NNN-TP-CONCESSAO (WS-IND) EQUAL 0
           MOVE "*" TO WSL1ESP.

           IF WS-NNN-TP-IRREGDIA (WS-IND) NOT EQUAL 0 AND 1
           MOVE "*" TO WSL1IRD.

           IF WS-NNN-TP-NPARTES (WS-IND) NOT EQUAL 0 AND 2 AND 4 AND 6
           AND 8
           MOVE "*" TO WSL1NPA.

           IF WS-NNN-TP-NPARTES (WS-IND) NOT EQUAL 0
           IF WS-NNN-TP-MACICA (WS-IND) EQUAL 0
           MOVE "*" TO WSL1NPA.

           IF WS-NNN-TP-IRREGULAR (WS-IND) NOT EQUAL 0 AND 1
           MOVE "*" TO WSL1IRR.

           IF WS-NNN-TP-BLOQUEIO (WS-IND) NOT EQUAL 0 AND 1
           MOVE "*" TO WSL1BLO.

           IF WS-NNN-TP-CONCESSAO (WS-IND) NOT EQUAL 0 AND 1 AND 2 AND 3
                                           AND 4 AND 5 AND 6
           MOVE "*" TO WSL1CON.

           IF WS-NNN-TP-CONCESSAO (WS-IND) NOT EQUAL 0 AND 1
           IF WS-NNN-TP-CONCESSAO (WS-IND) NOT EQUAL
              WS-NNN-FAIXARQUIVO  (WS-IND)
           MOVE "*" TO WSL1CON.


           IF WS-NNN-TP-MACICA (WS-IND)    NOT EQUAL 0 AND 1 AND 2 AND 3
                                           AND 4 AND 5 AND 6
           MOVE "*" TO WSL1MAC.

           IF WS-NNN-TP-MACICA    (WS-IND) NOT EQUAL 0
           IF WS-NNN-TP-MACICA    (WS-IND) NOT EQUAL
              WS-NNN-FAIXARQUIVO  (WS-IND)
           MOVE "*" TO WSL1MAC.

           IF WS-NNN-TP-RETORNO (WS-IND) NOT EQUAL 0 AND 1
           MOVE "*" TO WSL1RET.

           IF WS-NNN-TP-POSSUI (WS-IND) NOT EQUAL 0 AND 1
           MOVE "*" TO WSL1POS
           ELSE
           IF WS-NNN-TP-POSSUI (WS-IND) EQUAL 0
           IF ZERO EQUAL
           WS-NNN-TP-IRREGULAR (WS-IND) AND
           WS-NNN-TP-BLOQUEIO  (WS-IND) AND
           WS-NNN-TP-CONCESSAO (WS-IND) AND
           WS-NNN-TP-MACICA    (WS-IND) AND
           WS-NNN-TP-RETORNO   (WS-IND) AND
           WS-NNN-TP-ESPECIAL  (WS-IND) AND
           WS-NNN-TP-IRREGDIA  (WS-IND)
           NEXT SENTENCE
           ELSE
           MOVE "*" TO WSL1POS
           ELSE
           IF WS-NNN-TP-POSSUI (WS-IND) EQUAL 1
           IF ZERO EQUAL
           WS-NNN-TP-IRREGULAR (WS-IND) AND
           WS-NNN-TP-BLOQUEIO  (WS-IND) AND
           WS-NNN-TP-CONCESSAO (WS-IND) AND
           WS-NNN-TP-MACICA    (WS-IND) AND
           WS-NNN-TP-RETORNO   (WS-IND) AND
           WS-NNN-TP-ESPECIAL  (WS-IND) AND
           WS-NNN-TP-IRREGDIA  (WS-IND)
           MOVE "*" TO WSL1POS.

           IF "*" EQUAL
           WSL1BCO OR
           WSL1NOM OR
           WSL1FAI OR
           WSL1BPI OR
           WSL1NPA OR
           WSL1IRR OR
           WSL1BLO OR
           WSL1CON OR
           WSL1MAC OR
           WSL1RET OR
           WSL1POS OR
           WSL1ESP OR
           WSL1IRD
           MOVE 1 TO WS-ERRO.

       400-RELATORIO-CRITICA-FIM. EXIT.
       400-RELATORIO-CONTINUA.

           IF WS-LINHAS GREATER 36
           MOVE ZEROS TO WS-LINHAS
           PERFORM 400-RELATORIO THRU
                   400-RELATORIO-CABECALHO-FIM
           GO TO 400-RELATORIO-WRITE.

           IF WS-NNN-FAIXARQUIVO (WS-IND) EQUAL
              WS-NNN-FAIXARQUIVO (WS-IND - 1)
              MOVE 0 TO WSL2FAI.
           IF WS-NNN-BPI (WS-IND) EQUAL
              WS-NNN-BPI (WS-IND - 1)
              MOVE SPACES TO WSL2BPI.

       400-RELATORIO-WRITE.


           WRITE REG-BANCO FROM WS-LINHA2 BEFORE 1
           ADD 1 TO WS-LINHAS.

       400-RELATOU. EXIT.

       900-FIM-DA-TABELA.

           SET WS-IND TO 1.
           SEARCH WS-BANCOS-OC AT END NEXT SENTENCE
           WHEN WS-NNN-BANCO (WS-IND) NOT = WS-NNN-BANCO (WS-IND)
           NEXT SENTENCE.

           SET WS-INDEND TO WS-IND.
           SET WS-INDEND DOWN BY 1.

       900-FIM-DA-TABELA-FIM. EXIT.


