      $$SET LIST
      $$SET ERRORLIMIT = 9
      $$SET FEDLEVEL   = 5
      *$SET TADS
      $SET SHARING = PRIVATE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SSUBLIBBANCOTPBLOQUEIO.
       AUTHOR. RENATO VIANA www tutorialdocumenta com br.
       DATE-WRITTEN. 25/07/2000.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *

       01  WS-BANCOPIC9COMP            PIC 9(03) COMP.
       01  WS-PERGUNTAPICX12           PIC X(12).
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


       PROCEDURE DIVISION USING WS-BANCOPIC9COMP

                                WS-RESPOSTAPICX25.
       100-INICIO.


           MOVE "TP-BLOQUEIO"               TO WS-PERGUNTAPICX12.
           CALL "P/SUB/LIB/BANCO"        USING WS-BANCOPIC9COMP
                                               WS-PERGUNTAPICX12
                                               WS-RESPOSTAPICX25.
           EXIT PROGRAM.
