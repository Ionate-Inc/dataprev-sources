      *
      * UNISYS Y2K - TEAM 2000
      *
      $SET SHARING = PRIVATE
       IDENTIFICATION DIVISION.
        PROGRAM-ID. SSUBLIB2K601.
       ENVIRONMENT    DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ********** UNISYS Y2K - Linha alterada em 20/03/1999 15:18:44
      *77  WS-DATA-INICIO       PIC 9(06).
       77  WS-DATA-INICIO       PIC 9(08).
      ********** UNISYS Y2K - Linha alterada em 20/03/1999 15:18:44
      *77  WS-DATA-FIM          PIC 9(06).
       77  WS-DATA-FIM          PIC 9(08).
       77  WS-HORA-INICIO       PIC 9(06).
       77  WS-HORA-FIM          PIC 9(06).
       77  WS-INTERVALO         PIC 9(06).
       77  WS-RESTO             PIC 9(06).
       77  WS-DIAS-INICIO       PIC 9(05) BINARY.
       77  WS-DIAS-FIM          PIC 9(05) BINARY.
       77  WS-NUM-DIAS          PIC S9(05).
       77  WS-HORA-I            PIC 9(06).
       77  WS-HORA-F            PIC 9(06).
      *
       01  WS-HORA              PIC 9(06).
       01  WS-HORA-R REDEFINES WS-HORA.
           03 WS-HH             PIC 99.
           03 WS-MM             PIC 99.
           03 WS-SS             PIC 99.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING WS-DATA-INICIO WS-HORA-INICIO
                                WS-DATA-FIM    WS-HORA-FIM
                                WS-INTERVALO.
       INICIO.
           MOVE WS-HORA-INICIO TO WS-HORA
           COMPUTE WS-HORA-I = WS-HH * 3600 + WS-MM * 60 + WS-SS

      ********** UNISYS Y2K - Linha alterada em 20/03/1999 15:18:44
      *    CALL "ZDSEC IN DTP/L1" USING WS-DATA-INICIO
           CALL "ZDSEC4 IN DTP/L1" USING WS-DATA-INICIO
                        GIVING  WS-DIAS-INICIO
      ********** UNISYS Y2K - Linha alterada em 20/03/1999 15:18:44
      *    CALL "ZDSEC IN DTP/L1" USING WS-DATA-FIM
           CALL "ZDSEC4 IN DTP/L1" USING WS-DATA-FIM
                             GIVING   WS-DIAS-FIM

           COMPUTE WS-NUM-DIAS = WS-DIAS-FIM - WS-DIAS-INICIO

           MOVE WS-HORA-FIM TO WS-HORA
           COMPUTE WS-HORA-F = ((WS-HH * 3600 + WS-MM * 60 + WS-SS) +
                                (WS-NUM-DIAS * 86400))

      *    IF WS-HORA-I > WS-HORA-F
      *       DISPLAY "==> PARAMETROS INVALIDOS"
      *       DISPLAY "==> PERIODO FINAL E MENOR QUE PERIODO INICIAL"
      *       EXIT PROGRAM.

           COMPUTE WS-INTERVALO = (WS-HORA-F - WS-HORA-I)

           MOVE ZEROS TO WS-HORA
           IF WS-INTERVALO NOT = ZEROS
              DIVIDE WS-INTERVALO BY 3600 GIVING WS-HH
                                  REMAINDER WS-RESTO
              IF WS-RESTO NOT = ZEROS
                 DIVIDE WS-RESTO BY 60 GIVING WS-MM
                                 REMAINDER WS-SS.

           MOVE WS-HORA TO WS-INTERVALO

           EXIT PROGRAM.
