      ******************************************************************
      *              ROTINA PADRAO DE CHECKPOINT C74,C85               *
      ******************************************************************
       5000-CHECKPOINT.
           CALL "CALLCHECKPOINT IN MCPSUPPORT"
                USING  CHECKPOINTDEVICE
                       CHECKPOINTTYPE
                       COMPLETIONCODE
                       CHECKPOINTNUMBER
                       RESTARTFLAG
                GIVING RSLT.
           IF COMPLETIONCODE NOT = ZEROS
              MOVE  COMPLETIONCODE      TO  W77-ALFA-COMPLETIONCODE
              STRING "ERRO = " W77-ALFA-COMPLETIONCODE
                     ". UTILIZE JANELA APOIO;OPCAO 'CHECKP'"
                     DELIMITED  BY SIZE      INTO  W77-MSG-CKP
                     CALL "ZDISPLAY IN DTP/L1"
                                     USING  W77-MSG-CKP
                                            W77-STATUS-BACKUP-CKP.
           IF W77-CKP =  1
              IF RESTARTFLAG = 0  THEN PERFORM 4000-DISPLAY
                                       CHANGE ATTRIBUTE STATUS
                                              OF MYSELF TO -1
              ELSE                     MOVE 0 TO W77-CKP.
           PERFORM 4000-DISPLAY.
       5000-FIM. EXIT.
