      *********************************************************
      * AREA DE TRABALHO PARA ROTINA DE CHECKPOINT  (C74,C85) *
      * CHECKPOINTDEVICE:(17=PACK, 0=DISK)                    *
      * CHECKPOINTTYPE:  (1=LOCK, 0=PURGE)                    *
      *********************************************************
      *
       77 W77-MSG-CKP                   PIC X(132).
       77 W77-STATUS-BACKUP-CKP         PIC 9(011) BINARY VALUE 0.
      *
       77 W77-ALFA-COMPLETIONCODE       PIC X(011).
       77 W77-CKP          PIC  9(01).
        88 FAZ-CHECKPOINT  VALUE  1.
       77 CHECKPOINTDEVICE PIC S9(11) USAGE BINARY VALUE 17.
       77 CHECKPOINTTYPE   PIC S9(11) USAGE BINARY VALUE  1.
       77 COMPLETIONCODE   PIC S9(11) USAGE BINARY.
       77 CHECKPOINTNUMBER PIC S9(11) USAGE BINARY.
       77 RESTARTFLAG      PIC S9(11) USAGE BINARY.
       77 RSLT             PIC S9(11) USAGE BINARY.
      *********************************************************
