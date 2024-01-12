      *
      * UNISYS Y2K - TEAM 2000
      *
      *****************************************************************
      *                                                               *
      *  ARQUIVO DADOS COMPLEMENTARES DE CREDITO EM MEIO ALTERNATIVO  *
      *                                                               *
      *****************************************************************
      *
       01  CP50-REG1.
      *    ---------
           05  CP50-CHAVE.
      *        ----------
             07  CP50-DT-MOV-CREDITO        PIC 9(08) COMP.
      *          FORMATO: AAMMDD
      *
             07  CP50-DT-MOV-CREDITO-R REDEFINES CP50-DT-MOV-CREDITO.
                 10  CP50-DT-MOV-CREDITO-AAMM  PIC 9(06) COMP.
                 10  CP50-DT-MOV-CREDITO-DD    PIC 9(02) COMP.
      *
             07  CP50-NU-NB                 PIC 9(10) COMP.
             07  CP50-CS-NATUR-CREDITO      PIC 9(02) COMP.
             07  CP50-DT-FIM-PERIODO        PIC 9(08) COMP.
      *          FORMATO: AAMMDD
             07  CP50-DT-INI-PERIODO        PIC 9(08) COMP.
      *          FORMATO: AAMMDD
             07  CP50-ID-NIT                PIC 9(10) COMP.
             07  CP50-SEQ-REG               PIC 9(09) COMP.
             07  FILLER                     PIC 9(01) COMP.
           05  CP50-ID-OL-SOLIC-CRED        PIC 9(08) COMP.
           05  CP50-DT-SOLIC-CRED           PIC 9(08) COMP.
      *        FORMATO: AAMMDD
           05  CP50-NU-SEQ-SOLIC-CRED       PIC 9(04) COMP.
           05  CP50-NU-MATRIC-SOLIC         PIC 9(08) COMP.
           05  CP50-CS-MOTIVO-SOLIC         PIC 9(02) COMP.
           05  CP50-NU-NB-ANTERIOR          PIC 9(10) COMP.
           05  CP50-QT-MOT-PENDENCIA        PIC 9(02) COMP.
           05  CP50-CS-MOT-PENDENCIA        PIC 9(02) COMP OC 15 TIMES.
           05  CP50-DT-VALIDACAO-CRED       PIC 9(08) COMP.
      *        FORMATO: AAMMDD
           05  CP50-DT-LIM-VALIDACAO        PIC 9(08) COMP.
      *        FORMATO: AAMMDD
           05  CP50-HR-VALIDACAO-CRED       PIC 9(06) COMP.
           05  CP50-NU-MATR-VALIDACAO       PIC 9(08) COMP.
           05  CP50-CS-GRAU-PENDENCIA       PIC 9(02) COMP.
           05  CP50-ID-FONETICO             PIC 9(12) COMP.
           05  CP50-DT-NASCIMENTO           PIC 9(08) COMP.
      *        FORMATO: AAMMDD
           05  CP50-CS-MUN-NASCIMENTO       PIC 9(06) COMP.
           05  CP50-ID-FONETICO-MAE         PIC 9(12) COMP.
           05  CP50-DT-EVENTO               PIC 9(08) COMP.
      *        FORMATO: AAMMDD
           05  CP50-DT-ENVIO-PRISMA         PIC 9(08) COMP.
      *        FORMATO: AAMMDD
           05  CP50-DT-ENVIO-BANCO          PIC 9(08) COMP.
      *        FORMATO: AAMMDD
           05  CP50-NU-SEQ-REG-LOTE         PIC 9(07) COMP.
           05  CP50-NU-SEQ-REG-CADAST       PIC 9(07) COMP.
           05  CP50-IN-DIVERGE-RUBR         PIC 9(01) COMP.
           05  CP50-IN-ORIG-SOLIC-PAB       PIC 9(01) COMP.
           05  CP50-IN-ERRO-ADMIN           PIC 9(01) COMP.
           05  CP50-IN-CRED-EMPR-CONV       PIC 9(01) COMP.
           05  CP50-IN-CRED-MPG-CONC        PIC 9(01) COMP.
           05  CP50-CS-ORIG-CONTG-PAB       PIC 9(01) COMP.
           05  CP50-NU-MATRIC-REVISOR       PIC 9(08) COMP.
           05  CP50-IN-DADOS-ACAO-JUD       PIC 9(01) COMP.
           05  CP50-CS-DESVIO-CREDITO       PIC 9(02) COMP.
           05  CP50-NU-SEQ-DEP-IRSM         PIC 9(02) COMP.
           05  CP50-NU-SEQ-HERD-IRSM        PIC 9(02) COMP.
           05  CP50-NU-NB-PENSAO            PIC 9(10) COMP.
      *
           05  CP50-IN-PAB-REM-AUT-NPG      PIC 9(01) COMP.
           05  CP50-IN-PAB-NB-CESS          PIC 9(01) COMP.
           05  CP50-IN-ORI-PLAT-BAIXA       PIC 9(01) COMP.
      *
           05  CP50-IN-TEM-SEG-RE-CEN       PIC 9(01).
      *
           05  CP50-NM-RECEBEDOR            PIC X(28).
           05  CP50-NM-SEGURADO             PIC X(28).
           05  CP50-ID-PAIS                 PIC 9(03) COMP.
           05  CP50-QT-MESES-RRA            PIC 9(03) COMP.
           05  CP50-NU-SEQ-SOLI-COMPL       PIC 9(08) COMP.
           05  CP50-D2-SOL-CRED-ORIG        PIC 9(08) COMP.
           05  CP50-NU-SEQ-SOL-ORIG         PIC 9(04) COMP.
           05  CP50-DT-COMP-COR-MON         PIC 9(06) COMP.
           05  CP50-NU-MATR-VAL-ANT         PIC 9(08) COMP.
           05  CP50-HR-VALID-ANT            PIC 9(06) COMP.
           05  CP50-DT-VALID-ANT            PIC 9(08) COMP.
           05  CP50-CS-MOTIVO-CP            PIC 9(02) COMP.
           05  FILLER                       PIC X(30).
      *
