      *> 
      *>  UNISYS Y2K - TEAM 2000
      *> 
       
       
       
      *> ********$SET TADS
      *> ========================*
       IDENTIFICATION  DIVISION.
      *> ========================*
       PROGRAM-ID.      PSUBCPG050SPG.
       DATE-WRITTEN. *>CE    15/10/2012.
       DATE-COMPILED.
      *>CE *****************************************************************
      *>CE   OBJETIVO: GERAR UM ARQUIVO COM QTDE/VALOR DE CREDITOS EMITI-  *
      *>CE             DOS POR OP/MEIO-PAGTO/ORIGEM-CREDITO/DT-INI-VALIDA- *
      *>CE             DE/INDICADOR LOAS/ID-PAIS.                          *
      *>CE                                                                 *
      *>CE   HOSTNAME PRODUCAO: DTPRJCV3                                   *
      *>CE   USERCODE PRODUCAO: PDBATBEN                                   *
      *>CE   DIVISAO:           DDCOSUP                                    *
      *>CE   GRAU DIFICULDADE:                                             *
      *>CE *****************************************************************
      *>CE  ALTERACAO EM FEV/MAR 1998 - INCLUSAO DA DATA DE PROVISIONAMENTO*
      *>CE      MARKID "PROVIS"         SERAH GRAVADO UM NOVO ARQUIVO      *
      *>CE                              QUE EH UMA COPIA DO DSUBCPG050,    *
      *>CE                              TENDO INCLUIDO NELE A DATA DE PRO- *
      *>CE                              VISIONAMENTO (OPTOU-SE POR GRAVAR  *
      *>CE                              MAIS UM ARQUIVO PARA NAO MEXER EM  *
      *>CE                              TODA A ROTINA JAH EXISTENTE).      *
      *>CE                              NO INICIO DO PROGRAMA, QUANDO ESTI-*
      *>CE                              VER SENDO PROCESSADA A CONCESSAO,  *
      *>CE                              SERA MONTADA UMA TABELA COM A MENOR*
      *>CE                              DATA DE INICIO DE VALIDADE POR     *
      *>CE                              BANCO. (ESTA DATA SERA UTILIZADA NO*
      *>CE                              CALCULO DA DATA DE PROVISIONAMENTO)*
      *>CE                              ATENCAO: EM 26/03/98, POR SOLICITA-*
      *>CE                              CAO DO INSS, A DATA DE PROVISAO PA-*
      *>CE                              RA AGENC. PIONEIRAS EH 4 DIAS ANTES*
      *>CE                              (AO INVES DE 3)                    *
      *>CE *****************************************************************
      *>CE  ALTERACOES EM AGOSTO/2000 - MARKID EPU-SCF                     *
      *>CE                              . NAO SERAH MAIS GERADO O ARQUIVO  *
      *>CE                              DSUBCPG050 (QUE NAO EH USADO EM    *
      *>CE                              ROTINA ALGUMA)                     *
      *>CE                              . O ARQUIVO DE ENTRADA NAO EH MAIS *
      *>CE                              O DSUBCPG058, MAS O DSUBCPG058, QUE*
      *>CE                              TEM O LAYOUT DO DSUBCPG050, SENDO  *
      *>CE                              QUE OS CAMPOS MICRORREGIAO E MUNI- *
      *>CE                              CIPIO ESTAO ZERADOS E SERAO COMPLE-*
      *>CE                              TADOS NESTE PROGRAMA.              *
      *>CE *****************************************************************
      *>CE 
      *>CE ====================*
       ENVIRONMENT DIVISION.
      *> ====================*
      *> 
        CONFIGURATION SECTION.
      *> ----------------------*
        SOURCE-COMPUTER.   A16.
        OBJECT-COMPUTER.   A18.
        SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
      *> 
        INPUT-OUTPUT SECTION.
      *> ---------------------*
        FILE-CONTROL.
           SELECT DSUBCPG058     ASSIGN TO DISK.
           SELECT DSUBCPG051     ASSIGN TO DISK.
           SELECT DSUBCPG058SPG  ASSIGN TO DISK.
           SELECT DSUBCPG051SPG  ASSIGN TO DISK.
           SELECT WOFIS          ASSIGN TO DISK.
           SELECT WOFIS2         ASSIGN TO DISK.
           SELECT LSUBCPG050A    ASSIGN TO PRINTER.
      *> 
      *> =============*
       DATA DIVISION.
      *> =============*
        FILE SECTION.
      *> -------------*
      *> 
       FD DSUBCPG058
          RECORD CONTAINS  105 CHARACTERS
          BLOCK  CONTAINS  180 RECORDS
          VA OF  TITLE "D/SUB/CPG/058/SPG/SORT ON PKDADOS.".
      *> 
        COPYBOOK   B_SUB_CPG_050_SUL.
      *> ****************************************************************
      *>                                                                *
      *>     LIBRARY DO ARQUIVO QUE CONTEM QTDE/VALOR POR OP            *
      *>     CONTAINS 105 CHARACTERS - BLOCK 180 RECORDS
      *> ****************************************************************
      *> 
       01  DSUBCPG050-REG.
      *> 
           03  DSUBCPG050-NOVO-CHAVE.
               05 CP50-N-ID-ORGAO-PAGADOR     PIC 9(06).
               05 CP50-N-CS-MEIO-PAGTO        PIC 9(02).
               05 CP50-N-CS-ORIGEM-CREDITO    PIC 9(01).
               05 CP50-N-DT-INI-VALIDADE      PIC 9(08).
      *>         05 CP50-N-CS-ESP-LOAS          PIC 9(01).
           03 CP50-FILLER                     PIC 9(01).
           03 CP50-N-ID-BANCO                 PIC 9(03).
           03 CP50-N-ID-MICRO-REG-PREV        PIC 9(06).
      *> --- 08/FEV/2001 -------------------------------
           03 CP50-N-IN-CREDITO-SEAS          PIC 9(01).
           03 CP50-N-IN-CRED-PIONEIRA         PIC 9(01).
      *>      03 FILLER                          PIC 9(03).
      *>     03 CP50-CS-MUNIC-SINPAS          PIC 9(05).
      *>     03 CP50-CS-MUNIC-SINPAS-R REDEFINES CP50-CS-MUNIC-SINPAS.
      *>        05 CP50-CS-UF                 PIC 9(02).
      *>        05 FILLER                     PIC 9(03).
      *> --- 08/FEV/2001 -------------------------------
           03 CP50-N-ID-PREGAO                     PIC 9(02).
           03 CP50-N-ID-LOTE                       PIC 9(02).
           03 CP50-N-CS-ORIG-ORC                   PIC 9(01).
           03 CP50-N-CS-REDE-ECT                   PIC 9(01).
           03 CP50-N-CS-URBANO-RURAL               PIC 9(01).
           03 CP50-N-GR-ESPECIES                   PIC 9(02).
           03 CP50-N-ID-SAL-FAMILIA                PIC 9(01).
           03 CP50-N-ID-PAIS                       PIC 9(03).
           03 CP50-N-QTDE-CREDITOS                 PIC 9(08).
           03 CP50-N-VL-LIQ-CREDITOS               PIC 9(15)V99.
           03 CP50-N-QTDE-IR                       PIC 9(08).
           03 CP50-N-VL-IR                         PIC 9(15)V99.
           03 CP50-N-NU-NB                         PIC 9(10).
           03 CP50-N-CS-ESPECIE                    PIC 9(03).
       
       

        END-COPYBOOK.
      *>    COPY "B/SUB/CPG/050/JCM     ON PKPRG".
        FD DSUBCPG058SPG
           RECORD CONTAINS  1800 CHARACTERS
           BLOCK  CONTAINS    10 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 40000
           VALUE OF TITLE IS
            "D/SUB/CPG/058/SPG/SORT/R275 ON PKDADOS."
           SECURITYTYPE PUBLIC.
       
      *> NOVO LAYOUT REGISTRO DO DSUBCPG058SISPG.
      *> **      COPY "*B/SUB/CPG/058/SISPG  ON PKBDSGF".
        COPYBOOK   B_SUB_CPG_058_SPG_R275.
      *> ****************************************************************
      *>     LIBRARY LAYOUT DO ARQUIVO D/SUB/CPG/058/SPG PARA SUBSTITUIR*
      *>     O SCF COM  O NOVO SISPAGBEN.                               *
      *>     TAMANHO DO REGISTRO 1500 CARACTERES - BLOCO 14 REGISTROS   *
      *> OBS: EM TER 07/05/2013 INCLUIDO SISPG-VALOR-RUB301 NO DETALHE *
      *> ****************************************************************
       
      *> HEDAER SISPG-ID-REG-H = 1
       01  DSUBCPG058-REG-SISPG-HEADER.
           03  SISPG-ID-REG-H           PIC 9(01).
           03  SISPG-ORIG-CRED-H        PIC 9(01).
           03  SISPG-DT-PROC-H          PIC 9(08).
           03  SISPG-ID-CALAMIDADE-H    PIC 9(03).
           03  SISPG-NUM-PORTARIA-H     PIC X(15).
           03  SISPG-DT-PORTARIA-H      PIC 9(08).
           03  SISPG-DT-INI-VALIDADE-H  PIC 9(08).
           03  SISPG-DT-FIM-VALIDADE-H  PIC 9(08).
           03  SISPG-NU-SEQ-ARQ-H       PIC 9(01).
           03  SISPG-ID-ULT-ARQ-H       PIC 9(01).
           03  SISPG-LISTA-RUBR-H       PIC X(288).
           03  SISPG-LISTA-RUBR-H-R REDEFINES
               SISPG-LISTA-RUBR-H.
               05 SISPG-LISTA-RUBRICA-H OCCURS 96 TIMES.
                  07 SISPG-CS-RUBRICA-H PIC 9(03).
           03  FILLER                   PIC X(1458).
       
      *> DETALHE SISPG-ID-REG = 2
       01  DSUBCPG058-REG-SISPG.
           03  SISPG-ID-REG             PIC 9(01).
           03  SISPG-ID-BANCO           PIC 9(03).
           03  SISPG-ID-PAIS            PIC 9(03).
           03  SISPG-IN-CRED-PIONEIRA   PIC 9(01).
           03  SISPG-CS-MEIO-PAGTO      PIC 9(02).
           03  SISPG-DT-INI-VALIDADE    PIC 9(08).
           03  SISPG-CS-ORIG-ORC        PIC 9(01).
           03  SISPG-CS-URBANO-RURAL    PIC 9(01).
           03  SISPG-IN-CRED-EPU        PIC 9(01).
           03  SISPG-COD-CALAMIDADE     PIC 9(03).
           03  SISPG-IN-ADIANTE-13-SAL  PIC 9(01).
           03  SISPG-CS-ESPECIE         PIC 9(02).
           03  SISPG-ENT-CLASS          PIC 9(04).
           03  SISPG-QTD-ENT-CLASS      PIC 9(08).
           03  SISPG-QUANT-CREDITOS     PIC 9(10).
           03  SISPG-VALOR-BRUTO        PIC 9(15)V99.
           03  SISPG-VALOR-LIQUIDO      PIC 9(15)V99.
      *> Ate aqui, 71.79.80.83
           03  SISPG-VALOR-GRUPO-RUBRICAS.
               05  SISPG-VALOR-RUB104   PIC 9(12)V99.
               05  SISPG-VALOR-RUB105   PIC 9(12)V99.
               05  SISPG-VALOR-RUB107   PIC 9(12)V99.
               05  SISPG-VALOR-RUB108   PIC 9(12)V99.
               05  SISPG-VALOR-RUB112   PIC 9(12)V99.
               05  SISPG-VALOR-RUB136   PIC 9(12)V99.
               05  SISPG-VALOR-RUB154   PIC 9(12)V99.
               05  SISPG-VALOR-RUB160   PIC 9(12)V99.
               05  SISPG-VALOR-RUB161   PIC 9(12)V99.
               05  SISPG-VALOR-RUB162   PIC 9(12)V99.
               05  SISPG-VALOR-RUB201   PIC 9(12)V99.
               05  SISPG-VALOR-RUB202   PIC 9(12)V99.
               05  SISPG-VALOR-RUB203   PIC 9(12)V99.
               05  SISPG-VALOR-RUB204   PIC 9(12)V99.
               05  SISPG-VALOR-RUB205   PIC 9(12)V99.
               05  SISPG-VALOR-RUB206   PIC 9(12)V99.
               05  SISPG-VALOR-RUB207   PIC 9(12)V99.
               05  SISPG-VALOR-RUB208   PIC 9(12)V99.
               05  SISPG-VALOR-RUB209   PIC 9(12)V99.
               05  SISPG-VALOR-RUB210   PIC 9(12)V99.
               05  SISPG-VALOR-RUB214   PIC 9(12)V99.
               05  SISPG-VALOR-RUB215   PIC 9(12)V99.
               05  SISPG-VALOR-RUB216   PIC 9(12)V99.
               05  SISPG-VALOR-RUB217   PIC 9(12)V99.
               05  SISPG-VALOR-RUB218   PIC 9(12)V99.
               05  SISPG-VALOR-RUB219   PIC 9(12)V99.
               05  SISPG-VALOR-RUB220   PIC 9(12)V99.
               05  SISPG-VALOR-RUB221   PIC 9(12)V99.
               05  SISPG-VALOR-RUB222   PIC 9(12)V99.
               05  SISPG-VALOR-RUB223   PIC 9(12)V99.
               05  SISPG-VALOR-RUB224   PIC 9(12)V99.
               05  SISPG-VALOR-RUB225   PIC 9(12)V99.
               05  SISPG-VALOR-RUB226   PIC 9(12)V99.
               05  SISPG-VALOR-RUB227   PIC 9(12)V99.
               05  SISPG-VALOR-RUB228   PIC 9(12)V99.
               05  SISPG-VALOR-RUB229   PIC 9(12)V99.
               05  SISPG-VALOR-RUB230   PIC 9(12)V99.
               05  SISPG-VALOR-RUB231   PIC 9(12)V99.
               05  SISPG-VALOR-RUB232   PIC 9(12)V99.
               05  SISPG-VALOR-RUB233   PIC 9(12)V99.
               05  SISPG-VALOR-RUB234   PIC 9(12)V99.
               05  SISPG-VALOR-RUB236   PIC 9(12)V99.
               05  SISPG-VALOR-RUB237   PIC 9(12)V99.
               05  SISPG-VALOR-RUB238   PIC 9(12)V99.
               05  SISPG-VALOR-RUB239   PIC 9(12)V99.
               05  SISPG-VALOR-RUB240   PIC 9(12)V99.
               05  SISPG-VALOR-RUB241   PIC 9(12)V99.
               05  SISPG-VALOR-RUB242   PIC 9(12)V99.
               05  SISPG-VALOR-RUB243   PIC 9(12)V99.
               05  SISPG-VALOR-RUB244   PIC 9(12)V99.
               05  SISPG-VALOR-RUB245   PIC 9(12)V99.
               05  SISPG-VALOR-RUB246   PIC 9(12)V99.
               05  SISPG-VALOR-RUB247   PIC 9(12)V99.
               05  SISPG-VALOR-RUB248   PIC 9(12)V99.
               05  SISPG-VALOR-RUB249   PIC 9(12)V99.
               05  SISPG-VALOR-RUB250   PIC 9(12)V99.
               05  SISPG-VALOR-RUB251   PIC 9(12)V99.
               05  SISPG-VALOR-RUB252   PIC 9(12)V99.
               05  SISPG-VALOR-RUB253   PIC 9(12)V99.
               05  SISPG-VALOR-RUB254   PIC 9(12)V99.
               05  SISPG-VALOR-RUB255   PIC 9(12)V99.
               05  SISPG-VALOR-RUB256   PIC 9(12)V99.
               05  SISPG-VALOR-RUB257   PIC 9(12)V99.
               05  SISPG-VALOR-RUB258   PIC 9(12)V99.
               05  SISPG-VALOR-RUB259   PIC 9(12)V99.
               05  SISPG-VALOR-RUB260   PIC 9(12)V99.
               05  SISPG-VALOR-RUB261   PIC 9(12)V99.
               05  SISPG-VALOR-RUB262   PIC 9(12)V99.
               05  SISPG-VALOR-RUB263   PIC 9(12)V99.
               05  SISPG-VALOR-RUB264   PIC 9(12)V99.
               05  SISPG-VALOR-RUB265   PIC 9(12)V99.
               05  SISPG-VALOR-RUB266   PIC 9(12)V99.
               05  SISPG-VALOR-RUB267   PIC 9(12)V99.
               05  SISPG-VALOR-RUB268   PIC 9(12)V99.
               05  SISPG-VALOR-RUB269   PIC 9(12)V99.
               05  SISPG-VALOR-RUB270   PIC 9(12)V99.
               05  SISPG-VALOR-RUB271   PIC 9(12)V99.
               05  SISPG-VALOR-RUB272   PIC 9(12)V99.
               05  SISPG-VALOR-RUB273   PIC 9(12)V99.
               05  SISPG-VALOR-RUB274   PIC 9(12)V99.
               05  SISPG-VALOR-RUB275   PIC 9(12)V99.
               05  SISPG-VALOR-RUB301   PIC 9(12)V99.
               05  SISPG-VALOR-RUB905   PIC 9(12)V99.
               05  SISPG-VALOR-RUB909   PIC 9(12)V99.
               05  SISPG-VALOR-RUB910   PIC 9(12)V99.
               05  SISPG-VALOR-RUB911   PIC 9(12)V99.
               05  SISPG-VALOR-RUB912   PIC 9(12)V99.
               05  SISPG-VALOR-RUB916   PIC 9(12)V99.
               05  SISPG-VALOR-RUB917   PIC 9(12)V99.
               05  SISPG-VALOR-RUB921   PIC 9(12)V99.
               05  SISPG-VALOR-RUB925   PIC 9(12)V99.
               05  SISPG-VALOR-RUB926   PIC 9(12)V99.
               05  SISPG-VALOR-RUB927   PIC 9(12)V99.
               05  SISPG-VALOR-RUB928   PIC 9(12)V99.
               05  SISPG-VALOR-RUB929   PIC 9(12)V99.
               05  SISPG-VALOR-RUB933   PIC 9(12)V99.
           03  SISPG-VL REDEFINES SISPG-VALOR-GRUPO-RUBRICAS.
               05  SISPG-VL-RUBRICA OCCURS 96    PIC 9(012)V99.
      *> Total das rubricas = 1260.
      *> ABAIXO  05  FILLER  EQUIVALE     PIC X(185).
           03  OUTROS-CAMPOS-SISPG.
               05  SISPG-OCOR-RUB            PIC 9(02).
               05  SISPG-CS-RUB  PIC 9(03) OCCURS 30 TIMES.
               05  SISPG-BRANCO1             PIC X(04).
               05  SISPG-NB                  PIC 9(10).
            03  FILLER                        PIC X(267).
      *> 1323 + 37     03  FILLER                   PIC X(110).
       
      *> TRAILER SISPG-ID-REG-T = 3
       01  DSUBCPG058-REG-SISPG-T.
           03  SISPG-ID-REG-T           PIC 9(01).
           03  SISPG-QTD-REG-T          PIC 9(08).
           03  SISPG-TOT-VL-LIQ-T       PIC 9(15)V99.
           03  FILLER                   PIC X(1774).
      *> FIM DA BOOK BSUBCPG058SPG.

        END-COPYBOOK.
      *>      COPY "B/SUB/CPG/058/SPG/R275/2  ON PKPRG".
       
      *> 
       FD DSUBCPG051
          RECORD CONTAINS  120 CHARACTERS
          BLOCK  CONTAINS  150 RECORDS
          VA OF  TITLE "D/SUB/CPG/051/NOVO ON PKDADOS."
          SECURITYTYPE IS PUBLIC.
        COPYBOOK   B_SUB_CPG_051_SUL.
      *> ****************************************************************
      *>                                                                *
      *>     LIBRARY DO ARQUIVO QUE CONTEM QTDE/VALOR POR OP            *
      *>                                                                *
      *>     TAM. REGISTRO:120    FATOR DE BLOCO: 150  BLOCKSIZE: 36000 *
      *>                                                                *
      *> ****************************************************************
      *> 
       01  DSUBCPG051-REG.
           03 CP51-N-ID-ORGAO-PAGADOR         PIC 9(06).
           03 CP51-N-CS-MEIO-PAGTO            PIC 9(02).
           03 CP51-N-CS-ORIGEM-CREDITO        PIC 9(01).
           03 CP51-N-DT-INI-VALIDADE          PIC 9(08).
           03 CP51-N-DT-PROVISAO              PIC 9(08).
           03 FILLER                          PIC X(01).
           03 CP51-N-CS-AGENCIA-PIONEIRA      PIC 9(01).
           03 CP51-N-ID-BANCO                 PIC 9(03).
           03 CP51-N-ID-MICRO-REG-PREV        PIC 9(06).
           03 CP51-N-CS-MUNIC-SINPAS          PIC 9(05).
           03 CP51-N-CS-MUNIC-SINPAS-R REDEFINES CP51-N-CS-MUNIC-SINPAS.
              05 CP51-N-CS-UF                 PIC 9(02).
              05 FILLER                       PIC 9(03).
           03 CP51-N-ID-PREGAO                PIC 9(02).
           03 CP51-N-ID-LOTE                  PIC 9(02).
           03 CP51-N-CS-ORIG-ORC              PIC 9(01).
           03 CP51-N-CS-REDE-ECT              PIC 9(01).
           03 CP51-N-CS-URBANO-RURAL          PIC 9(01).
           03 CP51-N-GR-ESPECIES              PIC 9(02).
           03 CP51-N-ID-SAL-FAMILIA           PIC 9(01).
           03 CP51-N-ID-PAIS                  PIC 9(03).
           03 CP51-N-QTDE-CREDITOS            PIC 9(08).
           03 CP51-N-VALOR-LIQ-CREDITOS       PIC 9(15)V99.
           03 CP51-N-DT-PROC-CREDITO          PIC 9(08).
           03 CP51-N-IN-CREDITO-SEAS          PIC 9(01).
           03 FILLER                          PIC 9(02).
           03 CP51-N-QTDE-IR                  PIC 9(08).
           03 CP51-N-VL-IR                    PIC 9(15)V99.
           03 FILLER                          PIC X(05).
      *> 
      *>     DEFINICAO DA DT-PROVISAO:
      *> 
      *>     CONCESSAO
      *>     ---------
      *>         OBTER A MENOR DT-INI-VALIDADE DO PROCESSAMENTO DAQUELE
      *>                       BANCO = DT-X
      *>         SE ECV
      *>         ENTAO DESPREZAR DA PROVISAO
      *>         SENAO
      *>           SE ECT
      *>           ENTAO DT-PROVISAO  = DT-X - 4 DIAS UTEIS
      *>           SENAO
      *>              SE AGENCIA-PIONEIRA
      *>              ENTAO DT-PROVISAO = DT-X - 3 DIAS UTEIS
      *>              SENAO
      *>                 CARTAO => DT-PROVISAO = DT-INI-VALID - 1 DIA UTIL
      *>                 C/C    => DT-PROVISAO = DT-INI-VALID
      *> 
      *>     MACICA E PAB
      *>     ------------
      *>         SE ECV
      *>         ENTAO DESPREZAR DA PROVISAO
      *>         SENAO
      *>           SE PAB (MEIO-PAGTO)
      *>           ENTAO
      *>              DT-PROVISAO = DT-INI-VALIDADE
      *>           SENAO
      *>              SE ECT
      *>              ENTAO DT-PROVISAO = 4O.DIA UTIL ANTES DO 1O.DIA UTIL
      *>                                  DO MES DE PAGAMENTO
      *>              SENAO
      *>                  SE AGENCIA-PIONEIRA
      *>                  ENTAO DT-PROVISAO = 3O.DIA UTIL ANTES DO 1O.DIA
      *>                                      UTIL DO MES DE PAGAMENTO
      *>                  SENAO
      *>                     CARTAO => DT-PROVISAO = DT-INI-VALIDADE -
      *>                                             1 DIA UTIL
      *>                     C/C    => DT-PROVISAO = DT-INI-VALIDADE

        END-COPYBOOK.
      *> ** COPY "B/SUB/CPG/051/JCM ON PKPRG".
       
       
        FD DSUBCPG051SPG
           RECORD CONTAINS  1800 CHARACTERS
           BLOCK  CONTAINS    10 RECORDS
           VA OF AREAS IS 1000
           VA OF AREASIZE IS 26000
           VALUE OF TITLE IS "D/SUB/CPG/051/SPG/R275 ON PKDADOS."
           SECURITYTYPE PUBLIC.
       
      *>    COPY "B/SUB/CPG/051/SPG/R275/2  ON PKPRG".
        COPYBOOK   B_SUB_CPG_051_SPG_R275.
      *> NOVO LAYOUT REGISTRO DO DSUBCPG058SISPG.
      *>       COPY "*B/SUB/CPG/051/SPG/R267   ON PKBDSGF".
      *>        COPY "B/SUB/CPG/051/SPG/247  ON PKPRG".
      *>  01  REG-051-SPG  PIC X(1800).
      *> 
      *> ****************************************************************
      *>     LIBRARY LAYOUT DO ARQUIVO D/SUB/CPG/051/SPG   PARA SUBSTITU-
      *>     IR O SCF COM  O NOVO SISPAGBEN.                            *
      *>     TAMANHO DO REGISTRO 1800CARACTERES - BLOCO 10 REGISTROS   *
      *> ****************************************************************
      *> HEDAER SPG51-ID-REG-H = 1
       01  DSUBCPG051-REG-SPG51-HEADER.
           03  SPG51-ID-REG-H           PIC 9(01).
           03  SPG51-ORIGEM-CREDITO     PIC 9(01).
           03  SPG51-DT-PROC-H          PIC 9(08).
           03  SPG51-ID-CALAMIDADE-H    PIC 9(03).
           03  SPG51-NUM-PORTARIA-H     PIC X(15).
           03  SPG51-DT-PORTARIA-H      PIC 9(08).
           03  SPG51-DT-INI-VALIDADE-H  PIC 9(08).
           03  SPG51-DT-FIM-VALIDADE-H  PIC 9(08).
           03  SPG51-NU-SEQ-ARQ-H       PIC 9(01).
           03  SPG51-ID-ULT-ARQ-H       PIC 9(01).
           03  SPG51-LISTA-RUBR-H       PIC X(288).
           03  SPG51-LISTA-RUBR-H-R REDEFINES
               SPG51-LISTA-RUBR-H.
               05 SPG51-LISTA-RUBRICA-H OCCURS 96 TIMES.
                  07 SPG51-CS-RUBRICA-H PIC 9(03).
           03  FILLER                   PIC X(1458).
       
      *> DETALHE SPG51-ID-REG = 2
       01  DSUBCPG051-REG-SPG51.
           03  SPG51-ID-REG             PIC 9(01).
           03  SPG51-ID-BANCO           PIC 9(03).
           03  SPG51-ID-PAIS            PIC 9(03).
           03  SPG51-IN-CRED-PIONEIRA   PIC 9(01).
           03  SPG51-CS-MEIO-PAGTO      PIC 9(02).
           03  SPG51-DT-INI-VALIDADE    PIC 9(08).
           03  SPG51-CS-ORIG-ORC        PIC 9(01).
           03  SPG51-CS-URBANO-RURAL    PIC 9(01).
           03  SPG51-IN-CRED-EPU        PIC 9(01).
           03  SPG51-COD-CALAMIDADE     PIC 9(03).
           03  SPG51-IN-ADIANTE-13-SAL  PIC 9(01).
           03  SPG51-CS-ESPECIE         PIC 9(02).
           03  SPG51-ENT-CLASS          PIC 9(04).
           03  SPG51-QTD-ENT-CLASS      PIC 9(08).
           03  SPG51-QUANT-CREDITOS     PIC 9(10).
           03  SPG51-VALOR-BRUTO        PIC 9(15)V99.
           03  SPG51-VALOR-LIQUIDO      PIC 9(15)V99.
      *> ATE AQUI = 71 - 77 - 83
           03  SPG51-VALOR-GRUPO-RUBRICAS OCCURS 96 TIMES.
               05  SPG51-VL-RUBRICA     PIC 9(015)V99.
           03  FILLER                   PIC X(085).
       
      *> TRAILER SPG51-ID-REG-T = 3
       01  DSUBCPG051-REG-SPG51-TRAILER.
           03  SPG51-ID-REG-T           PIC 9(01).
           03  SPG51-QTD-REG-T          PIC 9(08).
           03  SPG51-TOT-VL-LIQ-T       PIC 9(15)V99.
           03  FILLER                   PIC X(1774).
      *> FIM DA BOOK BSUBCPG058SPG51

        END-COPYBOOK.
      *> 
       FD WOFIS
          RECORD CONTAINS 80 CHARACTERS
          BLOCK  CONTAINS 10 RECORDS
          VALUE  OF TITLE IS WS-OFIS-TITLE.
       01 REG-WOFIS               PIC X(80).
      *> 
       FD WOFIS2
          RECORD CONTAINS 80 CHARACTERS
          BLOCK  CONTAINS 10 RECORDS
          VALUE  OF TITLE IS WS-OFIS-TITLE2.
       01 REG-WOFIS2              PIC X(80).
      *> 
        FD LSUBCPG050A.
        01 LINHA050A              PIC X(133).
      *> 
      *> 
        WORKING-STORAGE SECTION.
      *> ------------------------*
      *> 
       77 WS-CP50-ID-BANCO               PIC 9(03) VA 0.
       77 WS-CP50-ID-ORGAO-PAGADOR       PIC 9(06) VA 0.
       77 WS-CP50-DT-INI-VALIDADE        PIC 9(08) VA 0.
       
       
       77 WS-EOF                         PIC X(03) VALUE "NAO".
       
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
       
       77 WS-PARAM1-ZDISPLAY             PIC X(80).
       77 WS-PARAM2-ZDISPLAY             PIC X(80).
       77 WS-INTEIRO-ZDISPLAY            PIC 9(01) BINARY VALUE 0.
       77 WS-MSG                         PIC X(150).
       77 WS-STATUS-BACKUP               PIC 9(11) BINARY.
       77 W77-ESQUEMA                    PIC 9(05) BINARY.
       77 WS-PAR-MOV-CREDITO             PIC X(04).
       77 WS-PAR-PSTC                    PIC X(02).
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *> 77 W77-DATA                       PIC 9(06) BINARY.
       77 W77-DATA                       PIC 9(08) BINARY.
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *> 77 W77-DATA-FIM                   PIC 9(06) BINARY.
       77 W77-DATA-FIM                   PIC 9(08) BINARY.
       77 W77-HORA                       PIC 9(06) BINARY.
       77 W77-HORA-FIM                   PIC 9(06) BINARY.
       77 WS-ESTIMATIVA                  PIC 9(06).
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *> 77 WS-DATA-INICIO-PROC            PIC 9(06).
       77 WS-DATA-INICIO-PROC            PIC 9(08).
       77 WS-HORA-INICIO-PROC            PIC 9(06).
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *> 77 WS-DATA-FIM-PROC               PIC 9(06).
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
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *> 77 WS-DT-INI-VALIDADE-ANT         PIC 9(06).
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
       
      *> Variaveis do SISPAGBEN.
       77 WS-CALAMIDADE-ANT              PIC 9(03) VALUE ZEROS.
       77 WS-ADIANT-13SAL-ANT            PIC 9(01) VALUE ZEROS.
       77 WS-SISPG-ID-REG-ANT            PIC 9(01) VALUE ZEROS.
       77 WS-VALOR-BRUTO-AUX             PIC 9(15)V99.
       77 WS-IND                         PIC 9(02) VALUE ZEROS.
       
       01  WS-TABELA-RUBRICAS.
           03 WS-TAB-RUBRICAS OCCURS 96 TIMES.
              05  WS-TAB-VL-RUBRICA     PIC 9(015)V99.
              05 FILLER                 PIC 9(001).
       
       
      *> 
      *> Variaveis usadas para o pregao.
       
       77 WS-ID-PREGAO-ANT               PIC 9(02) BINARY.
       77 WS-ID-LOTE-ANT                 PIC 9(02) BINARY.
       77 WS-CS-ORIG-ORCAM-ANT           PIC 9(01) BINARY.
       77 WS-CS-REDE-ECT-ANT             PIC 9(01) BINARY.
       77 WS-CS-URBANO-RURAL-ANT         PIC 9(01) BINARY.
       77 WS-CS-ESPECIE-ANT              PIC 9(02) BINARY.
       77 WS-ID-SAL-FAMILIA-ANT          PIC 9(01) BINARY.
       77 WS-ENT-CLASS-ANT               PIC 9(02) BINARY.
       
      *> Paises do Mercosul
       
       77 WS-ID-PAIS-ANT                 PIC 9(03) BINARY.
       
      *> 
       77 WS-TIPO-MICRO-REG              PIC 9(01).
      *> ***88 WS-AGENCIA-PIONEIRA         VALUE 6.
      *>    88 WS-AGENCIA-PIONEIRA         VALUE 1.
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:11
      *> 77 WS-DT-PROVIS                   PIC 9(06).
       77 WS-DT-PROVIS                   PIC 9(08).
      *> 
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
      *> 
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
      *> 
       01 WS-DT-AAAAMMDDV                PIC 9(08).
       01 WS-DT-AAAAMMDDV-R REDEFINES WS-DT-AAAAMMDDV.
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *>    03 WS-DT-ANO19V                PIC 9(02).
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *>    03 WS-DT-AAMMDDV               PIC 9(06).
          03 WS-DT-AAMMDDV               PIC 9(08).
      *> 
       01 WS-DATA-ACCEPT.
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *>    03 WS-DATA-AC             PIC 9(10) TYPE IS NUMERIC-DATE.
          03 WS-DATA-AC             PIC 9(08).
          03 WS-DATA-AC-R REDEFINES WS-DATA-AC.
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *>       05 WS-MES-AC           PIC 9(02).
             05 WS-ANO-AC           PIC 9(04).
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *>       05 FILLER              PIC X(01).
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *>       05 WS-DIA-AC           PIC 9(02).
             05 WS-MES-AC           PIC 9(02).
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *>       05 FILLER              PIC X(01).
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:12
      *>       05 WS-ANO-AC           PIC 9(04).
             05 WS-DIA-AC           PIC 9(02).
      *> 
       01 WS-DT-PROC                PIC 9(08).
       01 WS-DT-PROC-R REDEFINES WS-DT-PROC.
          03 WS-ANO-DT-PROC         PIC 9(04).
          03 WS-MES-DT-PROC         PIC 9(02).
          03 WS-DIA-DT-PROC         PIC 9(02).
      *> 
       01 WS-TABELA-BANCOS-DT.
          03 WS-DT-INI-VALIDADE-BANCO  OCCURS 999 TIMES PIC 9(08).
      *> 
      *> --------------------------------------------------------------
      *> 
      *> 
        01 CAB-01.
           03 FILLER                PIC X(18) VALUE "MPS/DATAPREV".
           03 FILLER                PIC X(46) VALUE
                   "I N S S - INSTITUTO NACIONAL DO SEGURO SOCIAL".
           03 FILLER                PIC X(06) VALUE "DATA: ".
           03 CAB01-DATA            PIC 99/99/9999.
      *> 
        01 CAB-02.
           03 FILLER              PIC X(28) VALUE "P/SUB/CPG/050/R275".
           03 FILLER                PIC X(40) VALUE
              "SISTEMA  UNICO  DE  BENEFICIO".
           03 FILLER                PIC X(06) VALUE "HORA: ".
           03 CAB02-HORA            PIC 99I:99I:99.
      *> 
        01 CAB-031.
           03 FILLER                PIC X(15) VALUE "LSUBCPG050A".
           03 FILLER                PIC X(02) VALUE SPACES.
           03 FILLER                PIC X(52) VALUE
              "***    GERACAO DE ARQ. REDUZIDO PARA O DARDO    ***".
           03 FILLER                PIC X(07) VALUE "PAG.: ".
           03 CAB031-PAGINA         PIC ZZZ.ZZ9.
      *> 
        01 CAB-031A.
           03 FILLER                PIC X(80) VALUE ALL "=".
      *> 
        01 CAB-041.
           03 FILLER                PIC X(34) VALUE
                                "TEMPO DE PROCESSAMENTO (HH:MM:SS):".
           03 FILLER                PIC X(18) VALUE
                                "       PARAMETROS:".
      *> 
        01 CAB-041A.
           03 FILLER                PIC X(35) VALUE ALL "=".
           03 FILLER                PIC X(18) VALUE
                                "   ===============".
      *> 
        01 CAB-050.
           03 FILLER                PIC X(27) VALUE
                                       "INICIO DO PROCESSAMENTO  : ".
           03 CAB05-HORA-INI        PIC 99I:99I:99.
           03 FILLER                PIC X(13) VALUE
                                       "   NUM.ESQ.: ".
           03 CAB05-NUM-ESQ         PIC 9(05).
      *> 
        01 CAB-061.
           03 FILLER                PIC X(27) VALUE
                                       "TERMINO DO PROCESSAMENTO : ".
           03 CAB06-HORA-FIM        PIC 99I:99I:99.
           03 FILLER                PIC X(13) VALUE  " PARAM PROC: ".
           03 CAB06-PARAM           PIC X(04).
      *> 
        01 CAB-071.
           03 FILLER                PIC X(27) VALUE
                                      "ESTIMATIVA DE TEMPO GASTO: ".
           03 CAB07-TEMPO-GASTO     PIC 99I:99I:99.
      *> 
        01 DET1.
           03 TEXTO-DET1            PIC X(46).
           03 FILLER                PIC X(02) VALUE ": ".
           03 QT-DET1               PIC ZZ.ZZZ.ZZ9.
           03 QT-DET1-R REDEFINES QT-DET1 PIC X(10).
           03 VL-DET1               PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
           03 VL-DET1-R REDEFINES VL-DET1 PIC X(22).
      *> 
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
      *> 
        COPYBOOK   B_SDC_LBD_SETTB00233.
      *> *****************************************************************
      *>  COPY-BOOK:    B/SDC/LBD/SETTB00233     ON PKBDSGF              *
      *>  ENTRY-POINT:  SETTB00233                                       *
      *> *****************************************************************
       77 RETORNO-TB00233        PIC 9(01) BINARY.
       01 FONTE-LAYOUT-TB00233.
          03 ITEM-TB00233-1      PIC X(17) VALUE "TB0023           ".
          03 ITEM-TB00233-2      PIC X(17) VALUE "NU-AAP           ".
          03 TAM-TB00233-2       PIC 9(06) VALUE       6.
          03 ITEM-TB00233-3      PIC X(17) VALUE "ID-AAP           ".
          03 TAM-TB00233-3       PIC 9(06) VALUE       6.
          03 ITEM-TB00233-4      PIC X(17) VALUE "CS-CATE-AAP      ".
          03 TAM-TB00233-4       PIC 9(06) VALUE       2.
          03 ITEM-TB00233-5      PIC X(17) VALUE "ID-APS           ".
          03 TAM-TB00233-5       PIC 9(06) VALUE       8.
          03 ITEM-TB00233-6      PIC X(17) VALUE "ID-MICRO-REG-PREV".
          03 TAM-TB00233-6       PIC 9(06) VALUE       6.
          03 ITEM-TB00233-7      PIC X(17) VALUE "ID-INSTIT-AAP    ".
          03 TAM-TB00233-7       PIC 9(06) VALUE       4.
          03 ITEM-TB00233-8      PIC X(17) VALUE "ID-GRSS          ".
          03 TAM-TB00233-8       PIC 9(06) VALUE       5.
          03 ITEM-TB00233-9      PIC X(17) VALUE "ID-PSS           ".
          03 TAM-TB00233-9       PIC 9(06) VALUE       8.
          03 ITEM-TB00233-10     PIC X(17) VALUE "NU-CAR           ".
          03 TAM-TB00233-10      PIC 9(06) VALUE       9.
          03 ITEM-TB00233-11     PIC X(17) VALUE "ID-PAF           ".
          03 TAM-TB00233-11      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-12     PIC X(17) VALUE "CS-SITUACAO-AAP  ".
          03 TAM-TB00233-12      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-13     PIC X(17) VALUE "CS-PORTE-LOTERICO".
          03 TAM-TB00233-13      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-14     PIC X(17) VALUE "DT-TRANSF-AAP    ".
          03 TAM-TB00233-14      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-15     PIC X(17) VALUE "CS-MUNI-SINPAS   ".
          03 TAM-TB00233-15      PIC 9(06) VALUE       5.
          03 ITEM-TB00233-16     PIC X(17) VALUE "NM-UF-SIGL       ".
          03 TAM-TB00233-16      PIC 9(06) VALUE       2.
          03 ITEM-TB00233-17     PIC X(17) VALUE "QT-BENEFICIO-AAP ".
          03 TAM-TB00233-17      PIC 9(06) VALUE       6.
          03 ITEM-TB00233-18     PIC X(17) VALUE "NM-AAP           ".
          03 TAM-TB00233-18      PIC 9(06) VALUE      40.
          03 ITEM-TB00233-19     PIC X(17) VALUE "TE-ENDERECO      ".
          03 TAM-TB00233-19      PIC 9(06) VALUE      55.
          03 ITEM-TB00233-20     PIC X(17) VALUE "NM-BAIRRO        ".
          03 TAM-TB00233-20      PIC 9(06) VALUE      27.
          03 ITEM-TB00233-21     PIC X(17) VALUE "NU-CEP           ".
          03 TAM-TB00233-21      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-22     PIC X(17) VALUE "IN-ATIVO-PREGAO  ".
          03 TAM-TB00233-22      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-23     PIC X(17) VALUE "IN-ATIVO-PREGAO-2".
          03 TAM-TB00233-23      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-24     PIC X(17) VALUE "IN-ATIVO-PREGAO-3".
          03 TAM-TB00233-24      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-25     PIC X(17) VALUE "ID-GRAF          ".
          03 TAM-TB00233-25      PIC 9(06) VALUE       5.
          03 ITEM-TB00233-26     PIC X(17) VALUE "IN-PAGA          ".
          03 TAM-TB00233-26      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-27     PIC X(17) VALUE "IN-ARRC          ".
          03 TAM-TB00233-27      PIC 9(06) VALUE       1.
          03 ITEM-TB00233-28     PIC X(17) VALUE "DT-COMANDO-EXCL  ".
          03 TAM-TB00233-28      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-29     PIC X(17) VALUE "DT-ABERTURA-AAP  ".
          03 TAM-TB00233-29      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-30     PIC X(17) VALUE "DT-FECHAMENTO-AAP".
          03 TAM-TB00233-30      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-31     PIC X(17) VALUE "ID-AAP-DESTINO   ".
          03 TAM-TB00233-31      PIC 9(06) VALUE       6.
          03 ITEM-TB00233-32     PIC X(17) VALUE "ID-MICRO-DESTINO ".
          03 TAM-TB00233-32      PIC 9(06) VALUE       6.
          03 ITEM-TB00233-33     PIC X(17) VALUE "ID-GESTOR-ATU    ".
          03 TAM-TB00233-33      PIC 9(06) VALUE      17.
          03 ITEM-TB00233-34     PIC X(17) VALUE "ID-GESTOR-EXC    ".
          03 TAM-TB00233-34      PIC 9(06) VALUE      17.
          03 ITEM-TB00233-35     PIC X(17) VALUE "DT-INIC-VALI     ".
          03 TAM-TB00233-35      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-36     PIC X(17) VALUE "DT-FIM-VALI      ".
          03 TAM-TB00233-36      PIC 9(06) VALUE       8.
          03 ITEM-TB00233-37     PIC X(17) VALUE "DT-EFET-COTE     ".
          03 TAM-TB00233-37      PIC 9(06) VALUE       8.
          03 DELIMITADOR-TB00233    PIC X(01) VALUE "#".
      *> 
       01 CODE-LAYOUT-TB00233       PIC X(01).
      *> 
       01 REGISTRO-TB00233.
          03 TB00233-CHAVE-GRUPO.
             05 CHAVE-ID-AAP            PIC 9(006).
          03 TB00233-CHAVE              REDEFINES
             TB00233-CHAVE-GRUPO        PIC X(006).
          03 FILLER                     PIC X(313).
      *> 
       01 REGISTRO-TB00233-R  REDEFINES  REGISTRO-TB00233.
          03 NU-AAP                     PIC 9(006).
          03 ID-AAP                     PIC 9(006).
          03 CS-CATE-AAP                PIC 9(002).
          03 ID-APS                     PIC 9(008).
          03 ID-MICRO-REG-PREV          PIC 9(006).
          03 ID-INSTIT-AAP              PIC 9(004).
          03 ID-GRSS                    PIC 9(005).
          03 ID-PSS                     PIC 9(008).
          03 NU-CAR                     PIC 9(009).
          03 ID-PAF                     PIC 9(008).
          03 CS-SITUACAO-AAP            PIC 9(001).
          03 CS-PORTE-LOTERICO          PIC X(001).
          03 DT-TRANSF-AAP              PIC 9(008).
          03 CS-MUNI-SINPAS             PIC 9(005).
          03 NM-UF-SIGL                 PIC X(002).
          03 QT-BENEFICIO-AAP           PIC 9(006).
          03 NM-AAP                     PIC X(040).
          03 TE-ENDERECO                PIC X(055).
          03 NM-BAIRRO                  PIC X(027).
          03 NU-CEP                     PIC 9(008).
          03 IN-ATIVO-PREGAO            PIC 9(001).
      *>    FALSO = 0  E  VERDADEIRO = 1
          03 IN-ATIVO-PREGAO-2          PIC 9(001).
      *>    FALSO = 0  E  VERDADEIRO = 1
          03 IN-ATIVO-PREGAO-3          PIC 9(001).
      *>    FALSO = 0  E  VERDADEIRO = 1
          03 ID-GRAF                    PIC 9(005).
          03 IN-PAGA                    PIC 9(001).
      *>    FALSO = 0  E  VERDADEIRO = 1
          03 IN-ARRC                    PIC 9(001).
      *>    FALSO = 0  E  VERDADEIRO = 1
          03 DT-COMANDO-EXCL            PIC 9(008).
          03 DT-ABERTURA-AAP            PIC 9(008).
          03 DT-FECHAMENTO-AAP          PIC 9(008).
          03 ID-AAP-DESTINO             PIC 9(006).
          03 ID-MICRO-DESTINO           PIC 9(006).
          03 ID-GESTOR-ATU              PIC X(017).
          03 ID-GESTOR-EXC              PIC X(017).
          03 DT-INIC-VALI               PIC 9(008).
          03 DT-FIM-VALI                PIC 9(008).
          03 DT-EFET-COTE               PIC 9(008).

        END-COPYBOOK.
      *> **  COPY "*B/SDC/LBD/SETTB00494 ON PKBDSGF".
      *> **  COPY "*B/SDC/LBD/SETTB00073 ON PKBDSGF".
        COPYBOOK   B_DTP_PAD_01_CHECKPOINT_WS.
      *> ********************************************************
      *>  AREA DE TRABALHO PARA ROTINA DE CHECKPOINT  (C74,C85) *
      *>  CHECKPOINTDEVICE:(17=PACK, 0=DISK)                    *
      *>  CHECKPOINTTYPE:  (1=LOCK, 0=PURGE)                    *
      *> ********************************************************
      *> 
       77 W77-MSG-CKP                   PIC X(132).
       77 W77-STATUS-BACKUP-CKP         PIC 9(011) BINARY VALUE 0.
      *> 
       77 W77-ALFA-COMPLETIONCODE       PIC X(011).
       77 W77-CKP          PIC  9(01).
        88 FAZ-CHECKPOINT  VALUE  1.
       77 CHECKPOINTDEVICE PIC S9(11) USAGE BINARY VALUE 17.
       77 CHECKPOINTTYPE   PIC S9(11) USAGE BINARY VALUE  1.
       77 COMPLETIONCODE   PIC S9(11) USAGE BINARY.
       77 CHECKPOINTNUMBER PIC S9(11) USAGE BINARY.
       77 RESTARTFLAG      PIC S9(11) USAGE BINARY.
       77 RSLT             PIC S9(11) USAGE BINARY.
      *> ********************************************************

        END-COPYBOOK.
      *> 
      *> 
      *> =================================================================
       PROCEDURE DIVISION
             USING  W77-ESQUEMA WS-PAR-MOV-CREDITO
                                      WS-PAR-PSTC W77-OFIS-CLASSE.
      *> =================================================================
      *> 
       DECLARATIVES.
      *> 
       DE-INTERRUPT SECTION.
           USE INTERRUPT PROCEDURE.
      *> 
       DE-INTERROMPE.
      *> 
      *> ****DISPLAY "LIDOS: " WS-REGS-LIDOS " GRAVS: " WS-REGS-GRAVADOS.
      *> 
       END DECLARATIVES.
      *> 
      *> 
      *> ****************************************************************
       PRINCIPAL SECTION.
      *> -----------------*
       INICIO.
      *> 
      *>    MOVE 12345  TO W77-ESQUEMA.
      *>    MOVE "MANU" TO WS-PAR-MOV-CREDITO.
      *>    MOVE "M03"  TO WS-PAR-PSTC.
      *>    MOVE 309    TO W77-OFIS-CLASSE.
        COPYBOOK   B_DTP_PAD_01_CHECKPOINT_PD1.
           CHANGE ATTRIBUTE LIBACCESS OF " MCPSUPPORT" TO BYFUNCTION.
           CHANGE ATTRIBUTE FUNCTIONNAME OF " MCPSUPPORT"
                                                    TO " MCPSUPPORT".
           PERFORM 5000-CHECKPOINT.

        END-COPYBOOK.
       
           ATTACH DE-INTERRUPT TO ATTRIBUTE EXCEPTIONEVENT OF MYSELF.
      *> 
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
      *> 
           PERFORM  1000-ABERTURA
              THRU  1000-FIM.
      *> 
           PERFORM 9000-LE-CPG058
              THRU 9000-FIM.
       
           PERFORM 10000-GRAVA-HEADER
               THRU 10000-FIM.
      *> 
           PERFORM 9000-LE-CPG058
              THRU 9000-FIM.
      *> 
           PERFORM 2000-LOOP
              THRU 2000-FIM
             UNTIL WS-EOF EQUAL "SIM".
      *> 
           PERFORM 11000-GRAVA-TRAILER
               THRU 11000-FIM.
      *> 
       
       
           PERFORM 3000-EMITIR-RELATORIO
              THRU 3000-FIM.
      *> 
           PERFORM 4000-FINALIZA
              THRU 4000-FIM.
      *> 
           IF WS-TESTE-CANCELAR-PROGRAMA = 1
              MOVE "ATENCAO !!! MEIOS DE PAGTO INVALIDOS "
                TO  WS-PARAM1-ZDISPLAY
              CALL "ZDISPLAY IN DTP/L1" USING WS-PARAM1-ZDISPLAY, 
                                              WS-INTEIRO-ZDISPLAY
              SET MYSELF (STATUS) TO -1.
      *> 
           STOP  RUN.
      *> 
      *> 
       0500-MONTA-TABELA.
      *> -----------------*
      *> 
          IF SISPG-ID-BANCO = 0
             ADD 1 TO WS-REGS-DESP-BANCO-0
             GO 0500-LE-PROXIMO.
      *> 
          IF WS-DT-INI-VALIDADE-BANCO ( SISPG-ID-BANCO ) = 0
             MOVE SISPG-DT-INI-VALIDADE  TO
             WS-DT-INI-VALIDADE-BANCO ( SISPG-ID-BANCO )
          ELSE
             IF WS-DT-INI-VALIDADE-BANCO ( SISPG-ID-BANCO ) >
                SISPG-DT-INI-VALIDADE
                MOVE SISPG-DT-INI-VALIDADE  TO
                WS-DT-INI-VALIDADE-BANCO ( SISPG-ID-BANCO ).
      *> 
       0500-LE-PROXIMO.
      *> ---------------*
      *> 
           PERFORM 9000-LE-CPG058
              THRU 9000-FIM.
      *> 
       0500-FIM.  EXIT.
      *> --------*
      *> 
      *> 
       1000-ABERTURA.
      *> -------------*
      *> 
           SET DSUBCPG058SPG(FILETYPE) TO 7.
           OPEN INPUT  DSUBCPG058SPG.
           CHANGE ATTRIBUTE SECURITYTYPE OF DSUBCPG051SPG TO PUBLIC.
            OPEN OUTPUT DSUBCPG051SPG.
      *> 
      *     CALL "LAYOUT IN DTP/LBD/TAB" USING   FONTE-LAYOUT-TB00233,
      *                                           CODE-LAYOUT-TB00233
      *                                  GIVING       RETORNO-TB00233
           IF RETORNO-TB00233 = 2
              MOVE "ERRO ABERTURA TB00233" TO WS-PARAM1-ZDISPLAY
              CALL "ZDISPLAY IN DTP/L1" USING WS-PARAM1-ZDISPLAY, 
                                              WS-INTEIRO-ZDISPLAY
              SET MYSELF(STATUS) TO -1.
       
      *> 
           CALL "ZDATA4 IN DTP/L1" GIVING W77-DATA.
           MOVE W77-DATA    TO  WS-DATA-INICIO-PROC.
      *> 
           CALL "ZHORA IN DTP/L1" GIVING W77-HORA.
      *> 
           MOVE W77-HORA  TO  WS-HORA-INICIO-PROC.
      *> 
           ACCEPT WS-DATA-AC FROM DATE YYYYMMDD.
           MOVE   WS-ANO-AC  TO   WS-ANO-DT-PROC.
           MOVE   WS-MES-AC  TO   WS-MES-DT-PROC.
           MOVE   WS-DIA-AC  TO   WS-DIA-DT-PROC.
       
       1000-FIM. EXIT.
      *> --------*
      *> 
       2000-LOOP.
      *> ----------*
       
           IF  SISPG-ID-REG-H NOT EQUAL 2
               PERFORM 9000-LE-CPG058
                  THRU 9000-FIM.
           MOVE SISPG-ID-REG-H TO WS-SISPG-ID-REG-ANT.
           MOVE SISPG-IN-CRED-PIONEIRA TO WS-IN-PION-ANT.
           PERFORM 2200-LOOP-PIONEIRA
               THRU 2200-FIM
             UNTIL WS-EOF EQUAL "SIM"
                  OR SISPG-ID-REG-H
                  NOT EQUAL WS-SISPG-ID-REG-ANT
                  OR SISPG-IN-CRED-PIONEIRA
                  NOT EQUAL WS-IN-PION-ANT.
       
      *> 
       2000-FIM. EXIT.
      *> --------*
       
      *> 
       
       2200-LOOP-PIONEIRA.
      *> -------------------*
      *> 
           MOVE SISPG-ID-BANCO TO WS-BANCO-ANT.
       
           PERFORM 2250-LOOP-ID-BANCO
              THRU 2250-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT.
       
      *> 
       2200-FIM. EXIT.
      *> ---------------*
      *> 
       2250-LOOP-ID-BANCO.
      *> -------------------*
      *> 
           MOVE SISPG-CS-MEIO-PAGTO TO WS-CS-MEIO-PAGTO-ANT.
       
           PERFORM 2300-LOOP-MEIO-PAGTO
              THRU 2300-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SISPG-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT.
      *> 
       2250-FIM. EXIT.
      *> --------*
      *> 
       2300-LOOP-MEIO-PAGTO.
      *> --------------------*
      *> 
           MOVE SISPG-DT-INI-VALIDADE TO WS-DT-INI-VALIDADE-ANT.
       
           PERFORM 2420-LOOP-DT-INI-VALIDADE
              THRU 2420-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SISPG-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SISPG-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT.
      *> 
       2300-FIM. EXIT.
      *> ---------------*
      *> 
       2420-LOOP-DT-INI-VALIDADE.
      *> -------------------------*
           MOVE SISPG-CS-ORIG-ORC TO  WS-CS-ORIG-ORCAM-ANT.
       
           PERFORM 2430-LOOP-CS-ORIG-ORCAM
              THRU 2430-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SISPG-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SISPG-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SISPG-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT.
      *> 
       2420-FIM. EXIT.
      *> ---------------*
      *> 
       2430-LOOP-CS-ORIG-ORCAM.
      *> -------------------------*
       
           MOVE SISPG-IN-CRED-EPU TO  WS-CS-REDE-ECT-ANT.
       
           PERFORM 2440-LOOP-CS-REDE-ECT
              THRU 2440-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SISPG-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SISPG-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SISPG-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SISPG-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT.
      *> 
       2430-FIM. EXIT.
      *> ---------------*
      *> 
       2440-LOOP-CS-REDE-ECT.
      *> -------------------------*
       
           MOVE SISPG-CS-URBANO-RURAL TO  WS-CS-URBANO-RURAL-ANT.
       
           PERFORM 2450-LOOP-CS-URBANO-RURAL
              THRU 2450-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SISPG-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SISPG-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SISPG-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SISPG-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SISPG-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT.
       
      *> 
       2440-FIM. EXIT.
      *> ---------------*
      *> 
       2450-LOOP-CS-URBANO-RURAL.
      *> -------------------------*
       
           MOVE SISPG-CS-ESPECIE TO  WS-CS-ESPECIE-ANT.
       
           PERFORM 2460-LOOP-CS-ESPECIE
              THRU 2460-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SISPG-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SISPG-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SISPG-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SISPG-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SISPG-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SISPG-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT.
       
      *> 
       2450-FIM. EXIT.
      *> --------*
      *> 
      *> 
       2460-LOOP-CS-ESPECIE.
      *> -------------------------*
       
           MOVE SISPG-ID-PAIS TO  WS-ID-PAIS-ANT.
      *> 
           PERFORM 2550-LOOP-ID-PAIS
              THRU 2550-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SISPG-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SISPG-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SISPG-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SISPG-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SISPG-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SISPG-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT
                   OR SISPG-ID-PAIS
                   NOT EQUAL WS-ID-PAIS-ANT.
       
       2460-FIM. EXIT.
      *> ---------------*
      *> 
       2550-LOOP-ID-PAIS.
      *> -------------------------*
       
           MOVE SISPG-COD-CALAMIDADE TO  WS-CALAMIDADE-ANT.
      *> 
           PERFORM 2600-LOOP-CALAMIDADE
              THRU 2600-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SISPG-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SISPG-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SISPG-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SISPG-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SISPG-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SISPG-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT
                   OR SISPG-ID-PAIS
                   NOT EQUAL WS-ID-PAIS-ANT
                   OR SISPG-COD-CALAMIDADE
                   NOT EQUAL WS-CALAMIDADE-ANT.
       
       2550-FIM. EXIT.
      *> ---------------*
      *> 
      *> 
       2600-LOOP-CALAMIDADE.
      *> -------------------------*
       
           MOVE SISPG-IN-ADIANTE-13-SAL TO  WS-ADIANT-13SAL-ANT.
      *> 
           PERFORM 2610-LOOP-13-SAL
              THRU 2610-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SISPG-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SISPG-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SISPG-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SISPG-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SISPG-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SISPG-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT
                   OR SISPG-ID-PAIS
                   NOT EQUAL WS-ID-PAIS-ANT
                   OR SISPG-COD-CALAMIDADE
                   NOT EQUAL WS-CALAMIDADE-ANT
                   OR SISPG-IN-ADIANTE-13-SAL
                   NOT EQUAL WS-ADIANT-13SAL-ANT.
       
       2600-FIM. EXIT.
      *> ---------------*
       
       2610-LOOP-13-SAL.
      *> ----------------------*
      *> 
           MOVE SISPG-ENT-CLASS TO WS-ENT-CLASS-ANT.
       
           PERFORM 2620-LOOP-ENTIDADES
              THRU 2620-FIM-ENT
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SISPG-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SISPG-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SISPG-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SISPG-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SISPG-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SISPG-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT
                   OR SISPG-ID-PAIS
                   NOT EQUAL WS-ID-PAIS-ANT
                   OR SISPG-COD-CALAMIDADE
                   NOT EQUAL WS-CALAMIDADE-ANT
                   OR SISPG-IN-ADIANTE-13-SAL
                   NOT EQUAL WS-ADIANT-13SAL-ANT
                   OR SISPG-ENT-CLASS
                   NOT EQUAL WS-ENT-CLASS-ANT.
      *> 
       
      *> 
       2610-FIM. EXIT.
      *> --------*
       
       2620-LOOP-ENTIDADES.
      *> ----------------------*
       
           MOVE 0 TO WS-DT-PROVIS.
       
           MOVE ZEROS            TO WS-QTDE-AUX WS-QTDE-IR-AUX
                                  WS-VALOR-BRUTO-AUX   WS-VALOR-AUX
                                  WS-QTDE-ENT-AUX      WS-VL-ENT-AUX.
       
           PERFORM 6000-ZERA-RUBRICAS THRU 6000-FIM
                   VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 96.
      *> 
           PERFORM 2620-LOOP-ESP-LOAS
              THRU 2620-FIM
             UNTIL WS-EOF EQUAL "SIM"
                   OR SISPG-ID-REG-H
                   NOT EQUAL WS-SISPG-ID-REG-ANT
                   OR SISPG-IN-CRED-PIONEIRA
                   NOT EQUAL WS-IN-PION-ANT
                   OR SISPG-ID-BANCO
                   NOT EQUAL WS-BANCO-ANT
                   OR SISPG-CS-MEIO-PAGTO
                   NOT EQUAL WS-CS-MEIO-PAGTO-ANT
                   OR SISPG-DT-INI-VALIDADE
                   NOT EQUAL WS-DT-INI-VALIDADE-ANT
                   OR SISPG-CS-ORIG-ORC
                   NOT EQUAL WS-CS-ORIG-ORCAM-ANT
                   OR SISPG-IN-CRED-EPU
                   NOT EQUAL WS-CS-REDE-ECT-ANT
                   OR SISPG-CS-URBANO-RURAL
                   NOT EQUAL WS-CS-URBANO-RURAL-ANT
                   OR SISPG-CS-ESPECIE
                   NOT EQUAL WS-CS-ESPECIE-ANT
                   OR SISPG-ID-PAIS
                   NOT EQUAL WS-ID-PAIS-ANT
                   OR SISPG-COD-CALAMIDADE
                   NOT EQUAL WS-CALAMIDADE-ANT
                   OR SISPG-IN-ADIANTE-13-SAL
                   NOT EQUAL WS-ADIANT-13SAL-ANT
                   OR SISPG-ENT-CLASS
                   NOT EQUAL WS-ENT-CLASS-ANT.
      *> 
           PERFORM 2630-GRAVAR-SAIDAS
              THRU 2630-FIM.
      *> 
       2620-FIM-ENT.
               EXIT.
      *> --------------*
       
       
       2620-LOOP-ESP-LOAS.
      *> ------------------*
      *> 
           ADD  1 TO WS-REGS-LIDOS
                     WS-REGS-CHECKPOINT.
      *> 
       
           IF WS-REGS-CHECKPOINT > 1000000
              DETACH DE-INTERRUPT
              PERFORM 5000-CHECKPOINT
              ATTACH DE-INTERRUPT TO ATTRIBUTE EXCEPTIONEVENT OF MYSELF
              MOVE 0 TO WS-REGS-CHECKPOINT.
       
           ADD  SISPG-QUANT-CREDITOS      TO WS-QTDE-AUX
                                           WS-QTDE-TOTAL.
           ADD  SISPG-VALOR-LIQUIDO TO WS-VALOR-AUX
                                           WS-VALOR-TOTAL.
       
           ADD  SISPG-VALOR-BRUTO   TO  WS-VALOR-BRUTO-AUX.
       
           ADD SISPG-QTD-ENT-CLASS TO WS-QTDE-ENT-AUX.
       
           PERFORM 7000-SOMA-RUBRICAS THRU 7000-FIM
                    VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 96.
       
      *> 
           PERFORM 9000-LE-CPG058
              THRU 9000-FIM.
      *> 
       2620-FIM.  EXIT.
      *> --------*
      *> 
      *> 
       2630-GRAVAR-SAIDAS.
      *> ------------------*
      *> 
      *>    GRAVANDO O DSUBCPG051 => DARAH ORIGEM AO COFRE2
      *> 
           IF WS-BANCO-ANT = 998 OR 996
              GO 2630-FIM.
      *> 
      *>      MOVE  ALL ZEROS                TO DSUBCPG051-REG-SPG51.
       
       
      *> *Moves DETALHE.
       
           MOVE  WS-SISPG-ID-REG-ANT      TO SPG51-ID-REG.
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
                   VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 96.
       
           WRITE DSUBCPG051-REG-SPG51.
       
       
           ADD   1 TO WS-REGS-GRAVADOS-051.
      *> 
       2630-FIM. EXIT.
      *> --------*
      *> 
      *> 
       3000-EMITIR-RELATORIO.
      *> ---------------------*
      *> 
           OPEN OUTPUT LSUBCPG050A.
           OPEN OUTPUT WOFIS.
      *> 
           CALL  "ZDATA4  IN DTP/L1" GIVING W77-DATA.
      *> 
           CALL  "ZHORA  IN DTP/L1" GIVING W77-HORA.
      *> 
           MOVE W77-DATA      TO  WS-DATA-FIM-PROC.
           MOVE W77-HORA      TO  WS-HORA-FIM-PROC.
      *> 
      *> ********* UNISYS Y2K - Linha alterada em 27/04/1999 15:47:14
      *>      CALL "P/SUB/LIB/601" USING  WS-DATA-INICIO-PROC
            CALL "P/SUB/LIB/2K/601" USING  WS-DATA-INICIO-PROC
                                        WS-HORA-INICIO-PROC
                                        WS-DATA-FIM-PROC
                                        WS-HORA-FIM-PROC
                                        WS-ESTIMATIVA.
      *> 
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
      *> 
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
      *> 
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
      *> 
      *>     LINHAS DE DETALHE
      *> 
           MOVE   SPACES TO VL-DET1-R.
           MOVE  "REGS LIDOS DSUBCPG058                " TO TEXTO-DET1.
           MOVE  WS-REGS-LIDOS                           TO QT-DET1.
           WRITE LINHA050A FROM DET1                        AFTER 2.
           WRITE REG-WOFIS FROM DET1.
      *> 
           MOVE  "REGS GRAVADOS NO DSUBCPG051          " TO TEXTO-DET1.
           MOVE   WS-REGS-GRAVADOS-051                   TO QT-DET1.
           WRITE  LINHA050A FROM DET1                       AFTER 1.
           WRITE REG-WOFIS FROM DET1.
      *> 
           MOVE  "TOTAL DE BENEFICIOS                  " TO TEXTO-DET1.
           MOVE  WS-QTDE-TOTAL                           TO QT-DET1.
           WRITE LINHA050A FROM DET1                        AFTER 2.
           WRITE REG-WOFIS FROM DET1.
      *> 
           MOVE   SPACES TO QT-DET1-R.
           MOVE  "TOTAL DE VALOR LIQUIDO               " TO TEXTO-DET1.
           MOVE   WS-VALOR-TOTAL                         TO VL-DET1.
           WRITE  LINHA050A FROM DET1                       AFTER 1.
           WRITE REG-WOFIS FROM DET1.
      *> 
           MOVE    SPACES TO VL-DET1-R.
           MOVE   "PESQS NA TB0023 COM ERRO            " TO TEXTO-DET1.
           MOVE   WS-OPS-ERRO-TB0023                     TO QT-DET1.
           WRITE  LINHA050A FROM DET1                       AFTER 2.
           WRITE REG-WOFIS FROM DET1.
      *> 
           MOVE   "PESQS NA TB0049 COM ERRO            " TO TEXTO-DET1.
           MOVE   WS-MRS-ERRO-TB0049                     TO QT-DET1.
           WRITE  LINHA050A FROM DET1                       AFTER 2.
           WRITE REG-WOFIS FROM DET1.
      *> 
           CLOSE LSUBCPG050A.
           CLOSE WOFIS SAVE.
      *> 
           IF WS-PAR-MOV-CREDITO NOT = "CONC"
              GO 3000-FIM.
      *> 
      *>     ENVIA OFIS COM A DATA DE PROCESSAMENTO DA CONCESSAO.
      *> 
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
      *> 
           MOVE   WS-PAR-PSTC    TO NUM-CONC-OFIS2.
           MOVE   WS-ANO-DT-PROC TO WS-DT-PROC-OFIS-ANO.
           MOVE   WS-MES-DT-PROC TO WS-DT-PROC-OFIS-MES.
           MOVE   WS-DIA-DT-PROC TO WS-DT-PROC-OFIS-DIA.
           WRITE  REG-WOFIS2  FROM  DET-OFIS2.
           CLOSE  WOFIS2 SAVE.
       3000-FIM. EXIT.
      *> 
       4000-FINALIZA.
          CLOSE DSUBCPG058SPG.
      *> 
          CLOSE DSUBCPG051SPG SAVE.
       4000-FIM. EXIT.
      *> 
       9000-LE-CPG058.
       
           READ DSUBCPG058SPG
                AT END
                    MOVE "SIM" TO WS-EOF.
      *> 
           IF SISPG-ID-REG-H OF DSUBCPG058SPG EQUAL 1
              ADD 1 TO WS-REGS-LIDOS
              GO TO 9000-FIM.
       
           IF SISPG-ID-REG-T OF DSUBCPG058SPG GREATER 2
              MOVE "SIM" TO WS-EOF
              ADD 1 TO WS-REGS-LIDOS
              GO TO 9000-FIM.
       
           IF WS-EOF NOT = "SIM"
              IF ( SISPG-CS-MEIO-PAGTO = 3  ) AND
                 ( SISPG-ID-BANCO NOT = 998 ) AND
                 ( SISPG-ID-BANCO NOT = 996 ) AND
                 ( WS-PAR-MOV-CREDITO NOT = "EMPR" ) AND
                 ( WS-PAR-MOV-CREDITO NOT = "GLOS" )
                 MOVE SISPG-ID-BANCO TO WS-CP50-ID-BANCO
                 MOVE CP50-N-ID-ORGAO-PAGADOR
                                           TO WS-CP50-ID-ORGAO-PAGADOR
                 MOVE SISPG-DT-INI-VALIDADE
                                          TO WS-CP50-DT-INI-VALIDADE
                 MOVE SPACES TO                    WS-MSG
                 STRING "NB COM MEIO PAGTO INV.REG LIDOS: "
                        "ID-BANCO: " WS-CP50-ID-BANCO
                        " ORGAO-PAGADOR: "WS-CP50-ID-ORGAO-PAGADOR
                        " DT-INI-VALIDADE: "WS-CP50-DT-INI-VALIDADE
                 DELIMITED BY SIZE INTO WS-MSG
                CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
                                               WS-STATUS-BACKUP
      *>                                              WS-INTEIRO-ZDISPLAY
                 MOVE SPACES TO                    WS-MSG
      *>            STRING WS-CP50-ID-BANCO "/"
      *>                   WS-CP50-ID-ORGAO-PAGADOR "/"
      *>                   WS-CP50-DT-INI-VALIDADE
      *>            DELIMITED BY SIZE INTO WS-MSG
      *>           CALL "ZDISPLAY IN DTP/L1" USING WS-MSG
      *>                                          WS-STATUS-BACKUP
      *>                                             WS-INTEIRO-ZDISPLAY
                 MOVE 0 TO WS-TESTE-CANCELAR-PROGRAMA
                 GO 9000-LE-CPG058.
       
       9000-FIM. EXIT.
      *> 
        COPYBOOK   B_DTP_PAD_01_CHECKPOINT_PD2.
      *> *****************************************************************
      *>               ROTINA PADRAO DE CHECKPOINT C74,C85               *
      *> *****************************************************************
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

        END-COPYBOOK.
      *> 
       4000-DISPLAY.
           MOVE WS-MSG-LIDOS TO W77-MSG
           CALL "ZDISPLAY IN DTP/L1" USING W77-MSG, 
                                           W77-STATUS-BACKUP-CKP.
       4000-FIM-DISPLAY. EXIT.
      *> 
      *> 
       2100-PESQ-DADOS-OP.
      *> ------------------*
       
           MOVE WS-ID-OP-ANT TO TB00233-CHAVE.
           CALL "SETTB00233 IN DTP/LBD/TAB" USING REGISTRO-TB00233, 
                                               CODE-LAYOUT-TB00233 
                                            GIVING RETORNO-TB00233.
      *> 
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
      *> --------*
      *> 
      *> 
       6000-ZERA-RUBRICAS.
      *> --------------------*
       
           MOVE ZEROS TO WS-TAB-VL-RUBRICA (WS-IND).
       
       6000-FIM.  EXIT.
      *> ---------------*
       
       7000-SOMA-RUBRICAS.
      *> --------------------*
       
           ADD SISPG-VL-RUBRICA(WS-IND) TO WS-TAB-VL-RUBRICA (WS-IND).
       
       7000-FIM. EXIT.
      *> --------------*
       
       8000-MOVE-RUBRICAS.
      *> --------------------*
       
           MOVE WS-TAB-VL-RUBRICA (WS-IND) TO SPG51-VL-RUBRICA(WS-IND).
       
       8000-FIM. EXIT.
      *> --------------*
       
       10000-GRAVA-HEADER.
      *> --------------------*
       
            MOVE ALL "00" TO DSUBCPG051-REG-SPG51.
      *> *Moves HEADER.
       
            MOVE  SISPG-ID-REG-H           TO SPG51-ID-REG-H
            MOVE  SISPG-DT-PROC-H          TO SPG51-DT-PROC-H
            MOVE  SISPG-ID-CALAMIDADE-H    TO SPG51-ID-CALAMIDADE-H
            MOVE  SISPG-NUM-PORTARIA-H     TO SPG51-NUM-PORTARIA-H
            MOVE  SISPG-DT-PORTARIA-H      TO SPG51-DT-PORTARIA-H
            MOVE  SISPG-DT-INI-VALIDADE-H  TO SPG51-DT-INI-VALIDADE-H
            MOVE  SISPG-DT-FIM-VALIDADE-H  TO SPG51-DT-FIM-VALIDADE-H
            MOVE  SISPG-NU-SEQ-ARQ-H       TO SPG51-NU-SEQ-ARQ-H
            MOVE  SISPG-ID-ULT-ARQ-H       TO SPG51-ID-ULT-ARQ-H
            MOVE  SISPG-LISTA-RUBR-H       TO SPG51-LISTA-RUBR-H
      *>      MOVE "10410510811213615420120220320420520620720820921021421
      *> -"16217218219220221222223224225226227228229230231232233234301905
      *> -"9910911912916917921925926927928929933" TO SPG51-LISTA-RUBR-H.
       
      *>      MOVE  SISPG-ORIG-CRED-H        TO SPG51-ORIGEM-CREDITO.
       
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
       
      *> 
       10000-FIM.  EXIT.
      *> ---------------*
      *> 
       11000-GRAVA-TRAILER.
      *> --------------------*
       
           MOVE ALL "00" TO DSUBCPG051-REG-SPG51-TRAILER.
           MOVE 9                       TO  SPG51-ID-REG-T.
           MOVE SISPG-TOT-VL-LIQ-T      TO  SPG51-TOT-VL-LIQ-T.
           ADD   1 TO WS-REGS-GRAVADOS-051.
           MOVE WS-REGS-GRAVADOS-051    TO  SPG51-QTD-REG-T
           WRITE DSUBCPG051-REG-SPG51-TRAILER.
       
       11000-FIM.  EXIT.
      *> ---------------*
       
      *> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *> %%%%%%%%%%%%%%   REFERENCIAS CRUZADAS DO FONTE     %%%%%%%%%%%%%%
      *> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       
      *>  LIBRARY - B/DTP/PAD/01/CHECKPOINT/PD2 CICLO = 0001VERSAO =   11
       
      *>  LIBRARY - B/DTP/PAD/01/CHECKPOINT/PD1 CICLO = 0001VERSAO =   01
       
      *>  LIBRARY - B/DTP/PAD/01/CHECKPOINT/WS  CICLO = 0001VERSAO =   06
       
      *>  LIBRARY - B/SDC/LBD/SETTB00233        CICLO = 0001VERSAO =   10
       
      *>  LIBRARY - B/SUB/CPG/051/SPG/R275      CICLO = 0001VERSAO =   02
       
      *>  LIBRARY - B/SUB/CPG/051/SUL           CICLO = 0001VERSAO =   00
       
      *>  LIBRARY - B/SUB/CPG/058/SPG/R275      CICLO = 0001VERSAO =   01
       
      *>  LIBRARY - B/SUB/CPG/050/SUL           CICLO = 0001VERSAO =   00
       
      *> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%