       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ENGORDA.
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
           SELECT       SAIDA          ASSIGN    TO   DISK
                        ORGANIZATION             IS   INDEXED
                        ACCESS         MODE      IS   SEQUENTIAL
                        FILE           STATUS    IS   ST-SAIDA
                        RECORD         KEY       IS   S-CHAVE.
           SELECT       SAILOG         ASSIGN    TO   DISK
                        ORGANIZATION             IS   LINE SEQUENTIAL
                        ACCESS         MODE      IS   SEQUENTIAL.

       DATA        DIVISION.
       FILE        SECTION.
       FD  SAIDA
           LABEL RECORD ARE STANDARD
           RECORD CONTAINS 209 CHARACTERS
           VALUE OF FILE-ID IS  "ENGORDA.DAT".
       01  REG-SAIDA.
           02  S-CHAVE.
               04 S-SEQ      PIC 9(09).
           02  S-CAMPO       PIC X(200).
       FD  SAILOG
           LABEL RECORD ARE STANDARD
           RECORD CONTAINS 50  CHARACTERS
           VALUE OF FILE-ID IS  "SAILOG.TXT".
       01  REG-SAILOG.
           02  SL-QTDE       PIC 9(09).
           02  FILLER        PIC X(02).
           02  SL-DATA       PIC X(06).
           02  FILLER        PIC X(02).
           02  SL-HORA       PIC X(06).
    
       WORKING-STORAGE  SECTION.
       77  ST-SAIDA               PIC X(002) VALUE SPACES.
       77  W-CONT                 PIC 9(009) VALUE ZEROS.
       77  W-GRAVA                PIC 9(009) VALUE ZEROS.
       77  W-CAMPO                PIC X(200) VALUE ALL "X".
       77  W-LIMITE               PIC 9(009) VALUE ZEROS.
       77  W-SN                   PIC X(001) VALUE SPACES.
       77  TECLA                  PIC 9(002) VALUE ZEROS.

       01  W-DATE.
           02 W-ANO   PIC 9(02).
           02 W-MES   PIC 9(02).
           02 W-DIA   PIC 9(02).

       01  W-TIME.
           02 W-HORA  PIC 9(02).
           02 W-MINU  PIC 9(02).
           02 W-SEGU  PIC 9(02).

       SCREEN SECTION.
       01  TELA1.
           02  LINE 10 COLUMN 10 VALUE "DIGITE A QUANTIDADE".
           02  LINE 12 COLUMN 10 VALUE "[         ]".
           02  LINE 14 COLUMN 10 VALUE "CONFIRME S/N".
           02  LINE 23 COLUMN 05 VALUE "<ESC> DESISTIR".

       01  ACCEPT-TELA1.
           02  LINE 12 COLUMN 11 PIC 9(09) USING W-LIMITE AUTO.
       01  ACCEPT-SN.
           02  LINE 14 COLUMN 23 PIC X(01) USING W-SN AUTO.

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY TELA1.
       ACCEPT-TELA.
           ACCEPT  ACCEPT-TELA1
           ACCEPT  TECLA       FROM ESCAPE KEY
           IF      TECLA       EQUAL 01
                   STOP RUN.
           IF      W-LIMITE    EQUAL ZEROS
                   GO  ACCEPT-TELA.
       CONFIRMA.
           ACCEPT  ACCEPT-SN
           IF      W-SN        EQUAL "N"
                   GO  ACCEPT-TELA
           ELSE
             IF    W-SN        NOT EQUAL "S"
                   GO  CONFIRMA.

           
           DISPLAY (23 05)     "                      ".
           OPEN    OUTPUT      SAILOG.
           OPEN    OUTPUT      SAIDA.
           IF      ST-SAIDA    EQUAL "00"
                   NEXT SENTENCE
           ELSE
             IF    ST-SAIDA    NOT EQUAL "30"
                   DISPLAY (23 05) "ERRO NA ABERTURA = " ST-SAIDA
                   STOP RUN.

           MOVE    ZEROS       TO         W-CONT
           ACCEPT  W-DATE FROM DATE
           ACCEPT  W-TIME FROM TIME
           MOVE    SPACES      TO         REG-SAILOG
           MOVE    W-DATE      TO         SL-DATA
           MOVE    W-TIME      TO         SL-HORA
           MOVE    W-CONT      TO         SL-QTDE
           WRITE   REG-SAILOG
           MOVE    ZEROS       TO         W-GRAVA.
     
           PERFORM PROCESSA  THRU      PROCESSOU.
           CLOSE   SAIDA  SAILOG. 
           STOP    RUN.

       PROCESSA.
           ADD     1         TO        W-CONT     W-GRAVA
           MOVE    W-CONT    TO        S-CHAVE
           MOVE    W-CAMPO   TO        S-CAMPO
           WRITE   REG-SAIDA
           if      st-saida  not  equal "00"
                   DISPLAY   (23 05)   "ERRO NA GRAVACAO, ST= " st-saida
                   CLOSE   SAIDA       SAILOG
                   STOP    RUN.

           IF      W-GRAVA   EQUAL     50000
                   ACCEPT  W-DATE FROM DATE
                   ACCEPT  W-TIME FROM TIME
                   MOVE    SPACES      TO         REG-SAILOG
                   MOVE    W-DATE      TO         SL-DATA
                   MOVE    W-TIME      TO         SL-HORA
                   MOVE    W-CONT      TO         SL-QTDE
                   WRITE   REG-SAILOG
                   DISPLAY (16 10)     W-CONT
                   MOVE    ZEROS       TO         W-GRAVA.

           IF      W-CONT  NOT         EQUAL      W-LIMITE
                   GO      PROCESSA.
       PROCESSOU.
           EXIT.

