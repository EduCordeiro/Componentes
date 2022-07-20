       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BINGON.
      *
      *  JUNTA 02 ARQUIVOS E
      *  RENUMERA DICONIX DOC COM 4610 BYTES
      *
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
           SELECT       ENTRADA1       ASSIGN    TO   DISK
                        ORGANIZATION             IS   LINE SEQUENTIAL
                        ACCESS         MODE      IS   SEQUENTIAL.
           SELECT       ENTRADA2       ASSIGN    TO   DISK
                        ORGANIZATION             IS   LINE SEQUENTIAL
                        ACCESS         MODE      IS   SEQUENTIAL.
           SELECT       SAIDA          ASSIGN    TO   DISK
                        ORGANIZATION             IS   LINE SEQUENTIAL
                        ACCESS         MODE      IS   SEQUENTIAL.

       DATA        DIVISION.
       FILE        SECTION.
       FD  ENTRADA1
           LABEL RECORD ARE STANDARD
           RECORD CONTAINS 4610 CHARACTERS
           VALUE OF FILE-ID IS  ARQ-ENTRADA1.
       01  REG-ENTRADA1.
           02  E-CAMPO1           PIC X(4600).
           02  FILLER             PIC X(0001).
           02  E-DCX1             PIC 9(0007).
           02  E-LADO1            PIC X(0002).

       FD  ENTRADA2
           LABEL RECORD ARE STANDARD
           RECORD CONTAINS 4610 CHARACTERS
           VALUE OF FILE-ID IS  ARQ-ENTRADA2.
       01  REG-ENTRADA2.
           02  E-CAMPO2           PIC X(4600).
           02  FILLER             PIC X(0001).
           02  E-DCX2             PIC 9(0007).
           02  E-LADO2            PIC X(0002).

       FD  SAIDA
           LABEL RECORD ARE STANDARD
           RECORD CONTAINS 4610 CHARACTERS
           VALUE OF FILE-ID IS  ARQ-SAIDA.   
       01  REG-SAIDA.
           02  S-CAMPO            PIC X(4600).
           02  FILLER             PIC X(0001).
           02  S-DCX              PIC 9(0007).
           02  S-LADO             PIC X(0002).
 
       WORKING-STORAGE  SECTION.
       77  W-LIDOS                PIC 9(007) VALUE ZEROS.
       77  W-GRAVADOS             PIC 9(007) VALUE ZEROS.
       77  ARQ-ENTRADA1           PIC X(030) VALUE SPACES.
       77  ARQ-ENTRADA2           PIC X(030) VALUE SPACES.
       77  ARQ-SAIDA              PIC X(030) VALUE SPACES.
       77  TECLA                  PIC X(002) VALUE SPACES.
       77  WSN                    PIC X(001) VALUE SPACES.
          

       SCREEN SECTION.
       01  TELA1.
           02  LINE 05 COLUMN 10 VALUE "BINGOS - CONCATENA€ÇO".
           02  LINE 10 COLUMN 10 VALUE "INFORME O PRIMEIRO ARQUIVO".
           02  LINE 12 COLUMN 10 VALUE
           "[                              ]".
           02  LINE 14 COLUMN 10 VALUE "INFORME O SEGUNDO ARQUIVO".
           02  LINE 16 COLUMN 10 VALUE
           "[                              ]".
           02  LINE 19 COLUMN 10 VALUE "INFORME O ARQUIVO DE SAIDA".
           02  LINE 21 COLUMN 10 VALUE
           "[                              ]".
           02  LINE 23 COLUMN 05 VALUE "<ESC> DESISTIR    <F1> VOLTAR".

       01  ACCEPT-ENTRADA1.
           02  LINE 12 COLUMN 11 PIC X(30) USING ARQ-ENTRADA1 AUTO.
       01  ACCEPT-ENTRADA2.
           02  LINE 16 COLUMN 11 PIC X(30) USING ARQ-ENTRADA2 AUTO.
       01  ACCEPT-SAIDA.   
           02  LINE 21 COLUMN 11 PIC X(30) USING ARQ-SAIDA AUTO.


       PROCEDURE DIVISION.
       INICIO.

           DISPLAY TELA1.
       RECEBE-E1.
           ACCEPT  ACCEPT-ENTRADA1
           ACCEPT  TECLA FROM ESCAPE KEY
           IF      TECLA EQUAL 01
                   GO TERMINO.
       RECEBE-E2.
           ACCEPT  ACCEPT-ENTRADA2
           ACCEPT  TECLA FROM ESCAPE KEY
           IF      TECLA EQUAL 01
                   GO TERMINO.
           IF      TECLA EQUAL 02
                   GO RECEBE-E1.
       RECEBE-S.
           ACCEPT  ACCEPT-SAIDA    
           ACCEPT  TECLA FROM ESCAPE KEY
           IF      TECLA EQUAL 01
                   GO TERMINO.
           IF      TECLA EQUAL 02
                   GO RECEBE-E2.
       RECEBE-SN.
           DISPLAY (23 05)   "CONFIRMA S/N                             "
           ACCEPT  (23 19)   WSN
           IF      WSN       EQUAL "S" OR "s"
                   NEXT SENTENCE
           ELSE IF WSN       EQUAL "N" OR "n"
                   GO        RECEBE-S
                ELSE
                   GO        RECEBE-SN.

           OPEN    INPUT     ENTRADA1  ENTRADA2 
                   OUTPUT    SAIDA.
           PERFORM PROCESSA1 THRU      PROCESSOU1.
           PERFORM PROCESSA2 THRU      PROCESSOU2.
           CLOSE   ENTRADA1  ENTRADA2  SAIDA.

       TERMINO.
           STOP    RUN.

       PROCESSA1.
           READ    ENTRADA1  AT        END
                   GO        PROCESSOU1.

           IF      E-LADO1   EQUAL     "-B"
                   NEXT      SENTENCE
           ELSE
                   ADD       1         TO    W-LIDOS.


           MOVE    E-CAMPO1            TO    S-CAMPO
           MOVE    W-LIDOS             TO    S-DCX
           MOVE    E-LADO1             TO    S-LADO


           WRITE   REG-SAIDA
           ADD     1                   TO    W-GRAVADOS
           DISPLAY (23 05) "MSG=>  GRAVADOS ... " W-GRAVADOS
           GO      PROCESSA1.
       PROCESSOU1.
           EXIT.

       PROCESSA2.
           READ    ENTRADA2  AT        END
                   GO        PROCESSOU2.


           IF      E-LADO2   EQUAL     "-B"
                   NEXT      SENTENCE
           ELSE
                   ADD       1         TO    W-LIDOS.

           MOVE    E-CAMPO2            TO    S-CAMPO
           MOVE    W-LIDOS             TO    S-DCX
           MOVE    E-LADO2             TO    S-LADO

           WRITE   REG-SAIDA
           ADD     1                   TO    W-GRAVADOS
           DISPLAY (23 05) "MSG=>  GRAVADOS ... " W-GRAVADOS
           GO      PROCESSA2.
       PROCESSOU2.
           EXIT.

