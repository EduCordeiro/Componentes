      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. ENGORDA.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT     SAIDA         ASSIGN TO DISK
                      ORGANIZATION         IS INDEXED
                      ACCESS        MODE   IS SEQUENTIAL
                      FILE          STATUS IS ST-SAIDA
                      RECORD        KEY    IS S-CHAVE.
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       FD  SAIDA
      *    LABEL  RECORD ARE STANDARD
           RECORD CONTAINS 209 CHARACTERS
           VALUE OF FILE-ID IS "ENGORDA.DAT".

       01  REG-SAIDA.
           02 S-CHAVE.
             04 S-SEQ  PIC 9(009).
           02 S-CAMPO  PIC X(200).


       WORKING-STORAGE SECTION.
      *-----------------------
       77  STOP_PGM PIC 9(001).
       77  ST-SAIDA PIC X(009) VALUE SPACES.
       77  W-CONT   PIC 9(009) VALUE ZEROS.
       77  W-GRAVA  PIC 9(003) VALUE ZEROS.
       77  W-CAMPO  PIC X(200) VALUE ALL "X".
       77  W-LIMITE PIC 9(009) VALUE ZEROS.
       77  W-SN     PIC X(001) VALUE SPACES.
       77  TECLA    PIC 9(002) VALUE ZEROS.

       01  W-DATE.
           02 W-ANO  PIC 9(02).
           02 W-MES  PIC 9(02).
           02 W-DIA  PIC 9(02).

       01  W-TIME.
           02 W-HORA  PIC 9(02).
           02 W-MINU  PIC 9(02).
           02 W-SEGU  PIC 9(02).

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
           DISPLAY "DIGITE A QUANTIDADE".
           ACCEPT W-LIMITE.

       CONFIRMA.

           OPEN OUTPUT SAIDA.
           IF ST-SAIDA EQUAL "00"
               NEXT SENTENCE
           ELSE
               IF ST-SAIDA NOT EQUAL "30"
                  DISPLAY "ERRO NA ABERTURA = " ST-SAIDA
                  STOP RUN.

           MOVE    ZEROS  TO   W-CONT
           ACCEPT  W-DATE FROM DATE
           ACCEPT  W-TIME FROM TIME

           PERFORM PROCESSA THRU PROCESSOU.

       PROCESSA.
           ADD        1  TO  W-CONT   W-GRAVA
           MOVE  W-CONT  TO  S-CHAVE
           MOVE  W-CAMPO TO S-CAMPO
           WRITE REG-SAIDA

           IF ST-SAIDA NOT EQUAL "00"
               DISPLAY "ERRO GRAVACAO, ST = " ST-SAIDA
               CLOSE SAIDA
               STOP RUN.

           IF W-CONT NOT EQUAL W-LIMITE
               GO PROCESSA.

       PROCESSOU.
           EXIT.

      *     DISPLAY "FIM PROGRAMA. PRECIONE ENTER PARA FECHAR."
      *     ACCEPT STOP_PGM.
            STOP RUN.
      ** add other procedures here
       END PROGRAM ENGORDA.
