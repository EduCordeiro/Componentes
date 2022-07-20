      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. LACO.
       AUTHOR. EDUARDO.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       77  STOP-PROGRAN    PIC 9(001) VALUES ZEROS.
       77  CONTADOR        PIC 9(003) VALUES ZEROS.
       01  DT-SISTEMA.
           02 ANO-SISTEMA  PIC 9(004) VALUES ZEROS.
           02 MES-SISTEMA  PIC 9(002) VALUES ZEROS.
           02 DIA-SISTEMA  PIC 9(002) VALUES ZEROS.
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       INICIO.
      **
      * The main procedure of the program
      **
           PERFORM PEGA-DATA-SISTEMA
           PERFORM EXIBE-TELA

           PERFORM VARYING CONTADOR FROM 1 BY 1 UNTIL CONTADOR > 10
               DISPLAY "CONTADOR..: " CONTADOR
           END-PERFORM

           DISPLAY "FIM DE PROGRAMA. PRECIONE ENTER PARA FECHAR"
           ACCEPT STOP-PROGRAN

            STOP RUN.
      ** add other procedures here
       PEGA-DATA-SISTEMA.
           ACCEPT DT-SISTEMA FROM DATE YYYYMMDD.

       EXIBE-TELA.
           DISPLAY "PROGRAMA BATCH....: "
           DISPLAY "DATA DE EXECUCAO..: " DIA-SISTEMA "/"
      -                                   MES-SISTEMA "/"
      -                                   ANO-SISTEMA.
       END PROGRAM LACO.
