      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. TABELA.
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
       77  INDEXTAB              PIC 9(002) VALUE ZEROS.
       77  CONTADOR              PIC 9(003) VALUE ZEROS.
       77  DIA-MES               PIC 9(002) VALUE ZEROS.
       77  DIA-SEMANA            PIC 9(001) VALUE ZEROS.
       77  ANO-BISSEXTO          PIC 9(004) VALUE ZEROS.
       77  ANO-BISS-PROVA        PIC 9(008) VALUE ZEROS.
       77  INTEIRODATA           PIC 9(008) VALUE ZEROS.

       01  TABELA.
           02 TABMES             PIC 9(001) VALUE ZEROS OCCURS 31 TIMES.

       01  DATA-SISTEMA.
           02 ANO-SISTEMA  PIC 9(004).
           02 MES-SISTEMA  PIC 9(002).
           02 DIA-SISTEMA  PIC 9(002).
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
           ACCEPT   DATA-SISTEMA FROM DATE YYYYMMDD

           EVALUATE MES-SISTEMA
           WHEN 1
             MOVE 31 TO DIA-MES
           WHEN 2
             MOVE 28 TO DIA-MES
             MOVE ZEROS TO ANO-BISSEXTO
             DIVIDE ANO-SISTEMA BY 4 GIVING ANO-BISSEXTO
             MOVE ANO-BISSEXTO TO ANO-BISS-PROVA
             SUBTRACT ANO-BISSEXTO FROM ANO-BISSEXTO
             IF ANO-BISSEXTO EQUALS ZEROS
                 MOVE 29 TO DIA-MES
             END-IF
           WHEN 3
             MOVE 31 TO DIA-MES
           WHEN 4
             MOVE 30 TO DIA-MES
           WHEN 5
             MOVE 31 TO DIA-MES
           WHEN 6
             MOVE 30 TO DIA-MES
           WHEN 7
             MOVE 31 TO DIA-MES
           WHEN 8
             MOVE 31 TO DIA-MES
           WHEN 9
             MOVE 30 TO DIA-MES
           WHEN 10
             MOVE 31 TO DIA-MES
           WHEN 11
             MOVE 30 TO DIA-MES
           WHEN 12
             MOVE 31 TO DIA-MES
           END-EVALUATE

           PERFORM VARYING INDEXTAB FROM 1 BY 1 UNTIL INDEXTAB > DIA-MES
               MOVE INDEXTAB TO DIA-SISTEMA
               MOVE DATA-SISTEMA TO INTEIRODATA
               MOVE FUNCTION REM(
               FUNCTION INTEGER-OF-DATE(INTEIRODATA), 7) TO DIA-SEMANA
           IF DIA-SEMANA >= 1 AND DIA-SEMANA <= 5
               MOVE 1 TO TABMES(INDEXTAB)
           END-IF

           END-PERFORM

           DISPLAY DATA-SISTEMA " - " MES-SISTEMA

           MOVE 1 TO CONTADOR
           PERFORM VARYING CONTADOR FROM 1 BY 1 UNTIL CONTADOR > 31
             DISPLAY CONTADOR " " TABMES(CONTADOR)
           END-PERFORM

            STOP RUN.
      ** add other procedures here
       END PROGRAM TABELA.
