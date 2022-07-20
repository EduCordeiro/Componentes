      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. LISTA_CLIENTE.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN TO "CLIENTE.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CLIENTE-CEP
           FILE STATUS IS FS-CLIENTE.
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       FD ENTRADA.
       01  REG-CLIENTE.
           02 CLIENTE-NOME     PIC X(040).
           02 CLIENTE-RUA      PIC X(080).
           02 CLIENTE-BAIRRO   PIC X(040).
           02 CLIENTE-CIDADE   PIC X(040).
           02 CLIENTE-UF       PIC X(003).
           02 CLIENTE-CEP      PIC 9(008).

       WORKING-STORAGE SECTION.
       77   STOP_PGM PIC 9(001).

       01  FS-CLIENTE.
           02 FS-CLIENTE-1  PIC X(001).
           02 FS-CLIENTE-2  PIC X(001).
           02 FS-CLIENTE-R REDEFINES FS-CLIENTE-2 PIC 99 COMP-X.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
           OPEN INPUT ENTRADA
           MOVE "SP" TO CLIENTE-UF



           IF FS-CLIENTE EQUAL ZEROS
               DISPLAY FS-CLIENTE
               READ ENTRADA NEXT
               PERFORM UNTIL FS-CLIENTE EQUAL "03" OR CLIENTE-UF <> "SP"
                 DISPLAY CLIENTE-NOME " - " CLIENTE-RUA
                 READ ENTRADA NEXT
               END-PERFORM
           END-IF
           CLOSE ENTRADA

      *      DISPLAY "FIM PROGRAMA. PRECIONE ENTER PARA FECHAR."
      *      ACCEPT STOP_PGM.
            STOP RUN.
      ** add other procedures here
       END PROGRAM LISTA_CLIENTE.
