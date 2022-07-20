      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. CALCAREA.
       author. EDUARDO

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
       77  LARGURA       PIC 9(003) VALUE ZEROS.
       77  ALTURA        PIC 9(003) VALUE ZEROS.
       77  AREA-RESULT   PIC 9(006) VALUE ZEROS.
       77  PAUSE_PROGRAN PIC 9(006) VALUE ZEROS.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      **
      * The main procedure of the program
      **
       INICIO.
           DISPLAY "CALCULO DE AREA (QUADRADO/RETANGULOS)"
           DISPLAY "LARGURA..: "
           ACCEPT LARGURA
           DISPLAY "ALTURA...: "
           ACCEPT ALTURA
           MULTIPLY LARGURA BY ALTURA GIVING AREA-RESULT
           DISPLAY "AREA.....: " AREA-RESULT

           DISPLAY "FIM DE PROGRAMA, PRECIONE ENTER PARA FECHAR"
           ACCEPT PAUSE_PROGRAN
            STOP RUN.
      ** add other procedures here
       END PROGRAM CALCAREA.
