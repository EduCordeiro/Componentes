       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SORT.
      *------------------------------------------------------------
      *------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                  DECIMAL-POINT  IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PENTR ASSIGN  TO  DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS  IS W-FS
           ACCESS MODE  IS SEQUENTIAL.

           SELECT PSORT  ASSIGN  TO  DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS  IS W-FS
           ACCESS MODE  IS SEQUENTIAL.

           SELECT SAIDA ASSIGN  TO  DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS  IS W-FS
           ACCESS MODE  IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PENTR
        LABEL RECORD IS STANDARD
        VALUE OF FILE-ID IS ENTRADA.
       01  REGENTR.
         03 FILLER     PIC X(3274).
         03 E-CHAVE    PIC X(07).
      
       SD  PSORT.
       01  REGSORT.
         03 FILLER     PIC X(3274).
         03 S-CHAVE    PIC X(07).

       FD  SAIDA
         LABEL RECORD IS STANDARD
         VALUE OF FILE-ID IS "SAIDA.TXT".
       01  REGSAIDA.
         03 FILLER     PIC X(3281).

       WORKING-STORAGE SECTION.
       77 LK-PATH         PIC X(50) VALUE SPACES.
       77 LK-NOME         PIC X(20) VALUE SPACES.
       77 LK-DATA         PIC X(08) VALUE SPACES.
       01  AREAS-DE-TRABALHO.
         03 W-IND         PIC 9(02) VALUE ZEROS.
         03 W-FIM         PIC X(01) VALUE SPACES.
         03 W-AUX         PIC X(01) VALUE SPACES.
         03 W-FS          PIC X(02) VALUE SPACES.
         03 ENTRADA.
            05 FILLER    PIC X(30) VALUE SPACES.
     *
      ****************************************************************
      *
       PROCEDURE DIVISION.
       INICIO.
            PERFORM TELA     THRU FIM-TELA

            OPEN INPUT PENTR
                 OUTPUT SAIDA

             SORT PSORT  ASCENDING KEY  S-CHAVE
                INPUT PROCEDURE IS LEGRAVA
                OUTPUT PROCEDURE IS GERSAIDA.

           CLOSE PENTR SAIDA.
           STOP RUN.
      *****************************************************************
       LEGRAVA SECTION.
       SORTEAR.
               READ PENTR AT END
                  GO TO FIM-LEGRAVA.

               RELEASE REGSORT FROM REGENTR
               GO TO SORTEAR.
       FIM-LEGRAVA.
               EXIT.
      *****************************************************************
       GERSAIDA SECTION.
       GERAR.
               RETURN PSORT AT END
                  GO TO FIM-GERSAIDA.

               WRITE REGSAIDA FROM REGSORT
               GO TO GERAR.
       FIM-GERSAIDA.
               EXIT.
      ****************************************************************
       TELA.
           DISPLAY (1 1) ERASE
           DISPLAY (10 20) "ENTRADA ".
           ACCEPT (10 28) ENTRADA.

       FIM-TELA.
           EXIT.
      ****************************************************************
