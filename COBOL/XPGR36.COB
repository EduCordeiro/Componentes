       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XPGR36.
      *------------------------------------------------------------
      * 10/11/2000
      * ABRIL - OF20534 - CARTA WELCOME
      * CHECA QTDE, FORMATA PEDIDO, MOSTRA TOTAL DE REGISTROS E
      * DE CARTAS QUE SERAO IMPRESSAS
      * CARTA WELCOME  - 16/05/2001 - MURAI/MARY/RANDAL/CHICO
      * ALTEROU A TABELA DE ASSINATURA DO MINORU SAN -> ROGERIO MINORU SUGA
      * E DO CARGO DO MARCILIANO JUNIOR PARA DIRETOR DE MARKETING
      * (NOME-GER E TB-CARGO
      * 19.03.2002 - VERSAO DO FPGR36 P/ XEROX, RECEBE E PASSA PARAMETROS
      * 10/05/2004 - ADAPTACAO DE NOVO LAYOUT
      * 26/07/2005 - NOVA WELCOME - ALTEROU CARTAS E CONCEITOS
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

           SELECT PSAIDA ASSIGN  TO  DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS  IS W-FS
           ACCESS MODE  IS SEQUENTIAL.

           SELECT PERRO ASSIGN  TO  DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS  IS W-FS
           ACCESS MODE  IS SEQUENTIAL.


           SELECT PFLAG ASSIGN  TO  DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS  IS W-FS
           ACCESS MODE  IS SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD  PENTR
        LABEL RECORD IS STANDARD
        VALUE OF FILE-ID IS "POUTROS.TXT".
      *-----------------> 1810 BYTES
       01  REGENTR-1.
         03 E-TIPO            PIC X(02).
         03 E-COD-IMPRES      PIC X(01).
         03 E-SEQ-REG         PIC 9(10).
         03 E-SEQ-CARNE       PIC 9(10).
         03 E-ASS-GERENTE.
           05 FILLER          PIC X(02).
           05 E-ASS-GER       PIC 9(03).
           05 FILLER          PIC X(06).
         03 E-CARTA           PIC X(07).
         03 E-CHAVE-ABRIL     PIC X(41).
         03 E-DADOS-ASSINANTE.
           05 E-PARTE1-ASS.
             07 E-COD-ASS     PIC X(09).
             07 E-NOME-ASS    PIC X(30).
             07 E-CHAMADA-ASS PIC X(20).
             07 E-END-ASS     PIC X(30).
             07 E-CID-ASS     PIC X(15).
             07 E-BAI-ASS     PIC X(15).
             07 E-UF-ASS      PIC X(02).
           05 E-CEP5-ASS      PIC 9(05).
           05 E-CEP3-ASS      PIC 9(03).
           05 E-SEXO-ASS      PIC X(01).
         03 E-DADOS-PRESENT.
           05 E-PARTE1-PRE.
             07 E-COD-PRE     PIC X(09).
             07 E-NOME-PRE    PIC X(30).
             07 E-CHAMADA-PRE PIC X(20).
             07 E-END-PRE     PIC X(30).
             07 E-CID-PRE     PIC X(15).
             07 E-BAI-PRE     PIC X(15).
             07 E-UF-PRE      PIC X(02).
           05 E-CEP5-PRE      PIC 9(05).
           05 E-CEP3-PRE      PIC 9(03).
           05 E-SEXO-PRE      PIC X(01).
           05 E-DT-PROC       PIC X(08).
         03 E-PROJETO OCCURS 6 TIMES.
           05 E-NR-PROJ       PIC X(03). 
           05 E-NUM-PROJ REDEFINES E-NR-PROJ.
             07 FILLER         PIC X(01). 
             07 E-REVISTA      PIC 9(02). 
           05 E-NOME-PROJ      PIC X(30).
           05 E-EDI-INIC       PIC X(07).
           05 E-EDI-FIM        PIC X(07).
           05 E-NR-COPIAS      PIC X(03).
           05 E-DT-VIG-INIC    PIC X(08).  
           05 E-MMAA-INIC      PIC X(17).
           05 E-DT-EDI-INIC    PIC X(10).  
           05 E-DT-VIG-FIM     PIC X(08).  
           05 E-MMAA-FIM       PIC X(17).
           05 E-DT-EDI-FIM     PIC X(10).  
           05 E-VL-COPIA       PIC 9(09).
           05 E-VL-ASS-CAPA    PIC 9(09).
           05 E-VIGENCIA       PIC X(05).
         03 E-EXT-TP           PIC 9(02).
         03 E-PARC-ATRAS-1     PIC 9(02).
         03 E-PARC-PAGAS       PIC 9(02).
         03 E-TOT-DEB-ATRAS    PIC 9(09).
         03 E-TOT-PAGO         PIC 9(09).
         03 E-PARC-ATRASADAS OCCURS 5 TIMES.
           05 E-VL-PARC        PIC 9(09).
           05 E-VENC.
             07 E-AA-VENC      PIC X(04).
             07 E-MM-VENC      PIC X(02).
             07 E-DD-VENC      PIC X(02).
           05 E-QTDE-PARC      PIC 9(02).
         03 FILLER             PIC X(22).
         03 E-CENTRAL-ATEND.
           05 E-0800           PIC X(12).
           05 E-GSP            PIC X(09).
         03 FILLER             PIC X(36).
         03 E-EDI-PERD-INI     PIC X(07).
         03 E-EDI-PERD-FIM     PIC X(07).
         03 E-DEBITO-CARTAO.
           05 E-SIGLA-DB         PIC X(02).     
           05 FILLER             PIC X(01).
           05 E-DB-CARTAO        PIC X(18).
         03 E-DB-BCO.
           05 E-BANCO          PIC X(03).
           05 E-AGENCIA        PIC X(05).     
           05 E-CONTA          PIC X(10).
         03 E-NOME-INST       PIC X(25).     
         03 FILLER            PIC X(11).
         03 E-TIPO-ENTREGA    PIC X(01).
         03 E-ENTR-DIRETA.
           05 E-PRIORI-DISTRIB PIC X(04).
           05 E-CENTRALIZ     PIC X(06).
           05 E-DISTRIBUIDOR  PIC X(06).
           05 E-AG-DIRETA     PIC X(20).
           05 E-ROTEIRO       PIC X(10).
      *---------------------> outras informacoes: ver layout original
         03 E-OUTRAS.
           05 FILLER          PIC X(06).
           05 EXT-ACAO.
             10 E-TP-ACAO     PIC X(01).
             10 E-COD-ACAO    PIC X(04).
             10 FILLER        PIC X(01).
           05 FILLER          PIC X(29).
           05 E-ANEXO123      PIC X(21).           
           05 FILLER          PIC X(44).
           05 E-EMPRESA       PIC X(10).
           05 FILLER          PIC X(58).
           05 E-GERENTE       PIC X(08).
           05 E-PRACA         PIC X(06).
           05 FILLER          PIC X(80).
       01  REGENTR-2.
         05 FILLER            PIC X(32).
         05 E-DATA-PROC.
          07 E-DD-PROC        PIC 9(02).
          07 E-MM-PROC        PIC 9(02).
          07 E-AA-PROC        PIC 9(04).
         05 E-QTDE-REG        PIC 9(10).
         05 E-QTDE-CARTAS     PIC 9(10).
         05 FILLER            PIC X(1750).

       FD  PSAIDA
        LABEL RECORD IS STANDARD
        VALUE OF FILE-ID IS "TEMP.TXT".
      *----------------------------------> 1047 BYTES
       01  REGSAI.
         03 S-TIPO         PIC X(02).
         03 S-CARTA        PIC X(07).
         03 S-SEQ-REG      PIC 9(10).
         03 S-SEQ-CARNE    PIC 9(10).
         03 S-ASS-GERENTE.
           05 FILLER       PIC X(02).
           05 S-ASS-GER    PIC 9(03).
           05 FILLER       PIC X(06).
         03 S-NOME-GER     PIC X(30).
         03 S-CARGO-GER    PIC X(35).
         03 S-CHAVE-ABRIL  PIC X(41).
         03 S-DADOS-ASSINANTE.
           05  S-PARTE1-ASS.
             07 S-COD-ASS    PIC X(09).
             07 S-NOME-ASS   PIC X(30).
             07 S-CHAMADA-ASS PIC X(20).
             07 S-END-ASS    PIC X(30).
             07 S-CID-ASS    PIC X(15).
             07 S-BAI-ASS    PIC X(15).
             07 S-UF-ASS     PIC X(02).
           05 S-CEP5-ASS     PIC X(05).
           05 S-TRACO-ASS    PIC X(01).
           05 S-CEP3-ASS     PIC X(03).
           05 S-SEXO-ASS     PIC X(01).
         03 S-DADOS-PRESENT.
           05 S-PARTE1-PRE.
             07 S-COD-PRE    PIC X(09).
             07 S-NOME-PRE   PIC X(30).
             07 S-CHAMADA-PRE PIC X(20).
             07 S-END-PRE    PIC X(30).
             07 S-CID-PRE    PIC X(15).
             07 S-BAI-PRE    PIC X(15).
             07 S-UF-PRE     PIC X(02).
           05 S-CEP5-PRE     PIC X(05).
           05 S-TRACO-PRE    PIC X(01).
           05 S-CEP3-PRE     PIC X(03).
           05 S-SEXO-PRE     PIC X(01).
         03 S-PROJETO.
           05 S-NR-PROJ     PIC 9(03).
           05 S-NUM-PROJ REDEFINES S-NR-PROJ.
             07 S-FILLER    PIC X(01).
             07 S-REVISTA   PIC 9(02).
           05 S-NOME-PROJ   PIC X(30).
           05 S-EDI-INIC    PIC X(07).
           05 S-EDI-FIM     PIC X(07).
           05 S-NR-COPIAS   PIC X(03).
           05 S-DT-VIG-INIC PIC X(08).
           05 S-MMAA-INIC   PIC X(17).
           05 S-DT-EDI-INIC PIC X(10).
           05 S-DT-VIG-FIM  PIC X(08).
           05 S-MMAA-FIM    PIC X(17).
           05 S-DT-EDI-FIM  PIC X(10).
           05 S-VL-PARC     PIC X(12).
           05 S-VL-ASS-CAPA PIC X(12).
           05 S-VIGENCIA    PIC X(05).
         03 S-QTDE-PARC     PIC X(02).
         03 S-VENC.
           05 S-DD-VENC    PIC X(02).
           05 S-TRACO1-VC  PIC X(01).
           05 S-MM-VENC    PIC X(02).
           05 S-TRACO2-VC  PIC X(01).
           05 S-AA-VENC    PIC X(04).
         03 S-CENTRAL-ATEND.
           05 S-0800        PIC X(12). 
           05 S-GSP         PIC X(09).
         03 S-DEBITO-CTA. 
           05 S-SIGLA-DB    PIC X(02).
           05 FILLER        PIC X(01).
           05 S-DB-CARTAO   PIC X(18).
         03 S-DB-BCO.
           05 S-BANCO       PIC X(03).
           05 S-AGENCIA     PIC X(05).
           05 S-CONTA       PIC X(10).
         03 S-NOME-INST     PIC X(25). 
         03 S-ENTR-DIRETA.
           05 S-PRIORI-DISTRIB PIC X(04).
           05 S-CENTRALIZ     PIC X(06).
           05 S-DISTRIBUIDOR  PIC X(06).
           05 S-AG-DIRETA     PIC X(20).
           05 S-ROTEIRO       PIC X(10).
      *---------------------> outras informacoes: ver layout original
         03 S-OUTRAS.
           05 FILLER          PIC X(06).
           05 S-EXT-ACAO.
             10 S-TP-ACAO     PIC X(01).
             10 S-COD-ACAO    PIC X(04).
             10 FILLER        PIC X(01).
           05 FILLER          PIC X(29).
           05 S-ANEXO123      PIC X(21).           
           05 FILLER          PIC X(44).
           05 S-EMPRESA       PIC X(10).
           05 FILLER          PIC X(58).
           05 S-GERENTE       PIC X(08).
           05 S-PRACA         PIC X(06).
           05 FILLER          PIC X(80).
         03 S-DT-PROC.
           05 S-DD-PROC    PIC X(02).
           05 S-TRACO1-DT  PIC X(01).
           05 S-MM-PROC    PIC X(02).
           05 S-TRACO2-DT  PIC X(01).
           05 S-AA-PROC    PIC X(04).
         03 S-MES-PROC     PIC X(12).
         03 S-CONTROLE-ABR  PIC X(56).
      *- TP ACAO/COD ACAO/SEQ REG/TOT REG/DT PROC/CARNE/CARTA
      *   1    /  4     / 10    /  10   / 8     / 10  / 7
      *

      *-----------------> 1810 BYTES
       FD  PERRO
        LABEL RECORD IS STANDARD
        VALUE OF FILE-ID IS "PERRO.TXT".
       01  REGERRO.
         03 FILLER            PIC X(1810).

       FD  PFLAG
        LABEL RECORD IS STANDARD
        VALUE OF FILE-ID IS "WOK.TXT".
       01  REGOK.
         03 FILLER         PIC X(10).

      *---------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 LK-PATH         PIC X(50) VALUE SPACES.
       77 LK-NOME         PIC X(20) VALUE SPACES.
       77 LK-DATA         PIC X(08) VALUE SPACES.
       01  AREAS-DE-TRABALHO.
         03 CONTROLE-ABR.
           05 TP-ACAO     PIC X(01).
           05 FILLER      PIC X(01) VALUE "/".
           05 COD-ACAO    PIC X(04).
           05 FILLER      PIC X(01) VALUE "/".
           05 SEQ-REG     PIC 9(10).
           05 FILLER      PIC X(01) VALUE "/".
           05 TOT-REG     PIC 9(10).
           05 FILLER      PIC X(01) VALUE "/".
           05 DT-PROC     PIC X(08).
           05 FILLER      PIC X(01) VALUE "/".
           05 CARNE       PIC 9(10).
           05 FILLER      PIC X(01) VALUE "/".
           05 CARTA       PIC X(07).
         03 W-FIM         PIC X(01) VALUE SPACES.
         03 W-AUX         PIC X(01) VALUE SPACES.
         03 W-FS          PIC X(02) VALUE SPACES.
         03 W-VALOR       PIC Z.ZZZ.ZZ9,99.
         03 W-INFOR       PIC 9(06) VALUE ZEROS.
         03 W-IND2        PIC 9(04) VALUE ZEROS.
         03 W-IND         PIC 9(04) VALUE ZEROS.
         03 W-INDPR       PIC 9(02) VALUE ZEROS.
         03 W-LIDOS       PIC 9(06) VALUE ZEROS.
         03 W-TIPO2       PIC X(01) VALUE SPACES.
         03 W-GRAVADOS    PIC 9(06) VALUE ZEROS.
         03 W-ED-VIG      PIC ZZ999 VALUE ZEROS.
         03 W-DT-PROC.
          05 W-DD-PROC     PIC 9(02).
          05 W-MM-PROC     PIC 9(02).
          05 W-AA-PROC     PIC 9(04).

         03 TB-MES.
           05 FILLER      PIC X(12) VALUE "janeiro de".
           05 FILLER      PIC X(12) VALUE "fevereiro de".
           05 FILLER      PIC X(12) VALUE "março de".
           05 FILLER      PIC X(12) VALUE "abril de".
           05 FILLER      PIC X(12) VALUE "maio de".
           05 FILLER      PIC X(12) VALUE "junho de".
           05 FILLER      PIC X(12) VALUE "julho de".
           05 FILLER      PIC X(12) VALUE "agosto de".
           05 FILLER      PIC X(12) VALUE "setembro de".
           05 FILLER      PIC X(12) VALUE "outubro de".
           05 FILLER      PIC X(12) VALUE "novembro de".
           05 FILLER      PIC X(12) VALUE "dezembro de".
         03 TB-MES-R REDEFINES TB-MES PIC X(12) OCCURS 12 TIMES.
         03 TB-ASSINAT.
           05 FILLER      PIC X(03) VALUE "001".
           05 FILLER      PIC X(03) VALUE "002".
           05 FILLER      PIC X(03) VALUE "003".
           05 FILLER      PIC X(03) VALUE "004".
           05 FILLER      PIC X(03) VALUE "005".
           05 FILLER      PIC X(03) VALUE "006".
           05 FILLER      PIC X(03) VALUE "007".
           05 FILLER      PIC X(03) VALUE "008".
           05 FILLER      PIC X(03) VALUE "009".
           05 FILLER      PIC X(03) VALUE "010".
           05 FILLER      PIC X(03) VALUE "011".
           05 FILLER      PIC X(03) VALUE "012".
           05 FILLER      PIC X(03) VALUE "013".
           05 FILLER      PIC X(03) VALUE "014".
           05 FILLER      PIC X(03) VALUE "015".
           05 FILLER      PIC X(03) VALUE "016".
           05 FILLER      PIC X(03) VALUE "017".
           05 FILLER      PIC X(03) VALUE "018".
           05 FILLER      PIC X(03) VALUE "019".
           05 FILLER      PIC X(03) VALUE "020".
           05 FILLER      PIC X(03) VALUE "021".
           05 FILLER      PIC X(03) VALUE "022".
           05 FILLER      PIC X(03) VALUE "023".
           05 FILLER      PIC X(03) VALUE "024".
           05 FILLER      PIC X(03) VALUE "025".
           05 FILLER      PIC X(03) VALUE "026".
           05 FILLER      PIC X(03) VALUE "027".
           05 FILLER      PIC X(03) VALUE "028".
           05 FILLER      PIC X(03) VALUE "029".
           05 FILLER      PIC X(03) VALUE "030".
           05 FILLER      PIC X(03) VALUE "031".
           05 FILLER      PIC X(03) VALUE "032".
           05 FILLER      PIC X(03) VALUE "033".
           05 FILLER      PIC X(03) VALUE "034".
           05 FILLER      PIC X(03) VALUE "035".
           05 FILLER      PIC X(03) VALUE "036".
           05 FILLER      PIC X(03) VALUE "037".
           05 FILLER      PIC X(03) VALUE "038".
           05 FILLER      PIC X(03) VALUE "039".
           05 FILLER      PIC X(03) VALUE "040".
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE SPACES.
           05 FILLER      PIC X(03) VALUE "099".
         03 TB-ASS-R REDEFINES TB-ASSINAT PIC X(03) OCCURS 99 TIMES.
         03 TB-NOME-GER.
           05 FILLER      PIC X(30) VALUE "Sandra Oliveira".
           05 FILLER      PIC X(30) VALUE "Wagner Nabuco".
           05 FILLER      PIC X(30) VALUE "Luis LAMS".
           05 FILLER      PIC X(30) VALUE
                         "Luis Augusto M. Da Silva(LAMS)".
           05 FILLER      PIC X(30) VALUE "Vicente Argentino".
           05 FILLER      PIC X(30) VALUE "Roberto Tancredi".
           05 FILLER      PIC X(30) VALUE "André Hermann".
           05 FILLER      PIC X(30) VALUE "Gilda Resende".
           05 FILLER      PIC X(30) VALUE "Rute Araujo".
           05 FILLER      PIC X(30) VALUE "Arthur Tavares".
           05 FILLER      PIC X(30) VALUE "Marciliano Junior".
           05 FILLER      PIC X(30) VALUE "Enilma Queiroz".
           05 FILLER      PIC X(30) VALUE "Sergio Fernandes".
           05 FILLER      PIC X(30) VALUE "Ernesto Giacometti Filho".
           05 FILLER      PIC X(30) VALUE "André D'Amato".
           05 FILLER      PIC X(30) VALUE "Mauro Calliari".
           05 FILLER      PIC X(30) VALUE "Willian Pereira".
           05 FILLER      PIC X(30) VALUE "Jairo Leal".
           05 FILLER      PIC X(30) VALUE "Geraldo Campos".
           05 FILLER      PIC X(30) VALUE "Ariane Medina".
           05 FILLER      PIC X(30) VALUE "Adriana Neves".
           05 FILLER      PIC X(30) VALUE "Martha Cajado".
           05 FILLER      PIC X(30) VALUE "Marcos A. Ribeiro".
           05 FILLER      PIC X(30) VALUE "Ana Camargo".
           05 FILLER      PIC X(30) VALUE "Marcelo Hummel".
           05 FILLER      PIC X(30) VALUE "Osmar Lara".
           05 FILLER      PIC X(30) VALUE "Cid Moretti".
           05 FILLER      PIC X(30) VALUE "Eliseu Urban".
           05 FILLER      PIC X(30) VALUE "Yuri Trafani".
           05 FILLER      PIC X(30) VALUE "Ricardo Dalmas".
           05 FILLER      PIC X(30) VALUE "Fernanda Belli".
           05 FILLER      PIC X(30) VALUE "Rogério Minoru Suga".
           05 FILLER      PIC X(30) VALUE "Sergio Monaco".
           05 FILLER      PIC X(30) VALUE "Eden Carvalho".
           05 FILLER      PIC X(30) VALUE "Guiomar Namo de Bello".
           05 FILLER      PIC X(30) VALUE "Barbara Miklasevicius".
           05 FILLER      PIC X(30) VALUE "Rita Teixeira".
           05 FILLER      PIC X(30) VALUE "Ricardo Packness".
           05 FILLER      PIC X(30) VALUE "Euvaldo Júnior".
           05 FILLER      PIC X(30) VALUE "Murilo Boccia".
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
         03 TB-GER-R REDEFINES TB-NOME-GER PIC X(30) OCCURS 99 TIMES.
         03 TB-CARGO.
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Diretor de Marketing".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE
                          "Relacionamento - Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE "Gerente de Assinaturas".
           05 FILLER      PIC X(35) VALUE
                             "Diretor de Marketing - Revista VEJA".
           05 FILLER      PIC X(35) VALUE
                              "Gerente de Circulação - Assinaturas".
           05 FILLER      PIC X(35) VALUE
                               "Relacionamento - Assinaturas".
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
           05 FILLER      PIC X(35) VALUE SPACES.
         03 TB-CARGO-R REDEFINES TB-CARGO PIC X(35) OCCURS 99 TIMES.
         03 TABELA-PROJ.
           05 FILLER      PIC X(30) VALUE "VEJA".
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE "EXAME".
           05 FILLER      PIC X(30) VALUE "NOVA".
           05 FILLER      PIC X(30) VALUE "CASA CLAUDIA".
           05 FILLER      PIC X(30) VALUE "CLAUDIA".
           05 FILLER      PIC X(30) VALUE "MANEQUIM".
           05 FILLER      PIC X(30) VALUE "CAPRICHO".
           05 FILLER      PIC X(30) VALUE "ARQUITETURA E CONSTRUÇÃO".
           05 FILLER      PIC X(30) VALUE "ESPECIAIS PLACAR".
           05 FILLER      PIC X(30) VALUE "INFO CORPORATE".
           05 FILLER      PIC X(30) VALUE "MANEQUIM PONTO CRUZ".
           05 FILLER      PIC X(30) VALUE "VEJA NA SALA DE AULA".
           05 FILLER      PIC X(30) VALUE "VOCÊ S.A.".
           05 FILLER      PIC X(30) VALUE "NOVA BELEZA".
           05 FILLER      PIC X(30) VALUE "MANEQUIM FAÇA E VENDA".
           05 FILLER      PIC X(30) VALUE "ANAMARIA".
           05 FILLER      PIC X(30) VALUE "REVISTA DA WEB".
           05 FILLER      PIC X(30) VALUE "PLAYBOY".
           05 FILLER      PIC X(30) VALUE "QUATRO RODAS".
           05 FILLER      PIC X(30) VALUE "MEN'S HEALTH".
           05 FILLER      PIC X(30) VALUE "INTERVIEW".
           05 FILLER      PIC X(30) VALUE "PLACAR".
           05 FILLER      PIC X(30) VALUE "BONS FLUIDOS".
           05 FILLER      PIC X(30) VALUE "NATIONAL GEOGRAPHIC".
           05 FILLER      PIC X(30) VALUE "CLAUDIA COZINHA".
           05 FILLER      PIC X(30) VALUE "RECREIO".
           05 FILLER      PIC X(30) VALUE "REVISTA MTV".
           05 FILLER      PIC X(30) VALUE "MUNDO DISNEY".
           05 FILLER      PIC X(30) VALUE "SALA DE AULA MEC".
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE "MARVEL COMICS".
           05 FILLER      PIC X(30) VALUE "DC COMICS".
           05 FILLER      PIC X(30) VALUE "ATIVIDADES DISNEY".
           05 FILLER      PIC X(30) VALUE "REVISTA DA BARBIE".
           05 FILLER      PIC X(30) VALUE "DIVERSÃO LOONEY TUNES".
           05 FILLER      PIC X(30) VALUE "PIAUÍ".
           05 FILLER      PIC X(30) VALUE "AÇÃO GAMES".
           05 FILLER      PIC X(30) VALUE "COLEÇÕES VEJA".
           05 FILLER      PIC X(30) VALUE "SPAWN".
           05 FILLER      PIC X(30) VALUE "REVISTA MEU DINHEIRO".
           05 FILLER      PIC X(30) VALUE "ESPECIAIS SUPERINTERESSANTE".
           05 FILLER      PIC X(30) VALUE "TUDO".
           05 FILLER      PIC X(30) VALUE "ESTILO DE VIDA".
           05 FILLER      PIC X(30) VALUE "MUNDO ESTRANHO".
           05 FILLER      PIC X(30) VALUE "WITCH".
           05 FILLER      PIC X(30) VALUE "melhoresemaiores.com".
           05 FILLER      PIC X(30) VALUE "INFO".
           05 FILLER      PIC X(30) VALUE "SAÚDE! É VITAL".
           05 FILLER      PIC X(30) VALUE "REVISTA DA SEMANA".
           05 FILLER      PIC X(30) VALUE "FLUIR".
           05 FILLER      PIC X(30) VALUE "SET".
           05 FILLER      PIC X(30) VALUE "BOA FORMA".
           05 FILLER      PIC X(30) VALUE "ARTE E DECORAÇÃO".
           05 FILLER      PIC X(30) VALUE "OS CAMINHOS DA TERRA".
           05 FILLER      PIC X(30) VALUE "CARÍCIA".
           05 FILLER      PIC X(30) VALUE "INFORMÁTICA EXAME".
           05 FILLER      PIC X(30) VALUE "VIAGEM & TURISMO".
           05 FILLER      PIC X(30) VALUE "NOVA ESCOLA".
           05 FILLER      PIC X(30) VALUE "HOME PC".
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE "PLACAR".
           05 FILLER      PIC X(30) VALUE "VIDA SIMPLES".
           05 FILLER      PIC X(30) VALUE "SUPERINTERESSANTE".
           05 FILLER      PIC X(30) VALUE "GUIA BRASIL ON LINE".
           05 FILLER      PIC X(30) VALUE "PLANETA PLAYBOY".
           05 FILLER      PIC X(30) VALUE "CYBER CLUB".
           05 FILLER      PIC X(30) VALUE "SUPERINTERESSANTE".
           05 FILLER      PIC X(30) VALUE "BRASIL 500 ANOS".
           05 FILLER      PIC X(30) VALUE "MELHORES PRÁTICAS".
           05 FILLER      PIC X(30) VALUE "ESPECIAL PLAYBOY".
           05 FILLER      PIC X(30) VALUE "ABRIL ASSINATURAS".
           05 FILLER      PIC X(30) VALUE "FAÇA E VENDA".
           05 FILLER      PIC X(30) VALUE "REVISTA DAS RELIGIÕES".
           05 FILLER      PIC X(30) VALUE "AVENTURAS NA HISTÓRIA".
           05 FILLER      PIC X(30) VALUE "ABRIL ASSINATURAS".
           05 FILLER      PIC X(30) VALUE "RSVP".
           05 FILLER      PIC X(30) VALUE "ELLE".
           05 FILLER      PIC X(30) VALUE "RSVP - CORTESIA".
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE "JULIA".
           05 FILLER      PIC X(30) VALUE "CONTIGO".
           05 FILLER      PIC X(30) VALUE "CARAS".
           05 FILLER      PIC X(30) VALUE "VIP EXAME".
           05 FILLER      PIC X(30) VALUE "ESPECIAIS CARA".
           05 FILLER      PIC X(30) VALUE "SPACES".
           05 FILLER      PIC X(30) VALUE "ONESHOT".
           05 FILLER      PIC X(30) VALUE "SPACES".
           05 FILLER      PIC X(30) VALUE "SPACES".
           05 FILLER      PIC X(30) VALUE "SPACES".
           05 FILLER      PIC X(30) VALUE "SPACES".
           05 FILLER      PIC X(30) VALUE "SPACES".
           05 FILLER      PIC X(30) VALUE "SPACES".
           05 FILLER      PIC X(30) VALUE "SPACES".
           05 FILLER      PIC X(30) VALUE "SPACES".
           05 FILLER      PIC X(30) VALUE "PACOTE/VENDAS CASADAS".
         03 TB-PROJ REDEFINES TABELA-PROJ PIC X(30) OCCURS 99 TIMES.
      *
      *
      ****************************************************************
      *
       PROCEDURE DIVISION CHAINING LK-PATH LK-NOME LK-DATA.    
       INICIO.
             OPEN INPUT  PENTR
                  OUTPUT PSAIDA PERRO.

            PERFORM TELA     THRU FIM-TELA
            PERFORM CONTAGEM THRU FIM-CONTAGEM

            PERFORM LER THRU FIM-LER
            PERFORM PROCESSA THRU FIM-PROCESSA
                   UNTIL W-FIM = "S".

           CLOSE PENTR PSAIDA PERRO.

           IF W-GRAVADOS = 0
              DISPLAY (4 1) ERASE
              DISPLAY (13 20) "Nao ha registro valido p/ geracao"
              DISPLAY (14 20) "do pedido, veja o relatorio de"
              DISPLAY (15 20) "critica."
              DISPLAY (17 20) "   Pressione a tecla <ENTER>"
              ACCEPT  (21 1)  W-AUX
              STOP RUN.

           OPEN OUTPUT PFLAG
           MOVE SPACES TO REGOK
           WRITE REGOK
           CLOSE PFLAG     

           CHAIN "XPGR303.INT" USING LK-PATH LK-NOME LK-DATA.
      *****************************************************************
       LER.
             READ PENTR AT END
                  MOVE "S" TO W-FIM W-TIPO2
                  GO   TO FIM-LER.

             IF E-TIPO NOT = "XX"
                 ADD 1 TO W-LIDOS
             ELSE
                 MOVE "S" TO W-TIPO2.
       FIM-LER.
            EXIT.
      *****************************************************************
       PROCESSA.
             IF E-TIPO NOT = "XX"
                PERFORM TIPO-01 THRU FIM-TIPO-01.

             PERFORM LER THRU FIM-LER.
       FIM-PROCESSA.
              EXIT.
      ****************************************************************
       TELA.
           DISPLAY (2 16)
           "ABRIL-MD WELCOME -XPGR36- FORMATACAO DO PEDIDO-CARTAS"
           DISPLAY (10 20) "Aguarde...".
       FIM-TELA.
           EXIT.
      ****************************************************************
       TIPO-01.
            MOVE SPACES       TO REGSAI

            MOVE E-TIPO       TO S-TIPO
            MOVE E-CARTA      TO S-CARTA
            MOVE E-SEQ-REG    TO S-SEQ-REG
            MOVE E-SEQ-CARNE  TO S-SEQ-CARNE
            MOVE E-ASS-GERENTE TO S-ASS-GERENTE
            MOVE E-PARTE1-ASS TO S-PARTE1-ASS
            MOVE E-CEP5-ASS   TO S-CEP5-ASS
            MOVE E-CEP3-ASS   TO S-CEP3-ASS
            MOVE "-"          TO S-TRACO-ASS
            MOVE E-SEXO-ASS   TO S-SEXO-ASS

            MOVE E-PARTE1-PRE TO S-PARTE1-PRE
            MOVE E-CEP5-PRE   TO S-CEP5-PRE
            MOVE E-CEP3-PRE   TO S-CEP3-PRE
            MOVE "-"          TO S-TRACO-PRE
            MOVE E-SEXO-PRE   TO S-SEXO-PRE

            MOVE W-DD-PROC    TO S-DD-PROC
            MOVE "/"          TO S-TRACO1-DT
            MOVE W-MM-PROC    TO S-MM-PROC
            MOVE "/"          TO S-TRACO2-DT
            MOVE W-AA-PROC    TO S-AA-PROC
            MOVE TB-MES-R(W-MM-PROC) TO S-MES-PROC
            MOVE E-ENTR-DIRETA TO S-ENTR-DIRETA
            MOVE E-OUTRAS      TO S-OUTRAS

            MOVE E-0800       TO S-0800
            MOVE E-GSP        TO S-GSP

            MOVE E-SIGLA-DB   TO S-SIGLA-DB
            MOVE E-DB-CARTAO  TO S-DB-CARTAO
            MOVE E-NOME-INST  TO S-NOME-INST

            MOVE E-BANCO      TO S-BANCO
            MOVE E-AGENCIA    TO S-AGENCIA
            MOVE E-CONTA      TO S-CONTA

            MOVE 1 TO W-IND
            PERFORM PROJETO THRU FIM-PROJETO
                     UNTIL W-IND > 5.
       FIM-TIPO-01.
           EXIT.
      ****************************************************************
       PROJETO.
              IF E-NOME-PROJ (W-IND) = SPACES  AND
                 E-NR-PROJ (W-IND) = SPACES
                 GO  TO  SAI-PROJETO.

      *----------------------------------------------------------------
      *  23/05/06 Excluir o projeto 987 nas condi‡oes abaixo:
      *----------------------------------------------------------------
           
            IF (E-NR-PROJ (W-IND) = "987")   AND
               (E-NR-COPIAS (W-IND) = "001") AND
               (E-VL-COPIA (W-IND) = 1988)
                   WRITE REGERRO FROM REGENTR-1
                   GO TO SAI-PROJETO.            


      *  pega nome direto da tabela  ----------- DAQUI GERAR CRITICA
      *        IF E-NOME-PROJ (W-IND) = SPACES
                  MOVE E-NR-PROJ (W-IND) TO W-INDPR
                  MOVE TB-PROJ (W-INDPR) TO E-NOME-PROJ (W-IND).

               IF E-NR-PROJ (W-IND) NOT NUMERIC  OR
                   E-VL-ASS-CAPA (W-IND) NOT NUMERIC OR
                   E-VL-PARC (W-IND) NOT NUMERIC
                   GO TO SAI-PROJETO.

               IF E-NR-PROJ (W-IND) < 900
                  GO    TO  SAI-PROJETO.
      *-------------------------------------------- ATE AQUI

              MOVE E-ASS-GER TO S-ASS-GER

      * ALTERADO CONFORME SOLICITACAO ODRACIL / AFONSO 16/05/01
      * CONDICAO ALTERADA EM 08/04/04 CONF. SOLICITACAO DO SR AFONSO.
      * AFONSO E-MAIL 20/07/04 

              IF E-ASS-GER EQUAL TO 020
                     MOVE 040 TO S-ASS-GER
                     MOVE 040 TO E-ASS-GER.

              IF E-ASS-GER EQUAL TO 020
                  IF E-NR-PROJ (W-IND) = 901
                     MOVE 038 TO S-ASS-GER
                     MOVE 038 TO E-ASS-GER
                   ELSE
                     IF E-NR-PROJ (W-IND) = 986
                        MOVE 011 TO S-ASS-GER
                        MOVE 011 TO E-ASS-GER
                     ELSE
                        IF E-NR-PROJ (W-IND) = 914
                           MOVE 022 TO S-ASS-GER
                           MOVE 022 TO E-ASS-GER.
         
            MOVE E-NR-PROJ (W-IND)   TO S-NR-PROJ

            MOVE E-NOME-PROJ (W-IND)   TO S-NOME-PROJ
            MOVE E-EDI-INIC (W-IND)    TO S-EDI-INIC
            MOVE E-DT-EDI-INIC (W-IND) TO S-DT-EDI-INIC
            MOVE E-MMAA-INIC (W-IND)   TO S-MMAA-INIC

            MOVE E-EDI-FIM (W-IND)    TO S-EDI-FIM
            MOVE E-DT-EDI-FIM (W-IND) TO S-DT-EDI-FIM
            MOVE E-MMAA-FIM (W-IND)   TO S-MMAA-FIM

            MOVE E-DT-VIG-INIC (W-IND) TO S-DT-VIG-INIC
            MOVE E-DT-VIG-FIM (W-IND)  TO S-DT-VIG-FIM
            MOVE E-NR-COPIAS (W-IND)   TO S-NR-COPIAS
            MOVE E-VIGENCIA (W-IND)    TO W-ED-VIG
            MOVE W-ED-VIG              TO S-VIGENCIA

            DIVIDE E-VL-PARC (W-IND) BY 100 GIVING W-VALOR
            MOVE W-VALOR       TO S-VL-PARC

            DIVIDE E-VL-ASS-CAPA (W-IND)  BY 100 GIVING W-VALOR
            MOVE W-VALOR       TO S-VL-ASS-CAPA

            MOVE E-QTDE-PARC (W-IND) TO S-QTDE-PARC

            MOVE E-DD-VENC (W-IND) TO S-DD-VENC
            MOVE "/"               TO S-TRACO1-VC
            MOVE E-MM-VENC (W-IND) TO S-MM-VENC
            MOVE "/"               TO S-TRACO2-VC
            MOVE E-AA-VENC (W-IND) TO S-AA-VENC

            MOVE 1  TO W-IND2.
            PERFORM ASSINAT THRU FIM-ASSINAT.

            MOVE TB-GER-R(W-IND2)   TO S-NOME-GER
            MOVE TB-CARGO-R(W-IND2) TO S-CARGO-GER

      *- CONTROLE DA ABRIL
       
            MOVE E-TP-ACAO   TO TP-ACAO
            MOVE E-COD-ACAO  TO COD-ACAO
            MOVE E-SEQ-REG   TO SEQ-REG
            MOVE W-DT-PROC   TO DT-PROC
            MOVE E-SEQ-CARNE TO CARNE
            MOVE E-CARTA     TO CARTA
            MOVE CONTROLE-ABR TO S-CONTROLE-ABR

            MOVE E-CHAVE-ABRIL TO S-CHAVE-ABRIL

            WRITE REGSAI.
            ADD 1 TO W-GRAVADOS.
       SAI-PROJETO.
            ADD 1 TO W-IND.
       FIM-PROJETO.
           EXIT.
      ****************************************************************
       CONTAGEM.
            PERFORM LER THRU FIM-LER

            IF W-TIPO2 = SPACES
               GO  TO CONTAGEM.

            IF E-TIPO NOT = "XX"
               DISPLAY (3 1) ERASE
               DISPLAY (17 20) "Tipo de reg desconhecido: " e-tipo
               DISPLAY (18 20) "processo cancelado, Tecle <ENTER>"
               DISPLAY (19 20) "reg. nr." w-lidos
               ACCEPT (21 1) W-AUX
               STOP RUN.

            MOVE SPACES TO W-FIM
            SUBTRACT 1 FROM E-QTDE-REG GIVING W-INFOR
            MOVE W-INFOR TO TOT-REG
            MOVE E-DATA-PROC  TO W-DT-PROC
            CLOSE PENTR
            OPEN INPUT PENTR.
            MOVE SPACES TO W-FIM.
            MOVE 0      TO W-LIDOS.
       FIM-CONTAGEM.
           EXIT.
      ****************************************************************
       ASSINAT.
            IF E-ASS-GER NOT = TB-ASS-R(W-IND2)
               ADD 1 TO W-IND2
               IF  W-IND2 NOT GREATER THAN 99
                  GO TO ASSINAT
               ELSE
                 DISPLAY (4 1) ERASE
                 display (15 20) "Assinatura de gerente nao consta"
                 DISPLAY (16 20) "na tabela interna - verificar"
                 DISPLAY (17 20) "reg. nr." w-lidos
                 DISPLAY (18 20) "cod.ass. " e-ass-ger
                 DISPLAY (19 20) "processo cancelado, Tecle <ENTER>"
                 ACCEPT  (19 53) W-AUX
                 STOP RUN.

       FIM-ASSINAT.
           EXIT.
      ****************************************************************
