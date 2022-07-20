       IDENTIFICATION DIVISION.                                 
       PROGRAM-ID.    XPGR47.
      *------------------------------------------------------------
      * 06/03/2001
      * ABRIL - OF20700 - CARTA WELCOME
      * CONSISTE, CRITICA E LISTA REGISTROS
      * 19/03/2002 - VERSAO DO FPGR47 P/ XEROX, RECEBE E REPASSA
      *               PARAMETROS
      * 15/03/2004 - alteracao de layout
      * 25/07/2005 - nova welcome - alteracao de cartas/conceito
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

       DATA DIVISION.                                           
       FILE SECTION.                                            
       FD  PENTR 
        LABEL RECORD IS STANDARD
        VALUE OF FILE-ID IS LK-PATH.
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
           05 E-VL-PARC        PIC 9(09).
           05 E-VL-ASS-CAPA    PIC 9(09).
           05 E-VIGENCIA       PIC X(05).
         03 E-QTDE-PARC        PIC 9(02).
         03 E-PARC-ATRAS-1     PIC 9(02).
         03 E-PARC-PAGAS       PIC 9(02).
         03 E-TOT-DEB-ATRAS    PIC 9(09).
         03 E-TOT-PAGO         PIC 9(09).
         03 E-PARC-ATRASADAS OCCURS 5 TIMES.
           05 E-VL-PARC-ATRAS  PIC 9(09).
           05 E-VENC.
             07 E-DD-VENC-ATRAS  PIC X(02).
             07 E-MM-VENC-ATRAS  PIC X(02).
             07 E-AA-VENC-ATRAS  PIC X(04).
           05 E-PARC-ATRAS-2   PIC 9(02).
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
        VALUE OF FILE-ID IS W-SAIDA.
       01  REGSAI.
         03 FILLER PIC X(120).
      *---------------------------------------------------------
       WORKING-STORAGE SECTION.                                 
       77 LK-PATH          PIC X(50) VALUE SPACES.
       77 LK-NOME          PIC X(20) VALUE SPACES.
       77 LK-DATA          PIC X(08) VALUE SPACES.
       01  AREAS-DE-TRABALHO.                                   
         03 CABEC1.
           05 FILLER      PIC X(32) VALUE 
           "ABRIL - MD WELCOME - proc.Abr.: ".
           05 CAB-DDA     PIC X(02).
           05 FILLER      PIC X(01) VALUE "/".
           05 CAB-MMA     PIC X(02).
           05 FILLER      PIC X(01) VALUE "/".
           05 CAB-AAA     PIC X(04).
           05 FILLER      PIC X(16) VALUE "  proc. Finger: ".
           05 CAB-DT-FING  PIC X(08).
           05 FILLER      PIC X(01) VALUE SPACES.
           05 CAB-PEDIDO  PIC X(09).
           05 FILLER      PIC X(10) VALUE " TOT.REG: ".
           05 CAB-TOTAL   PIC X(06).
         03 CABEC2.
           05 FILLER      PIC X(100) VALUE 
         "* VERIFICAR VALORES, NR. E NOME DO PROJETO P/ OS REG. ABAIXO".
         03 CABEC3.
           05 FILLER      PIC X(07) VALUE "SEQ.ARQ".
           05 FILLER      PIC X(02) VALUE SPACES.
           05 FILLER      PIC X(30) VALUE "ASSINANTE".
           05 FILLER      PIC X(02) VALUE SPACES.
           05 FILLER      PIC X(07) VALUE "NR.PROJ".         
           05 FILLER      PIC X(02) VALUE  SPACES.
           05 FILLER      PIC X(30) VALUE "NOME PROJ.". 
           05 FILLER      PIC X(02) VALUE SPACES.
           05 FILLER      PIC X(11) VALUE "VLR.ASS".
           05 FILLER      PIC X(02) VALUE  SPACES.
           05 FILLER      PIC X(08) VALUE "VLR.PARC".
         03 CABEC4.
           05 CAB-SEQ     PIC X(09) VALUE SPACES.
           05 CAB-NOME    PIC X(32) VALUE SPACES.
           05 CAB-NR-PROJ      PIC X(09) VALUE SPACES.
           05 CAB-PROJ    PIC X(32) VALUE SPACES.
           05 CAB-VLR-ASS PIC X(11).
           05 FILLER      PIC X(02) VALUE SPACES.
           05 CAB-VLR-PARC PIC X(11).
         03 W-FIM         PIC X(01) VALUE SPACES.
         03 W-TIPO2       PIC X(01) VALUE SPACES.
         03 W-LIDOS       PIC 9(06) VALUE ZEROS.
         03 W-FS          PIC X(02).
         03 W-IND         PIC 9(04) VALUE ZEROS.
         03 W-IND2        PIC 9(04).
         03 W-AUX         PIC X(01) VALUE SPACES.
         03 W-SAIDA.
            05 FILLER    PIC X(03) VALUE "CRI".
            05 W-NOME-C  PIC X(20) VALUE SPACES.
            05 T-NOME-R REDEFINES W-NOME-C PIC X(01) OCCURS 20 TIMES.
         03 W-VALOR       PIC Z.ZZZ.ZZ9,99.
         03 W-NR-PROJ.
            05 W-NOVE     PIC X(01).
            05 FILLER     PIC X(02).     
         03 W-PEDIDO      PIC X(06) VALUE SPACES.
         03 W-DT-PROC.
          05 W-DD-PROC     PIC 9(02).
          05 W-MM-PROC     PIC 9(02).
          05 W-AA-PROC     PIC 9(04).
         03 W-DT-SYS       PIC 9(06).
         03 W-DT-SYS-R REDEFINES W-DT-SYS.
           05 W-AA-SYS    PIC 9(02).
           05 W-MM-SYS    PIC 9(02).
           05 W-DD-SYS    PIC 9(02).

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
      *-----------------------------------------------------------
       PROCEDURE DIVISION CHAINING LK-PATH LK-NOME LK-DATA.
       INICIO.                 
      *       move "c:\abril\desenv\TESTE.TXT" to lk-path
      *       move "teste.txt" to lk-nome
      *       move "14/05/04" to lk-data

             MOVE LK-NOME TO W-NOME-C
             PERFORM MONTA-ARQ THRU FIM-MONTA-ARQ
                                                   
             OPEN INPUT  PENTR
                  OUTPUT PSAIDA.

            PERFORM TELA     THRU FIM-TELA
            PERFORM CONTAGEM THRU FIM-CONTAGEM
     
      *-------------------------------------------- MONTA CABECALHOS
            MOVE E-DD-PROC   TO CAB-DDA
            MOVE E-MM-PROC   TO CAB-MMA
            MOVE E-AA-PROC   TO CAB-AAA
            ACCEPT W-DT-SYS FROM DATE
            MOVE LK-DATA  TO CAB-DT-FING
            MOVE LK-NOME   TO CAB-PEDIDO
            MOVE W-LIDOS  TO CAB-TOTAL
            WRITE REGSAI FROM CABEC1
            MOVE SPACES TO REGSAI
            WRITE REGSAI
            WRITE REGSAI FROM CABEC2
            MOVE SPACES TO REGSAI
            WRITE REGSAI
            WRITE REGSAI FROM CABEC3
            MOVE SPACES TO REGSAI
            WRITE REGSAI
            MOVE 0      TO W-LIDOS

            PERFORM LER THRU FIM-LER
            
            PERFORM PROCESSA THRU FIM-PROCESSA
                   UNTIL W-FIM = "S".

           CLOSE PENTR PSAIDA.

      *     DISPLAY (10 10) "CRITICA CONCLUIDA                       "
      *     DISPLAY (13 10) "- Enviar uma via ao cliente"
      *     ACCEPT  (10 60)  W-AUX

           CHAIN "XPGR42.INT" USING LK-PATH LK-NOME LK-DATA.
      *****************************************************************
       LER.
      *       DISPLAY (10 2) "TIPO " E-TIPO
           
             READ PENTR AT END 
                  MOVE "S" TO W-FIM
                  GO   TO FIM-LER.


             IF E-TIPO = "XX"
                 MOVE "S" TO W-TIPO2 
                 GO TO FIM-LER.

             ADD 1 TO W-LIDOS.
       FIM-LER.
            EXIT.
      *****************************************************************
       PROCESSA.
             IF E-TIPO = "NO" OR E-TIPO = "DN" OR E-TIPO = "PR"
                PERFORM TIPO-01 THRU FIM-TIPO-01.

             PERFORM LER THRU FIM-LER.
       FIM-PROCESSA.
              EXIT.
      ****************************************************************
       TELA.
           DISPLAY (1 1) ERASE
           DISPLAY (2 18) 
           "ABRIL- XPGR47 - MALA WELCOME -  CONSISTE PEDIDO"
      *     DISPLAY (10 20) "Digite nr do pedido: " 
      *     ACCEPT (10 41) W-PEDIDO
           DISPLAY (13 20) "AGUARDE..".
       FIM-TELA.
           EXIT.
      ****************************************************************
       TIPO-01.
            MOVE SPACES       TO REGSAI
            MOVE 1 TO W-IND
            PERFORM PROJETO THRU FIM-PROJETO
                     UNTIL W-IND > 6.
       FIM-TIPO-01.
           EXIT.
      ****************************************************************
       PROJETO.
              IF E-NR-PROJ(W-IND) = SPACES AND 
                 E-NOME-PROJ(W-IND) = SPACES
                 GO TO SAI-PROJETO.
      *----

           IF (E-REVISTA(W-IND) NOT NUMERIC) OR
            (E-NR-PROJ(W-IND) NOT NUMERIC)
           DISPLAY (03 1) ERASE
           DISPLAY (07 15) "ATENCAO   ATENCAO   ATENCAO  ATENCAO!!!!!!"
           DISPLAY (10 5) "Proj invalido - processamento suspenso     "
           DISPLAY (11 5) "anotar os dados abaixo e comunicar   "
           DISPLAY (12 5) "o PCP e o Sr. Afonso da Abril   "
           DISPLAY (13 5) "processar este pedido somente c/ instrucoes"
           DISPLAY (14 5) "do cliente"
           DISPLAY (15 5) "registro nr: " w-lidos
           DISPLAY (16 5) "proj nr: " e-nr-proj (w-ind)
           DISPLAY (17 5) "nome do proj: " e-nome-proj(w-ind)
           DISPLAY (20 5) "Pressione a tecla <ENTER>"
           ACCEPT   (20 31)  W-AUX
           STOP RUN.
      *--       

      *------------------------------------------ TESTA SE HA LOGO P/ O REG   
           MOVE E-REVISTA(W-IND) TO W-IND2
           IF (TB-PROJ(W-IND2) = SPACES) AND 
              (E-NR-PROJ(W-IND) NOT < 900)
           DISPLAY (03 1) ERASE
           DISPLAY (07 15) "ATENCAO   ATENCAO   ATENCAO  ATENCAO!!!!!!"
           DISPLAY (10 5) "Proj invalido - processamento suspenso     "
           DISPLAY (11 5) "anotar os dados abaixo e comunicar   "
           DISPLAY (12 5) "o PCP e o Sr. Afonso da Abril   "
           DISPLAY (13 5) "processar este pedido somente c/ instrucoes"
           DISPLAY (14 5) "do cliente"
           DISPLAY (15 5) "registro nr: " w-lidos
           DISPLAY (16 5) "proj nr: " e-nr-proj (w-ind)
           DISPLAY (17 5) "nome do proj: " e-nome-proj(w-ind)
           DISPLAY (20 5) "Pressione a tecla <ENTER>"
           ACCEPT   (20 31)  W-AUX
           STOP RUN.

      *--       
              MOVE E-NR-PROJ(W-IND) TO W-NR-PROJ
              IF W-NOVE NOT = "9" 
                 PERFORM IMPRIME THRU FIM-IMPRIME
                 GO  TO  SAI-PROJETO.

               IF  E-VL-ASS-CAPA (W-IND) NOT NUMERIC OR
                   E-VL-PARC (W-IND) NOT NUMERIC
                   PERFORM IMPRIME THRU FIM-IMPRIME.
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
               DISPLAY (03 1) ERASE
               DISPLAY (17 20) "Tipo de reg desconhecido: " e-tipo
               DISPLAY (18 20)
               "processo cancelado, pressione a tecla <ENTER>"
               DISPLAY (19 20) "reg. nr." w-lidos
               ACCEPT  (22 01) W-AUX
               STOP RUN.

            MOVE SPACES TO W-FIM
            MOVE E-DT-PROC  TO W-DT-PROC
            CLOSE PENTR
            OPEN INPUT PENTR.
            MOVE SPACES TO W-FIM.

       FIM-CONTAGEM.
           EXIT.
      ****************************************************************
       IMPRIME.
            MOVE W-LIDOS  TO CAB-SEQ
            MOVE E-NOME-ASS TO CAB-NOME
            MOVE E-NR-PROJ (W-IND)   TO CAB-NR-PROJ
            MOVE E-NOME-PROJ (W-IND)   TO CAB-PROJ
            MOVE E-VL-ASS-CAPA (W-IND)    TO CAB-VLR-ASS
            MOVE E-VL-PARC (W-IND)   TO CAB-VLR-PARC
     
            IF E-VL-ASS-CAPA(W-IND) NUMERIC
                DIVIDE E-VL-ASS-CAPA (W-IND)  BY 100 GIVING W-VALOR
               MOVE W-VALOR             TO CAB-VLR-ASS.

            IF E-VL-PARC(W-IND) NUMERIC
                DIVIDE E-VL-PARC (W-IND) BY 100 GIVING W-VALOR
                MOVE W-VALOR       TO CAB-VLR-PARC.

            WRITE REGSAI FROM CABEC4.

       FIM-IMPRIME.
           EXIT.
      ****************************************************************
       MONTA-ARQ.
               ADD 1 TO W-IND.
               IF T-NOME-R(W-IND) NOT = "."
                  GO TO MONTA-ARQ.
               ADD 1 TO W-IND
               MOVE "R" TO T-NOME-R(W-IND)
               ADD 1 TO W-IND
               MOVE "E" TO T-NOME-R(W-IND)
               ADD 1 TO W-IND
               MOVE "L" TO T-NOME-R(W-IND).
       FIM-MONTA-ARQ.
               EXIT.
