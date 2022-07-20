-- --------------------------------------------
-- MUDA O DELIMITADOR
-- --------------------------------------------
DELIMITER ;

-- --------------------------------------------
-- SETA A BASE DE WORK
-- --------------------------------------------
create database if not exists test;
USE test;

-- --------------------------------------------
-- EXCLUI TABELAS E STORED PROCEDURES JÁ EXISTENTES
-- --------------------------------------------
drop procedure if exists insertDados;
drop procedure if exists getNomeOperadora;

drop table if exists test.CEDO_TESTE;
drop table if exists test.debug;

-- --------------------------------------------
-- CRIA TABELAS
-- --------------------------------------------
create table if not exists test.CEDO_TESTE(
  CONTRATO       VARCHAR(013),
  CIF            VARCHAR(034),
  NOME_OPERADORA VARCHAR(100)
);

create table if not exists test.debug(
  TOTAL INT
);

-- --------------------------------------------
-- MUDA O DELIMITADOR
-- --------------------------------------------
delimiter |
-- --------------------------------------------

-- --------------------------------------------
-- CRIA STORED PROCEDURE
-- --------------------------------------------
CREATE PROCEDURE insertDados()
BEGIN

  -- ------------------------------------------------
  -- Definição de variáveis utilizadas na Procedure
  -- ------------------------------------------------
  DECLARE iContLinhas        int  DEFAULT 0;
  DECLARE existe_mais_linhas INT DEFAULT 0;
  DECLARE sNomeOperadora     char(100);
  DECLARE sOperadora         char(03);
  DECLARE sContrato          char(13);
  DECLARE sCif               char(34);

  -- ------------------------------------------------------------------------------------------------------------
  -- Definição do cursor
  -- ------------------------------------------------------------------------------------------------------------
  DECLARE meuCursor                 CURSOR FOR (SELECT contrato, COD_BARRAS_FAC FROM cedo.cedo_net limit 1000);
  -- -----------------------------------------------------------------------------------------------------------

  -- ------------------------------------------------------------------
  -- Definição da variável de controle de looping do cursor
  -- ------------------------------------------------------------------
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET existe_mais_linhas=1;

  -- --------------------------------------------
  -- Abertura do cursor
  -- --------------------------------------------
  OPEN meuCursor;
  -- AGORA TODOS OS COMANDO PODEM SER EXCECUTADOS COMO SELECT, INSERT, E CHAMADAS DE PROCEDIMENTOS
  -- SOMENTE APÓS O CURSOR FOR ABERTO
  -- --------------------------------------------

   delete from test.CEDO_TESTE;
   delete from test.debug;

  -- --------------------------------------------
  -- Looping de execução do cursor
  -- --------------------------------------------
  meuLoop: LOOP

       FETCH meuCursor INTO sContrato, sCif;

       -- --------------------------------------------
       -- Controle de existir mais registros na tabela
       -- --------------------------------------------
       IF existe_mais_linhas = 1 THEN
         LEAVE meuLoop;
       END IF;
       -- --------------------------------------------

       set sOperadora = mid(sContrato, 1, 3);

       -- --------------------------------------------
       -- EXECUTA A STORED PROCEDURE
       -- --------------------------------------------
       call getNomeOperadora(sOperadora, sNomeOperadora);

       -- --------------------------------------------
       -- INSERE OS DADOS NA TABELA
       -- --------------------------------------------------------------------------------------------------
    	 INSERT INTO test.CEDO_TESTE(CONTRATO, CIF, NOME_OPERADORA) VALUES(sContrato, sCif, sNomeOperadora);
       -- --------------------------------------------------------------------------------------------------

       -- --------------------------------------------
       -- INCREMENTA O CONTADOR
       -- --------------------------------------------
       SET iContLinhas = iContLinhas + 1;
       -- --------------------------------------------


   -- --------------------------------------------
   -- Retorna para a primeira linha do loop
   -- --------------------------------------------
  END LOOP meuLoop;

  -- --------------------------------------------
  -- FECHA O CURSOR
  -- --------------------------------------------
  CLOSE meuCursor;
  -- --------------------------------------------

  -- --------------------------------------------
  -- ALIMENTA A TABELA DE DEBUG
  -- --------------------------------------------
  INSERT INTO test.debug(TOTAL) VALUES(iContLinhas);


END;

|

-- ---------------------------------------------------------------------------------------
-- CRIA UMA STORED PROCEDURE
-- ---------------------------------------------------------------------------------------
CREATE PROCEDURE getNomeOperadora(in CodigoOperadora int, out nomeOperadora varchar(100))
begin

  -- ------------------------------------------------
  -- Definição de variáveis utilizadas na Procedure
  -- ------------------------------------------------
  DECLARE existe_mais_linhas INT DEFAULT 0;
  DECLARE sNomeOperadora     char(100);

  -- ------------------------------------------------------------------------------------------------------------
  -- Definição do cursor
  -- ------------------------------------------------------------------------------------------------------------
  DECLARE meuCursorTabelaOperadoras CURSOR FOR (SELECT fantasia FROM controle_operadoras_net.tabela_operadoras_net where operadora = CodigoOperadora);
  -- ------------------------------------------------------------------------------------------------------------


  -- ------------------------------------------------------------------
  -- Definição da variável de controle de looping do cursor
  -- ------------------------------------------------------------------
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET existe_mais_linhas=1;

  -- --------------------------------------------
  -- Abertura do cursor
  -- --------------------------------------------
  OPEN meuCursorTabelaOperadoras;



  FETCH meuCursorTabelaOperadoras INTO nomeOperadora;


  -- --------------------------------------------
  -- FECHA O CURSOR
  -- --------------------------------------------
  CLOSE meuCursorTabelaOperadoras;

end;