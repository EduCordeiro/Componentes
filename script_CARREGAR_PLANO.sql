/* CRIA A TABELA  */
DROP TABLE IF EXISTS plano_de_triagem.net_default;
CREATE TABLE  plano_de_triagem.net_default (
  cepini varchar(8) default NULL,
  cepfin varchar(8) default NULL,
  cdd varchar(40) default NULL,
  ctc varchar(40) default NULL,
  seq int(10) unsigned NOT NULL auto_increment,
  PRIMARY KEY  (seq)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=utf8;

/* COMANDO PARA CARREGAR PLANO */
LOAD DATA INFILE 'C:/Documentos/NET/NET_FATURA/20150128/PLANO_NOVO.csv'
   INTO TABLE plano_de_triagem.net_default
    CHARACTER SET latin1
    FIELDS
        TERMINATED BY ','
    LINES
        TERMINATED BY '\r\n';



/* COMANDO PARA TROCAR OS CARACTER xA0 POR x20, ESPA�OS */
select * from plano_de_triagem.net_default
where cdd in(
  SELECT cdd FROM plano_de_triagem.net_default
  WHERE LOCATE('A0', HEX(cdd)) > 0
);


/* COMANDO PARA TROCAR CARACTERES ESPECIAIS NO CDD */
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'a') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'a') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'e') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'o') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'u') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'a') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'o') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'a') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'e') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'i') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'o') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'u') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'c') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'u') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'A') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'A') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'E') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'O') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'U') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'A') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'O') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'A') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'E') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'I') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'O') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'U') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'C') WHERE LOCATE('�', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, '�', 'U') WHERE LOCATE('�', CDD) > 0;

/* COMANDO PARA TROCAR CARACTERES ESPECIAIS NO CTC */
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'a') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'a') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'e') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'o') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'u') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'a') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'o') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'a') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'e') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'i') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'o') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'u') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'c') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'u') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'A') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'A') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'E') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'O') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'U') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'A') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'O') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'A') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'E') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'I') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'O') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'U') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'C') WHERE LOCATE('�', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, '�', 'U') WHERE LOCATE('�', CTC) > 0;
