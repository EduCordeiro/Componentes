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



/* COMANDO PARA TROCAR OS CARACTER xA0 POR x20, ESPAÇOS */
select * from plano_de_triagem.net_default
where cdd in(
  SELECT cdd FROM plano_de_triagem.net_default
  WHERE LOCATE('A0', HEX(cdd)) > 0
);


/* COMANDO PARA TROCAR CARACTERES ESPECIAIS NO CDD */
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'à', 'a') WHERE LOCATE('à', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'â', 'a') WHERE LOCATE('â', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'ê', 'e') WHERE LOCATE('ê', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'ô', 'o') WHERE LOCATE('ô', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'û', 'u') WHERE LOCATE('û', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'ã', 'a') WHERE LOCATE('ã', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'õ', 'o') WHERE LOCATE('õ', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'á', 'a') WHERE LOCATE('á', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'é', 'e') WHERE LOCATE('é', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'í', 'i') WHERE LOCATE('í', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'ó', 'o') WHERE LOCATE('ó', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'ú', 'u') WHERE LOCATE('ú', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'ç', 'c') WHERE LOCATE('ç', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'ü', 'u') WHERE LOCATE('ü', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'À', 'A') WHERE LOCATE('À', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Â', 'A') WHERE LOCATE('Â', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Ê', 'E') WHERE LOCATE('Ê', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Ô', 'O') WHERE LOCATE('Ô', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Û', 'U') WHERE LOCATE('Û', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Ã', 'A') WHERE LOCATE('Ã', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Õ', 'O') WHERE LOCATE('Õ', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Á', 'A') WHERE LOCATE('Á', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'É', 'E') WHERE LOCATE('É', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Í', 'I') WHERE LOCATE('Í', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Ó', 'O') WHERE LOCATE('Ó', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Ú', 'U') WHERE LOCATE('Ú', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Ç', 'C') WHERE LOCATE('Ç', CDD) > 0;
UPDATE plano_de_triagem.net_default SET CDD = REPLACE(CDD, 'Ü', 'U') WHERE LOCATE('Ü', CDD) > 0;

/* COMANDO PARA TROCAR CARACTERES ESPECIAIS NO CTC */
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'à', 'a') WHERE LOCATE('à', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'â', 'a') WHERE LOCATE('â', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'ê', 'e') WHERE LOCATE('ê', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'ô', 'o') WHERE LOCATE('ô', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'û', 'u') WHERE LOCATE('û', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'ã', 'a') WHERE LOCATE('ã', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'õ', 'o') WHERE LOCATE('õ', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'á', 'a') WHERE LOCATE('á', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'é', 'e') WHERE LOCATE('é', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'í', 'i') WHERE LOCATE('í', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'ó', 'o') WHERE LOCATE('ó', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'ú', 'u') WHERE LOCATE('ú', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'ç', 'c') WHERE LOCATE('ç', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'ü', 'u') WHERE LOCATE('ü', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'À', 'A') WHERE LOCATE('À', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Â', 'A') WHERE LOCATE('Â', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Ê', 'E') WHERE LOCATE('Ê', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Ô', 'O') WHERE LOCATE('Ô', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Û', 'U') WHERE LOCATE('Û', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Ã', 'A') WHERE LOCATE('Ã', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Õ', 'O') WHERE LOCATE('Õ', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Á', 'A') WHERE LOCATE('Á', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'É', 'E') WHERE LOCATE('É', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Í', 'I') WHERE LOCATE('Í', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Ó', 'O') WHERE LOCATE('Ó', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Ú', 'U') WHERE LOCATE('Ú', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Ç', 'C') WHERE LOCATE('Ç', CTC) > 0;
UPDATE plano_de_triagem.net_default SET CTC = REPLACE(CTC, 'Ü', 'U') WHERE LOCATE('Ü', CTC) > 0;
