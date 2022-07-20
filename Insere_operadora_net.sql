insert into controle_operadoras_net.tabela_operadoras_net(
SELECT razaosocial, "948", base, cartao, fantasia, cidade, of, of_digito, cnpj, operadora_referencia, acf, sto, codigo_adm_contrato, codigo_ori_postagem, cep_origem, codigo_reserva, contrato, foi_anchieta, uf
       FROM controle_operadoras_net.tabela_operadoras_net
where operadora = 3
);
insert into controle_operadoras_net.tabela_operadoras_net_cobranca(
SELECT razaosocial, "948", base, cartao, fantasia, cidade, of, of_digito, cnpj, operadora_referencia, acf, sto, codigo_adm_contrato, codigo_ori_postagem, cep_origem, codigo_reserva, contrato, foi_anchieta, uf
       FROM controle_operadoras_net.tabela_operadoras_net_cobranca
where operadora = 3
);
insert into controle_operadoras_net.tabela_operadoras_net_comercial(
SELECT razaosocial, "948", base, cartao, fantasia, cidade, of, of_digito, cnpj, operadora_referencia, acf, sto, codigo_adm_contrato, codigo_ori_postagem, cep_origem, codigo_reserva, contrato, foi_anchieta, uf
       FROM controle_operadoras_net.tabela_operadoras_net_comercial
where operadora = 3
);
insert into controle_operadoras_net.tabela_operadoras_net_faturamento(
SELECT razaosocial, "948", base, cartao, fantasia, cidade, of, of_digito, cnpj, operadora_referencia, acf, sto, codigo_adm_contrato, codigo_ori_postagem, cep_origem, codigo_reserva, contrato, foi_anchieta, uf
       FROM controle_operadoras_net.tabela_operadoras_net_faturamento
where operadora = 3
);