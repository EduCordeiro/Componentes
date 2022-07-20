
///// sortparms //////////////////////
	unsigned char gnusort_parm1[81];
	unsigned char gnusort_parm2[81];
//



	if (rs_filesize[1]==0l)
		goto sort_rs2;
	strcpy (gnusort_parm1,temp_dir);
	strcpy (gnusort_parm2,temp_dir);
	strcat (gnusort_parm1,save_num_pedido);
	strcat (gnusort_parm2,save_num_pedido);
	strcat (gnusort_parm1,".RL1");
	strcat (gnusort_parm2,".SR1");


	i2 = spawnl (P_WAIT,"GNUSORT.EXE","GNUSORT","-k1.1,1.10",
		gnusort_parm1,"-o",gnusort_parm2,NULL);
	if (i2!=0)
		{
		obtem_data ();
		cprintf("Falha no SORT. FALTA arquivo ou o GNUSORT.EXE no DIR dos Programas!\r\n");
		sprintf(linha_log,"s Falha no SORT. FALTA arquivo ou o GNUSORT.EXE no DIR dos Programas!\r\n",data_hora);
		grava_log ();
		cprintf("Num Pedido: %s \r\n",save_num_pedido);
		sprintf(linha_log,"%s Num Pedido: %s \r\n",data_hora,save_num_pedido);
		grava_log ();
		close (logfile);
		exit (-1);
		}
