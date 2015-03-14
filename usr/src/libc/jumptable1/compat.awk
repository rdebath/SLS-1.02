	{
		printf "\t\t\t$1 == \"%s\" || \\\n", substr ($3, 2, length ($3) -1);
	}
