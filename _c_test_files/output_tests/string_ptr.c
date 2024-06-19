int	main()
{
	unsigned char *str = (unsigned char *)"hello world!";
	unsigned char *str_malloc = (unsigned char *)malloc(sizeof(char) * 13);

	for (int i = 0; i < 13; i++)
	{
		str_malloc[i] = str[i];
	}

	printf("%s\n", str_malloc);

	return 0;
}