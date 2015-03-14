#include <X11/Xauth.h>

main (argc, argv)
char	**argv;
{
    Xauth   test_data;
    char    *name, *data, *file;
    int	    state = 0;
    FILE    *output;

    while (*++argv) {
	if (!strcmp (*argv, "-file"))
	    file = *++argv;
	else if (state == 0) {
	    name = *argv;
	    ++state;
	} else if (state == 1) {
	    data = *argv;
	    ++state;
	}
    }
    if(!file) {
	fprintf (stderr, "No file\n");
	exit (1);
    }
    test_data.family = 0;
    test_data.address_length = 0;
    test_data.address = "";
    test_data.number_length = 0;
    test_data.number = "";
    test_data.name_length = strlen (name);
    test_data.name = name;
    test_data.data_length = strlen (data);
    test_data.data = data;
    output = fopen (file, "w");
    if (output) {
	XauWriteAuth (output, &test_data);
	fclose (output);
    }
}
