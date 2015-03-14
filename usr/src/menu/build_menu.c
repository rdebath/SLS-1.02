# include <stdio.h>
# include "menu.h"

# define	TOPLINE		"PSC Menu Builder Utility"
# define	MIDDLE		"Help Window"

typedef enum FLAG {true,false};

menu_ent menu_b[9];

int catch();

box_screen()
/*
	Draw a box around the screen.
*/
{
	int i;
	char buf[256];

	Sprintf(buf,"%80s"," ");
	start_rev();
	move_csr(0,FIRST_LINE);
	print_str(buf);
	move_csr(0,STATUS_LINE);
	print_str(buf);
	for(i=1;i<22;i++){
	   move_csr(0,i);
	   outc(' ');
	   move_csr(79,i);
	   outc(' ');
	}
	end_rev();
}

erase_line(length)
int length;
{
	int i;
	
	for(i=0;i<length;i++)
	   outc(' ');
}

clr_helpwin()
{
	move_csr(5,13);
	erase_line(70);
	move_csr(5,14);
	erase_line(70);
	move_csr(5,15);
	erase_line(70);
	move_csr(5,16);
	erase_line(70);
	move_csr(5,17);
	erase_line(70);
	move_csr(5,18);
	erase_line(70);
	move_csr(5,19);
	erase_line(70);
	move_csr(5,20);
	erase_line(70);
	move_csr(5,21);
	erase_line(70);
}

display_help()
{
	move_csr(5,14);
	hi_lite("Screen Header:");
	print_str("  Menu Topic description.");
	move_csr(5,15);
	hi_lite("Command name :");
	print_str("  Command to be executed.");
	move_csr(5,16);
	hi_lite("Menu Comment :");
	print_str("  Line that will appear in the Menu");
	move_csr(5,17);
	print_str("                For This command.");
	move_csr(5,18);
	hi_lite("Help File    :");
	print_str("  Name of the help file for this command.");
}

show_key_types()
{
	clr_helpwin();
	move_csr(5,13);
	hi_lite("Key");
	move_csr(11,13);
	hi_lite("Description");
	move_csr(5,15);
	hi_lite("+");
	move_csr(11,15);
	print_str("Command output goes to Menu Window, no inter, no cursor.");
	move_csr(5,16);
	hi_lite("*");
	move_csr(11,16);
	print_str("Command is executed in a sub-shell.");
	move_csr(5,17);
	hi_lite("&");
	move_csr(11,17);
	print_str("This Option specifies the command is a menu to load.");
	move_csr(5,18);
	hi_lite("@");
	move_csr(11,18);
	print_str("Specifies that the menu to be loaded is a Main Menu.");
	move_csr(5,19);
	hi_lite("1");
	move_csr(11,19);
	print_str("Exec a regular shell and ask a file name.");
}

print_to_file()
{
	FILE *fp;
	int i;
	
	if((fp = fopen("menu.out","w")) == (FILE *)NULL)
	   print_str("Nope");
	else{
	   fprintf(fp,"$ %s\n",menu_b[0].desc);
	   for(i=1;i<8;i++){
	      if(menu_b[i].key != '\0'){
	         fprintf(fp,"%c %s\n",menu_b[i].key,menu_b[i].cmd);
	         fprintf(fp,"# %s\n",menu_b[i].help_fil);
	         fprintf(fp,"? %s\n",menu_b[i].desc);
	      }
	   }
	}
	fclose(fp);
}

main()
{
	int i;
	char buf[LINESZ], key;
	enum FLAG no_key = false, done = false;

	if(get_term() == -1){
	   Fprintf(stderr,"Sorry, Bad termcap entry. Exiting.");
	   exit(1);
	}
	setup_sigs();
	save_tty();
	no_echo();
	cbreak();
	clr_scr();
	box_screen();
	move_csr(40-(strlen(TOPLINE)/2),0);
	hi_lite(TOPLINE);
	move_csr(0,11);
	Sprintf(buf,"%80s"," ");
	hi_lite(buf);
	move_csr(40-(strlen(MIDDLE)/2),11);
	hi_lite(MIDDLE);
	move_csr(5,3);
	print_str("Screen Header:     ");
	read_str(menu_b[0].desc,30);
	i = 0;
	while(done == false && i < 8){
	   i++;
	   display_help();
	   move_csr(5,5);
	   print_str("Command Name:        ");
	   read_str(menu_b[i].cmd,50);
	   if(strlen(menu_b[i].cmd) == 0)
	     done = true;
	   if(done == false){
	     move_csr(5,6);
	     print_str("Menu Comment:        ");
	     read_str(menu_b[i].desc,30);
	     move_csr(5,7);
	     print_str("Help file Name:      ");
	     read_str(menu_b[i].help_fil,50);
	     move_csr(5,8);
	     print_str("Menu Key Type:       ");
	     show_key_types();
	     no_key = false;
	     do{
	        key = getchar();
	        switch(key){
		    case '#':
		    case '@':
		    case '+':
		    case '*':
		    case '&':
		    case '?':
		    case '1':
		    case '2':
		    case '3':
		    case '4':
	                no_key = true;
		    break;
		    case '\n':
		    case '\r':
		    break;
		    default:
		    break;
	        }
	     }while(no_key == false);
	     menu_b[i].key = key;
	     move_csr(26,5);
	     erase_line(strlen(menu_b[i].cmd)+3);
	     move_csr(26,6);
	     erase_line(strlen(menu_b[i].desc)+3);
	     move_csr(26,7);
	     erase_line(strlen(menu_b[i].help_fil)+3);
	     clr_helpwin();
	   }
	}
	print_to_file();
	move_csr(0,23);
	echo();
	no_cbreak();
	exit(0);
}
