/**************************************************************************
      
       Project :  Xspread
       team    :  James Cornelius
		  Michael Frey
		  Dan Gruber
		  Fang Wang

       History :  Mouse Implementation
			--"click" to a cell
			--select menu
			--more benifits refer to DOC
                  Graphs
			--modified Bar graph
			--added Pie graph
			--added line graph
			--added stack bar graph
			--legends option avaibale
                  Matrix Operation
			--added "Matrix" menu
			--matrix operatoins: 
                                 .addition
				 .subtraction
				 .multplication
				 .inversion
                        --solving system of linear equations by using
			  inversion and multiplication
                  Bugs Fixes and Special Features:
			--changed standard entry mode to "automatic"
			  entry mode.Allow keys and mouse "clicks"
			  to update edited cells.
                        --corrected menu to digress up one level
			  when <ESC> is pressed,or mouse is "clicked"
			  outside of menu.
                        --improved error soubroutine so there is no
			  pause on errors
                        --corrected the use of "@" functions to modify
			  the cell immediately when pressing <enetr> key.
                        --fixed "format" option in Menu

     Cousre Name : Introduction to Software Engineering Computer Science 536
     Begin       : 9/4/91
     End         : 12/11/91
     

     Mouse implementation:  modifications were made in the following files:
	                -- sc.c
			-- scXstuff.c

     Graph implementation:  modifications were made in the following files:
			-- graphic_main.c
			-- graphic_gvar.h
			-- plot_XY.c
			-- plot_line.c
			-- plot_bar.c
			-- plot_stacked_bar.c
			-- plot_pie.c

     Matrix implementation:  modifications were made in the following files:
			-- sc.c

