//-------------------------------------------------------------------//

//  RLaB plot function. Maybe someday plotting will be built-in, but
//  for now we will do it via this user-function and some global
//  variables. This user-function is only an example. If it does not
//  suit your needs, you should modify it so that it does.

//  Syntax: plot ( x )

//  Description: 

//  plot() takes a single argument which can either be a MATRIX, or a
//  STRING. If x is a MATRIX then the columns are plotted versus the
//  1st column. If x is a STRING then the string is passed directly to
//  GNUPLOT with a newline added to the end.

//  This function could stand some improvement. The ability to plot
//  3-d data (3-columns), ability to plot complex data, etc... 

//-------------------------------------------------------------------//

//
// pinfo is a global list used to hold data that we need from plot()
// to plot(). 
//

pinfo = <<>>;			// Create the global table
pinfo.init = 0;			// Used in plot() as an init flag
pinfo.tmpf = "|rm -f `cat`";	// Used to clean up tmp files
pinfo.files = "";		// A list of tmp files to plot from
pinfo.gnuplot = "|gnuplot";	// The STRING used to write to GNUPLOT
pinfo.term = "set term X11\n";	// The GNUPLOT terminal type

plot = function ( data ) 
{
  local(ans, filenm, i, nplot, plot_cmd, tmps);

  // Initialize

  plot_cmd = "plot"; tmps = "";

  if(!pinfo.init) 
  {
    fprintf(pinfo.gnuplot, "%s", pinfo.term);
    fprintf(pinfo.gnuplot, "set data style lines\n");
    fprintf(pinfo.gnuplot, "set grid\n");
    pinfo.init = 1;
  }

  if(class(data) == "string") 
  {
    fprintf(pinfo.gnuplot, "%s\n", data);

  else

    if(class(data) != "matrix") 
    {
      error("must supply a MATRIX to plot");
    }

    // Close any previously opened files.
    close(pinfo.files);

    //
    // Loop on the number of columns in [data], writing pairs of
    // columns to tmp files. At the same time create the GNUPLOT
    // command to plot the data in each of the files.
    //

    nplot = max ( [1, data.nc - 1] );
    if (nplot > data.nr)
    {
      printf (" Plot %i columns, are you sure (y/n) ? ", data.nc);
      ans = getline ("stdin");
      if (ans.[1] != "y")
      {
        return 0;
      }
    }

    for(i in 1:nplot)
    { 
      // tmp-files
      filenm.[i] = "";
      sprintf(filenm.[i], "rlab-tmpf-%i", i);

      if(nplot == 1) 
      {
        write (filenm.[i], real (data) );
      else
        write (filenm.[i], real (data[;1, i+1]));
      }
      close(filenm.[i]);

      // plot-command
      if(i == 1) 
      {
        sprintf(tmps, " '%s' title '%s-%i'", filenm.[i], name(data), i+1);
      else
        sprintf(tmps, ", '%s' title '%s-%i'", filenm.[i], name(data), i+1);
      }

      plot_cmd = plot_cmd + tmps;
      pinfo.files = pinfo.files + " " + filenm.[i];
    }

    //
    // Send the plot command to GNUPLOT
    //
  
    fprintf (pinfo.gnuplot, "%s\n", plot_cmd);
  
    //
    // Generate the shell command which will delete the tmp
    // files when the command is closed (via pclose()).
    //
  
    fprintf(pinfo.tmpf, pinfo.files);
  }
};
