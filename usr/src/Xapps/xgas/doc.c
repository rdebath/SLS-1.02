char *doc[]={"\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
                              Ideal Gas Simulation\n\
                                  Larry Medwin\n\
                                   April 1991\n\
\n\
\n\
\n\
\n\
\n\
          1.  The Physical Model\n\
\n\
          This is physical simulation of the behavior of an Ideal Gas.\n\
          The gas is represented by molecules in a chamber, which con-\n\
          sists of two boxes separated by a wall with a hole.  Gas\n\
          molecules move around with velocities determined by their\n\
          temperature:\n\
\n\
              Kinetic energy = Thermal energy, or\n\
                 1  2\n\
                 - m v  = K  T\n\
                 2         b\n\
\n\
          Molecular trajectories are linear (inertial) until a colli-\n\
          sion with a wall.  These collisions are inelastic; that is,\n\
          energy is not conserved.  For example, if the wall tempera-\n\
          ture is greater than the molecule temperature, the molecule\n\
          emerges from the collision with increased velocity.  In\n\
          addition, the angle of reflection is only approximately\n\
          equal to the angle of incidence; a random component added to\n\
          the trajectories allows the molecules to approach equili-\n\
          brium.\n\
\n\
          Since the gas is ideal, collisions between molecules are not\n\
          considered.\n\
\n\
\n\
          2.  The Implementation\n\
\n\
          As shown in Figure 1, the Ideal Gas Demo consists of the\n\
          chamber consisting of two boxes, which contain the\n\
          molecules, two temperature controls, and the temperature\n\
          readings under the two separately heated portions of the\n\
          chamber.  There is a wall with a hole separating the two\n\
          boxes of the chamber.\n\
",
"\
\n\
\n\
          2.1.  Widget Hierarchy\n\
\n\
          The HP Widget set was used in the original version; a port\n\
          to the Athena Widget set was implemented by Dave Sternlicht,\n\
",
"\
          of the X Consortium.  The widget hierarchy is described in\n\
          Figure 1.\n\
\n\
\n\
\n\
\n\
          Ideal Gas Simulation                                       1\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
          ____________________________________________________________\n\
\n\
\n\
          toplevel (applicationShell)\n\
              frame (form)\n\
                  quit (command)\n\
                  run (toggle)\n\
                  pause (toggle)\n\
                  step (command)\n\
                  help (command)\n\
                      helpShell (toplevelShell)\n\
                          helpFrame (form)\n\
                              helpQuit (command)\n\
                              helpQuick (command)\n\
                              helpMan (command)\n\
                              helpDoc (command)\n\
                          helpText (asciiText)\n\
                  chamber[].control (scrollbar)\n\
                  chamber[].display (label)\n\
",
"\
                  clock (label)\n\
                  lab (gas)\n\
\n\
                           Figure 1. Widget Hierarchy\n\
          ____________________________________________________________\n\
\n\
\n\
          The widget hierarchy includes two shell widgets accessible\n\
          to the window manager.  One is the toplevel widget, the\n\
          parent all widgets.  It contains a form widget which, in\n\
          turn, contains all the controls, displays, and the simula-\n\
          tion area.  The top edge of the frame has a row of command\n\
          buttons.  Two of these buttons, run and pause, form a radio\n\
          group, to indicate whether the simulation is in run mode or\n\
          pause mode.  There are two scrollbars, one on each side of\n\
          the lab area.  The scrollbar is used to set the temperature,\n\
          which is displayed in the label below it.  Also below the\n\
          lab is the clock display, which reports the simulated time.\n\
",
"\
          The other shell is a help popup.  It contains a text widget\n\
          which can scroll through the man page, quick-help (a list of\n\
          the mouse button actions), and this document.\n\
\n\
          Two Graphics Contexts (GC) are used in the gas widget.  One\n\
          describes the walls of the chamber, and the other describes\n\
          the molecules, which are filled rectangles.  Since the\n\
          molecules move, and must be erased before shown in their new\n\
          position, they are drawn using the XOR function.  This\n\
          allows them to be erased by drawing them twice in the same\n\
          position.\n\
\n\
          A trick is used to speed up the drawing of the molecules.\n\
          The plural XFillRectangles call is passed an XRectangles\n\
\n\
\n\
          Ideal Gas Simulation                                       2\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
          array which contains two entries for each molecule.  These\n\
",
"\
          entries contain the old position and the new position.  On\n\
          alternate timesteps, either the odd or even elements of this\n\
          array are updated with the new molecule positions.  The\n\
          result is that the old positions are redrawn and erased,\n\
          while the new positions are drawn for the first time and\n\
          remain visible.  This reduces the annoying \"flicker\" that\n\
          results from two separate calls to XDrawRectangles using a\n\
          single array, in which the no molecules are visible, while\n\
          the XRectangle array is being updated.  Care must be taken\n\
          to properly update this array in the addMolecule and expose\n\
          event callbacks.\n\
\n\
          A bitmap was created with bitmap(1), and compiled into the\n\
          program to be used as an icon.\n\
\n\
\n\
          2.2.  The Data Structures\n\
\n\
          There are two main data structures: the molecules and the\n\
",
"\
          chamber (boxes).  These are defined in the \"gas.h\" header\n\
          file.\n\
\n\
          An array of molecule data structures contains the equation\n\
          of motion of the molecule, the time of its next collision,\n\
          and its kinetic energy (expressed as a temperature).\n\
\n\
          The chamber is an array of two boxes.  Each box consists of\n\
          an array of 6 walls, one of which is an opening to the other\n\
          box.  The temperature is associated with each box; this tem-\n\
          perature is updated by scrollbar callbacks.\n\
\n\
          Each wall and corner in the box has an enumerated \"type.\"\n\
          This type, TOP, BOTTOM, LEFT, or RIGHT, NW, SW, SE, NE, is\n\
          used as an index into a WallParam array.  The WallParam\n\
          array contains the coefficients of the reflection and rota-\n\
          tion matrices used in the computation of collisions with the\n\
          walls.\n\
",
"\
\n\
\n\
          2.3.  Physics and Algorithms\n\
\n\
          Calculations of molecule positions are repeated every\n\
          timestep.  The simulation begins by calculating the new\n\
          positions of each molecules.  After each time step, the\n\
          value of the variable \"labData.time\" is incremented by the\n\
          value \"labData.timestepSize.\"\n\
\n\
          At each timestep, the \"dynamics\" routine is called for each\n\
          molecule.  The time of next collision of a given molecule is\n\
          contained in molecule[i].collisionTime.  This is compared\n\
          with the current time (labData.time) to determine if a\n\
          molecule will collide with a wall during this timestep.  In\n\
\n\
\n\
          Ideal Gas Simulation                                       3\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
          this case, the \"collide\" routine is called; otherwise the\n\
          \"inertia\" routine is called.  Note that knowing the time of\n\
          the next collision allows the timestep routines to ignore\n\
",
"\
          the walls until the collision actually occurs.\n\
\n\
          The inertia routine just solves for the molecule's location\n\
          using the equations of motion associated with the molecule.\n\
\n\
          The collision is much more complicated.  First, the trajec-\n\
          tory is reflected by the wall or corner.  A molecule that\n\
          hits corner has both its x and y velocity components multi-\n\
          plied by -1.  A molecule that hits a wall has either its x\n\
          or y velocity component multiplied by -1, depending on which\n\
          way it would bounce in a completely elastic collision (bil-\n\
          liard balls).\n\
\n\
",
"\
          The reflection angle deviates from an exact reflection by a\n\
          random component, which is determined by the X resource\n\
          \"randomBounce.\" At the same time, the temperature of the\n\
          molecule approaches the wall temperature at a rate deter-\n\
          mined by the X resource \"equilibrium.\" Now that the new\n\
          \"temperature\" and angle of trajectory have been determined,\n\
          the new equations of motion are calculated.  Using these\n\
          equations of motion, the next wall collision is found,\n\
          including the possibility of moving to the other chamber.\n\
          This process is repeated until the next collision time is\n\
          after the end of this timestep, Then the inertia routine is\n\
          called to compute the position of the molecule at the end of\n\
          the timestep.\n\
\n\
\n\
          2.4.  Run/Pause/Step control\n\
\n\
          The main computations are performed at regular intervals\n\
          (every \"delay\" milliseconds, using the X resources) via the\n\
          timeout mechanism.  The intrinsics call XtAddTimeOut causes\n\
          a callback to be called after a given delay.  In \"run\" mode,\n\
",
"\
          XtAddTimeOut is automatically called at the end of each\n\
          timestep.  Switching to \"pause\" mode removes this callback\n\
          using XtRemoveTimeOut.  In \"pause\" mode, the \"step\" button\n\
          will perform a timestep calculation without calling\n\
          XtAddTimeOut.\n\
\n\
\n\
          2.5.  Performance and accuracy\n\
\n\
          100 molecules were simulated with a timestep of 3\n\
          microseconds.  The computation of 2.0 simulated milliseconds\n\
          required about 75 CPU seconds.\n\
\n\
          The physical conditions of this simulation represent very\n\
          high vacuum (very low density of molecules per cm squared --\n\
          this is a two dimensional simulation).  The approach to\n\
\n\
\n\
          Ideal Gas Simulation                                       4\n\
",
"\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
          equilibrium occurs only through wall collisions.  Because of\n\
          the line of sight trajectories, and the large aperture\n\
          between the chambers, equilibrium is never reached.  How-\n\
          ever, the approach towards equilibrium is evident from the\n\
          migration of molecules from the warm to the cold chamber.\n\
\n\
\n\
          2.6.  Bugs, enhancements, and Numerical Considerations\n\
\n\
          Doubles are used for the coefficients of the equations of\n\
          motion and the collision times.  Originally floats were used\n\
          for these numbers, but the equations of motion are numeri-\n\
          cally unstable, and would cause errors after some 10,000 to\n\
          15,000 timesteps.  Using doubles allows some 375,000\n\
          timesteps to be computed before this instability causes an\n\
          error.\n\
\n\
          A better solution was to compute the trajectories using\n\
          integer arithmetic.  The endpoints (collision positions) of\n\
",
"\
          each trajectory are integer locations on the walls.  The\n\
          actual coefficients of the trajectory and the time until the\n\
          next collision are floating point numbers, but they are\n\
          recomputed from new integer endpoints at each collision.\n\
          Using this new scheme, xgas has simulated 100 molecules for\n\
          more than a million timesteps.\n\
\n\
\n\
          3.  Appendix\n\
\n\
          Figure 2 is a list of X resources which were created espe-\n\
          cially for controlling some of the physical aspects of the\n\
          simulation, and a typical value.  Figure 3 is a schematic of\n\
          the chamber which identifies the array indices of the points\n\
          and lines making up the walls.\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
          Ideal Gas Simulation                                       5\n\
\n\
\n\
\n\
\n\
\n\
",
"\
\n\
\n\
\n\
\n\
\n\
              ____________________________________________________\n\
\n\
              #\n\
              #   timestepSize in microseconds\n\
              XGas*timestepSize:               3.0\n\
              #\n\
              # delay in milliseconds\n\
              #   Real time between timestep computations.\n\
              #   This doesn't overload my XR4 server running on a Sun 3/110.\n\
              XGas*delay:                      30\n\
              #\n\
              # randomBounce\n\
              #   0.0:    Angle of reflection equals angle of incidence.\n\
              #   1.0:    The two angles are unrelated.\n\
              XGas*randomBounce:               0.2\n\
              #\n\
              #   0.0:    No kinetic energy is exchanged with the wall\n\
              #             during a collision\n\
              #   1.0:    The molecule emerges with a kinetic energy\n\
              #             corresponding to the wall temperature.\n\
              XGas*equilibrium:                0.9\n\
              #\n\
              #   maxMolecules\n\
              #           maximum number of molecules that can be created\n\
              #             with the mouse\n\
              XGas*maxMolecules:               100\n\
              #\n\
",
"\
\n\
              ____________________________________________________\n\
\n\
\n\
              Figure 2. Controlling Simulation Parameters with X Resources\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
          Ideal Gas Simulation                                       6\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
",
"\
              ____________________________________________________\n\
\n\
                   p[0]                 p[2]                  p[4]\n\
                     +--------------------+--------------------+\n\
                     |        w2          |        w2          |\n\
                     |                    |                    |\n\
                     |                  w1|w1                  |\n\
                     |                    |                    |\n\
                     |                    |                    |\n\
                     |                    +p[6]                |\n\
                     |                                         |\n\
                     |w3                w0 w0                w3|\n\
                     |                                         |\n\
                     |                    +p[7]                |\n\
                     |                    |                    |\n\
                     |                    |                    |\n\
                     |                  w5|w5                  |\n\
                     |                    |                    |\n\
                     |        w4          |         w4         |\n\
                     +--------------------+--------------------+\n\
                   p[1]                 p[3]                  p[5]\n\
\n\
\n\
              ____________________________________________________\n\
\n\
\n\
                    Figure 3. Assignment of walls and points\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
\n\
          Ideal Gas Simulation                                       7\n\
\n\
\n\
\n\
",
"\0"};
