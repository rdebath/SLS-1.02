#include "defs.h"

void define_shapes()
{

    /* begin shape 0 definition, four rotations */

    shape[0].table[0][0] = 0;  
    shape[0].table[1][0] = 15;  /* #### */
    shape[0].table[2][0] = 0; 
    shape[0].table[3][0] = 0; 
    shape[0].pointv[0] = 5; 

    shape[0].table[0][1] = 4;   /*  #   */
    shape[0].table[1][1] = 4;   /*  #   */
    shape[0].table[2][1] = 4;   /*  #   */
    shape[0].table[3][1] = 4;   /*  #   */
    shape[0].pointv[1] = 8; 

    shape[0].table[0][2] = 0; 
    shape[0].table[1][2] = 15; /* #### */
    shape[0].table[2][2] = 0; 
    shape[0].table[3][2] = 0; 
    shape[0].pointv[2] = 5; 

    shape[0].table[0][3] = 4;   /*  #    */
    shape[0].table[1][3] = 4;   /*  #    */
    shape[0].table[2][3] = 4;   /*  #    */
    shape[0].table[3][3] = 4;   /*  #    */
    shape[0].pointv[2] = 8; 

    shape[0].width = 4; 
    shape[0].height = 1; 
    shape[0].offset = 153;
    shape[0].color = RED;

    /* begin shape 1 definition, four rotations */

    shape[1].table[0][0] = 12;  /* ##   */
    shape[1].table[1][0] = 12;  /* ##   */
    shape[1].table[2][0] = 0;   /*      */
    shape[1].table[3][0] = 0;   /*      */
    shape[1].pointv[0] = 6; 

    shape[1].table[0][1] = 12;  /* ##   */
    shape[1].table[1][1] = 12;  /* ##   */
    shape[1].table[2][1] = 0;   /*      */
    shape[1].table[3][1] = 0;   /*      */
    shape[1].pointv[1] = 6; 

    shape[1].table[0][2] = 12;  /* ##   */
    shape[1].table[1][2] = 12;  /* ##   */
    shape[1].table[2][2] = 0;   /*      */
    shape[1].table[3][2] = 0;   /*      */
    shape[1].pointv[2] = 6; 

    shape[1].table[0][3] = 12;  /* ##   */
    shape[1].table[1][3] = 12;  /* ##   */
    shape[1].table[2][3] = 0;   /*      */
    shape[1].table[3][3] = 0;   /*      */
    shape[1].pointv[3] = 6; 

    shape[1].width = 2; 
    shape[1].height = 2; 
    shape[1].offset = 0;
    shape[1].color = ORANGE;

    /* begin shape 2 definition, four rotations */

    shape[2].table[0][0] = 4;  /*  #  */
    shape[2].table[1][0] = 14; /* ### */
    shape[2].table[2][0] = 0;  /*     */
    shape[2].table[3][0] = 0;  /*     */
    shape[2].pointv[0] = 5; 

    shape[2].table[0][1] = 4;  /*  #  */
    shape[2].table[1][1] = 6;  /*  ## */
    shape[2].table[2][1] = 4;  /*  #  */
    shape[2].table[3][1] = 0;  /*     */
    shape[2].pointv[0] = 5; 

    shape[2].table[0][2] = 0;  /*     */
    shape[2].table[1][2] = 14; /* ### */
    shape[2].table[2][2] = 4;  /*  #  */
    shape[2].table[3][2] = 0;  /*     */
    shape[2].pointv[2] = 6; 

    shape[2].table[0][3] = 4;  /*  #  */
    shape[2].table[1][3] = 12; /* ##  */
    shape[2].table[2][3] = 4;  /*  #  */
    shape[2].table[3][3] = 0;  /*     */
    shape[2].pointv[3] = 5; 

    shape[2].width = 3; 
    shape[2].height = 2; 
    shape[2].offset = 24;
    shape[2].color = YELLOW;

    /* begin shape 3 definition, four rotations */

    shape[3].table[0][0] = 12; /* ##  */
    shape[3].table[1][0] = 6;  /*  ## */
    shape[3].table[2][0] = 0;  /*     */
    shape[3].table[3][0] = 0;  /*     */
    shape[3].pointv[0] = 6; 

    shape[3].table[0][1] = 4;  /*  #  */
    shape[3].table[1][1] = 12; /* ##  */
    shape[3].table[2][1] = 8;  /* #   */
    shape[3].table[3][1] = 0;  /*     */
    shape[3].pointv[1] = 7; 

    shape[3].table[0][2] = 12; /* ##  */
    shape[3].table[1][2] = 6;  /*  ## */
    shape[3].table[2][2] = 0;  /*     */
    shape[3].table[3][2] = 0;  /*     */
    shape[3].pointv[2] = 6; 

    shape[3].table[0][3] = 4;  /*  #  */
    shape[3].table[1][3] = 12; /* ##  */
    shape[3].table[2][3] = 8;  /* #   */
    shape[3].table[3][3] = 0;  /*     */
    shape[3].pointv[3] = 7; 

    shape[3].width = 3; 
    shape[3].height = 2; 
    shape[3].offset = 0;
    shape[3].color = GREEN;

    /* begin shape 4 definition, four rotations */

    shape[4].table[0][0] = 6;  /*  ## */
    shape[4].table[1][0] = 12; /* ##  */
    shape[4].table[2][0] = 0;  /*     */
    shape[4].table[3][0] = 0;  /*     */
    shape[4].pointv[0] = 6; 

    shape[4].table[0][1] = 8;  /* #   */
    shape[4].table[1][1] = 12; /* ##  */
    shape[4].table[2][1] = 4;  /*  #  */
    shape[4].table[3][1] = 0;  /*     */
    shape[4].pointv[1] = 7; 

    shape[4].table[0][2] = 6;  /*  ## */
    shape[4].table[1][2] = 12; /* ##  */
    shape[4].table[2][2] = 0;  /*     */
    shape[4].table[3][2] = 0;  /*     */
    shape[4].pointv[2] = 6; 

    shape[4].table[0][3] = 8;  /* #   */
    shape[4].table[1][3] = 12; /* ##  */
    shape[4].table[2][3] = 4;  /*  #  */
    shape[4].table[3][3] = 0;  /*     */
    shape[4].pointv[3] = 7; 

    shape[4].width = 3; 
    shape[4].height = 2; 
    shape[4].offset = 0;
    shape[4].color = BLUE;

    /* begin shape 5 definition, four rotations */

    shape[5].table[0][0] = 2;  /*   # */
    shape[5].table[1][0] = 14; /* ### */
    shape[5].table[2][0] = 0;  /*     */
    shape[5].table[3][0] = 0;  /*     */
    shape[5].pointv[0] = 6; 

    shape[5].table[0][1] = 8;  /* #   */
    shape[5].table[1][1] = 8;  /* #   */
    shape[5].table[2][1] = 12; /* ##  */
    shape[5].table[3][1] = 0;  /*     */
    shape[5].pointv[1] = 7; 

    shape[5].table[0][2] = 14; /* ### */
    shape[5].table[1][2] = 8;  /* #   */
    shape[5].table[2][2] = 0;  /*     */
    shape[5].table[3][2] = 0;  /*     */
    shape[5].pointv[2] = 6; 

    shape[5].table[0][3] = 12; /* ##  */
    shape[5].table[1][3] = 4;  /*  #  */
    shape[5].table[2][3] = 4;  /*  #  */
    shape[5].table[3][3] = 0;  /*     */
    shape[5].pointv[3] = 7; 

    shape[5].width = 3; 
    shape[5].height = 2; 
    shape[5].offset = 0;
    shape[5].color = CYAN;

    /* begin shape 6 definition, four rotations */

    shape[6].table[0][0] = 14; /* ### */
    shape[6].table[1][0] = 2;  /*   # */
    shape[6].table[2][0] = 0;  /*     */
    shape[6].table[3][0] = 0;  /*     */
    shape[6].pointv[0] = 6; 

    shape[6].table[0][1] = 4;  /*  #  */
    shape[6].table[1][1] = 4;  /*  #  */
    shape[6].table[2][1] = 12; /* ##  */
    shape[6].table[3][1] = 0;  /*     */
    shape[6].pointv[1] = 7; 

    shape[6].table[0][2] = 8;  /* #   */
    shape[6].table[1][2] = 14; /* ### */
    shape[6].table[2][2] = 0;  /*     */
    shape[6].table[3][2] = 0;  /*     */
    shape[6].pointv[2] = 6; 

    shape[6].table[0][3] = 12; /* ##  */
    shape[6].table[1][3] = 8;  /* #   */
    shape[6].table[2][3] = 8;  /* #   */
    shape[6].table[3][3] = 0;  /*     */
    shape[6].pointv[3] = 7; 

    shape[6].width = 3; 
    shape[6].height = 2; 
    shape[6].offset = 0;
    shape[6].color = VIOLET;
}

store_shape(shape_no, xpos, ypos, rot)
        int     shape_no, xpos, ypos, rot;
{
        int     i;

        for (i = 0; i < 4; i++) {
                if (shape[shape_no].table[i][rot] & 8)
                        grid[xpos][ypos + i] = 1;
                if (shape[shape_no].table[i][rot] & 4)
                        grid[xpos + 1][ypos + i] = 1;
                if (shape[shape_no].table[i][rot] & 2)
                        grid[xpos + 2][ypos + i] = 1;
                if (shape[shape_no].table[i][rot] & 1)
                        grid[xpos + 3][ypos + i] = 1;
        }
}

create_shape()
{
		shape_no = next_no;
		rot = next_rot;
        next_no = rand() % 7;
        next_rot = rand() % 4;
        xpos = (UWIDTH / 2) - 1;
        ypos = -4;
}

