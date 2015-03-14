#include "defs.h"

block_can_drop(shape_no, xpos, ypos, rot)
        int     shape_no, xpos, ypos, rot;
{
        int     y1, c;

        c = 3;

        while ((c >= 0) && ((shape[shape_no].table[c][rot] & 8) == 0))
                c--;
        y1 = ypos + c + 1;
        if ((c != -1) && (y1 >= 0))
                if ((y1 == UHEIGHT) || (grid[xpos][y1] != 0))
                        return (0);

        c = 3;
        while ((c >= 0) && ((shape[shape_no].table[c][rot] & 4) == 0))
                c--;
        y1 = ypos + c + 1;
        if ((c != -1) && (y1 >= 0))
                if ((y1 == UHEIGHT) || (grid[xpos + 1][y1] != 0))
                        return (0);

        c = 3;
        while ((c >= 0) && ((shape[shape_no].table[c][rot] & 2) == 0))
                c--;
        y1 = ypos + c + 1;
        if ((c != -1) && (y1 >= 0))
                if ((y1 == UHEIGHT) || (grid[xpos + 2][y1] != 0))
                        return (0);

        c = 3;
        while ((c >= 0) && ((shape[shape_no].table[c][rot] & 1) == 0))
                c--;
        y1 = ypos + c + 1;
        if ((c != -1) && (y1 >= 0))
                if ((y1 == UHEIGHT) || (grid[xpos + 3][y1] != 0))
                        return (0);

        return 1;
}

block_can_left(shape_no, xpos, ypos, rot)
        int     shape_no, xpos, ypos, rot;
{
        int     x1, c;
        int     y0, y1, y2, y3;
        int     t0, t1, t2, t3;

        t0 = shape[shape_no].table[0][rot];     /* Bit map of 1st Row */
        t1 = shape[shape_no].table[1][rot];     /* Bit map of 2nd Row */
        t2 = shape[shape_no].table[2][rot];     /* Bit map of 3rd Row */
        t3 = shape[shape_no].table[3][rot];     /* Bit map of 4th Row */

        y0 = ypos;
        y1 = ypos + 1;
        y2 = ypos + 2;
        y3 = ypos + 3;

        c = 3;
        while ((c >= 0) && ((t0 & (1 << c)) == 0))
                c--;
        x1 = xpos - 1 + (3 - c);
        if (c != -1)
                if ((x1 < 0) || ((y0 >= 0) && (grid[x1][y0] != 0)))
                        return (0);

        c = 3;
        while ((c >= 0) && ((t1 & (1 << c)) == 0))
                c--;
        x1 = xpos - 1 + (3 - c);
        if (c != -1)
                if ((x1 < 0) || ((y1 >= 0) && (grid[x1][y1] != 0)))
                        return (0);

        c = 3;
        while ((c >= 0) && ((t2 & (1 << c)) == 0))
                c--;
        x1 = xpos - 1 + (3 - c);
        if (c != -1)
                if ((x1 < 0) || ((y2 >= 0) && (grid[x1][y2] != 0)))
                        return (0);

        c = 3;
        while ((c >= 0) && ((t3 & (1 << c)) == 0))
                c--;
        x1 = xpos - 1 + (3 - c);
        if (c != -1)
                if ((x1 < 0) || ((y3 >= 0) && (grid[x1][y3] != 0)))
                        return (0);

        return 1;
}

block_can_right(shape_no, xpos, ypos, rot)
        int     shape_no, xpos, ypos, rot;
{
        int     x1, c;
        int     y0, y1, y2, y3;
        int     t0, t1, t2, t3;

        t0 = shape[shape_no].table[0][rot];     /* Bit map of 1st Row */
        t1 = shape[shape_no].table[1][rot];     /* Bit map of 2nd Row */
        t2 = shape[shape_no].table[2][rot];     /* Bit map of 3rd Row */
        t3 = shape[shape_no].table[3][rot];     /* Bit map of 4th Row */

        y0 = ypos;
        y1 = ypos + 1;
        y2 = ypos + 2;
        y3 = ypos + 3;

        c = 0;
        while ((c < 4) && ((t0 & (1 << c)) == 0))
                c++;
        x1 = xpos + 1 + (3 - c);
        if ((c != 4) && (x1 >= 0))
                if ((x1 == UWIDTH) || ((y0 >= 0) && (grid[x1][y0] != 0)))
                        return (0);

        c = 0;
        while ((c < 4) && ((t1 & (1 << c)) == 0))
                c++;
        x1 = xpos + 1 + (3 - c);
        if ((c != 4) && (x1 >= 0))
                if ((x1 == UWIDTH) || ((y1 >= 0) && (grid[x1][y1] != 0)))
                        return (0);

        c = 0;
        while ((c < 4) && ((t2 & (1 << c)) == 0))
                c++;
        x1 = xpos + 1 + (3 - c);
        if ((c != 4) && (x1 >= 0))
                if ((x1 == UWIDTH) || ((y2 >= 0) && (grid[x1][y2] != 0)))
                        return (0);

        c = 0;
        while ((c < 4) && ((t3 & (1 << c)) == 0))
                c++;
        x1 = xpos + 1 + (3 - c);
        if ((c != 4) && (x1 >= 0))
                if ((x1 == UWIDTH) || ((y3 >= 0) && (grid[x1][y3] != 0)))
                        return (0);

        return 1;
}

remove_full_lines(y)
        int     y;
{
        int     y1, y2, x;

        for (y1 = y; y1 < y + 4 && y1 < UHEIGHT; y1++) {
                for (x = 0; x < UWIDTH; x++)
                        if (grid[x][y1] == 0)
                                break;
                if ( x == UWIDTH ) {
                        for (y2 = y1; y2 > 0; y2--)
                                for (x = 0; x < UWIDTH; x++)
                                        grid[x][y2] = grid[x][y2 - 1];
                        for (x = 0; x < UWIDTH; x++)
                                grid[x][0] = 0;
			scdelete(y1);
			scinsert(0);
                        rows++;
                }
        }
}

check_rot(shape_no, xpos, ypos, newrot)
        int     shape_no, xpos, ypos, newrot;
{
        int     i;
        int     ti;             /* Bit map of i'th row    */
        int     yi;             /* Y position on i'th row */
        int     x0, x1, x2, x3;

        x0 = xpos;
        x1 = xpos + 1;
        x2 = xpos + 2;
        x3 = xpos + 3;
        yi = ypos;

        for (i = 0; i < 4; yi++, i++) {
                if ((yi) >= 0) {
                        ti = shape[shape_no].table[i][newrot];
                        if (ti & 8)
                                if ((x0 < 0) || (x0 >= UWIDTH) || (grid[x0][yi] == 1))
                                        return 0;
                        if (ti & 4)
                                if ((x1 < 0) || (x1 >= UWIDTH) || (grid[x1][yi] == 1))
                                        return 0;
                        if (ti & 2)
                                if ((x2 < 0) || (x2 >= UWIDTH) || (grid[x2][yi] == 1))
                                        return 0;
                        if (ti & 1)
                                if ((x3 < 0) || (x3 >= UWIDTH) || (grid[x3][yi] == 1))
                                        return 0;
                }
        }
        return 1;
}
