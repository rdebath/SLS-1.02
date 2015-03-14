#define InArray(x,y) (((x) >= 0 && (x) < width) && (y >= 0 && y < height))

extern int minefield[50][20];

#define MINE 9
/*
    bit 5   : Marked
    bit 4   : Cover/uncovered
*/
#define MARKED  0x20
#define COVERED 0x10

#define IsMine(x,y) ((InArray(x,y) && (minefield[x][y] & 0xf) == MINE) ? 1 : 0)
#define IsBlank(x,y) (minefield[x][y] ? 0 : 1)
#define IsMarked(x,y) ((InArray(x,y) && (minefield[x][y] & MARKED)) ? 1 : 0)
#define IsVisible(x,y) ((minefield[x][y] & COVERED) ? 0 : 1)

