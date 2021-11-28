#define MAXANGLE 40 /* maximum value of the angle factor */
#define MAXSCALE 100 /* maximum value of the scale factor */
#define TWO_PI 6.2831853
#define STACK_SIZE 40 /* maximum depth of branches */
#define LEFT 2 /* window parameters */
#define TOP 42
#define RIGHT 510
#define BOTTOM 340
struct TURTLE { /* turtle position and orientation */
double x;
double y;
short int dir;
};
typedef struct TURTLE TURTLE;
struct PIXEL { /* turtle position in screen */
short int h, v; /* coordinates */
};
typedef struct PIXEL PIXEL;
struct BOX { /* the bounding box of the fractal */
double xmin, xmax;
double ymin, ymax;
};
typedef struct BOX BOX;