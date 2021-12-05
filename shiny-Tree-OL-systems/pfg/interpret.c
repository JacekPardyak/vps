#include <quickdraw.h>
#include <font.h>
#include <window.h>
#include <math.h>
#include "generate.h"
#include "interpret.h"
/********************************************************************/
/* Create the graphics environment in which the curve will be */
/* drawn. Find the bounding box of the curve , center it and draw */
/* using the specified parameters. */
/********************************************************************/
interpret(string, pPtr)
char *string;
Parameter *pPtr;
{
Rect boundsRect; /* Needed to open a window on the Mac... */
WindowRecord wRecord;
WindowPtr picture;
short int inc; /* The step size */
PIXEL start; /* Starting position of the turtle. */
BOX boundBox; /* Bounding box of the curve */
/* Check the value of the angle factor */
if (pPtr->angle > MAXANGLE) {
printf("Angle factor too big (%d > %d)\n",
pPtr->angle, MAXANGLE);
exit(1);
}
/* Compute the bounding rectangle of the curve, assuming */
/* that the mouse starts at the point (0,0) and the step */
/* size is equal to 1. */
start.h = start.v = 0;
inc = 1;
draw(string, &start, inc, pPtr->angle, 0, &boundBox);
/* Given the bounding rectangle and the size factor, */
/* center figure in the window. */
SetDrawParam(&start, &inc, &boundBox, pPtr->scale);
/* Initialize drawing environment. */
InitGraf(&thePort);
InitFonts();
InitWindows();
SetRect(&boundsRect, LEFT, TOP, RIGHT, BOTTOM);
picture = NewWindow(&wRecord, &boundsRect, ctop(pPtr->filename),
                    257, documentProc, (WindowPtr) 0, 257, 0L);
OpenPort (thePort);
SelectWindow (picture);
SetPort(picture);
HideCursor();
/* Draw curve. */
draw(string, &start, inc, pPtr->angle, 1, &boundBox);
ClosePort(thePort);
}
/********************************************************************/
/* Draw figure by according to the "string". The parameters have */
/* the following meaning: */
/* */
/* string the string being interpreted */
/* startPtr initial position of the turtle */
/* inc step size */
/* angFac the angle factor */
/* flag 0 - make all lines invisible, 1 - draw. The value */
/* of 0 is used when calculating the bounding */
/* rectangle. */
/* boxPtr Pointer to the structure returning the coordinates */
/* of the bounding rectangle. */
/* */
/* The following string symbols are interpreted by the turtle: */
/* + - | [ ] F f */
/********************************************************************/
draw(string, startPtr, inc, angFac, flag, boxPtr)
  char *string;
PIXEL *startPtr;
short int inc, angFac, flag;
BOX *boxPtr;
{
  double SI[MAXANGLE], CO[MAXANGLE];
  TURTLE tu, stack[STACK_SIZE], *stackPtr, *bottom, *top;
  char c;
  int i, halfangFac;
  double ang = -TWO_PI/4;
  
  char *str;
  str = string;
  /* Set stack limits. */
  stackPtr = bottom = stack;
  top = stack + STACK_SIZE - 1;
  /* Precalculate coordinates of turtle steps in all possible */
  /* directions. */
  for(i=0; i<angFac; i++) {
    SI[i] = inc * sin(ang);
    CO[i] = inc * cos(ang);
    ang += TWO_PI/angFac;
  };
  halfangFac = angFac/2; /* needed to interpret the symbol | */
  angFac--; /* more convenient in comparisons */
  /* Initialize the bounding rectangle and the starting position */
  /* of the turtle for the purpose of bounding rectangle */
  /* calculation. */
  boxPtr->xmin = boxPtr->xmax = tu.x = (double) (startPtr->h) + 0.5;
  boxPtr->ymin = boxPtr->ymax = tu.y = (double) (startPtr->v) + 0.5;
  tu.dir=0;
  /* Move the turtle to an appropriate starting position in */
  /* order to center the final drawing. */
  if (flag)
    MoveTo(startPtr->h, startPtr->v);
  /* Scan the string and interpret its consecutive symbols. */
  while ((c = *str++) != ’\0’) {
    switch (c) {
    case ’+’: /* Turn right */
  if(tu.dir<angFac)
    ++tu.dir;
  else
    tu.dir=0;
  break;
    case ’-’: /* Turn left */
  if(tu.dir>0)
    
    --tu.dir;
  else
    tu.dir=angFac;
  break;
    case ’|’: /* Turn over */
  if(tu.dir>=halfangFac)
    tu.dir-=halfangFac;
  else
    tu.dir+=halfangFac;
  break;
    case ’[’: /* Push curent state on stack */
  if(stackPtr >= top) {
    printf("Too many [\n");
    exit(1);
  }
  TurtleCopy(stackPtr, &tu);
  stackPtr++;
  break;
    case ’]’: /* Pop state from the stack */
  if(stackPtr <= bottom) {
    printf("Too many ]\n");
    exit(1);
  }
  --stackPtr;
  TurtleCopy(&tu, stackPtr);
  if (flag)
    MoveTo((short)(tu.x), (short)(tu.y));
  break;
    case ’F’: /* Move forward and draw a line */
  tu.x += CO[tu.dir];
      tu.y += SI[tu.dir];
      if (flag)
        LineTo((short)(tu.x), (short)(tu.y));
      else
        BoxUpdate(&tu, boxPtr);
      break;
    case ’f’: /* Move forward without drawing */
  tu.x += CO[tu.dir];
      tu.y += SI[tu.dir];
      if (flag)
        MoveTo((short)(tu.x), (short)(tu.y));
      else
        BoxUpdate(&tu, boxPtr);
      break;
    default: /* Room for extensions */
  break;
    }
  }
  
}
/********************************************************************/
/* Copy TURTLE structure "fromPtr" to "toPtr". */
/********************************************************************/
TurtleCopy(toPtr, fromPtr)
  TURTLE *toPtr, *fromPtr;
{
    toPtr->x = fromPtr->x;
    toPtr->y = fromPtr->y;
    toPtr->dir = fromPtr->dir;
}
/********************************************************************/
/* Check whether the turle moves outside the current box. If it */
/* does, adjust box boundary. */
/********************************************************************/
BoxUpdate(tuPtr, boxPtr)
  TURTLE *tuPtr;
BOX *boxPtr;
{
  if (tuPtr->x < boxPtr->xmin)
    boxPtr->xmin = tuPtr->x;
  if (tuPtr->x > boxPtr->xmax)
    boxPtr->xmax = tuPtr->x;
  if (tuPtr->y < boxPtr->ymin)
    boxPtr->ymin = tuPtr->y;
  if (tuPtr->y > boxPtr->ymax)
    boxPtr->ymax = tuPtr->y;
}
/********************************************************************/
/* Set the starting point and the step increment given the bounding */
/* box and the scale factor. */
/********************************************************************/
SetDrawParam(startPtr, incPtr, boxPtr, scale)
  PIXEL *startPtr;
short int *incPtr;
BOX *boxPtr;
int scale;
{
  double xscale, yscale, sc;
  xscale = (RIGHT - LEFT)/(boxPtr->xmax - boxPtr->xmin);
  yscale = (BOTTOM - TOP)/(boxPtr->ymax - boxPtr->ymin);
  
  if(xscale>yscale)
    sc = yscale;
  else
    sc = xscale;
  *incPtr = (int) floor ((double) ((sc * scale)/MAXSCALE));
  startPtr->h = (short)
    (RIGHT - LEFT - *incPtr * (boxPtr->xmin + boxPtr->xmax - 1.0))/2;
  startPtr->v = (short)
    (BOTTOM - TOP - *incPtr * (boxPtr->ymin + boxPtr->ymax - 1.0))/2;
}