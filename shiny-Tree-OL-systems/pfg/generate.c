/********************************************************************/
/* Plant and Fractal Generator (pfg) */
/* Author: Przemyslaw Prusinkiewicz */
/* Copyright (C) 1988: Przemyslaw Prusinkiewicz */
/* */
/* This program is made available free of charge for educational */
/* purposes only. The program cannot be sold, included entirely */
/* or in part in commercial programs, or used for any commercial */
/* purposes without a writtenpermission from the author. */
/* from the author. */
/********************************************************************/
/********************************************************************/
/* Pfg was developed and tested on Macintosh 512K using Aztec C */
/* compiler Version 1.06. The compiling procedure is: */
/* cc generate.c */
/* cc interpret.c */
/* ln -o pfg generate.c interpret.c -lm -lc */
/********************************************************************/
/********************************************************************/
/* Pfg produces images of two-dimensional structures specified by */
/* L-systems with turtle interpretation. The program supports */
/* bracketed L-systems with no context, one-sided context and */
/* two-sided context. */
/********************************************************************/
/********************************************************************/
/* An example data file is given below: */
/* */
/* Derivation length: 30 */
/* angle factor: 16 */
/* scale factor: 100 */
/* axiom: F1F1F1 */
/* ignore: +-F */
/* 0 < 0 > 0 --> 0 */
/* 0 < 0 > 1 --> 1[-F1F1] */
/* 0 < 1 > 0 --> 1 */
/* 0 < 1 > 1 --> 1 */
/* 1 < 0 > 0 --> 0 */
/* 1 < 0 > 1 --> 1F1 */
/* 1 < 1 > 0 --> 1 */
/* 1 < 1 > 1 --> F0 */
/* * < + > * --> - */
/* * < - > * --> + */
/* */

/* The *angle factor* determines the angle increment: */
/* delta = 360 degrees / (angle factor). */
/* */
/* The *scale factor* determines the size of the resulting */
/* image. In principle, 100 denotes a full-screen image, */
/* 0 denotes an image reduced to a single pixel. */
/* However, in order to reduce distortion, the image */
/* is always scaled in such a way that the turtle step */
/* is equal to an integer number of pixels. */
/* */
/* Characters listed after the *ignore* keyword */
/* are considered as nonexistant while context matching. */
/* Usually these characters represent geometric information */
/* which is irrelevent from the viewpoint of interaction */
/* between the components of the modelled structure. */
/* */
/* The separators < and > must be present in each production. */
/* The empty strings are denoted by symbol *. All components of a */
/* production (including the strict predecessor) may contain */
/* several letters. For example, AB < CDE > FG --> A[B]A */
/* is a valid production which replaces substring CDE by A[B]A. */
/* The context lengths may vary from one production to another */
/* within a particular production set. */
/* Brackets can appear only in the production successor. */
/********************************************************************/
#include <stdio.h>
#include "generate.h"
/********************************************************************/
/* "main" organizes computation */
/********************************************************************/
main(argc, argv)
  int argc;
char *argv[];
{
  char *title, *ctop();
  char *string, *Derive();
  char axiom[MAXAXIOM];
  static char ignore[MAXCHARS]; /* ... when context matching */
Production prodSet[MAXPROD];
Parameter p;
if (argc != 2) {
  printf("Usage: %s filename\n", argv[0]);
  exit(1);
}

p.filename = argv[1];
input(axiom, ignore, prodSet, &p); /* read the input file */
string = Derive(axiom, ignore, prodSet, p.n); /* generate */
interpret(string, &p); /* interpret */
}
/********************************************************************/
/* Function "input" reads the input file. */
/********************************************************************/
input(axiom, ignore, prodSet, pPtr)
  char axiom[];
char ignore[];
Production prodSet[];
Parameter *pPtr;
{
  FILE *fp, *fopen();
  char lcontext[MAXAXIOM], rcontext[MAXAXIOM], predecessor[MAXAXIOM];
  char successor[MAXAXIOM];
  char ignore_buf[MAXCHARS];
  char *inpStr; /* It will contain the contexts, the strict
 predecessors and the successors of all
 productions, separated by the null characters */
char *malloc();
int i;
if ((fp = fopen(pPtr->filename, "r")) == NULL) {
  printf("Can’t open %s\n", pPtr->filename);
  exit(1);
}
if ((inpStr = malloc(MAXSTR)) == NULL) {
  printf("Can’t allocate inpStr\n");
  exit(1);
}
*inpStr++ = ’\0’; /* Start inpStr with the null character - needed to terminate search for the left context */
fscanf(fp, "derivation length: %d\n", &pPtr->n);
fscanf(fp, "angle factor: %d\n", &pPtr->angle);
fscanf(fp, "scale factor: %d\n", &pPtr->scale);
fscanf(fp, "axiom: %s\n", axiom);
fscanf(fp, "ignore: %s", ignore_buf);
/* Set flags corresponding to the ignored characters to 1. */
for (i=0; i<MAXCHARS; i++)
  ignore[i] = 0;

for (i=0; ignore_buf[i] !=0; i++) {
  ignore[ignore_buf[i]] = 1;
}
/* Read productions and enter them to the "prodSet" structures */
for (i=0; 1; i++, prodSet++) {
  prodSet->pred = inpStr; /* to mark last production */
if (fscanf(fp, "%s < %s > %s --> %s",
           lcontext, predecessor, rcontext, successor) == EOF)
  break;
enter(lcontext, &prodSet->lConLen, &prodSet->lCon, &inpStr);
enter(predecessor, &prodSet->predLen, &prodSet->pred, &inpStr);
enter(rcontext, &prodSet->rConLen, &prodSet->rCon, &inpStr);
enter(successor, &prodSet->succLen, &prodSet->succ, &inpStr);
};
prodSet->predLen = 0;
printf("%d productions read\n", i);
}
/********************************************************************/
/* Function "enter" is used to copy "string" to "inpStr" and fill */
/* fields in a "prodSet" structure". */
/********************************************************************/
enter(string, lenPtr, prodStrHandle, inpStrHandle)
  char *string;
int *lenPtr;
char **prodStrHandle, **inpStrHandle;
{
  if (*string == ’*’) {
    *lenPtr = 0;
    **inpStrHandle = ’\0’;
  }
  else {
    *lenPtr = strlen(string);
    strcpy(*inpStrHandle, string);
  }
  *prodStrHandle = *inpStrHandle;
  *inpStrHandle += *lenPtr + 1;
}
/********************************************************************/
/* Given an axiom, a set of productions and a derivation length, */
/* find the generated string and return a pointer to it. */
/* The parallel operation of an L-system is simulated as follows. */
/* First, string1 (containing the axiom) is scanned from the */

/* left to the right. Its consecutive substrings are matched with */
/* the predecessors of productions. The appropriate successors are */
/* appended to the end of string2. After the entire string1 is */
/* scanned, string2 becomes string 1. The process is repeated until */
/* the desired derivation length is achieved. */
/********************************************************************/
char *Derive(axiom, ignore, prodSet, n)
  char axiom[];
char ignore[];
Production prodSet[];
int n;
{
  char *curPtr, *nextPtr, *tempPtr, *limPtr;
  char *string1, *string2;
  int i;
  Production *FindProd();
  if ((string1 = malloc(MAXSTR)) == NULL) {
    printf("pfg: can’t allocate string1\n");
    exit(1);
  }
  if ((string2 = malloc(MAXSTR)) == NULL) {
    printf("pfg: can’t allocate string2\n");
    exit(1);
  }
  for (i=0; i < MAXSTR; i++)
    *(string1+i) = *(string2+i) = ’\0’;
  ++string1;
  ++string2; /* start with \0 for proper context handling */
strcpy(string1, axiom);
limPtr = string2 + MAXSTR - MAXAXIOM;
for (i=1; i<=n; i++) {
  printf("Computing derivation step %d\n", i);
  curPtr = string1;
  nextPtr = string2;
  while (*curPtr != ’\0’) {
    ApplyProd(&curPtr, &nextPtr,
              FindProd(curPtr, prodSet, ignore));
    if(nextPtr > limPtr) {
      printf("String too long");
      exit(1);
    }
    *nextPtr = ’\0’;
  }
  tempPtr = string1;
  string1 = string2;
  
  string2 = tempPtr;
}
return(string1);
}
/********************************************************************/
/* Copy the successor of production *prodPtr starting at location */
/* *nextHandle. Update curHandle and nextHandle. */
/********************************************************************/
ApplyProd(curHandle, nextHandle, prodPtr)
  char **curHandle, **nextHandle;
Production *prodPtr;
{
  if (prodPtr != NULL) {
    strcpy(*nextHandle, prodPtr->succ);
    *curHandle += prodPtr->predLen;
    *nextHandle += prodPtr->succLen;
  }
  else {
    **nextHandle = **curHandle;
    ++(*nextHandle);
    ++(*curHandle);
  }
}
/********************************************************************/
/* Given a pointer to a string and a set of productions, return the */
/* pointer to the first applicable production or NULL if no */
/* matching production can be found. The set of productions is */
/* scanned in the same order in which productions are listed in */
/* the input file. */
/*********************************************************************/
Production *FindProd(curPtr, prodSet, ignore)
  char *curPtr;
Production prodSet[];
char ignore[];
{
  while (prodSet->predLen != 0) {
    if (prefix(prodSet->pred, curPtr) ||
        rcondiff(prodSet->rCon, curPtr + prodSet->predLen, ignore) ||
        lcondiff(prodSet->lCon + prodSet->lConLen - 1,
                 curPtr - 1, ignore))
      ++prodSet;
    else
      return(prodSet);
  }
  
  /* Predecessor not found */
  return(NULL);
}
/*********************************************************************/
/* Check, whether string s1 is a prefix of the string s2. */
/*********************************************************************/
prefix(s1, s2)
  char *s1, *s2;
{
    while (*s1 != ’\0’)
      if (*s2++ != *s1++)
        return(1);
      return(0);
}
/********************************************************************/
/* Check, whether string s1 matches s2 as the right context. */
/* Ignore specified symbols and skip branches. */
/********************************************************************/
rcondiff(s1, s2, ignore)
  char *s1, *s2;
char ignore[];
{
  char *skipright();
  while(1) {
    if (*s1 == ’\0’)
      return(0);
    if(ignore[*s2])
      s2++;
    else if (*s2 == ’[’)
      s2 = skipright(s2+1) +1;
    else if (*s1++ != *s2++)
      return(1);
  }
}
/********************************************************************/
/* Skip a branch while searching for the right context. The branch */
/* may contain subbranches. */
/********************************************************************/
char *skipright(s)
  char *s;
{
    int level = 0;
    while (*s != ’\0’) {
      switch (*s) {
      case ’[’:
        ++level;
        break;
      case ’]’:
        if(level == 0)
          return(s);
        else
          --level;
        break;
      default:
        break;
      }
      s++;
    }
    return(s);
}
/********************************************************************/
/* Check, whether string s1 matches s2 as the left context. */
/* Ignore specified symbols and skip branches. The parent branch */
/* belongs to the left context of a child branch. */
/********************************************************************/
lcondiff(s1, s2, ignore)
  char *s1, *s2;
char ignore[];
{
  char *skipleft();
  while(1) {
    if(*s1 == ’\0’)
      return(0);
    if(ignore[*s2] || (*s2 == ’[’))
      s2--;
    else if (*s2 == ’]’)
      s2 = skipleft(s2);
    else {
      if (*s1-- != *s2--)
        return(1);
    }
  }
}
/********************************************************************/
/* Skip a branch while searching for the left context. The branch */
/* may contain subbranches. */
/********************************************************************/
char *skipleft(s)
  char *s;
{
    int level = 0;
    s--;
    while(*s != ’\0’) {
      switch(*s) {
      case ’]’:
        ++level;
        break;
      case ’[’:
        if(level == 0)
          return(--s);
        else
          --level;
        break;
      default:
        break;
      }
      s--;
    }
    return(s);
}

