#define MAXCHARS 256 /* the number of ASCII characters */
#define MAXIGNORE 50 /* maximum number of ignored symbols */
#define MAXPROD 50 /* maximum number of productions */
#define MAXAXIOM 100 /* maximum length of the axiom */
#define MAXSTR 30000 /* maximum length of the derived string */
/********************************************************************/
/* Structure "Parameter" collects various input parameters */
/********************************************************************/
struct Parameter {
char *filename; /* input file name */
int n; /* derivation length */
int angle; /* the angle factor */
int scale; /* the scaling factor */
};
typedef struct Parameter Parameter;
/********************************************************************/
/* "Production" is a structure which contains pointers to the first */
/* characters and the lengthes of: the left context, the strict */
/* predecessor, the right context and the production successor. */
/* The actual strings are stored linearly in the array inpStr. */
/* (They are separated by the ’\0’ character). */
/********************************************************************/
struct Production {
char *lCon; /* the the left context */
int lConLen; /* the length of the left context */
char *pred; /* the strict predecessor */
int predLen; /* the length of the strict predecessor */
char *rCon; /* the right context */
int rConLen; /* the length of the right context */
char *succ; /* the successor */
int succLen; /* the length of the successor */
};
typedef struct Production Production;