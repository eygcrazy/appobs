typedef struct node node;

struct node {
  void *item;
  node *next;
};

node *xs = ...        // filled with (int *) elements
void *p = xs->item;   // OK
char *q = (char *) p;   // uh oh
char c = *q;     
