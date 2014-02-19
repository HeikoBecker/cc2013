int foo;
void* bar;
char (**ptr);
struct s { int i; char c; };
struct s func(int, char ch);
int func2(int**);
struct { void* v; int (*f) (void); } a; 
int func3(char(**)(int,char));
