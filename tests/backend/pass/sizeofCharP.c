int f(char* c){

return 1;
}

int main(void){
 int i;
 char * c;
 c = 0;
 i = sizeof(f(c));
 i = sizeof(f(0));
 return sizeof(c);

}
