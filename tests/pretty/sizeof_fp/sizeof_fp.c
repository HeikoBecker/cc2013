int main(int *x) {
&x = sizeof(void (*)(int)); // get the size of a pointer to a int->void function
&x = sizeof(void (*)(int, char)); // get the size of a pointer to a (int, char)->void function
&x = sizeof(x+x); // expression behind sizeof, but () looks like it may contain a type name
}
