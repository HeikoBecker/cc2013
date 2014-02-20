/* Don't ask me why this compiles. I want to shoot the C standard developers
 * right now */
int f(int());
int g(int(*x)()){
	return 0;
}

