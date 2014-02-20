/* The syscall.c file with it's header from the OS project of last semester.
 * Just some random real code, restricted to our subset */
void syscall_init (void);

void *syscall_get_user_esp(void);

/* This is ugly as hell, but call this when the process terminates */
void syscall_proc_terminated (int pid);

void **user_esps;

struct intr_frame {
	int x;
	int eax;
};

struct handler {
  struct uni_o_n{
    int (*func0)(void);
    int (*func1)(int);
    int (*func2)(int,int);
    int (*func3)(int,int,int);
  };
  int num_args;
  char valid;
} *handlers;

void syscall_handler (struct intr_frame *);

void intr_register_int(int a, int b, int c, char*);

int INTR_ON; int SYS_SEEK; int SYS_EXEC; int SYS_EXIT;
int SYS_HALT; int SYS_MMAP; int SYS_OPEN; int SYS_READ; int SYS_TELL; int SYS_WAIT;
int SYS_CHDIR; int SYS_WRITE; int SYS_CREATE; int SYS_REMOVE; int SYS_CLOSE;
int SYS_MKDIR; int SYS_READDIR; int SYS_ISDIR;
int sys_seek; int sys_exec; int sys_exit; int sys_halt; int sys_mmap; int sys_open;
int sys_read; int sys_tell; int sys_wait; int sys_write; int sys_create; int  sys_remove;
int sys_filesize;
int sys_close; int SYS_FILESIZE; int SYS_MUNMAP; int sys_munmap; int sys_chdir; int sys_mkdir;
int sys_readdir; int SYS_INUMBER; int sys_isdir; int sys_inumber; 
void register_handler(int a, int b, int c);

/* now the real stuff */
void
syscall_init (void) 
{
  intr_register_int (30, 3, INTR_ON, "syscall");
  register_handler(SYS_HALT,sys_halt,0);
  register_handler(SYS_EXIT,sys_exit,1);
  register_handler(SYS_EXEC,sys_exec,1);
  register_handler(SYS_WAIT,sys_wait,1);
  register_handler(SYS_WRITE,sys_write,3);
  register_handler(SYS_READ,sys_read,3);
  register_handler(SYS_OPEN,sys_open,1);
  register_handler(SYS_CREATE,sys_create,2);
  register_handler(SYS_REMOVE,sys_remove,1);
  register_handler(SYS_SEEK,sys_seek,2);
  register_handler(SYS_TELL,sys_tell,1);
  register_handler(SYS_CLOSE,sys_close,1);
  register_handler(SYS_FILESIZE,sys_filesize,1);
  register_handler(SYS_MMAP,sys_mmap,2);
  register_handler(SYS_MUNMAP,sys_munmap,1);
  register_handler(SYS_CHDIR, sys_chdir, 1);
  register_handler(SYS_MKDIR, sys_mkdir, 1);
  register_handler(SYS_READDIR, sys_readdir, 2);
  register_handler(SYS_ISDIR, sys_isdir, 1);
  register_handler(SYS_INUMBER, sys_inumber, 1);
}
void *
syscall_get_user_esp (void)
{
  int pid;
  return user_esps[pid];
}
void 
syscall_proc_terminated (int pid)
{
  user_esps[pid]=0;
}

void
syscall_handler (struct intr_frame *f)
{
  int num;
  struct handler hdl;
  /* save some casts */
  int *esp;
  int arg1;int arg2;int arg3;
  int ret;

  hdl=handlers[num];
  if(!hdl.valid)
  {
	  ;
  }

  f->eax=ret;
  return;
}
