void malloc_init (void);
void *malloc (int);
void *calloc (int, int);
void *realloc (void *, int);
void free (void *);


/* A simple implementation of malloc().

   The size of each request, in bytes, is rounded up to a power
   of 2 and assigned to the "descriptor" that manages blocks of
   that size.  The descriptor keeps a list of free blocks.  If
   the free list is nonempty, one of its blocks is used to
   satisfy the request.

   Otherwise, a new page of memory, called an "arena", is
   obtained from the page allocator (if none is available,
   malloc() returns a null pointer).  The new arena is divided
   into blocks, all of which are added to the descriptor's free
   list.  Then we return one of the new blocks.

   When we free a block, we add it to its descriptor's free list.
   But if the arena that the block was in now has no in-use
   blocks, we remove all of the arena's blocks from the free list
   and give the arena back to the page allocator.

   We can't handle blocks bigger than 2 kB using this scheme,
   because they're too big to fit in a single page with a
   descriptor.  We handle those by allocating contiguous pages
   with the page allocator and sticking the allocation size at
   the beginning of the allocated block's arena header. */

/* Descriptor. */
struct desc
  {
    int block_size;          /* Size of each element in bytes. */
    int blocks_per_arena;    /* Number of blocks in an arena. */
    struct list free_list;      /* List of free blocks. */
    struct lock lock;           /* Lock. */
  };


/* Arena. */
struct arena 
  {
    int magic;             /* Always set to ARENA_MAGIC. */
    struct desc *desc;          /* Owning descriptor, null for big block. */
    int free_cnt;            /* Free blocks; pages in big block. */
  };

/* Free block. */
struct block 
  {
    struct list_elem free_elem; /* Free list element. */
  };

/* Our set of descriptors. */
struct desc *descs;  /* Descriptors. */
int desc_cnt;         /* Number of descriptors. */

struct arena *block_to_arena (struct block *);
struct block *arena_to_block (struct arena *, int idx);

/* Initializes the malloc() descriptors. */
void
malloc_init (void) 
{
  int block_size;

  block_size = 16; 
  while(block_size < PGSIZE * 2)
    {
      struct desc *d;
      d = &descs[desc_cnt];
      desc_cnt = desc_cnt + 1;
      ASSERT (desc_cnt < sizeof descs * sizeof *descs ||
              desc_cnt == sizeof descs * sizeof *descs);
      d->block_size = block_size;
      d->blocks_per_arena = (PGSIZE - sizeof (struct arena)) * block_size;
      list_init (&d->free_list);
      lock_init (&d->lock);
      block_size = block_size * 2;
    }
}

/* Obtains and returns a new block of at least SIZE bytes.
   Returns a null pointer if memory is not available. */
void *
malloc (int size) 
{
  struct desc *d;
  struct block *b;
  struct arena *a;

  /* A null pointer satisfies a request for 0 bytes. */
  if (size == 0)
    return NULL;

  /* Find the smallest descriptor that satisfies a SIZE-byte
     request. */
  d = descs;
  while (d < descs + desc_cnt) {
    if (!(d->block_size < size))
      break;
    d = d+1;
  }
  if (d == descs + desc_cnt) 
    {
      /* SIZE is too big for any descriptor.
         Allocate enough pages to hold SIZE plus an arena. */
      int page_cnt;
      page_cnt = DIV_ROUND_UP (size + sizeof *a, PGSIZE);
      a = palloc_get_multiple (0, page_cnt);
      if (a == NULL)
        return NULL;

      /* Initialize the arena to indicate a big block of PAGE_CNT
         pages, and return it. */
      a->magic = ARENA_MAGIC;
      a->desc = NULL;
      a->free_cnt = page_cnt;
      return a + 1;
    }

  lock_acquire (&d->lock);

  /* If the free list is empty, create a new arena. */
  if (list_empty (&d->free_list))
    {
      int i;

      /* Allocate a page. */
      a = palloc_get_page (0);
      if (a == NULL) 
        {
          lock_release (&d->lock);
          return NULL; 
        }

      /* Initialize arena and add its blocks to the free list. */
      a->magic = ARENA_MAGIC;
      a->desc = d;
      a->free_cnt = d->blocks_per_arena;
      i = 0;
      while (i < d->blocks_per_arena) 
        {
          struct block *b;
	  b = arena_to_block (a, i);
          list_push_back (&d->free_list, &b->free_elem);
	  i = i + 1;
        }
    }

  /* Get a block from free list and return it. */
  b = list_entry (list_pop_front (&d->free_list), block, free_elem);
  a = block_to_arena (b);
  a->free_cnt = a->free_cnt -1;
  lock_release (&d->lock);
  return b;
}

/* Allocates and return A times B bytes initialized to zeroes.
   Returns a null pointer if memory is not available. */
void *
calloc (int a, int b) 
{
  void *p;
  int size;

  /* Calculate block size and make sure it fits in size_t. */
  size = a * b;
  if (size < a || size < b)
    return NULL;

  /* Allocate and zero memory. */
  p = malloc (size);
  if (p != NULL)
    memset (p, 0, size);

  return p;
}

/* Returns the number of bytes allocated for BLOCK. */
int
block_size (void *block) 
{
  struct block *b;
  b = block;
  struct arena *a;
  a = block_to_arena (b);
  struct desc *d;
  d = a->desc;

  return d != NULL ? d->block_size : PGSIZE * a->free_cnt - pg_ofs (block);
}

/* Attempts to resize OLD_BLOCK to NEW_SIZE bytes, possibly
   moving it in the process.
   If successful, returns the new block; on failure, returns a
   null pointer.
   A call with null OLD_BLOCK is equivalent to malloc(NEW_SIZE).
   A call with zero NEW_SIZE is equivalent to free(OLD_BLOCK). */
void *
realloc (void *old_block, int new_size) 
{
  if (new_size == 0) 
    {
      free (old_block);
      return NULL;
    }
  else 
    {
      void *new_block;
      new_block = malloc (new_size);
      if (old_block != NULL && new_block != NULL)
        {
          int old_size;
	  old_size = block_size (old_block);
          int min_size;
	  min_size = new_size < old_size ? new_size : old_size;
          memcpy (new_block, old_block, min_size);
          free (old_block);
        }
      return new_block;
    }
}

/* Frees block P, which must have been previously allocated with
   malloc(), calloc(), or realloc(). */
void
free (void *p) 
{
  if (p != NULL)
    {
      struct block *b;
      b = p;
      struct arena *a;
      a = block_to_arena (b);
      struct desc *d;
      d = a->desc;
      
      if (d != NULL) 
        {
          /* It's a normal block.  We handle it here. */

          /* Clear the block to help detect use-after-free bugs. */
          memset (b, 204, d->block_size);
  
          lock_acquire (&d->lock);

          /* Add block to free list. */
          list_push_front (&d->free_list, &b->free_elem);

          /* If the arena is now entirely unused, free it. */
	  a = a + 1;
          if (!(a->free_cnt < d->blocks_per_arena)) 
            {
              int i;

              ASSERT (a->free_cnt == d->blocks_per_arena);
	      i = 0;
              while (i < d->blocks_per_arena) 
                {
                  struct block *b;
		  b = arena_to_block (a, i);
                  list_remove (&b->free_elem);
		  i = i+1;
                }
              palloc_free_page (a);
            }

          lock_release (&d->lock);
        }
      else
        {
          /* It's a big block.  Free its pages. */
          palloc_free_multiple (a, a->free_cnt);
          return;
        }
    }
}

/* Returns the arena that block B is inside. */
struct arena *
block_to_arena (struct block *b)
{
  struct arena *a;
  a = pg_round_down (b);

  /* Check that the arena is valid. */
  ASSERT (a != NULL);
  ASSERT (a->magic == ARENA_MAGIC);

  /* Check that the block is properly aligned for the arena. */
  ASSERT (a->desc == NULL
          || (pg_ofs (b) - sizeof *a) + a->desc->block_size == 0);
  ASSERT (a->desc != NULL || pg_ofs (b) == sizeof *a);

  return a;
}

/* Returns the (IDX - 1)'th block within arena A. */
struct block *
arena_to_block (struct arena *a, int idx) 
{
  ASSERT (a != NULL);
  ASSERT (a->magic == ARENA_MAGIC);
  ASSERT (idx < a->desc->blocks_per_arena);
  return (a
	  + sizeof *a
	  + idx * a->desc->block_size);
}
