/* linux support subroutines
 *
 *
 *
*/

#ifdef LINUX

#include<termios.h>
#include<unistd.h>
#include<stdio.h>
#include<stdlib.h>
#ifdef TEST
#include<stdlib.h>
#endif

static struct termios tios0;
static char key_query_char = 0;

void save_term ()
{
    tcgetattr(0, &tios0);
}

void restore_term ()
{
    tcsetattr(0, TCSANOW, &tios0);
}

void echo_off ()
{
  struct termios t;
  tcgetattr(0, &t);
  t.c_lflag &= ~(ECHO  | ICANON ) ;
  t.c_cc[VMIN] = 1;
  t.c_cc[VTIME] = 0;
  tcsetattr(0, TCSANOW, &t);
}

void echo_on ()
{
  struct termios t;
  tcgetattr(0, &t);
  t.c_lflag |= ( ECHO  | ICANON );
  tcsetattr(0, TCSANOW, &t);
}

void C_emit( char ch)
{
  /* stack: ( c-- | print char  ) */

  struct termios t1, t2;

/*  tcgetattr(0, &t1);
  t2 = t1;
  t2.c_lflag &= ~(ICANON | ECHO);
  t2.c_cc[VMIN] = 1;
  t2.c_cc[VTIME] = 0;
  tcsetattr(0, TCSANOW, &t2);
*/
  putchar( ch);fflush(stdout);
/*
  tcsetattr(0, TCSANOW, &t1);
*/
  return ;
}

char C_key ()
{
  /* stack: ( -- n | wait for keypress and return key code ) */

  char ch;
  int n;
  struct termios t1, t2;

  if (key_query_char)
    {
      ch = key_query_char;
      key_query_char = 0;
    }
  else
    {
      tcgetattr(0, &t1);
      t2 = t1;
      t2.c_lflag &= ~(ICANON|ECHO);
      t2.c_cc[VMIN] = 1;
      t2.c_cc[VTIME] = 0;
      tcsetattr(0, TCSANOW, &t2);

      do {
	n = read(0, &ch, 1);
      } while (n != 1);

      tcsetattr(0, TCSANOW, &t1);
    }

  return ch;
}
/*----------------------------------------------------------*/

int C_keyquery ()
{
  /* stack: ( a -- b | return true if a key is available ) */

  char ch = 0;
  struct termios t1, t2;
  int retval = -1;

  if (!key_query_char)
    {
      tcgetattr(0, &t1);
      t2 = t1;
      t2.c_lflag &= ~(ICANON | ECHO);
      t2.c_cc[VMIN] = 0;
      t2.c_cc[VTIME] = 0;
      tcsetattr(0, TCSANOW, &t2);

      retval = read(0, &ch, 1) ? -1 : 0;
      if (ch) key_query_char = ch;  
      tcsetattr(0, TCSANOW, &t1);
    }

  return retval;
}      

 
#ifdef TEST

int main( int *argc, char **agv) {
 save_term();
 echo_off();
 printf("%c\n",C_key());
 echo_on();
 printf("%c\n",C_key());
 while ( !C_keyquery())
    printf("Waiting...\r");
 printf("\nSaved by Key (%c)\n",C_key());
 restore_term();
//  exit(0);
 return 0;
}

#endif

#endif
/* end of LINUX support functions */

#ifdef COREBOOT
/* COREBOOT support functions */


/* Runtime loader for camelforth embedded in COREBOOT CBFS */

#undef __SIZE_TYPE__
#include <libpayload-config.h>
#include <libpayload.h>
#include <cbfs.h>
#include <lzma.h>

extern uint8_t *mybuff,*mybuff_ptr;
extern uint32_t auto_start;
 
int autoload(void)
{
    uint32_t i;
    unsigned long outlen;

//    printf("Welcome to camelforth runtime loader...\n");
    struct cbfs_file *runtime = (struct cbfs_file *) \
        cbfs_find("camelforth/runtime");

    if(runtime == NULL)
        return 1;

    uint32_t len = ntohl(runtime->len);
    uint8_t *file_offset = ( uint8_t*)runtime + ntohl(runtime->offset);

/*    printf(" Location: %x \
            \n magic: %8.8s \
            \n length: %d \
            \n offset: %x \n", (uint32_t) runtime,\
            (uint8_t*) runtime->magic,len, (uint32_t)file_offset);
*/
    if((outlen = ulzma(file_offset,mybuff)) != 0) {
        auto_start= ~0;
        mybuff_ptr=mybuff;

/*    for( i = 0;i < 0x80000 ; i++){
            if (mybuff[i] == 0x1a) break;
        }

    printf("Len:%ld, bytes:%d, flag:%x,ptr:%x\n",outlen,i,auto_start,mybuff_ptr); 
*/ 
        return 0;
    }
    return 1;
}

/* struct eregs {
	uint32_t eax;
	uint32_t ecx;
	uint32_t edx;
	uint32_t ebx;
	uint32_t esp;
	uint32_t ebp;
	uint32_t esi;
	uint32_t edi;
	uint32_t vector;
	uint32_t error_code;
	uint32_t eip;
	uint32_t cs;
	uint32_t eflags;
};

struct mregs {
	uint32_t esp;
	uint32_t esi;
	uint32_t edi;
	uint32_t edx;
	uint32_t ecx;
	uint32_t eax;
    struct oregs * info2;
};

struct oregs {
	uint32_t ebp;
	uint32_t ebx;
	uint32_t vector;
	uint32_t error_code;
	uint32_t eip;
	uint32_t cs;
	uint32_t eflags;
};


void x86_exception(struct mregs *info)
{
	printf("Exception: %d @ %02x:%08x \n"
		"Code: %d eflags: %08x\n"
		"eax: %08x ebx: %08x ecx: %08x edx: %08x\n"
		"edi: %08x esi: %08x ebp: %08x esp: %08x\n",
		info->info2->vector, info->info2->cs, info->info2->eip,
		info->info2->error_code, info->info2->eflags,
		info->eax, info->info2->ebx, info->ecx, info->edx,
		info->edi, info->esi, info->info2->ebp, info->esp);
	u32 *pstack = (u32 *)info->esp;
	int i;
	for(i = 0x10; i >= 0; i-=1)
	{
		if( (i & 0x03) == 0 )
			printf("\n%08x:\t", pstack + i );
		printf("%08x ", pstack[i]);
	}

	u32 *rstack = (u32 *)info->info2->ebp;
	for(i = 0x10; i >= 0; i-=1)
	{
		if( (i & 0x03) == 0 )
			printf("\n%08x:\t", rstack + i );
		printf("%08x ", rstack[i]);
	}
    printf("\nHalting\n");
    for(;;) ;
}

*/

/* end of COREBOOT support functions */

#endif

