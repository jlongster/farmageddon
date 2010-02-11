# 1 "ftgl.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "ftgl.c"
# 46 "ftgl.c"
# 1 "/usr/local/include/gambit.h" 1 3
# 466 "/usr/local/include/gambit.h" 3
# 1 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/limits.h" 1 3 4






# 1 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/syslimits.h" 1 3 4
# 8 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/limits.h" 2 3 4


# 1 "/usr/include/limits.h" 1 3 4
# 63 "/usr/include/limits.h" 3 4
# 1 "/usr/include/sys/cdefs.h" 1 3 4
# 64 "/usr/include/limits.h" 2 3 4
# 1 "/usr/include/machine/limits.h" 1 3 4







# 1 "/usr/include/i386/limits.h" 1 3 4
# 40 "/usr/include/i386/limits.h" 3 4
# 1 "/usr/include/i386/_limits.h" 1 3 4
# 41 "/usr/include/i386/limits.h" 2 3 4
# 9 "/usr/include/machine/limits.h" 2 3 4
# 65 "/usr/include/limits.h" 2 3 4
# 1 "/usr/include/sys/syslimits.h" 1 3 4
# 66 "/usr/include/limits.h" 2 3 4
# 11 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/limits.h" 2 3 4
# 467 "/usr/local/include/gambit.h" 2 3
# 745 "/usr/local/include/gambit.h" 3
# 1 "/usr/include/wchar.h" 1 3 4
# 70 "/usr/include/wchar.h" 3 4
# 1 "/usr/include/_types.h" 1 3 4
# 27 "/usr/include/_types.h" 3 4
# 1 "/usr/include/sys/_types.h" 1 3 4
# 33 "/usr/include/sys/_types.h" 3 4
# 1 "/usr/include/machine/_types.h" 1 3 4
# 34 "/usr/include/machine/_types.h" 3 4
# 1 "/usr/include/i386/_types.h" 1 3 4
# 37 "/usr/include/i386/_types.h" 3 4
typedef signed char __int8_t;



typedef unsigned char __uint8_t;
typedef short __int16_t;
typedef unsigned short __uint16_t;
typedef int __int32_t;
typedef unsigned int __uint32_t;
typedef long long __int64_t;
typedef unsigned long long __uint64_t;

typedef long __darwin_intptr_t;
typedef unsigned int __darwin_natural_t;
# 70 "/usr/include/i386/_types.h" 3 4
typedef int __darwin_ct_rune_t;





typedef union {
 char __mbstate8[128];
 long long _mbstateL;
} __mbstate_t;

typedef __mbstate_t __darwin_mbstate_t;


typedef long int __darwin_ptrdiff_t;





typedef long unsigned int __darwin_size_t;





typedef __builtin_va_list __darwin_va_list;





typedef int __darwin_wchar_t;




typedef __darwin_wchar_t __darwin_rune_t;


typedef int __darwin_wint_t;




typedef unsigned long __darwin_clock_t;
typedef __uint32_t __darwin_socklen_t;
typedef long __darwin_ssize_t;
typedef long __darwin_time_t;
# 35 "/usr/include/machine/_types.h" 2 3 4
# 34 "/usr/include/sys/_types.h" 2 3 4
# 58 "/usr/include/sys/_types.h" 3 4
struct __darwin_pthread_handler_rec
{
 void (*__routine)(void *);
 void *__arg;
 struct __darwin_pthread_handler_rec *__next;
};
struct _opaque_pthread_attr_t { long __sig; char __opaque[56]; };
struct _opaque_pthread_cond_t { long __sig; char __opaque[40]; };
struct _opaque_pthread_condattr_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_mutex_t { long __sig; char __opaque[56]; };
struct _opaque_pthread_mutexattr_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_once_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_rwlock_t { long __sig; char __opaque[192]; };
struct _opaque_pthread_rwlockattr_t { long __sig; char __opaque[16]; };
struct _opaque_pthread_t { long __sig; struct __darwin_pthread_handler_rec *__cleanup_stack; char __opaque[1168]; };
# 94 "/usr/include/sys/_types.h" 3 4
typedef __int64_t __darwin_blkcnt_t;
typedef __int32_t __darwin_blksize_t;
typedef __int32_t __darwin_dev_t;
typedef unsigned int __darwin_fsblkcnt_t;
typedef unsigned int __darwin_fsfilcnt_t;
typedef __uint32_t __darwin_gid_t;
typedef __uint32_t __darwin_id_t;
typedef __uint64_t __darwin_ino64_t;

typedef __darwin_ino64_t __darwin_ino_t;



typedef __darwin_natural_t __darwin_mach_port_name_t;
typedef __darwin_mach_port_name_t __darwin_mach_port_t;
typedef __uint16_t __darwin_mode_t;
typedef __int64_t __darwin_off_t;
typedef __int32_t __darwin_pid_t;
typedef struct _opaque_pthread_attr_t
   __darwin_pthread_attr_t;
typedef struct _opaque_pthread_cond_t
   __darwin_pthread_cond_t;
typedef struct _opaque_pthread_condattr_t
   __darwin_pthread_condattr_t;
typedef unsigned long __darwin_pthread_key_t;
typedef struct _opaque_pthread_mutex_t
   __darwin_pthread_mutex_t;
typedef struct _opaque_pthread_mutexattr_t
   __darwin_pthread_mutexattr_t;
typedef struct _opaque_pthread_once_t
   __darwin_pthread_once_t;
typedef struct _opaque_pthread_rwlock_t
   __darwin_pthread_rwlock_t;
typedef struct _opaque_pthread_rwlockattr_t
   __darwin_pthread_rwlockattr_t;
typedef struct _opaque_pthread_t
   *__darwin_pthread_t;
typedef __uint32_t __darwin_sigset_t;
typedef __int32_t __darwin_suseconds_t;
typedef __uint32_t __darwin_uid_t;
typedef __uint32_t __darwin_useconds_t;
typedef unsigned char __darwin_uuid_t[16];
typedef char __darwin_uuid_string_t[37];
# 28 "/usr/include/_types.h" 2 3 4
# 39 "/usr/include/_types.h" 3 4
typedef int __darwin_nl_item;
typedef int __darwin_wctrans_t;

typedef __uint32_t __darwin_wctype_t;
# 71 "/usr/include/wchar.h" 2 3 4







typedef __darwin_size_t size_t;




typedef __darwin_mbstate_t mbstate_t;




typedef __darwin_ct_rune_t ct_rune_t;




typedef __darwin_rune_t rune_t;





typedef __darwin_wchar_t wchar_t;
# 111 "/usr/include/wchar.h" 3 4
# 1 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/stdarg.h" 1 3 4
# 43 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 105 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/stdarg.h" 3 4
typedef __gnuc_va_list va_list;
# 112 "/usr/include/wchar.h" 2 3 4
# 1 "/usr/include/stdio.h" 1 3 4
# 75 "/usr/include/stdio.h" 3 4
typedef __darwin_off_t off_t;
# 87 "/usr/include/stdio.h" 3 4
typedef __darwin_off_t fpos_t;
# 98 "/usr/include/stdio.h" 3 4
struct __sbuf {
 unsigned char *_base;
 int _size;
};


struct __sFILEX;
# 132 "/usr/include/stdio.h" 3 4
typedef struct __sFILE {
 unsigned char *_p;
 int _r;
 int _w;
 short _flags;
 short _file;
 struct __sbuf _bf;
 int _lbfsize;


 void *_cookie;
 int (*_close)(void *);
 int (*_read) (void *, char *, int);
 fpos_t (*_seek) (void *, fpos_t, int);
 int (*_write)(void *, const char *, int);


 struct __sbuf _ub;
 struct __sFILEX *_extra;
 int _ur;


 unsigned char _ubuf[3];
 unsigned char _nbuf[1];


 struct __sbuf _lb;


 int _blksize;
 fpos_t _offset;
} FILE;



extern FILE *__stdinp;
extern FILE *__stdoutp;
extern FILE *__stderrp;




# 248 "/usr/include/stdio.h" 3 4

void clearerr(FILE *);
int fclose(FILE *);
int feof(FILE *);
int ferror(FILE *);
int fflush(FILE *);
int fgetc(FILE *);
int fgetpos(FILE * , fpos_t *);
char *fgets(char * , int, FILE *);



FILE *fopen(const char * , const char * ) __asm("_" "fopen" );

int fprintf(FILE * , const char * , ...) ;
int fputc(int, FILE *);
int fputs(const char * , FILE * ) __asm("_" "fputs" );
size_t fread(void * , size_t, size_t, FILE * );
FILE *freopen(const char * , const char * ,
     FILE * ) __asm("_" "freopen" );
int fscanf(FILE * , const char * , ...) ;
int fseek(FILE *, long, int);
int fsetpos(FILE *, const fpos_t *);
long ftell(FILE *);
size_t fwrite(const void * , size_t, size_t, FILE * ) __asm("_" "fwrite" );
int getc(FILE *);
int getchar(void);
char *gets(char *);

extern const int sys_nerr;
extern const char *const sys_errlist[];

void perror(const char *);
int printf(const char * , ...) ;
int putc(int, FILE *);
int putchar(int);
int puts(const char *);
int remove(const char *);
int rename (const char *, const char *);
void rewind(FILE *);
int scanf(const char * , ...) ;
void setbuf(FILE * , char * );
int setvbuf(FILE * , char * , int, size_t);
int sprintf(char * , const char * , ...) ;
int sscanf(const char * , const char * , ...) ;
FILE *tmpfile(void);
char *tmpnam(char *);
int ungetc(int, FILE *);
int vfprintf(FILE * , const char * , va_list) ;
int vprintf(const char * , va_list) ;
int vsprintf(char * , const char * , va_list) ;

int asprintf(char **, const char *, ...) ;
int vasprintf(char **, const char *, va_list) ;










char *ctermid(char *);

char *ctermid_r(char *);




FILE *fdopen(int, const char *) __asm("_" "fdopen" );


char *fgetln(FILE *, size_t *);

int fileno(FILE *);
void flockfile(FILE *);

const char
 *fmtcheck(const char *, const char *);
int fpurge(FILE *);

int fseeko(FILE *, off_t, int);
off_t ftello(FILE *);
int ftrylockfile(FILE *);
void funlockfile(FILE *);
int getc_unlocked(FILE *);
int getchar_unlocked(void);

int getw(FILE *);

int pclose(FILE *);



FILE *popen(const char *, const char *) __asm("_" "popen" );

int putc_unlocked(int, FILE *);
int putchar_unlocked(int);

int putw(int, FILE *);
void setbuffer(FILE *, char *, int);
int setlinebuf(FILE *);

int snprintf(char * , size_t, const char * , ...) ;
char *tempnam(const char *, const char *) __asm("_" "tempnam" );
int vfscanf(FILE * , const char * , va_list) ;
int vscanf(const char * , va_list) ;
int vsnprintf(char * , size_t, const char * , va_list) ;
int vsscanf(const char * , const char * , va_list) ;

FILE *zopen(const char *, const char *, int);








FILE *funopen(const void *,
  int (*)(void *, char *, int),
  int (*)(void *, const char *, int),
  fpos_t (*)(void *, fpos_t, int),
  int (*)(void *));

# 383 "/usr/include/stdio.h" 3 4

int __srget(FILE *);
int __svfscanf(FILE *, const char *, va_list) ;
int __swbuf(int, FILE *);








static __inline int __sputc(int _c, FILE *_p) {
 if (--_p->_w >= 0 || (_p->_w >= _p->_lbfsize && (char)_c != '\n'))
  return (*_p->_p++ = _c);
 else
  return (__swbuf(_c, _p));
}
# 443 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/secure/_stdio.h" 1 3 4
# 31 "/usr/include/secure/_stdio.h" 3 4
# 1 "/usr/include/secure/_common.h" 1 3 4
# 32 "/usr/include/secure/_stdio.h" 2 3 4
# 42 "/usr/include/secure/_stdio.h" 3 4
extern int __sprintf_chk (char * , int, size_t,
     const char * , ...)
  ;




extern int __snprintf_chk (char * , size_t, int, size_t,
      const char * , ...)
  ;




extern int __vsprintf_chk (char * , int, size_t,
      const char * , va_list)
  ;




extern int __vsnprintf_chk (char * , size_t, int, size_t,
       const char * , va_list)
  ;
# 444 "/usr/include/stdio.h" 2 3 4
# 113 "/usr/include/wchar.h" 2 3 4
# 1 "/usr/include/time.h" 1 3 4
# 69 "/usr/include/time.h" 3 4
# 1 "/usr/include/_structs.h" 1 3 4
# 24 "/usr/include/_structs.h" 3 4
# 1 "/usr/include/sys/_structs.h" 1 3 4
# 88 "/usr/include/sys/_structs.h" 3 4
struct timespec
{
 __darwin_time_t tv_sec;
 long tv_nsec;
};
# 25 "/usr/include/_structs.h" 2 3 4
# 70 "/usr/include/time.h" 2 3 4







typedef __darwin_clock_t clock_t;
# 87 "/usr/include/time.h" 3 4
typedef __darwin_time_t time_t;


struct tm {
 int tm_sec;
 int tm_min;
 int tm_hour;
 int tm_mday;
 int tm_mon;
 int tm_year;
 int tm_wday;
 int tm_yday;
 int tm_isdst;
 long tm_gmtoff;
 char *tm_zone;
};
# 113 "/usr/include/time.h" 3 4
extern char *tzname[];


extern int getdate_err;

extern long timezone __asm("_" "timezone" );

extern int daylight;


char *asctime(const struct tm *);
clock_t clock(void) __asm("_" "clock" );
char *ctime(const time_t *);
double difftime(time_t, time_t);
struct tm *getdate(const char *);
struct tm *gmtime(const time_t *);
struct tm *localtime(const time_t *);
time_t mktime(struct tm *) __asm("_" "mktime" );
size_t strftime(char * , size_t, const char * , const struct tm * ) __asm("_" "strftime" );
char *strptime(const char * , const char * , struct tm * ) __asm("_" "strptime" );
time_t time(time_t *);


void tzset(void);



char *asctime_r(const struct tm * , char * );
char *ctime_r(const time_t *, char *);
struct tm *gmtime_r(const time_t * , struct tm * );
struct tm *localtime_r(const time_t * , struct tm * );


time_t posix2time(time_t);



void tzsetwall(void);
time_t time2posix(time_t);
time_t timelocal(struct tm * const);
time_t timegm(struct tm * const);



int nanosleep(const struct timespec *, struct timespec *) __asm("_" "nanosleep" );


# 114 "/usr/include/wchar.h" 2 3 4
# 1 "/usr/include/_wctype.h" 1 3 4
# 47 "/usr/include/_wctype.h" 3 4
typedef __darwin_wint_t wint_t;




typedef __darwin_wctype_t wctype_t;
# 63 "/usr/include/_wctype.h" 3 4
# 1 "/usr/include/ctype.h" 1 3 4
# 69 "/usr/include/ctype.h" 3 4
# 1 "/usr/include/runetype.h" 1 3 4
# 81 "/usr/include/runetype.h" 3 4
typedef struct {
 __darwin_rune_t __min;
 __darwin_rune_t __max;
 __darwin_rune_t __map;
 __uint32_t *__types;
} _RuneEntry;

typedef struct {
 int __nranges;
 _RuneEntry *__ranges;
} _RuneRange;

typedef struct {
 char __name[14];
 __uint32_t __mask;
} _RuneCharClass;

typedef struct {
 char __magic[8];
 char __encoding[32];

 __darwin_rune_t (*__sgetrune)(const char *, __darwin_size_t, char const **);
 int (*__sputrune)(__darwin_rune_t, char *, __darwin_size_t, char **);
 __darwin_rune_t __invalid_rune;

 __uint32_t __runetype[(1 <<8 )];
 __darwin_rune_t __maplower[(1 <<8 )];
 __darwin_rune_t __mapupper[(1 <<8 )];






 _RuneRange __runetype_ext;
 _RuneRange __maplower_ext;
 _RuneRange __mapupper_ext;

 void *__variable;
 int __variable_len;




 int __ncharclasses;
 _RuneCharClass *__charclasses;
} _RuneLocale;




extern _RuneLocale _DefaultRuneLocale;
extern _RuneLocale *_CurrentRuneLocale;

# 70 "/usr/include/ctype.h" 2 3 4
# 145 "/usr/include/ctype.h" 3 4

unsigned long ___runetype(__darwin_ct_rune_t);
__darwin_ct_rune_t ___tolower(__darwin_ct_rune_t);
__darwin_ct_rune_t ___toupper(__darwin_ct_rune_t);


static __inline int
isascii(int _c)
{
 return ((_c & ~0x7F) == 0);
}
# 164 "/usr/include/ctype.h" 3 4

int __maskrune(__darwin_ct_rune_t, unsigned long);



static __inline int
__istype(__darwin_ct_rune_t _c, unsigned long _f)
{



 return (isascii(_c) ? !!(_DefaultRuneLocale.__runetype[_c] & _f)
  : !!__maskrune(_c, _f));

}

static __inline __darwin_ct_rune_t
__isctype(__darwin_ct_rune_t _c, unsigned long _f)
{



 return (_c < 0 || _c >= (1 <<8 )) ? 0 :
  !!(_DefaultRuneLocale.__runetype[_c] & _f);

}
# 204 "/usr/include/ctype.h" 3 4

__darwin_ct_rune_t __toupper(__darwin_ct_rune_t);
__darwin_ct_rune_t __tolower(__darwin_ct_rune_t);



static __inline int
__wcwidth(__darwin_ct_rune_t _c)
{
 unsigned int _x;

 if (_c == 0)
  return (0);
 _x = (unsigned int)__maskrune(_c, 0xe0000000L|0x00040000L);
 if ((_x & 0xe0000000L) != 0)
  return ((_x & 0xe0000000L) >> 30);
 return ((_x & 0x00040000L) != 0 ? 1 : -1);
}






static __inline int
isalnum(int _c)
{
 return (__istype(_c, 0x00000100L|0x00000400L));
}

static __inline int
isalpha(int _c)
{
 return (__istype(_c, 0x00000100L));
}

static __inline int
isblank(int _c)
{
 return (__istype(_c, 0x00020000L));
}

static __inline int
iscntrl(int _c)
{
 return (__istype(_c, 0x00000200L));
}


static __inline int
isdigit(int _c)
{
 return (__isctype(_c, 0x00000400L));
}

static __inline int
isgraph(int _c)
{
 return (__istype(_c, 0x00000800L));
}

static __inline int
islower(int _c)
{
 return (__istype(_c, 0x00001000L));
}

static __inline int
isprint(int _c)
{
 return (__istype(_c, 0x00040000L));
}

static __inline int
ispunct(int _c)
{
 return (__istype(_c, 0x00002000L));
}

static __inline int
isspace(int _c)
{
 return (__istype(_c, 0x00004000L));
}

static __inline int
isupper(int _c)
{
 return (__istype(_c, 0x00008000L));
}


static __inline int
isxdigit(int _c)
{
 return (__isctype(_c, 0x00010000L));
}

static __inline int
toascii(int _c)
{
 return (_c & 0x7F);
}

static __inline int
tolower(int _c)
{
        return (__tolower(_c));
}

static __inline int
toupper(int _c)
{
        return (__toupper(_c));
}


static __inline int
digittoint(int _c)
{
 return (__maskrune(_c, 0x0F));
}

static __inline int
ishexnumber(int _c)
{
 return (__istype(_c, 0x00010000L));
}

static __inline int
isideogram(int _c)
{
 return (__istype(_c, 0x00080000L));
}

static __inline int
isnumber(int _c)
{
 return (__istype(_c, 0x00000400L));
}

static __inline int
isphonogram(int _c)
{
 return (__istype(_c, 0x00200000L));
}

static __inline int
isrune(int _c)
{
 return (__istype(_c, 0xFFFFFFF0L));
}

static __inline int
isspecial(int _c)
{
 return (__istype(_c, 0x00100000L));
}
# 64 "/usr/include/_wctype.h" 2 3 4







static __inline int
iswalnum(wint_t _wc)
{
 return (__istype(_wc, 0x00000100L|0x00000400L));
}

static __inline int
iswalpha(wint_t _wc)
{
 return (__istype(_wc, 0x00000100L));
}

static __inline int
iswcntrl(wint_t _wc)
{
 return (__istype(_wc, 0x00000200L));
}

static __inline int
iswctype(wint_t _wc, wctype_t _charclass)
{
 return (__istype(_wc, _charclass));
}

static __inline int
iswdigit(wint_t _wc)
{
 return (__isctype(_wc, 0x00000400L));
}

static __inline int
iswgraph(wint_t _wc)
{
 return (__istype(_wc, 0x00000800L));
}

static __inline int
iswlower(wint_t _wc)
{
 return (__istype(_wc, 0x00001000L));
}

static __inline int
iswprint(wint_t _wc)
{
 return (__istype(_wc, 0x00040000L));
}

static __inline int
iswpunct(wint_t _wc)
{
 return (__istype(_wc, 0x00002000L));
}

static __inline int
iswspace(wint_t _wc)
{
 return (__istype(_wc, 0x00004000L));
}

static __inline int
iswupper(wint_t _wc)
{
 return (__istype(_wc, 0x00008000L));
}

static __inline int
iswxdigit(wint_t _wc)
{
 return (__isctype(_wc, 0x00010000L));
}

static __inline wint_t
towlower(wint_t _wc)
{
        return (__tolower(_wc));
}

static __inline wint_t
towupper(wint_t _wc)
{
        return (__toupper(_wc));
}
# 176 "/usr/include/_wctype.h" 3 4

wctype_t
 wctype(const char *);

# 115 "/usr/include/wchar.h" 2 3 4


wint_t btowc(int);
wint_t fgetwc(FILE *);
wchar_t *fgetws(wchar_t * , int, FILE * );
wint_t fputwc(wchar_t, FILE *);
int fputws(const wchar_t * , FILE * );
int fwide(FILE *, int);
int fwprintf(FILE * , const wchar_t * , ...) ;
int fwscanf(FILE * , const wchar_t * , ...) ;
wint_t getwc(FILE *);
wint_t getwchar(void);
size_t mbrlen(const char * , size_t, mbstate_t * );
size_t mbrtowc(wchar_t * , const char * , size_t,
     mbstate_t * );
int mbsinit(const mbstate_t *);
size_t mbsrtowcs(wchar_t * , const char ** , size_t,
     mbstate_t * );
wint_t putwc(wchar_t, FILE *);
wint_t putwchar(wchar_t);
int swprintf(wchar_t * , size_t, const wchar_t * ,
     ...) ;
int swscanf(const wchar_t * , const wchar_t * , ...) ;
wint_t ungetwc(wint_t, FILE *);
int vfwprintf(FILE * , const wchar_t * ,
     __darwin_va_list) ;
int vswprintf(wchar_t * , size_t, const wchar_t * ,
     __darwin_va_list) ;
int vwprintf(const wchar_t * , __darwin_va_list) ;
size_t wcrtomb(char * , wchar_t, mbstate_t * );
wchar_t *wcscat(wchar_t * , const wchar_t * );
wchar_t *wcschr(const wchar_t *, wchar_t);
int wcscmp(const wchar_t *, const wchar_t *);
int wcscoll(const wchar_t *, const wchar_t *);
wchar_t *wcscpy(wchar_t * , const wchar_t * );
size_t wcscspn(const wchar_t *, const wchar_t *);
size_t wcsftime(wchar_t * , size_t, const wchar_t * ,
     const struct tm * ) __asm("_" "wcsftime" );
size_t wcslen(const wchar_t *);
wchar_t *wcsncat(wchar_t * , const wchar_t * , size_t);
int wcsncmp(const wchar_t *, const wchar_t *, size_t);
wchar_t *wcsncpy(wchar_t * , const wchar_t * , size_t);
wchar_t *wcspbrk(const wchar_t *, const wchar_t *);
wchar_t *wcsrchr(const wchar_t *, wchar_t);
size_t wcsrtombs(char * , const wchar_t ** , size_t,
     mbstate_t * );
size_t wcsspn(const wchar_t *, const wchar_t *);
wchar_t *wcsstr(const wchar_t * , const wchar_t * );
size_t wcsxfrm(wchar_t * , const wchar_t * , size_t);
int wctob(wint_t);
double wcstod(const wchar_t * , wchar_t ** );
wchar_t *wcstok(wchar_t * , const wchar_t * ,
     wchar_t ** );
long wcstol(const wchar_t * , wchar_t ** , int);
unsigned long
  wcstoul(const wchar_t * , wchar_t ** , int);
wchar_t *wmemchr(const wchar_t *, wchar_t, size_t);
int wmemcmp(const wchar_t *, const wchar_t *, size_t);
wchar_t *wmemcpy(wchar_t * , const wchar_t * , size_t);
wchar_t *wmemmove(wchar_t *, const wchar_t *, size_t);
wchar_t *wmemset(wchar_t *, wchar_t, size_t);
int wprintf(const wchar_t * , ...) ;
int wscanf(const wchar_t * , ...) ;


int vfwscanf(FILE * , const wchar_t * ,
     __darwin_va_list) ;
int vswscanf(const wchar_t * , const wchar_t * ,
     __darwin_va_list) ;
int vwscanf(const wchar_t * , __darwin_va_list) ;
float wcstof(const wchar_t * , wchar_t ** );
long double
 wcstold(const wchar_t * , wchar_t ** ) ;

long long
 wcstoll(const wchar_t * , wchar_t ** , int);
unsigned long long
 wcstoull(const wchar_t * , wchar_t ** , int);

int wcswidth(const wchar_t *, size_t);
int wcwidth(wchar_t);



size_t mbsnrtowcs(wchar_t * , const char ** , size_t,
     size_t, mbstate_t * );
size_t wcslcat(wchar_t *, const wchar_t *, size_t);
size_t wcslcpy(wchar_t *, const wchar_t *, size_t);
size_t wcsnrtombs(char * , const wchar_t ** , size_t,
     size_t, mbstate_t * );







# 746 "/usr/local/include/gambit.h" 2 3
# 790 "/usr/local/include/gambit.h" 3
# 1 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/float.h" 1 3 4
# 791 "/usr/local/include/gambit.h" 2 3
# 1283 "/usr/local/include/gambit.h" 3
# 1 "/usr/include/setjmp.h" 1 3 4
# 26 "/usr/include/setjmp.h" 3 4
# 1 "/usr/include/machine/setjmp.h" 1 3 4
# 37 "/usr/include/machine/setjmp.h" 3 4
# 1 "/usr/include/i386/setjmp.h" 1 3 4
# 47 "/usr/include/i386/setjmp.h" 3 4
typedef int jmp_buf[((9 * 2) + 3 + 16)];
typedef int sigjmp_buf[((9 * 2) + 3 + 16) + 1];
# 65 "/usr/include/i386/setjmp.h" 3 4

int setjmp(jmp_buf);
void longjmp(jmp_buf, int);


int _setjmp(jmp_buf);
void _longjmp(jmp_buf, int);
int sigsetjmp(sigjmp_buf, int);
void siglongjmp(sigjmp_buf, int);



void longjmperror(void);


# 38 "/usr/include/machine/setjmp.h" 2 3 4
# 27 "/usr/include/setjmp.h" 2 3 4
# 1284 "/usr/local/include/gambit.h" 2 3
# 6598 "/usr/local/include/gambit.h" 3
# 1 "/usr/include/math.h" 1 3 4
# 28 "/usr/include/math.h" 3 4
# 1 "/usr/include/architecture/i386/math.h" 1 3 4
# 49 "/usr/include/architecture/i386/math.h" 3 4
 typedef float float_t;
 typedef double double_t;
# 108 "/usr/include/architecture/i386/math.h" 3 4
extern unsigned int __math_errhandling ( void );
# 128 "/usr/include/architecture/i386/math.h" 3 4
extern int __fpclassifyf(float );
extern int __fpclassifyd(double );
extern int __fpclassify (long double);
# 163 "/usr/include/architecture/i386/math.h" 3 4
 static __inline__ int __inline_isfinitef (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isfinited (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isfinite (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_isinff (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isinfd (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isinf (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_isnanf (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnand (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnan (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_isnormalf (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnormald (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnormal (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_signbitf (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_signbitd (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_signbit (long double) __attribute__ ((always_inline));

 static __inline__ int __inline_isinff( float __x ) { return __builtin_fabsf(__x) == __builtin_inff(); }
 static __inline__ int __inline_isinfd( double __x ) { return __builtin_fabs(__x) == __builtin_inf(); }
 static __inline__ int __inline_isinf( long double __x ) { return __builtin_fabsl(__x) == __builtin_infl(); }
 static __inline__ int __inline_isfinitef( float __x ) { return __x == __x && __builtin_fabsf(__x) != __builtin_inff(); }
 static __inline__ int __inline_isfinited( double __x ) { return __x == __x && __builtin_fabs(__x) != __builtin_inf(); }
 static __inline__ int __inline_isfinite( long double __x ) { return __x == __x && __builtin_fabsl(__x) != __builtin_infl(); }
 static __inline__ int __inline_isnanf( float __x ) { return __x != __x; }
 static __inline__ int __inline_isnand( double __x ) { return __x != __x; }
 static __inline__ int __inline_isnan( long double __x ) { return __x != __x; }
 static __inline__ int __inline_signbitf( float __x ) { union{ float __f; unsigned int __u; }__u; __u.__f = __x; return (int)(__u.__u >> 31); }
 static __inline__ int __inline_signbitd( double __x ) { union{ double __f; unsigned int __u[2]; }__u; __u.__f = __x; return (int)(__u.__u[1] >> 31); }
 static __inline__ int __inline_signbit( long double __x ){ union{ long double __ld; struct{ unsigned int __m[2]; short __sexp; }__p; }__u; __u.__ld = __x; return (int) (((unsigned short) __u.__p.__sexp) >> 15); }
 static __inline__ int __inline_isnormalf( float __x ) { float fabsf = __builtin_fabsf(__x); if( __x != __x ) return 0; return fabsf < __builtin_inff() && fabsf >= 1.17549435e-38F; }
 static __inline__ int __inline_isnormald( double __x ) { double fabsf = __builtin_fabs(__x); if( __x != __x ) return 0; return fabsf < __builtin_inf() && fabsf >= 2.2250738585072014e-308; }
 static __inline__ int __inline_isnormal( long double __x ) { long double fabsf = __builtin_fabsl(__x); if( __x != __x ) return 0; return fabsf < __builtin_infl() && fabsf >= 3.36210314311209350626e-4932L; }
# 253 "/usr/include/architecture/i386/math.h" 3 4
extern double acos( double );
extern float acosf( float );

extern double asin( double );
extern float asinf( float );

extern double atan( double );
extern float atanf( float );

extern double atan2( double, double );
extern float atan2f( float, float );

extern double cos( double );
extern float cosf( float );

extern double sin( double );
extern float sinf( float );

extern double tan( double );
extern float tanf( float );

extern double acosh( double );
extern float acoshf( float );

extern double asinh( double );
extern float asinhf( float );

extern double atanh( double );
extern float atanhf( float );

extern double cosh( double );
extern float coshf( float );

extern double sinh( double );
extern float sinhf( float );

extern double tanh( double );
extern float tanhf( float );

extern double exp ( double );
extern float expf ( float );

extern double exp2 ( double );
extern float exp2f ( float );

extern double expm1 ( double );
extern float expm1f ( float );

extern double log ( double );
extern float logf ( float );

extern double log10 ( double );
extern float log10f ( float );

extern double log2 ( double );
extern float log2f ( float );

extern double log1p ( double );
extern float log1pf ( float );

extern double logb ( double );
extern float logbf ( float );

extern double modf ( double, double * );
extern float modff ( float, float * );

extern double ldexp ( double, int );
extern float ldexpf ( float, int );

extern double frexp ( double, int * );
extern float frexpf ( float, int * );

extern int ilogb ( double );
extern int ilogbf ( float );

extern double scalbn ( double, int );
extern float scalbnf ( float, int );

extern double scalbln ( double, long int );
extern float scalblnf ( float, long int );

extern double fabs( double );
extern float fabsf( float );

extern double cbrt( double );
extern float cbrtf( float );

extern double hypot ( double, double );
extern float hypotf ( float, float );

extern double pow ( double, double );
extern float powf ( float, float );

extern double sqrt( double );
extern float sqrtf( float );

extern double erf( double );
extern float erff( float );

extern double erfc( double );
extern float erfcf( float );






extern double lgamma( double );
extern float lgammaf( float );

extern double tgamma( double );
extern float tgammaf( float );

extern double ceil ( double );
extern float ceilf ( float );

extern double floor ( double );
extern float floorf ( float );

extern double nearbyint ( double );
extern float nearbyintf ( float );

extern double rint ( double );
extern float rintf ( float );

extern long int lrint ( double );
extern long int lrintf ( float );

extern double round ( double );
extern float roundf ( float );

extern long int lround ( double );
extern long int lroundf ( float );




    extern long long int llrint ( double );
    extern long long int llrintf ( float );

    extern long long int llround ( double );
    extern long long int llroundf ( float );



extern double trunc ( double );
extern float truncf ( float );

extern double fmod ( double, double );
extern float fmodf ( float, float );

extern double remainder ( double, double );
extern float remainderf ( float, float );

extern double remquo ( double, double, int * );
extern float remquof ( float, float, int * );

extern double copysign ( double, double );
extern float copysignf ( float, float );

extern double nan( const char * );
extern float nanf( const char * );

extern double nextafter ( double, double );
extern float nextafterf ( float, float );

extern double fdim ( double, double );
extern float fdimf ( float, float );

extern double fmax ( double, double );
extern float fmaxf ( float, float );

extern double fmin ( double, double );
extern float fminf ( float, float );

extern double fma ( double, double, double );
extern float fmaf ( float, float, float );

extern long double acosl(long double);
extern long double asinl(long double);
extern long double atanl(long double);
extern long double atan2l(long double, long double);
extern long double cosl(long double);
extern long double sinl(long double);
extern long double tanl(long double);
extern long double acoshl(long double);
extern long double asinhl(long double);
extern long double atanhl(long double);
extern long double coshl(long double);
extern long double sinhl(long double);
extern long double tanhl(long double);
extern long double expl(long double);
extern long double exp2l(long double);
extern long double expm1l(long double);
extern long double logl(long double);
extern long double log10l(long double);
extern long double log2l(long double);
extern long double log1pl(long double);
extern long double logbl(long double);
extern long double modfl(long double, long double *);
extern long double ldexpl(long double, int);
extern long double frexpl(long double, int *);
extern int ilogbl(long double);
extern long double scalbnl(long double, int);
extern long double scalblnl(long double, long int);
extern long double fabsl(long double);
extern long double cbrtl(long double);
extern long double hypotl(long double, long double);
extern long double powl(long double, long double);
extern long double sqrtl(long double);
extern long double erfl(long double);
extern long double erfcl(long double);






extern long double lgammal(long double);

extern long double tgammal(long double);
extern long double ceill(long double);
extern long double floorl(long double);
extern long double nearbyintl(long double);
extern long double rintl(long double);
extern long int lrintl(long double);
extern long double roundl(long double);
extern long int lroundl(long double);



    extern long long int llrintl(long double);
    extern long long int llroundl(long double);


extern long double truncl(long double);
extern long double fmodl(long double, long double);
extern long double remainderl(long double, long double);
extern long double remquol(long double, long double, int *);
extern long double copysignl(long double, long double);
extern long double nanl(const char *);
extern long double nextafterl(long double, long double);
extern double nexttoward(double, long double);
extern float nexttowardf(float, long double);
extern long double nexttowardl(long double, long double);
extern long double fdiml(long double, long double);
extern long double fmaxl(long double, long double);
extern long double fminl(long double, long double);
extern long double fmal(long double, long double, long double);
# 510 "/usr/include/architecture/i386/math.h" 3 4
extern double __inf( void );
extern float __inff( void );
extern long double __infl( void );
extern float __nan( void );


extern double j0 ( double );

extern double j1 ( double );

extern double jn ( int, double );

extern double y0 ( double );

extern double y1 ( double );

extern double yn ( int, double );

extern double scalb ( double, double );
# 546 "/usr/include/architecture/i386/math.h" 3 4
extern int signgam;
# 560 "/usr/include/architecture/i386/math.h" 3 4
extern long int rinttol ( double );

extern long int roundtol ( double );
# 571 "/usr/include/architecture/i386/math.h" 3 4
struct exception {
 int type;
 char *name;
 double arg1;
 double arg2;
 double retval;
};
# 600 "/usr/include/architecture/i386/math.h" 3 4
extern int finite ( double );

extern double gamma ( double );




extern int matherr ( struct exception * );





extern double significand ( double );




extern double drem ( double, double );
# 29 "/usr/include/math.h" 2 3 4
# 6599 "/usr/local/include/gambit.h" 2 3




typedef struct ___jmpbuf_struct
  {
    jmp_buf buf;
  } ___jmpbuf_struct;
# 6617 "/usr/local/include/gambit.h" 3
typedef void *___VARIANT;
# 6743 "/usr/local/include/gambit.h" 3
typedef long* ___symkey_struct[5+0];



typedef struct ___glo_struct
  {
    long val, prm, next;
  } ___glo_struct;



typedef struct ___processor_state_struct
  {
    long *stack_trip;
    long *stack_limit;
    long *fp;
    long *stack_start;
    long *stack_break;

    long *heap_limit;
    long *hp;

    long current_thread;
    long run_queue;

    long r[5];
    long pc;
    long temp1;
    long temp2;
    long temp3;
    long temp4;

    int na;
    int intr_enabled;
    int intr_flag[8];

    long glo_list_head;
    long glo_list_tail;

    long executable_wills;
    long nonexecutable_wills;

    void (*dummy8) (void);
    void (*dummy7) (void);
    void (*dummy6) (void);
    void (*dummy5) (void);
    void (*dummy4) (void);
    void (*dummy3) (void);
    void (*dummy2) (void);
    void (*dummy1) (void);


    ___jmpbuf_struct *catcher;
# 6811 "/usr/local/include/gambit.h" 3
  } ___processor_state_struct, *___processor_state;

typedef long (*___host) (___processor_state);

typedef struct ___label_struct
  {
    long header;
    long entry_or_descr;
    void* host_label;
    ___host host;
  } ___label_struct;



typedef struct ___module_struct
  {
    int version;
    int kind;
    char* name;
    int flags;
    long* *glo_tbl; int glo_count; int sup_count;
    char* *glo_names;
    long* *sym_tbl; int sym_count;
    char* *sym_names;
    long* *key_tbl; int key_count;
    char* *key_names;
    long *lp;
    ___label_struct *lbl_tbl; int lbl_count;
    long *ofd_tbl; int ofd_length;
    long *cns_tbl; int cns_count;
    long* *sub_tbl; int sub_count;
    long (*init_proc) (void);
    char* script_line;
    struct ___module_struct *next;

    void (*dummy8) (void);
    void (*dummy7) (void);
    void (*dummy6) (void);
    void (*dummy5) (void);
    void (*dummy4) (void);
    void (*dummy3) (void);
    void (*dummy2) (void);
    void (*dummy1) (void);
  } ___module_struct;



typedef struct ___linkfile_struct
  {
    int version;
    int kind;
    char* name;
    long* *sym_list;
    long* *key_list;
    void **linker_tbl;

    void (*dummy8) (void);
    void (*dummy7) (void);
    void (*dummy6) (void);
    void (*dummy5) (void);
    void (*dummy4) (void);
    void (*dummy3) (void);
    void (*dummy2) (void);
    void (*dummy1) (void);
  } ___linkfile_struct;



typedef union ___mod_or_lnk_union
  {
    ___module_struct module;
    ___linkfile_struct linkfile;
  } *___mod_or_lnk;



typedef struct ___program_startup_info_struct
  {
    unsigned short* *argv;
    unsigned short* script_line;
# 6900 "/usr/local/include/gambit.h" 3
  } ___program_startup_info_struct;



typedef struct ___global_state_struct
  {
    ___processor_state_struct pstate;

    double nb_gcs;
    double gc_user_time;
    double gc_sys_time;
    double gc_real_time;
    double bytes_allocated_minus_occupied;

    double last_gc_user_time;
    double last_gc_sys_time;
    double last_gc_real_time;
    double last_gc_heap_size;
    double last_gc_alloc;
    double last_gc_live;
    double last_gc_movable;
    double last_gc_nonmovable;

    long handler_sfun_conv_error;
    long handler_cfun_conv_error;
    long handler_stack_limit;
    long handler_heap_limit;
    long handler_not_proc;
    long handler_not_proc_glo;
    long handler_wrong_nargs;
    long handler_get_rest;
    long handler_get_key;
    long handler_get_key_rest;
    long handler_force;
    long handler_return_to_c;
    long handler_break;
    long internal_return;
    long dynamic_env_bind_return;

    long symbol_table;
    long keyword_table;
    long command_line;
    long program_descr;

    void (*dummy8) (void);
    void (*dummy7) (void);
    void (*dummy6) (void);
    void (*dummy5) (void);
    void (*dummy4) (void);
    void (*dummy3) (void);
    void (*dummy2) (void);
    void (*dummy1) (void);


    double (*fabs)
       (double x);

    double (*floor)
       (double x);

    double (*ceil)
       (double x);

    double (*exp)
       (double x);

    double (*log)
       (double x);

    double (*sin)
       (double x);

    double (*cos)
       (double x);

    double (*tan)
       (double x);

    double (*asin)
       (double x);

    double (*acos)
       (double x);

    double (*atan)
       (double x);
# 6999 "/usr/local/include/gambit.h" 3
    double (*sqrt)
       (double x);





    int (*setjmp)
       (jmp_buf env);





    int (*___iswalpha)
       (unsigned int x);

    int (*___iswdigit)
       (unsigned int x);

    int (*___iswspace)
       (unsigned int x);

    int (*___iswupper)
       (unsigned int x);

    int (*___iswlower)
       (unsigned int x);

    unsigned int (*___towupper)
       (unsigned int x);

    unsigned int (*___towlower)
       (unsigned int x);

    long (*___string_collate)
       (long s1, long s2);


    long (*___string_collate_ci)
       (long s1, long s2);


    double (*___copysign)
       (double x, double y);


    int (*___isfinite)
       (double x);

    int (*___isnan)
       (double x);

    double (*___trunc)
       (double x);

    double (*___round)
       (double x);


    double (*___atan2)
       (double y, double x);




    double (*___pow)
       (double x, double y);



    long (*___S64_from_SM32_fn)
       (int val);

    long (*___S64_from_SM32_UM32_fn)
       (int hi32, unsigned int lo32);


    long (*___S64_from_LONGLONG_fn)
       (long long val);

    long long (*___S64_to_LONGLONG_fn)
       (long val);

    int (*___S64_fits_in_width_fn)
       (long val, int width);


    unsigned long (*___U64_from_UM32_fn)
       (unsigned int val);

    unsigned long (*___U64_from_UM32_UM32_fn)
       (unsigned int hi32, unsigned int lo32);


    unsigned long (*___U64_from_ULONGLONG_fn)
       (unsigned long long val);

    unsigned long long (*___U64_to_ULONGLONG_fn)
       (unsigned long val);

    int (*___U64_fits_in_width_fn)
       (unsigned long val, int width);


    unsigned long (*___U64_mul_UM32_UM32_fn)
       (unsigned int x, unsigned int y);


    unsigned long (*___U64_add_U64_U64_fn)
       (unsigned long x, unsigned long y);


    long (*___SCMOBJ_to_S8)
       (long obj, signed char *x, int arg_num);



    long (*___SCMOBJ_to_U8)
       (long obj, unsigned char *x, int arg_num);



    long (*___SCMOBJ_to_S16)
       (long obj, short *x, int arg_num);



    long (*___SCMOBJ_to_U16)
       (long obj, unsigned short *x, int arg_num);



    long (*___SCMOBJ_to_S32)
       (long obj, int *x, int arg_num);



    long (*___SCMOBJ_to_U32)
       (long obj, unsigned int *x, int arg_num);



    long (*___SCMOBJ_to_S64)
       (long obj, long *x, int arg_num);



    long (*___SCMOBJ_to_U64)
       (long obj, unsigned long *x, int arg_num);



    long (*___SCMOBJ_to_F32)
       (long obj, float *x, int arg_num);



    long (*___SCMOBJ_to_F64)
       (long obj, double *x, int arg_num);



    long (*___SCMOBJ_to_CHAR)
       (long obj, char *x, int arg_num);



    long (*___SCMOBJ_to_SCHAR)
       (long obj, signed char *x, int arg_num);



    long (*___SCMOBJ_to_UCHAR)
       (long obj, unsigned char *x, int arg_num);



    long (*___SCMOBJ_to_ISO_8859_1)
       (long obj, unsigned char *x, int arg_num);



    long (*___SCMOBJ_to_UCS_2)
       (long obj, unsigned short *x, int arg_num);



    long (*___SCMOBJ_to_UCS_4)
       (long obj, unsigned int *x, int arg_num);



    long (*___SCMOBJ_to_WCHAR)
       (long obj, wchar_t *x, int arg_num);



    long (*___SCMOBJ_to_SHORT)
       (long obj, short *x, int arg_num);



    long (*___SCMOBJ_to_USHORT)
       (long obj, unsigned short *x, int arg_num);



    long (*___SCMOBJ_to_INT)
       (long obj, int *x, int arg_num);



    long (*___SCMOBJ_to_UINT)
       (long obj, unsigned int *x, int arg_num);



    long (*___SCMOBJ_to_LONG)
       (long obj, long *x, int arg_num);



    long (*___SCMOBJ_to_ULONG)
       (long obj, unsigned long *x, int arg_num);



    long (*___SCMOBJ_to_LONGLONG)
       (long obj, long long *x, int arg_num);



    long (*___SCMOBJ_to_ULONGLONG)
       (long obj, unsigned long long *x, int arg_num);



    long (*___SCMOBJ_to_FLOAT)
       (long obj, float *x, int arg_num);



    long (*___SCMOBJ_to_DOUBLE)
       (long obj, double *x, int arg_num);



    long (*___SCMOBJ_to_STRUCT)
       (long obj, void **x, long tags, int arg_num);




    long (*___SCMOBJ_to_UNION)
       (long obj, void **x, long tags, int arg_num);




    long (*___SCMOBJ_to_TYPE)
       (long obj, void **x, long tags, int arg_num);




    long (*___SCMOBJ_to_POINTER)
       (long obj, void **x, long tags, int arg_num);




    long (*___SCMOBJ_to_NONNULLPOINTER)
       (long obj, void **x, long tags, int arg_num);




    long (*___SCMOBJ_to_FUNCTION)
       (long obj, void *converter, void **x, int arg_num);




    long (*___SCMOBJ_to_NONNULLFUNCTION)
       (long obj, void *converter, void **x, int arg_num);




    long (*___SCMOBJ_to_BOOL)
       (long obj, int *x, int arg_num);



    long (*___SCMOBJ_to_STRING)
       (long obj, void **x, int arg_num, int char_encoding, int fudge);





    long (*___SCMOBJ_to_NONNULLSTRING)
       (long obj, void **x, int arg_num, int char_encoding, int fudge);





    long (*___SCMOBJ_to_NONNULLSTRINGLIST)
       (long obj, void **x, int arg_num, int char_encoding);




    long (*___SCMOBJ_to_CHARSTRING)
       (long obj, char **x, int arg_num);



    long (*___SCMOBJ_to_NONNULLCHARSTRING)
       (long obj, char **x, int arg_num);



    long (*___SCMOBJ_to_NONNULLCHARSTRINGLIST)
       (long obj, char ***x, int arg_num);



    long (*___SCMOBJ_to_ISO_8859_1STRING)
       (long obj, unsigned char* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLISO_8859_1STRING)
       (long obj, unsigned char* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLISO_8859_1STRINGLIST)
       (long obj, unsigned char* **x, int arg_num);



    long (*___SCMOBJ_to_UTF_8STRING)
       (long obj, char* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLUTF_8STRING)
       (long obj, char* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLUTF_8STRINGLIST)
       (long obj, char* **x, int arg_num);



    long (*___SCMOBJ_to_UTF_16STRING)
       (long obj, unsigned short* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLUTF_16STRING)
       (long obj, unsigned short* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLUTF_16STRINGLIST)
       (long obj, unsigned short* **x, int arg_num);



    long (*___SCMOBJ_to_UCS_2STRING)
       (long obj, unsigned short* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLUCS_2STRING)
       (long obj, unsigned short* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLUCS_2STRINGLIST)
       (long obj, unsigned short* **x, int arg_num);



    long (*___SCMOBJ_to_UCS_4STRING)
       (long obj, unsigned int* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLUCS_4STRING)
       (long obj, unsigned int* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLUCS_4STRINGLIST)
       (long obj, unsigned int* **x, int arg_num);



    long (*___SCMOBJ_to_WCHARSTRING)
       (long obj, wchar_t* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLWCHARSTRING)
       (long obj, wchar_t* *x, int arg_num);



    long (*___SCMOBJ_to_NONNULLWCHARSTRINGLIST)
       (long obj, wchar_t* **x, int arg_num);



    long (*___SCMOBJ_to_VARIANT)
       (long obj, ___VARIANT *x, int arg_num);



    long (*___release_foreign)
       (long obj);

    long (*___release_pointer)
       (void *x);

    long (*___release_function)
       (void *x);

    void (*___addref_function)
       (void *x);

    void (*___release_string)
       (void *x);

    void (*___addref_string)
       (void *x);

    void (*___release_string_list)
       (void *x);

    void (*___addref_string_list)
       (void *x);

    void (*___release_variant)
       (___VARIANT x);

    void (*___addref_variant)
       (___VARIANT x);

    long (*___S8_to_SCMOBJ)
       (signed char x, long *obj, int arg_num);



    long (*___U8_to_SCMOBJ)
       (unsigned char x, long *obj, int arg_num);



    long (*___S16_to_SCMOBJ)
       (short x, long *obj, int arg_num);



    long (*___U16_to_SCMOBJ)
       (unsigned short x, long *obj, int arg_num);



    long (*___S32_to_SCMOBJ)
       (int x, long *obj, int arg_num);



    long (*___U32_to_SCMOBJ)
       (unsigned int x, long *obj, int arg_num);



    long (*___S64_to_SCMOBJ)
       (long x, long *obj, int arg_num);



    long (*___U64_to_SCMOBJ)
       (unsigned long x, long *obj, int arg_num);



    long (*___F32_to_SCMOBJ)
       (float x, long *obj, int arg_num);



    long (*___F64_to_SCMOBJ)
       (double x, long *obj, int arg_num);



    long (*___CHAR_to_SCMOBJ)
       (char x, long *obj, int arg_num);



    long (*___SCHAR_to_SCMOBJ)
       (signed char x, long *obj, int arg_num);



    long (*___UCHAR_to_SCMOBJ)
       (unsigned char x, long *obj, int arg_num);



    long (*___ISO_8859_1_to_SCMOBJ)
       (unsigned char x, long *obj, int arg_num);



    long (*___UCS_2_to_SCMOBJ)
       (unsigned short x, long *obj, int arg_num);



    long (*___UCS_4_to_SCMOBJ)
       (unsigned int x, long *obj, int arg_num);



    long (*___WCHAR_to_SCMOBJ)
       (wchar_t x, long *obj, int arg_num);



    long (*___SHORT_to_SCMOBJ)
       (short x, long *obj, int arg_num);



    long (*___USHORT_to_SCMOBJ)
       (unsigned short x, long *obj, int arg_num);



    long (*___INT_to_SCMOBJ)
       (int x, long *obj, int arg_num);



    long (*___UINT_to_SCMOBJ)
       (unsigned int x, long *obj, int arg_num);



    long (*___LONG_to_SCMOBJ)
       (long x, long *obj, int arg_num);



    long (*___ULONG_to_SCMOBJ)
       (unsigned long x, long *obj, int arg_num);



    long (*___LONGLONG_to_SCMOBJ)
       (long long x, long *obj, int arg_num);



    long (*___ULONGLONG_to_SCMOBJ)
       (unsigned long long x, long *obj, int arg_num);



    long (*___FLOAT_to_SCMOBJ)
       (float x, long *obj, int arg_num);



    long (*___DOUBLE_to_SCMOBJ)
       (double x, long *obj, int arg_num);



    long (*___STRUCT_to_SCMOBJ)
       (void *x, long tags, long (*release_fn) (void *ptr), long *obj, int arg_num);





    long (*___UNION_to_SCMOBJ)
       (void *x, long tags, long (*release_fn) (void *ptr), long *obj, int arg_num);





    long (*___TYPE_to_SCMOBJ)
       (void *x, long tags, long (*release_fn) (void *ptr), long *obj, int arg_num);





    long (*___POINTER_to_SCMOBJ)
       (void *x, long tags, long (*release_fn) (void *ptr), long *obj, int arg_num);





    long (*___NONNULLPOINTER_to_SCMOBJ)
       (void *x, long tags, long (*release_fn) (void *ptr), long *obj, int arg_num);





    long (*___FUNCTION_to_SCMOBJ)
       (void *x, long *obj, int arg_num);



    long (*___NONNULLFUNCTION_to_SCMOBJ)
       (void *x, long *obj, int arg_num);



    long (*___BOOL_to_SCMOBJ)
       (int x, long *obj, int arg_num);



    long (*___STRING_to_SCMOBJ)
       (void *x, long *obj, int arg_num, int char_encoding);




    long (*___NONNULLSTRING_to_SCMOBJ)
       (void *x, long *obj, int arg_num, int char_encoding);




    long (*___NONNULLSTRINGLIST_to_SCMOBJ)
       (void *x, long *obj, int arg_num, int char_encoding);




    long (*___CHARSTRING_to_SCMOBJ)
       (char *x, long *obj, int arg_num);



    long (*___NONNULLCHARSTRING_to_SCMOBJ)
       (char *x, long *obj, int arg_num);



    long (*___NONNULLCHARSTRINGLIST_to_SCMOBJ)
       (char **x, long *obj, int arg_num);



    long (*___ISO_8859_1STRING_to_SCMOBJ)
       (unsigned char* x, long *obj, int arg_num);



    long (*___NONNULLISO_8859_1STRING_to_SCMOBJ)
       (unsigned char* x, long *obj, int arg_num);



    long (*___NONNULLISO_8859_1STRINGLIST_to_SCMOBJ)
       (unsigned char* *x, long *obj, int arg_num);



    long (*___UTF_8STRING_to_SCMOBJ)
       (char* x, long *obj, int arg_num);



    long (*___NONNULLUTF_8STRING_to_SCMOBJ)
       (char* x, long *obj, int arg_num);



    long (*___NONNULLUTF_8STRINGLIST_to_SCMOBJ)
       (char* *x, long *obj, int arg_num);



    long (*___UTF_16STRING_to_SCMOBJ)
       (unsigned short* x, long *obj, int arg_num);



    long (*___NONNULLUTF_16STRING_to_SCMOBJ)
       (unsigned short* x, long *obj, int arg_num);



    long (*___NONNULLUTF_16STRINGLIST_to_SCMOBJ)
       (unsigned short* *x, long *obj, int arg_num);



    long (*___UCS_2STRING_to_SCMOBJ)
       (unsigned short* x, long *obj, int arg_num);



    long (*___NONNULLUCS_2STRING_to_SCMOBJ)
       (unsigned short* x, long *obj, int arg_num);



    long (*___NONNULLUCS_2STRINGLIST_to_SCMOBJ)
       (unsigned short* *x, long *obj, int arg_num);



    long (*___UCS_4STRING_to_SCMOBJ)
       (unsigned int* x, long *obj, int arg_num);



    long (*___NONNULLUCS_4STRING_to_SCMOBJ)
       (unsigned int* x, long *obj, int arg_num);



    long (*___NONNULLUCS_4STRINGLIST_to_SCMOBJ)
       (unsigned int* *x, long *obj, int arg_num);



    long (*___WCHARSTRING_to_SCMOBJ)
       (wchar_t* x, long *obj, int arg_num);



    long (*___NONNULLWCHARSTRING_to_SCMOBJ)
       (wchar_t* x, long *obj, int arg_num);



    long (*___NONNULLWCHARSTRINGLIST_to_SCMOBJ)
       (wchar_t* *x, long *obj, int arg_num);



    long (*___VARIANT_to_SCMOBJ)
       (___VARIANT x, long *obj, int arg_num);



    long (*___CHARSTRING_to_UCS_2STRING)
       (char *str_char, unsigned short* *str_UCS_2);


    void (*___free_UCS_2STRING)
       (unsigned short* str_UCS_2);

    long (*___NONNULLCHARSTRINGLIST_to_NONNULLUCS_2STRINGLIST)
       (char **str_list_char, unsigned short* **str_list_UCS_2);


    void (*___free_NONNULLUCS_2STRINGLIST)
       (unsigned short* *str_list_UCS_2);

    long (*___make_sfun_stack_marker)
       (long *marker, long proc_or_false);


    void (*___kill_sfun_stack_marker)
       (long marker);

    void *(*___alloc_rc)
       (unsigned long bytes);

    void (*___release_rc)
       (void *ptr);

    void (*___addref_rc)
       (void *ptr);

    long (*___data_rc)
       (void *ptr);

    void (*___set_data_rc)
       (void *ptr, long val);


    long (*___alloc_scmobj)
       (int subtype, long bytes, int kind);



    void (*___release_scmobj)
       (long obj);

    long (*___make_pair)
       (long car, long cdr, int kind);



    long (*___make_vector)
       (long length, long init, int kind);



    void (*___still_obj_refcount_inc)
       (long obj);

    void (*___still_obj_refcount_dec)
       (long obj);

    long (*___gc_hash_table_ref)
       (long ht, long key);


    long (*___gc_hash_table_set)
       (long ht, long key, long val);



    long (*___gc_hash_table_rehash)
       (long ht_src, long ht_dst);


    unsigned long (*___get_min_heap) (void);
    void (*___set_min_heap)
       (unsigned long bytes);

    unsigned long (*___get_max_heap) (void);
    void (*___set_max_heap)
       (unsigned long bytes);

    int (*___get_live_percent) (void);
    void (*___set_live_percent)
       (int percent);

    int (*___get_standard_level) (void);
    void (*___set_standard_level)
       (int level);

    int (*___set_debug_settings)
       (int mask, int new_settings);


    ___program_startup_info_struct *(*___get_program_startup_info) (void);
    void (*___cleanup) (void);
    void (*___cleanup_and_exit_process)
       (int status);

    long (*___call)
       (int nargs, long proc, long marker);



    void (*___propagate_error)
       (long err);







    void (*___raise_interrupt)
       (int code);

    void (*___begin_interrupt_service) (void);
    int (*___check_interrupt)
       (int code);

    void (*___end_interrupt_service)
       (int code);

    void (*___disable_interrupts) (void);
    void (*___enable_interrupts) (void);
    void *(*___alloc_mem)
       (unsigned long bytes);

    void (*___free_mem)
       (void *ptr);


  } ___global_state_struct;


static long ___lp;








typedef struct ___setup_params_struct
  {
    int version;
    unsigned short* *argv;
    unsigned long min_heap;
    unsigned long max_heap;
    int live_percent;
    long (*gc_hook) (long avail, long live);
    void (*display_error) (char **msgs);
    void (*fatal_error) (char **msgs);
    int standard_level;
    int debug_settings;
    int file_settings;
    int terminal_settings;
    int stdio_settings;
    unsigned short* gambcdir;
    unsigned short* *gambcdir_map;
    unsigned short* remote_dbg_addr;
    unsigned short* rpc_server_addr;
    ___mod_or_lnk (*linker) (___global_state_struct*);

    void (*dummy8) (void);
    void (*dummy7) (void);
    void (*dummy6) (void);
    void (*dummy5) (void);
    void (*dummy4) (void);
    void (*dummy3) (void);
    void (*dummy2) (void);
    void (*dummy1) (void);
  } ___setup_params_struct;
# 8080 "/usr/local/include/gambit.h" 3
extern long ___S64_from_SM32_fn
   (int val);

extern long ___S64_from_SM32_UM32_fn
   (int hi32, unsigned int lo32);


extern long ___S64_from_LONGLONG_fn
   (long long val);

extern long long ___S64_to_LONGLONG_fn
   (long val);

extern int ___S64_fits_in_width_fn
   (long val, int width);


extern unsigned long ___U64_from_UM32_fn
   (unsigned int val);

extern unsigned long ___U64_from_UM32_UM32_fn
   (unsigned int hi32, unsigned int lo32);


extern unsigned long ___U64_from_ULONGLONG_fn
   (unsigned long long val);

extern unsigned long long ___U64_to_ULONGLONG_fn
   (unsigned long val);

extern int ___U64_fits_in_width_fn
   (unsigned long val, int width);


extern unsigned long ___U64_mul_UM32_UM32_fn
   (unsigned int x, unsigned int y);


extern unsigned long ___U64_add_U64_U64_fn
   (unsigned long x, unsigned long y);


extern long ___SCMOBJ_to_S8
   (long obj, signed char *x, int arg_num);



extern long ___SCMOBJ_to_U8
   (long obj, unsigned char *x, int arg_num);



extern long ___SCMOBJ_to_S16
   (long obj, short *x, int arg_num);



extern long ___SCMOBJ_to_U16
   (long obj, unsigned short *x, int arg_num);



extern long ___SCMOBJ_to_S32
   (long obj, int *x, int arg_num);



extern long ___SCMOBJ_to_U32
   (long obj, unsigned int *x, int arg_num);



extern long ___SCMOBJ_to_S64
   (long obj, long *x, int arg_num);



extern long ___SCMOBJ_to_U64
   (long obj, unsigned long *x, int arg_num);



extern long ___SCMOBJ_to_F32
   (long obj, float *x, int arg_num);



extern long ___SCMOBJ_to_F64
   (long obj, double *x, int arg_num);



extern long ___SCMOBJ_to_CHAR
   (long obj, char *x, int arg_num);



extern long ___SCMOBJ_to_SCHAR
   (long obj, signed char *x, int arg_num);



extern long ___SCMOBJ_to_UCHAR
   (long obj, unsigned char *x, int arg_num);



extern long ___SCMOBJ_to_ISO_8859_1
   (long obj, unsigned char *x, int arg_num);



extern long ___SCMOBJ_to_UCS_2
   (long obj, unsigned short *x, int arg_num);



extern long ___SCMOBJ_to_UCS_4
   (long obj, unsigned int *x, int arg_num);



extern long ___SCMOBJ_to_WCHAR
   (long obj, wchar_t *x, int arg_num);



extern long ___SCMOBJ_to_SHORT
   (long obj, short *x, int arg_num);



extern long ___SCMOBJ_to_USHORT
   (long obj, unsigned short *x, int arg_num);



extern long ___SCMOBJ_to_INT
   (long obj, int *x, int arg_num);



extern long ___SCMOBJ_to_UINT
   (long obj, unsigned int *x, int arg_num);



extern long ___SCMOBJ_to_LONG
   (long obj, long *x, int arg_num);



extern long ___SCMOBJ_to_ULONG
   (long obj, unsigned long *x, int arg_num);



extern long ___SCMOBJ_to_LONGLONG
   (long obj, long long *x, int arg_num);



extern long ___SCMOBJ_to_ULONGLONG
   (long obj, unsigned long long *x, int arg_num);



extern long ___SCMOBJ_to_FLOAT
   (long obj, float *x, int arg_num);



extern long ___SCMOBJ_to_DOUBLE
   (long obj, double *x, int arg_num);



extern long ___SCMOBJ_to_STRUCT
   (long obj, void **x, long tags, int arg_num);




extern long ___SCMOBJ_to_UNION
   (long obj, void **x, long tags, int arg_num);




extern long ___SCMOBJ_to_TYPE
   (long obj, void **x, long tags, int arg_num);




extern long ___SCMOBJ_to_POINTER
   (long obj, void **x, long tags, int arg_num);




extern long ___SCMOBJ_to_NONNULLPOINTER
   (long obj, void **x, long tags, int arg_num);




extern long ___SCMOBJ_to_FUNCTION
   (long obj, void *converter, void **x, int arg_num);




extern long ___SCMOBJ_to_NONNULLFUNCTION
   (long obj, void *converter, void **x, int arg_num);




extern long ___SCMOBJ_to_BOOL
   (long obj, int *x, int arg_num);



extern long ___SCMOBJ_to_STRING
   (long obj, void **x, int arg_num, int char_encoding, int fudge);





extern long ___SCMOBJ_to_NONNULLSTRING
   (long obj, void **x, int arg_num, int char_encoding, int fudge);





extern long ___SCMOBJ_to_NONNULLSTRINGLIST
   (long obj, void **x, int arg_num, int char_encoding);




extern long ___SCMOBJ_to_CHARSTRING
   (long obj, char **x, int arg_num);



extern long ___SCMOBJ_to_NONNULLCHARSTRING
   (long obj, char **x, int arg_num);



extern long ___SCMOBJ_to_NONNULLCHARSTRINGLIST
   (long obj, char ***x, int arg_num);



extern long ___SCMOBJ_to_ISO_8859_1STRING
   (long obj, unsigned char* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLISO_8859_1STRING
   (long obj, unsigned char* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLISO_8859_1STRINGLIST
   (long obj, unsigned char* **x, int arg_num);



extern long ___SCMOBJ_to_UTF_8STRING
   (long obj, char* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLUTF_8STRING
   (long obj, char* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLUTF_8STRINGLIST
   (long obj, char* **x, int arg_num);



extern long ___SCMOBJ_to_UTF_16STRING
   (long obj, unsigned short* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLUTF_16STRING
   (long obj, unsigned short* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLUTF_16STRINGLIST
   (long obj, unsigned short* **x, int arg_num);



extern long ___SCMOBJ_to_UCS_2STRING
   (long obj, unsigned short* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLUCS_2STRING
   (long obj, unsigned short* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLUCS_2STRINGLIST
   (long obj, unsigned short* **x, int arg_num);



extern long ___SCMOBJ_to_UCS_4STRING
   (long obj, unsigned int* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLUCS_4STRING
   (long obj, unsigned int* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLUCS_4STRINGLIST
   (long obj, unsigned int* **x, int arg_num);



extern long ___SCMOBJ_to_WCHARSTRING
   (long obj, wchar_t* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLWCHARSTRING
   (long obj, wchar_t* *x, int arg_num);



extern long ___SCMOBJ_to_NONNULLWCHARSTRINGLIST
   (long obj, wchar_t* **x, int arg_num);



extern long ___SCMOBJ_to_VARIANT
   (long obj, ___VARIANT *x, int arg_num);



extern long ___release_foreign
   (long obj);

extern long ___release_pointer
   (void *x);

extern long ___release_function
   (void *x);

extern void ___addref_function
   (void *x);

extern void ___release_string
   (void *x);

extern void ___addref_string
   (void *x);

extern void ___release_string_list
   (void *x);

extern void ___addref_string_list
   (void *x);

extern void ___release_variant
   (___VARIANT x);

extern void ___addref_variant
   (___VARIANT x);

extern long ___S8_to_SCMOBJ
   (signed char x, long *obj, int arg_num);



extern long ___U8_to_SCMOBJ
   (unsigned char x, long *obj, int arg_num);



extern long ___S16_to_SCMOBJ
   (short x, long *obj, int arg_num);



extern long ___U16_to_SCMOBJ
   (unsigned short x, long *obj, int arg_num);



extern long ___S32_to_SCMOBJ
   (int x, long *obj, int arg_num);



extern long ___U32_to_SCMOBJ
   (unsigned int x, long *obj, int arg_num);



extern long ___S64_to_SCMOBJ
   (long x, long *obj, int arg_num);



extern long ___U64_to_SCMOBJ
   (unsigned long x, long *obj, int arg_num);



extern long ___F32_to_SCMOBJ
   (float x, long *obj, int arg_num);



extern long ___F64_to_SCMOBJ
   (double x, long *obj, int arg_num);



extern long ___CHAR_to_SCMOBJ
   (char x, long *obj, int arg_num);



extern long ___SCHAR_to_SCMOBJ
   (signed char x, long *obj, int arg_num);



extern long ___UCHAR_to_SCMOBJ
   (unsigned char x, long *obj, int arg_num);



extern long ___ISO_8859_1_to_SCMOBJ
   (unsigned char x, long *obj, int arg_num);



extern long ___UCS_2_to_SCMOBJ
   (unsigned short x, long *obj, int arg_num);



extern long ___UCS_4_to_SCMOBJ
   (unsigned int x, long *obj, int arg_num);



extern long ___WCHAR_to_SCMOBJ
   (wchar_t x, long *obj, int arg_num);



extern long ___SHORT_to_SCMOBJ
   (short x, long *obj, int arg_num);



extern long ___USHORT_to_SCMOBJ
   (unsigned short x, long *obj, int arg_num);



extern long ___INT_to_SCMOBJ
   (int x, long *obj, int arg_num);



extern long ___UINT_to_SCMOBJ
   (unsigned int x, long *obj, int arg_num);



extern long ___LONG_to_SCMOBJ
   (long x, long *obj, int arg_num);



extern long ___ULONG_to_SCMOBJ
   (unsigned long x, long *obj, int arg_num);



extern long ___LONGLONG_to_SCMOBJ
   (long long x, long *obj, int arg_num);



extern long ___ULONGLONG_to_SCMOBJ
   (unsigned long long x, long *obj, int arg_num);



extern long ___FLOAT_to_SCMOBJ
   (float x, long *obj, int arg_num);



extern long ___DOUBLE_to_SCMOBJ
   (double x, long *obj, int arg_num);



extern long ___STRUCT_to_SCMOBJ
   (void *x, long tags, long (*release_fn) (void *ptr), long *obj, int arg_num);





extern long ___UNION_to_SCMOBJ
   (void *x, long tags, long (*release_fn) (void *ptr), long *obj, int arg_num);





extern long ___TYPE_to_SCMOBJ
   (void *x, long tags, long (*release_fn) (void *ptr), long *obj, int arg_num);





extern long ___POINTER_to_SCMOBJ
   (void *x, long tags, long (*release_fn) (void *ptr), long *obj, int arg_num);





extern long ___NONNULLPOINTER_to_SCMOBJ
   (void *x, long tags, long (*release_fn) (void *ptr), long *obj, int arg_num);





extern long ___FUNCTION_to_SCMOBJ
   (void *x, long *obj, int arg_num);



extern long ___NONNULLFUNCTION_to_SCMOBJ
   (void *x, long *obj, int arg_num);



extern long ___BOOL_to_SCMOBJ
   (int x, long *obj, int arg_num);



extern long ___STRING_to_SCMOBJ
   (void *x, long *obj, int arg_num, int char_encoding);




extern long ___NONNULLSTRING_to_SCMOBJ
   (void *x, long *obj, int arg_num, int char_encoding);




extern long ___NONNULLSTRINGLIST_to_SCMOBJ
   (void *x, long *obj, int arg_num, int char_encoding);




extern long ___CHARSTRING_to_SCMOBJ
   (char *x, long *obj, int arg_num);



extern long ___NONNULLCHARSTRING_to_SCMOBJ
   (char *x, long *obj, int arg_num);



extern long ___NONNULLCHARSTRINGLIST_to_SCMOBJ
   (char **x, long *obj, int arg_num);



extern long ___ISO_8859_1STRING_to_SCMOBJ
   (unsigned char* x, long *obj, int arg_num);



extern long ___NONNULLISO_8859_1STRING_to_SCMOBJ
   (unsigned char* x, long *obj, int arg_num);



extern long ___NONNULLISO_8859_1STRINGLIST_to_SCMOBJ
   (unsigned char* *x, long *obj, int arg_num);



extern long ___UTF_8STRING_to_SCMOBJ
   (char* x, long *obj, int arg_num);



extern long ___NONNULLUTF_8STRING_to_SCMOBJ
   (char* x, long *obj, int arg_num);



extern long ___NONNULLUTF_8STRINGLIST_to_SCMOBJ
   (char* *x, long *obj, int arg_num);



extern long ___UTF_16STRING_to_SCMOBJ
   (unsigned short* x, long *obj, int arg_num);



extern long ___NONNULLUTF_16STRING_to_SCMOBJ
   (unsigned short* x, long *obj, int arg_num);



extern long ___NONNULLUTF_16STRINGLIST_to_SCMOBJ
   (unsigned short* *x, long *obj, int arg_num);



extern long ___UCS_2STRING_to_SCMOBJ
   (unsigned short* x, long *obj, int arg_num);



extern long ___NONNULLUCS_2STRING_to_SCMOBJ
   (unsigned short* x, long *obj, int arg_num);



extern long ___NONNULLUCS_2STRINGLIST_to_SCMOBJ
   (unsigned short* *x, long *obj, int arg_num);



extern long ___UCS_4STRING_to_SCMOBJ
   (unsigned int* x, long *obj, int arg_num);



extern long ___NONNULLUCS_4STRING_to_SCMOBJ
   (unsigned int* x, long *obj, int arg_num);



extern long ___NONNULLUCS_4STRINGLIST_to_SCMOBJ
   (unsigned int* *x, long *obj, int arg_num);



extern long ___WCHARSTRING_to_SCMOBJ
   (wchar_t* x, long *obj, int arg_num);



extern long ___NONNULLWCHARSTRING_to_SCMOBJ
   (wchar_t* x, long *obj, int arg_num);



extern long ___NONNULLWCHARSTRINGLIST_to_SCMOBJ
   (wchar_t* *x, long *obj, int arg_num);



extern long ___VARIANT_to_SCMOBJ
   (___VARIANT x, long *obj, int arg_num);



extern long ___CHARSTRING_to_UCS_2STRING
   (char *str_char, unsigned short* *str_UCS_2);


extern void ___free_UCS_2STRING
   (unsigned short* str_UCS_2);

extern long ___NONNULLCHARSTRINGLIST_to_NONNULLUCS_2STRINGLIST
   (char **str_list_char, unsigned short* **str_list_UCS_2);


extern void ___free_NONNULLUCS_2STRINGLIST
   (unsigned short* *str_list_UCS_2);

extern long ___make_sfun_stack_marker
   (long *marker, long proc_or_false);


extern void ___kill_sfun_stack_marker
   (long marker);




extern void* ___alloc_rc
   (unsigned long bytes);

extern void ___release_rc
   (void *ptr);

extern void ___addref_rc
   (void *ptr);

extern long ___data_rc
   (void *ptr);

extern void ___set_data_rc
   (void *ptr, long val);


extern long ___alloc_scmobj
   (int subtype, long bytes, int kind);



extern void ___release_scmobj
   (long obj);

extern long ___make_pair
   (long car, long cdr, int kind);



extern long ___make_vector
   (long length, long init, int kind);



extern void ___still_obj_refcount_inc
   (long obj);

extern void ___still_obj_refcount_dec
   (long obj);

extern long ___gc_hash_table_ref
   (long ht, long key);


extern long ___gc_hash_table_set
   (long ht, long key, long val);



extern long ___gc_hash_table_rehash
   (long ht_src, long ht_dst);





extern ___global_state_struct ___gstate;
extern int ___iswalpha
   (unsigned int x);

extern int ___iswdigit
   (unsigned int x);

extern int ___iswspace
   (unsigned int x);

extern int ___iswupper
   (unsigned int x);

extern int ___iswlower
   (unsigned int x);

extern unsigned int ___towupper
   (unsigned int x);

extern unsigned int ___towlower
   (unsigned int x);

extern long ___string_collate
   (long s1, long s2);


extern long ___string_collate_ci
   (long s1, long s2);


extern double ___copysign
   (double x, double y);


extern int ___isfinite
   (double x);

extern int ___isnan
   (double x);

extern double ___trunc
   (double x);

extern double ___round
   (double x);


extern double ___atan2
   (double y, double x);




extern double ___pow
   (double x, double y);



extern void ___setup_params_reset
   (struct ___setup_params_struct *setup_params);

extern long ___setup
   (struct ___setup_params_struct *setup_params);

extern unsigned long ___get_min_heap (void);
extern void ___set_min_heap
   (unsigned long bytes);

extern unsigned long ___get_max_heap (void);
extern void ___set_max_heap
   (unsigned long bytes);

extern int ___get_live_percent (void);
extern void ___set_live_percent
   (int percent);

extern int ___get_standard_level (void);
extern void ___set_standard_level
   (int level);

extern int ___set_debug_settings
   (int mask, int new_settings);


extern ___program_startup_info_struct* ___get_program_startup_info
   (void);
extern void ___cleanup (void);
extern void ___cleanup_and_exit_process
   (int status);

extern long ___call
   (int nargs, long proc, long marker);



extern void ___propagate_error
   (long err);







extern void ___raise_interrupt
   (int code);

extern void ___begin_interrupt_service (void);
extern int ___check_interrupt
    (int code);

extern void ___end_interrupt_service
   (int code);

extern void ___disable_interrupts (void);
extern void ___enable_interrupts (void);



extern void * ___alloc_mem
   (unsigned long bytes);

extern void ___free_mem
   (void *ptr);
# 47 "ftgl.c" 2

extern ___symkey_struct ___S_FTGLFont_2a_;

extern ___glo_struct ___G__20_ftgl;
extern ___glo_struct ___G__20_ftgl_23_0;
extern ___glo_struct ___G__20_ftgl_23_1;
extern ___glo_struct ___G__20_ftgl_23_2;
extern ___glo_struct ___G__20_ftgl_23_3;
extern ___glo_struct ___G__20_ftgl_23_4;
extern ___glo_struct ___G__20_ftgl_23_5;
extern ___glo_struct ___G_FTGL__RENDER__ALL;
extern ___glo_struct ___G_car;
extern ___glo_struct ___G_ftglCreateBufferFont;
extern ___glo_struct ___G_ftglCreateTextureFont;
extern ___glo_struct ___G_ftglGetFontError;
extern ___glo_struct ___G_ftglGetFontFaceSize;
extern ___glo_struct ___G_ftglRenderFont;
extern ___glo_struct ___G_ftglSetFontFaceSize;

static long* ___sym_tbl[1];





















static long ___cns_tbl[]={
 ((((2<<3))<<(3 +5))+((1)<<3)+(6)),((((long)(-3))<<2)+2),((((long)(-1-0))<<2)+3)
};




# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 1
# 32 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h"
typedef double FTGL_DOUBLE;
typedef float FTGL_FLOAT;




# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/ft2build.h" 1
# 56 "/usr/local/iphone/iPhoneSimulator3.1.2/include/ft2build.h"
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftheader.h" 1
# 57 "/usr/local/iphone/iPhoneSimulator3.1.2/include/ft2build.h" 2
# 39 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h" 1
# 33 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftconfig.h" 1
# 42 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftconfig.h"
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftoption.h" 1
# 26 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftoption.h"

# 702 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftoption.h"

# 43 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftconfig.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftstdlib.h" 1
# 36 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftstdlib.h"
# 1 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/stddef.h" 1 3 4
# 152 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/stddef.h" 3 4
typedef long int ptrdiff_t;
# 37 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftstdlib.h" 2
# 60 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftstdlib.h"
# 1 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/limits.h" 1 3 4






# 1 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/syslimits.h" 1 3 4
# 8 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/limits.h" 2 3 4
# 61 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftstdlib.h" 2
# 76 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftstdlib.h"
# 1 "/usr/include/string.h" 1 3 4
# 70 "/usr/include/string.h" 3 4
typedef __darwin_ssize_t ssize_t;
# 80 "/usr/include/string.h" 3 4

void *memchr(const void *, int, size_t);
int memcmp(const void *, const void *, size_t);
void *memcpy(void *, const void *, size_t);
void *memmove(void *, const void *, size_t);
void *memset(void *, int, size_t);

char *stpcpy(char *, const char *);
char *strcasestr(const char *, const char *);

char *strcat(char *, const char *);
char *strchr(const char *, int);
int strcmp(const char *, const char *);
int strcoll(const char *, const char *);
char *strcpy(char *, const char *);
size_t strcspn(const char *, const char *);
char *strerror(int) __asm("_" "strerror" );
int strerror_r(int, char *, size_t);
size_t strlen(const char *);
char *strncat(char *, const char *, size_t);
int strncmp(const char *, const char *, size_t);
char *strncpy(char *, const char *, size_t);

char *strnstr(const char *, const char *, size_t);

char *strpbrk(const char *, const char *);
char *strrchr(const char *, int);
size_t strspn(const char *, const char *);
char *strstr(const char *, const char *);
char *strtok(char *, const char *);
size_t strxfrm(char *, const char *, size_t);



void *memccpy(void *, const void *, int, size_t);
char *strtok_r(char *, const char *, char **);
char *strdup(const char *);

int bcmp(const void *, const void *, size_t);
void bcopy(const void *, void *, size_t);
void bzero(void *, size_t);
int ffs(int);
int ffsl(long);
int fls(int);
int flsl(long);
char *index(const char *, int);
void memset_pattern4(void *, const void *, size_t);
void memset_pattern8(void *, const void *, size_t);
void memset_pattern16(void *, const void *, size_t);
char *rindex(const char *, int);
int strcasecmp(const char *, const char *);
size_t strlcat(char *, const char *, size_t);
size_t strlcpy(char *, const char *, size_t);
void strmode(int, char *);
int strncasecmp(const char *, const char *, size_t);
char *strsep(char **, const char *);
char *strsignal(int sig);
void swab(const void * , void * , ssize_t);










# 1 "/usr/include/secure/_string.h" 1 3 4
# 55 "/usr/include/secure/_string.h" 3 4
static __inline void *
__inline_memcpy_chk (void *__dest, const void *__src, size_t __len)
{
  return __builtin___memcpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}






static __inline void *
__inline_memmove_chk (void *__dest, const void *__src, size_t __len)
{
  return __builtin___memmove_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}






static __inline void *
__inline_memset_chk (void *__dest, int __val, size_t __len)
{
  return __builtin___memset_chk (__dest, __val, __len, __builtin_object_size (__dest, 0));
}






static __inline char *
__inline_strcpy_chk (char * __dest, const char * __src)
{
  return __builtin___strcpy_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}







static __inline char *
__inline_stpcpy_chk (char *__dest, const char *__src)
{
  return __builtin___stpcpy_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}







static __inline char *
__inline_strncpy_chk (char * __dest, const char * __src,
        size_t __len)
{
  return __builtin___strncpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 2 > 1));
}






static __inline char *
__inline_strcat_chk (char * __dest, const char * __src)
{
  return __builtin___strcat_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}






static __inline char *
__inline_strncat_chk (char * __dest, const char * __src,
        size_t __len)
{
  return __builtin___strncat_chk (__dest, __src, __len, __builtin_object_size (__dest, 2 > 1));
}
# 149 "/usr/include/string.h" 2 3 4
# 77 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftstdlib.h" 2
# 118 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftstdlib.h"
# 1 "/usr/include/stdlib.h" 1 3 4
# 61 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/Availability.h" 1 3 4
# 126 "/usr/include/Availability.h" 3 4
# 1 "/usr/include/AvailabilityInternal.h" 1 3 4
# 127 "/usr/include/Availability.h" 2 3 4
# 62 "/usr/include/stdlib.h" 2 3 4



# 1 "/usr/include/sys/wait.h" 1 3 4
# 79 "/usr/include/sys/wait.h" 3 4
typedef enum {
 P_ALL,
 P_PID,
 P_PGID
} idtype_t;






typedef __darwin_pid_t pid_t;




typedef __darwin_id_t id_t;
# 116 "/usr/include/sys/wait.h" 3 4
# 1 "/usr/include/sys/signal.h" 1 3 4
# 73 "/usr/include/sys/signal.h" 3 4
# 1 "/usr/include/sys/appleapiopts.h" 1 3 4
# 74 "/usr/include/sys/signal.h" 2 3 4







# 1 "/usr/include/machine/signal.h" 1 3 4
# 34 "/usr/include/machine/signal.h" 3 4
# 1 "/usr/include/i386/signal.h" 1 3 4
# 39 "/usr/include/i386/signal.h" 3 4
typedef int sig_atomic_t;
# 55 "/usr/include/i386/signal.h" 3 4
# 1 "/usr/include/i386/_structs.h" 1 3 4
# 56 "/usr/include/i386/signal.h" 2 3 4
# 35 "/usr/include/machine/signal.h" 2 3 4
# 82 "/usr/include/sys/signal.h" 2 3 4
# 154 "/usr/include/sys/signal.h" 3 4
# 1 "/usr/include/sys/_structs.h" 1 3 4
# 57 "/usr/include/sys/_structs.h" 3 4
# 1 "/usr/include/machine/_structs.h" 1 3 4
# 31 "/usr/include/machine/_structs.h" 3 4
# 1 "/usr/include/i386/_structs.h" 1 3 4
# 38 "/usr/include/i386/_structs.h" 3 4
# 1 "/usr/include/mach/i386/_structs.h" 1 3 4
# 43 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_i386_thread_state
{
    unsigned int __eax;
    unsigned int __ebx;
    unsigned int __ecx;
    unsigned int __edx;
    unsigned int __edi;
    unsigned int __esi;
    unsigned int __ebp;
    unsigned int __esp;
    unsigned int __ss;
    unsigned int __eflags;
    unsigned int __eip;
    unsigned int __cs;
    unsigned int __ds;
    unsigned int __es;
    unsigned int __fs;
    unsigned int __gs;
};
# 89 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_fp_control
{
    unsigned short __invalid :1,
        __denorm :1,
    __zdiv :1,
    __ovrfl :1,
    __undfl :1,
    __precis :1,
      :2,
    __pc :2,





    __rc :2,






             :1,
      :3;
};
typedef struct __darwin_fp_control __darwin_fp_control_t;
# 147 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_fp_status
{
    unsigned short __invalid :1,
        __denorm :1,
    __zdiv :1,
    __ovrfl :1,
    __undfl :1,
    __precis :1,
    __stkflt :1,
    __errsumm :1,
    __c0 :1,
    __c1 :1,
    __c2 :1,
    __tos :3,
    __c3 :1,
    __busy :1;
};
typedef struct __darwin_fp_status __darwin_fp_status_t;
# 191 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_mmst_reg
{
 char __mmst_reg[10];
 char __mmst_rsrv[6];
};
# 210 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_xmm_reg
{
 char __xmm_reg[16];
};
# 232 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_i386_float_state
{
 int __fpu_reserved[2];
 struct __darwin_fp_control __fpu_fcw;
 struct __darwin_fp_status __fpu_fsw;
 __uint8_t __fpu_ftw;
 __uint8_t __fpu_rsrv1;
 __uint16_t __fpu_fop;
 __uint32_t __fpu_ip;
 __uint16_t __fpu_cs;
 __uint16_t __fpu_rsrv2;
 __uint32_t __fpu_dp;
 __uint16_t __fpu_ds;
 __uint16_t __fpu_rsrv3;
 __uint32_t __fpu_mxcsr;
 __uint32_t __fpu_mxcsrmask;
 struct __darwin_mmst_reg __fpu_stmm0;
 struct __darwin_mmst_reg __fpu_stmm1;
 struct __darwin_mmst_reg __fpu_stmm2;
 struct __darwin_mmst_reg __fpu_stmm3;
 struct __darwin_mmst_reg __fpu_stmm4;
 struct __darwin_mmst_reg __fpu_stmm5;
 struct __darwin_mmst_reg __fpu_stmm6;
 struct __darwin_mmst_reg __fpu_stmm7;
 struct __darwin_xmm_reg __fpu_xmm0;
 struct __darwin_xmm_reg __fpu_xmm1;
 struct __darwin_xmm_reg __fpu_xmm2;
 struct __darwin_xmm_reg __fpu_xmm3;
 struct __darwin_xmm_reg __fpu_xmm4;
 struct __darwin_xmm_reg __fpu_xmm5;
 struct __darwin_xmm_reg __fpu_xmm6;
 struct __darwin_xmm_reg __fpu_xmm7;
 char __fpu_rsrv4[14*16];
 int __fpu_reserved1;
};
# 308 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_i386_exception_state
{
    unsigned int __trapno;
    unsigned int __err;
    unsigned int __faultvaddr;
};
# 326 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_x86_debug_state32
{
 unsigned int __dr0;
 unsigned int __dr1;
 unsigned int __dr2;
 unsigned int __dr3;
 unsigned int __dr4;
 unsigned int __dr5;
 unsigned int __dr6;
 unsigned int __dr7;
};
# 358 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_x86_thread_state64
{
 __uint64_t __rax;
 __uint64_t __rbx;
 __uint64_t __rcx;
 __uint64_t __rdx;
 __uint64_t __rdi;
 __uint64_t __rsi;
 __uint64_t __rbp;
 __uint64_t __rsp;
 __uint64_t __r8;
 __uint64_t __r9;
 __uint64_t __r10;
 __uint64_t __r11;
 __uint64_t __r12;
 __uint64_t __r13;
 __uint64_t __r14;
 __uint64_t __r15;
 __uint64_t __rip;
 __uint64_t __rflags;
 __uint64_t __cs;
 __uint64_t __fs;
 __uint64_t __gs;
};
# 413 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_x86_float_state64
{
 int __fpu_reserved[2];
 struct __darwin_fp_control __fpu_fcw;
 struct __darwin_fp_status __fpu_fsw;
 __uint8_t __fpu_ftw;
 __uint8_t __fpu_rsrv1;
 __uint16_t __fpu_fop;


 __uint32_t __fpu_ip;
 __uint16_t __fpu_cs;

 __uint16_t __fpu_rsrv2;


 __uint32_t __fpu_dp;
 __uint16_t __fpu_ds;

 __uint16_t __fpu_rsrv3;
 __uint32_t __fpu_mxcsr;
 __uint32_t __fpu_mxcsrmask;
 struct __darwin_mmst_reg __fpu_stmm0;
 struct __darwin_mmst_reg __fpu_stmm1;
 struct __darwin_mmst_reg __fpu_stmm2;
 struct __darwin_mmst_reg __fpu_stmm3;
 struct __darwin_mmst_reg __fpu_stmm4;
 struct __darwin_mmst_reg __fpu_stmm5;
 struct __darwin_mmst_reg __fpu_stmm6;
 struct __darwin_mmst_reg __fpu_stmm7;
 struct __darwin_xmm_reg __fpu_xmm0;
 struct __darwin_xmm_reg __fpu_xmm1;
 struct __darwin_xmm_reg __fpu_xmm2;
 struct __darwin_xmm_reg __fpu_xmm3;
 struct __darwin_xmm_reg __fpu_xmm4;
 struct __darwin_xmm_reg __fpu_xmm5;
 struct __darwin_xmm_reg __fpu_xmm6;
 struct __darwin_xmm_reg __fpu_xmm7;
 struct __darwin_xmm_reg __fpu_xmm8;
 struct __darwin_xmm_reg __fpu_xmm9;
 struct __darwin_xmm_reg __fpu_xmm10;
 struct __darwin_xmm_reg __fpu_xmm11;
 struct __darwin_xmm_reg __fpu_xmm12;
 struct __darwin_xmm_reg __fpu_xmm13;
 struct __darwin_xmm_reg __fpu_xmm14;
 struct __darwin_xmm_reg __fpu_xmm15;
 char __fpu_rsrv4[6*16];
 int __fpu_reserved1;
};
# 517 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_x86_exception_state64
{
    unsigned int __trapno;
    unsigned int __err;
    __uint64_t __faultvaddr;
};
# 535 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_x86_debug_state64
{
 __uint64_t __dr0;
 __uint64_t __dr1;
 __uint64_t __dr2;
 __uint64_t __dr3;
 __uint64_t __dr4;
 __uint64_t __dr5;
 __uint64_t __dr6;
 __uint64_t __dr7;
};
# 39 "/usr/include/i386/_structs.h" 2 3 4
# 48 "/usr/include/i386/_structs.h" 3 4
struct __darwin_mcontext32
{
 struct __darwin_i386_exception_state __es;
 struct __darwin_i386_thread_state __ss;
 struct __darwin_i386_float_state __fs;
};
# 68 "/usr/include/i386/_structs.h" 3 4
struct __darwin_mcontext64
{
 struct __darwin_x86_exception_state64 __es;
 struct __darwin_x86_thread_state64 __ss;
 struct __darwin_x86_float_state64 __fs;
};
# 91 "/usr/include/i386/_structs.h" 3 4
typedef struct __darwin_mcontext64 *mcontext_t;
# 32 "/usr/include/machine/_structs.h" 2 3 4
# 58 "/usr/include/sys/_structs.h" 2 3 4
# 75 "/usr/include/sys/_structs.h" 3 4
struct __darwin_sigaltstack
{
 void *ss_sp;
 __darwin_size_t ss_size;
 int ss_flags;
};
# 128 "/usr/include/sys/_structs.h" 3 4
struct __darwin_ucontext
{
 int uc_onstack;
 __darwin_sigset_t uc_sigmask;
 struct __darwin_sigaltstack uc_stack;
 struct __darwin_ucontext *uc_link;
 __darwin_size_t uc_mcsize;
 struct __darwin_mcontext64 *uc_mcontext;



};
# 218 "/usr/include/sys/_structs.h" 3 4
typedef struct __darwin_sigaltstack stack_t;
# 227 "/usr/include/sys/_structs.h" 3 4
typedef struct __darwin_ucontext ucontext_t;
# 155 "/usr/include/sys/signal.h" 2 3 4
# 163 "/usr/include/sys/signal.h" 3 4
typedef __darwin_pthread_attr_t pthread_attr_t;




typedef __darwin_sigset_t sigset_t;
# 178 "/usr/include/sys/signal.h" 3 4
typedef __darwin_uid_t uid_t;


union sigval {

 int sival_int;
 void *sival_ptr;
};





struct sigevent {
 int sigev_notify;
 int sigev_signo;
 union sigval sigev_value;
 void (*sigev_notify_function)(union sigval);
 pthread_attr_t *sigev_notify_attributes;
};


typedef struct __siginfo {
 int si_signo;
 int si_errno;
 int si_code;
 pid_t si_pid;
 uid_t si_uid;
 int si_status;
 void *si_addr;
 union sigval si_value;
 long si_band;
 unsigned long __pad[7];
} siginfo_t;
# 292 "/usr/include/sys/signal.h" 3 4
union __sigaction_u {
 void (*__sa_handler)(int);
 void (*__sa_sigaction)(int, struct __siginfo *,
         void *);
};


struct __sigaction {
 union __sigaction_u __sigaction_u;
 void (*sa_tramp)(void *, int, int, siginfo_t *, void *);
 sigset_t sa_mask;
 int sa_flags;
};




struct sigaction {
 union __sigaction_u __sigaction_u;
 sigset_t sa_mask;
 int sa_flags;
};
# 354 "/usr/include/sys/signal.h" 3 4
typedef void (*sig_t)(int);
# 371 "/usr/include/sys/signal.h" 3 4
struct sigvec {
 void (*sv_handler)(int);
 int sv_mask;
 int sv_flags;
};
# 390 "/usr/include/sys/signal.h" 3 4
struct sigstack {
 char *ss_sp;
 int ss_onstack;
};
# 412 "/usr/include/sys/signal.h" 3 4

void (*signal(int, void (*)(int)))(int);

# 117 "/usr/include/sys/wait.h" 2 3 4
# 1 "/usr/include/sys/resource.h" 1 3 4
# 76 "/usr/include/sys/resource.h" 3 4
# 1 "/usr/include/sys/_structs.h" 1 3 4
# 100 "/usr/include/sys/_structs.h" 3 4
struct timeval
{
 __darwin_time_t tv_sec;
 __darwin_suseconds_t tv_usec;
};
# 77 "/usr/include/sys/resource.h" 2 3 4
# 88 "/usr/include/sys/resource.h" 3 4
typedef __uint64_t rlim_t;
# 142 "/usr/include/sys/resource.h" 3 4
struct rusage {
 struct timeval ru_utime;
 struct timeval ru_stime;
# 153 "/usr/include/sys/resource.h" 3 4
 long ru_maxrss;

 long ru_ixrss;
 long ru_idrss;
 long ru_isrss;
 long ru_minflt;
 long ru_majflt;
 long ru_nswap;
 long ru_inblock;
 long ru_oublock;
 long ru_msgsnd;
 long ru_msgrcv;
 long ru_nsignals;
 long ru_nvcsw;
 long ru_nivcsw;


};
# 213 "/usr/include/sys/resource.h" 3 4
struct rlimit {
 rlim_t rlim_cur;
 rlim_t rlim_max;
};
# 235 "/usr/include/sys/resource.h" 3 4

int getpriority(int, id_t);

int getiopolicy_np(int, int);

int getrlimit(int, struct rlimit *) __asm("_" "getrlimit" );
int getrusage(int, struct rusage *);
int setpriority(int, id_t, int);

int setiopolicy_np(int, int, int);

int setrlimit(int, const struct rlimit *) __asm("_" "setrlimit" );

# 118 "/usr/include/sys/wait.h" 2 3 4
# 193 "/usr/include/sys/wait.h" 3 4
# 1 "/usr/include/machine/endian.h" 1 3 4
# 37 "/usr/include/machine/endian.h" 3 4
# 1 "/usr/include/i386/endian.h" 1 3 4
# 99 "/usr/include/i386/endian.h" 3 4
# 1 "/usr/include/sys/_endian.h" 1 3 4
# 124 "/usr/include/sys/_endian.h" 3 4
# 1 "/usr/include/libkern/_OSByteOrder.h" 1 3 4
# 66 "/usr/include/libkern/_OSByteOrder.h" 3 4
# 1 "/usr/include/libkern/i386/_OSByteOrder.h" 1 3 4
# 44 "/usr/include/libkern/i386/_OSByteOrder.h" 3 4
static __inline__
__uint16_t
_OSSwapInt16(
    __uint16_t _data
)
{
    return ((_data << 8) | (_data >> 8));
}

static __inline__
__uint32_t
_OSSwapInt32(
    __uint32_t _data
)
{



    __asm__ ("bswap   %0" : "+r" (_data));
    return _data;

}
# 91 "/usr/include/libkern/i386/_OSByteOrder.h" 3 4
static __inline__
__uint64_t
_OSSwapInt64(
    __uint64_t _data
)
{
    __asm__ ("bswap   %0" : "+r" (_data));
    return _data;
}
# 67 "/usr/include/libkern/_OSByteOrder.h" 2 3 4
# 125 "/usr/include/sys/_endian.h" 2 3 4
# 100 "/usr/include/i386/endian.h" 2 3 4
# 38 "/usr/include/machine/endian.h" 2 3 4
# 194 "/usr/include/sys/wait.h" 2 3 4







union wait {
 int w_status;



 struct {

  unsigned int w_Termsig:7,
    w_Coredump:1,
    w_Retcode:8,
    w_Filler:16;







 } w_T;





 struct {

  unsigned int w_Stopval:8,
    w_Stopsig:8,
    w_Filler:16;






 } w_S;
};
# 254 "/usr/include/sys/wait.h" 3 4

pid_t wait(int *) __asm("_" "wait" );
pid_t waitpid(pid_t, int *, int) __asm("_" "waitpid" );

int waitid(idtype_t, id_t, siginfo_t *, int) __asm("_" "waitid" );


pid_t wait3(int *, int, struct rusage *);
pid_t wait4(pid_t, int *, int, struct rusage *);


# 66 "/usr/include/stdlib.h" 2 3 4

# 1 "/usr/include/alloca.h" 1 3 4
# 35 "/usr/include/alloca.h" 3 4

void *alloca(size_t);

# 68 "/usr/include/stdlib.h" 2 3 4
# 97 "/usr/include/stdlib.h" 3 4
typedef struct {
 int quot;
 int rem;
} div_t;

typedef struct {
 long quot;
 long rem;
} ldiv_t;


typedef struct {
 long long quot;
 long long rem;
} lldiv_t;
# 134 "/usr/include/stdlib.h" 3 4
extern int __mb_cur_max;
# 144 "/usr/include/stdlib.h" 3 4

void abort(void) __attribute__((__noreturn__));
int abs(int) __attribute__((__const__));
int atexit(void (*)(void));
double atof(const char *);
int atoi(const char *);
long atol(const char *);

long long
  atoll(const char *);

void *bsearch(const void *, const void *, size_t,
     size_t, int (*)(const void *, const void *));
void *calloc(size_t, size_t);
div_t div(int, int) __attribute__((__const__));
void exit(int) __attribute__((__noreturn__));
void free(void *);
char *getenv(const char *);
long labs(long) __attribute__((__const__));
ldiv_t ldiv(long, long) __attribute__((__const__));

long long
  llabs(long long);
lldiv_t lldiv(long long, long long);

void *malloc(size_t);
int mblen(const char *, size_t);
size_t mbstowcs(wchar_t * , const char * , size_t);
int mbtowc(wchar_t * , const char * , size_t);
int posix_memalign(void **, size_t, size_t);
void qsort(void *, size_t, size_t,
     int (*)(const void *, const void *));
int rand(void);
void *realloc(void *, size_t);
void srand(unsigned);
double strtod(const char *, char **) __asm("_" "strtod" );
float strtof(const char *, char **) __asm("_" "strtof" );
long strtol(const char *, char **, int);
long double
  strtold(const char *, char **) ;

long long
  strtoll(const char *, char **, int);

unsigned long
  strtoul(const char *, char **, int);

unsigned long long
  strtoull(const char *, char **, int);

int system(const char *) __asm("_" "system" );
size_t wcstombs(char * , const wchar_t * , size_t);
int wctomb(char *, wchar_t);


void _Exit(int) __attribute__((__noreturn__));
long a64l(const char *);
double drand48(void);
char *ecvt(double, int, int *, int *);
double erand48(unsigned short[3]);
char *fcvt(double, int, int *, int *);
char *gcvt(double, int, char *);
int getsubopt(char **, char * const *, char **);
int grantpt(int);

char *initstate(unsigned, char *, size_t);



long jrand48(unsigned short[3]);
char *l64a(long);
void lcong48(unsigned short[7]);
long lrand48(void);
char *mktemp(char *);
int mkstemp(char *);
long mrand48(void);
long nrand48(unsigned short[3]);
int posix_openpt(int);
char *ptsname(int);
int putenv(char *) __asm("_" "putenv" );
long random(void);
int rand_r(unsigned *);

char *realpath(const char * , char * ) __asm("_" "realpath" "$DARWIN_EXTSN");



unsigned short
 *seed48(unsigned short[3]);
int setenv(const char *, const char *, int) __asm("_" "setenv" );

void setkey(const char *) __asm("_" "setkey" );



char *setstate(const char *);
void srand48(long);

void srandom(unsigned);



int unlockpt(int);

int unsetenv(const char *) __asm("_" "unsetenv" );






# 1 "/usr/include/machine/types.h" 1 3 4
# 37 "/usr/include/machine/types.h" 3 4
# 1 "/usr/include/i386/types.h" 1 3 4
# 70 "/usr/include/i386/types.h" 3 4
# 1 "/usr/include/i386/_types.h" 1 3 4
# 71 "/usr/include/i386/types.h" 2 3 4







typedef signed char int8_t;

typedef unsigned char u_int8_t;


typedef short int16_t;

typedef unsigned short u_int16_t;


typedef int int32_t;

typedef unsigned int u_int32_t;


typedef long long int64_t;

typedef unsigned long long u_int64_t;


typedef int64_t register_t;






typedef __darwin_intptr_t intptr_t;



typedef unsigned long uintptr_t;




typedef u_int64_t user_addr_t;
typedef u_int64_t user_size_t;
typedef int64_t user_ssize_t;
typedef int64_t user_long_t;
typedef u_int64_t user_ulong_t;
typedef int64_t user_time_t;
typedef int64_t user_off_t;







typedef u_int64_t syscall_arg_t;
# 38 "/usr/include/machine/types.h" 2 3 4
# 256 "/usr/include/stdlib.h" 2 3 4


typedef __darwin_dev_t dev_t;




typedef __darwin_mode_t mode_t;



u_int32_t
  arc4random(void);
void arc4random_addrandom(unsigned char *dat, int datlen);
void arc4random_stir(void);

int atexit_b(void (^)(void));
void *bsearch_b(const void *, const void *, size_t,
     size_t, int (^)(const void *, const void *));



char *cgetcap(char *, const char *, int);
int cgetclose(void);
int cgetent(char **, char **, const char *);
int cgetfirst(char **, char **);
int cgetmatch(const char *, const char *);
int cgetnext(char **, char **);
int cgetnum(char *, const char *, long *);
int cgetset(const char *);
int cgetstr(char *, const char *, char **);
int cgetustr(char *, const char *, char **);

int daemon(int, int) __asm("_" "daemon" "$1050") __attribute__((deprecated,visibility("default")));
char *devname(dev_t, mode_t);
char *devname_r(dev_t, mode_t, char *buf, int len);
char *getbsize(int *, long *);
int getloadavg(double [], int);
const char
 *getprogname(void);

int heapsort(void *, size_t, size_t,
     int (*)(const void *, const void *));

int heapsort_b(void *, size_t, size_t,
     int (^)(const void *, const void *));

int mergesort(void *, size_t, size_t,
     int (*)(const void *, const void *));

int mergesort_b(void *, size_t, size_t,
     int (^)(const void *, const void *));

void psort(void *, size_t, size_t,
     int (*)(const void *, const void *));

void psort_b(void *, size_t, size_t,
     int (^)(const void *, const void *));

void psort_r(void *, size_t, size_t, void *,
     int (*)(void *, const void *, const void *));

void qsort_b(void *, size_t, size_t,
     int (^)(const void *, const void *));

void qsort_r(void *, size_t, size_t, void *,
     int (*)(void *, const void *, const void *));
int radixsort(const unsigned char **, int, const unsigned char *,
     unsigned);
void setprogname(const char *);
int sradixsort(const unsigned char **, int, const unsigned char *,
     unsigned);
void sranddev(void);
void srandomdev(void);
void *reallocf(void *, size_t);

long long
  strtoq(const char *, char **, int);
unsigned long long
  strtouq(const char *, char **, int);

extern char *suboptarg;
void *valloc(size_t);







# 119 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftstdlib.h" 2
# 44 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftconfig.h" 2



# 153 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftconfig.h"
# 1 "/usr/include/AvailabilityMacros.h" 1 3 4
# 154 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftconfig.h" 2
# 185 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftconfig.h"
  typedef signed short FT_Int16;
  typedef unsigned short FT_UInt16;



  typedef signed int FT_Int32;
  typedef unsigned int FT_UInt32;
# 206 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftconfig.h"
  typedef int FT_Fast;
  typedef unsigned int FT_UFast;
# 471 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/config/ftconfig.h"

# 34 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fterrors.h" 1
# 90 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fterrors.h"
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftmoderr.h" 1
# 101 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftmoderr.h"
  enum {



  FT_Mod_Err_Base = 0,
  FT_Mod_Err_Autofit = 0,
  FT_Mod_Err_BDF = 0,
  FT_Mod_Err_Cache = 0,
  FT_Mod_Err_CFF = 0,
  FT_Mod_Err_CID = 0,
  FT_Mod_Err_Gzip = 0,
  FT_Mod_Err_LZW = 0,
  FT_Mod_Err_OTvalid = 0,
  FT_Mod_Err_PCF = 0,
  FT_Mod_Err_PFR = 0,
  FT_Mod_Err_PSaux = 0,
  FT_Mod_Err_PShinter = 0,
  FT_Mod_Err_PSnames = 0,
  FT_Mod_Err_Raster = 0,
  FT_Mod_Err_SFNT = 0,
  FT_Mod_Err_Smooth = 0,
  FT_Mod_Err_TrueType = 0,
  FT_Mod_Err_Type1 = 0,
  FT_Mod_Err_Type42 = 0,
  FT_Mod_Err_Winfonts = 0,



  FT_Mod_Err_Max };
# 91 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fterrors.h" 2
# 162 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fterrors.h"
  enum {




# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fterrdef.h" 1
# 34 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fterrdef.h"
  FT_Err_Ok = 0x00,


  FT_Err_Cannot_Open_Resource = 0x01 + 0,

  FT_Err_Unknown_File_Format = 0x02 + 0,

  FT_Err_Invalid_File_Format = 0x03 + 0,

  FT_Err_Invalid_Version = 0x04 + 0,

  FT_Err_Lower_Module_Version = 0x05 + 0,

  FT_Err_Invalid_Argument = 0x06 + 0,

  FT_Err_Unimplemented_Feature = 0x07 + 0,

  FT_Err_Invalid_Table = 0x08 + 0,

  FT_Err_Invalid_Offset = 0x09 + 0,

  FT_Err_Array_Too_Large = 0x0A + 0,




  FT_Err_Invalid_Glyph_Index = 0x10 + 0,

  FT_Err_Invalid_Character_Code = 0x11 + 0,

  FT_Err_Invalid_Glyph_Format = 0x12 + 0,

  FT_Err_Cannot_Render_Glyph = 0x13 + 0,

  FT_Err_Invalid_Outline = 0x14 + 0,

  FT_Err_Invalid_Composite = 0x15 + 0,

  FT_Err_Too_Many_Hints = 0x16 + 0,

  FT_Err_Invalid_Pixel_Size = 0x17 + 0,




  FT_Err_Invalid_Handle = 0x20 + 0,

  FT_Err_Invalid_Library_Handle = 0x21 + 0,

  FT_Err_Invalid_Driver_Handle = 0x22 + 0,

  FT_Err_Invalid_Face_Handle = 0x23 + 0,

  FT_Err_Invalid_Size_Handle = 0x24 + 0,

  FT_Err_Invalid_Slot_Handle = 0x25 + 0,

  FT_Err_Invalid_CharMap_Handle = 0x26 + 0,

  FT_Err_Invalid_Cache_Handle = 0x27 + 0,

  FT_Err_Invalid_Stream_Handle = 0x28 + 0,




  FT_Err_Too_Many_Drivers = 0x30 + 0,

  FT_Err_Too_Many_Extensions = 0x31 + 0,




  FT_Err_Out_Of_Memory = 0x40 + 0,

  FT_Err_Unlisted_Object = 0x41 + 0,




  FT_Err_Cannot_Open_Stream = 0x51 + 0,

  FT_Err_Invalid_Stream_Seek = 0x52 + 0,

  FT_Err_Invalid_Stream_Skip = 0x53 + 0,

  FT_Err_Invalid_Stream_Read = 0x54 + 0,

  FT_Err_Invalid_Stream_Operation = 0x55 + 0,

  FT_Err_Invalid_Frame_Operation = 0x56 + 0,

  FT_Err_Nested_Frame_Access = 0x57 + 0,

  FT_Err_Invalid_Frame_Read = 0x58 + 0,




  FT_Err_Raster_Uninitialized = 0x60 + 0,

  FT_Err_Raster_Corrupted = 0x61 + 0,

  FT_Err_Raster_Overflow = 0x62 + 0,

  FT_Err_Raster_Negative_Height = 0x63 + 0,




  FT_Err_Too_Many_Caches = 0x70 + 0,




  FT_Err_Invalid_Opcode = 0x80 + 0,

  FT_Err_Too_Few_Arguments = 0x81 + 0,

  FT_Err_Stack_Overflow = 0x82 + 0,

  FT_Err_Code_Overflow = 0x83 + 0,

  FT_Err_Bad_Argument = 0x84 + 0,

  FT_Err_Divide_By_Zero = 0x85 + 0,

  FT_Err_Invalid_Reference = 0x86 + 0,

  FT_Err_Debug_OpCode = 0x87 + 0,

  FT_Err_ENDF_In_Exec_Stream = 0x88 + 0,

  FT_Err_Nested_DEFS = 0x89 + 0,

  FT_Err_Invalid_CodeRange = 0x8A + 0,

  FT_Err_Execution_Too_Long = 0x8B + 0,

  FT_Err_Too_Many_Function_Defs = 0x8C + 0,

  FT_Err_Too_Many_Instruction_Defs = 0x8D + 0,

  FT_Err_Table_Missing = 0x8E + 0,

  FT_Err_Horiz_Header_Missing = 0x8F + 0,

  FT_Err_Locations_Missing = 0x90 + 0,

  FT_Err_Name_Table_Missing = 0x91 + 0,

  FT_Err_CMap_Table_Missing = 0x92 + 0,

  FT_Err_Hmtx_Table_Missing = 0x93 + 0,

  FT_Err_Post_Table_Missing = 0x94 + 0,

  FT_Err_Invalid_Horiz_Metrics = 0x95 + 0,

  FT_Err_Invalid_CharMap_Format = 0x96 + 0,

  FT_Err_Invalid_PPem = 0x97 + 0,

  FT_Err_Invalid_Vert_Metrics = 0x98 + 0,

  FT_Err_Could_Not_Find_Context = 0x99 + 0,

  FT_Err_Invalid_Post_Table_Format = 0x9A + 0,

  FT_Err_Invalid_Post_Table = 0x9B + 0,




  FT_Err_Syntax_Error = 0xA0 + 0,

  FT_Err_Stack_Underflow = 0xA1 + 0,

  FT_Err_Ignore = 0xA2 + 0,




  FT_Err_Missing_Startfont_Field = 0xB0 + 0,

  FT_Err_Missing_Font_Field = 0xB1 + 0,

  FT_Err_Missing_Size_Field = 0xB2 + 0,

  FT_Err_Missing_Chars_Field = 0xB3 + 0,

  FT_Err_Missing_Startchar_Field = 0xB4 + 0,

  FT_Err_Missing_Encoding_Field = 0xB5 + 0,

  FT_Err_Missing_Bbx_Field = 0xB6 + 0,

  FT_Err_Bbx_Too_Big = 0xB7 + 0,

  FT_Err_Corrupted_Font_Header = 0xB8 + 0,

  FT_Err_Corrupted_Font_Glyphs = 0xB9 + 0,
# 168 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fterrors.h" 2



  FT_Err_Max };
# 35 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h" 1
# 25 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h" 1
# 26 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h"

# 66 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h"
  typedef struct FT_MemoryRec_* FT_Memory;
# 88 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h"
  typedef void*
  (*FT_Alloc_Func)( FT_Memory memory,
                    long size );
# 109 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h"
  typedef void
  (*FT_Free_Func)( FT_Memory memory,
                   void* block );
# 142 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h"
  typedef void*
  (*FT_Realloc_Func)( FT_Memory memory,
                      long cur_size,
                      long new_size,
                      void* block );
# 171 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h"
  struct FT_MemoryRec_
  {
    void* user;
    FT_Alloc_Func alloc;
    FT_Free_Func free;
    FT_Realloc_Func realloc;
  };
# 196 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h"
  typedef struct FT_StreamRec_* FT_Stream;
# 209 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h"
  typedef union FT_StreamDesc_
  {
    long value;
    void* pointer;

  } FT_StreamDesc;
# 246 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h"
  typedef unsigned long
  (*FT_Stream_IoFunc)( FT_Stream stream,
                       unsigned long offset,
                       unsigned char* buffer,
                       unsigned long count );
# 266 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h"
  typedef void
  (*FT_Stream_CloseFunc)( FT_Stream stream );
# 320 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftsystem.h"
  typedef struct FT_StreamRec_
  {
    unsigned char* base;
    unsigned long size;
    unsigned long pos;

    FT_StreamDesc descriptor;
    FT_StreamDesc pathname;
    FT_Stream_IoFunc read;
    FT_Stream_CloseFunc close;

    FT_Memory memory;
    unsigned char* cursor;
    unsigned char* limit;

  } FT_StreamRec;






# 26 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h" 1
# 37 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"

# 59 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef signed long FT_Pos;
# 75 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef struct FT_Vector_
  {
    FT_Pos x;
    FT_Pos y;

  } FT_Vector;
# 102 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef struct FT_BBox_
  {
    FT_Pos xMin, yMin;
    FT_Pos xMax, yMax;

  } FT_BBox;
# 158 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef enum FT_Pixel_Mode_
  {
    FT_PIXEL_MODE_NONE = 0,
    FT_PIXEL_MODE_MONO,
    FT_PIXEL_MODE_GRAY,
    FT_PIXEL_MODE_GRAY2,
    FT_PIXEL_MODE_GRAY4,
    FT_PIXEL_MODE_LCD,
    FT_PIXEL_MODE_LCD_V,

    FT_PIXEL_MODE_MAX

  } FT_Pixel_Mode;
# 281 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef struct FT_Bitmap_
  {
    int rows;
    int width;
    int pitch;
    unsigned char* buffer;
    short num_grays;
    char pixel_mode;
    char palette_mode;
    void* palette;

  } FT_Bitmap;
# 354 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef struct FT_Outline_
  {
    short n_contours;
    short n_points;

    FT_Vector* points;
    char* tags;
    short* contours;

    int flags;

  } FT_Outline;
# 526 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef int
  (*FT_Outline_MoveToFunc)( const FT_Vector* to,
                            void* user );
# 553 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef int
  (*FT_Outline_LineToFunc)( const FT_Vector* to,
                            void* user );
# 584 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef int
  (*FT_Outline_ConicToFunc)( const FT_Vector* control,
                             const FT_Vector* to,
                             void* user );
# 616 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef int
  (*FT_Outline_CubicToFunc)( const FT_Vector* control1,
                             const FT_Vector* control2,
                             const FT_Vector* to,
                             void* user );
# 663 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef struct FT_Outline_Funcs_
  {
    FT_Outline_MoveToFunc move_to;
    FT_Outline_LineToFunc line_to;
    FT_Outline_ConicToFunc conic_to;
    FT_Outline_CubicToFunc cubic_to;

    int shift;
    FT_Pos delta;

  } FT_Outline_Funcs;
# 750 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef enum FT_Glyph_Format_
  {
    FT_GLYPH_FORMAT_NONE = ( ( (unsigned long)0 << 24 ) | ( (unsigned long)0 << 16 ) | ( (unsigned long)0 << 8 ) | (unsigned long)0 ),

    FT_GLYPH_FORMAT_COMPOSITE = ( ( (unsigned long)'c' << 24 ) | ( (unsigned long)'o' << 16 ) | ( (unsigned long)'m' << 8 ) | (unsigned long)'p' ),
    FT_GLYPH_FORMAT_BITMAP = ( ( (unsigned long)'b' << 24 ) | ( (unsigned long)'i' << 16 ) | ( (unsigned long)'t' << 8 ) | (unsigned long)'s' ),
    FT_GLYPH_FORMAT_OUTLINE = ( ( (unsigned long)'o' << 24 ) | ( (unsigned long)'u' << 16 ) | ( (unsigned long)'t' << 8 ) | (unsigned long)'l' ),
    FT_GLYPH_FORMAT_PLOTTER = ( ( (unsigned long)'p' << 24 ) | ( (unsigned long)'l' << 16 ) | ( (unsigned long)'o' << 8 ) | (unsigned long)'t' )

  } FT_Glyph_Format;
# 834 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef struct FT_RasterRec_* FT_Raster;
# 863 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef struct FT_Span_
  {
    short x;
    unsigned short len;
    unsigned char coverage;

  } FT_Span;
# 908 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef void
  (*FT_SpanFunc)( int y,
                  int count,
                  const FT_Span* spans,
                  void* user );
# 940 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef int
  (*FT_Raster_BitTest_Func)( int y,
                             int x,
                             void* user );
# 968 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef void
  (*FT_Raster_BitSet_Func)( int y,
                            int x,
                            void* user );
# 1076 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef struct FT_Raster_Params_
  {
    const FT_Bitmap* target;
    const void* source;
    int flags;
    FT_SpanFunc gray_spans;
    FT_SpanFunc black_spans;
    FT_Raster_BitTest_Func bit_test;
    FT_Raster_BitSet_Func bit_set;
    void* user;
    FT_BBox clip_box;

  } FT_Raster_Params;
# 1115 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef int
  (*FT_Raster_NewFunc)( void* memory,
                        FT_Raster* raster );
# 1133 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef void
  (*FT_Raster_DoneFunc)( FT_Raster raster );
# 1166 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef void
  (*FT_Raster_ResetFunc)( FT_Raster raster,
                          unsigned char* pool_base,
                          unsigned long pool_size );
# 1192 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef int
  (*FT_Raster_SetModeFunc)( FT_Raster raster,
                            unsigned long mode,
                            void* args );
# 1234 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef int
  (*FT_Raster_RenderFunc)( FT_Raster raster,
                           const FT_Raster_Params* params );
# 1260 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftimage.h"
  typedef struct FT_Raster_Funcs_
  {
    FT_Glyph_Format glyph_format;
    FT_Raster_NewFunc raster_new;
    FT_Raster_ResetFunc raster_reset;
    FT_Raster_SetModeFunc raster_set_mode;
    FT_Raster_RenderFunc raster_render;
    FT_Raster_DoneFunc raster_done;

  } FT_Raster_Funcs;






# 27 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h" 2

# 1 "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/stddef.h" 1 3 4
# 29 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h" 2



# 104 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef unsigned char FT_Bool;
# 116 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef signed short FT_FWord;
# 128 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef unsigned short FT_UFWord;
# 139 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef signed char FT_Char;
# 150 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef unsigned char FT_Byte;
# 161 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef const FT_Byte* FT_Bytes;
# 172 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef FT_UInt32 FT_Tag;
# 183 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef char FT_String;
# 194 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef signed short FT_Short;
# 205 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef unsigned short FT_UShort;
# 216 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef signed int FT_Int;
# 227 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef unsigned int FT_UInt;
# 238 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef signed long FT_Long;
# 249 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef unsigned long FT_ULong;
# 260 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef signed short FT_F2Dot14;
# 272 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef signed long FT_F26Dot6;
# 284 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef signed long FT_Fixed;
# 296 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef int FT_Error;
# 307 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef void* FT_Pointer;
# 320 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef size_t FT_Offset;
# 333 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef ptrdiff_t FT_PtrDist;
# 350 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef struct FT_UnitVector_
  {
    FT_F2Dot14 x;
    FT_F2Dot14 y;

  } FT_UnitVector;
# 381 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef struct FT_Matrix_
  {
    FT_Fixed xx, xy;
    FT_Fixed yx, yy;

  } FT_Matrix;
# 402 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef struct FT_Data_
  {
    const FT_Byte* pointer;
    FT_Int length;

  } FT_Data;
# 424 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef void (*FT_Generic_Finalizer)(void* object);
# 455 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef struct FT_Generic_
  {
    void* data;
    FT_Generic_Finalizer finalizer;

  } FT_Generic;
# 511 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef struct FT_ListNodeRec_* FT_ListNode;
# 522 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef struct FT_ListRec_* FT_List;
# 540 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef struct FT_ListNodeRec_
  {
    FT_ListNode prev;
    FT_ListNode next;
    void* data;

  } FT_ListNodeRec;
# 563 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"
  typedef struct FT_ListRec_
  {
    FT_ListNode head;
    FT_ListNode tail;

  } FT_ListRec;
# 583 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/fttypes.h"

# 36 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h" 2



# 234 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_Glyph_Metrics_
  {
    FT_Pos width;
    FT_Pos height;

    FT_Pos horiBearingX;
    FT_Pos horiBearingY;
    FT_Pos horiAdvance;

    FT_Pos vertBearingX;
    FT_Pos vertBearingY;
    FT_Pos vertAdvance;

  } FT_Glyph_Metrics;
# 288 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_Bitmap_Size_
  {
    FT_Short height;
    FT_Short width;

    FT_Pos size;

    FT_Pos x_ppem;
    FT_Pos y_ppem;

  } FT_Bitmap_Size;
# 329 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_LibraryRec_ *FT_Library;
# 342 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_ModuleRec_* FT_Module;
# 354 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_DriverRec_* FT_Driver;
# 368 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_RendererRec_* FT_Renderer;
# 393 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_FaceRec_* FT_Face;
# 423 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_SizeRec_* FT_Size;
# 444 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_GlyphSlotRec_* FT_GlyphSlot;
# 476 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_CharMapRec_* FT_CharMap;
# 649 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef enum FT_Encoding_
  {
    FT_ENCODING_NONE = ( ( (FT_UInt32)(0) << 24 ) | ( (FT_UInt32)(0) << 16 ) | ( (FT_UInt32)(0) << 8 ) | (FT_UInt32)(0) ),

    FT_ENCODING_MS_SYMBOL = ( ( (FT_UInt32)('s') << 24 ) | ( (FT_UInt32)('y') << 16 ) | ( (FT_UInt32)('m') << 8 ) | (FT_UInt32)('b') ),
    FT_ENCODING_UNICODE = ( ( (FT_UInt32)('u') << 24 ) | ( (FT_UInt32)('n') << 16 ) | ( (FT_UInt32)('i') << 8 ) | (FT_UInt32)('c') ),

    FT_ENCODING_SJIS = ( ( (FT_UInt32)('s') << 24 ) | ( (FT_UInt32)('j') << 16 ) | ( (FT_UInt32)('i') << 8 ) | (FT_UInt32)('s') ),
    FT_ENCODING_GB2312 = ( ( (FT_UInt32)('g') << 24 ) | ( (FT_UInt32)('b') << 16 ) | ( (FT_UInt32)(' ') << 8 ) | (FT_UInt32)(' ') ),
    FT_ENCODING_BIG5 = ( ( (FT_UInt32)('b') << 24 ) | ( (FT_UInt32)('i') << 16 ) | ( (FT_UInt32)('g') << 8 ) | (FT_UInt32)('5') ),
    FT_ENCODING_WANSUNG = ( ( (FT_UInt32)('w') << 24 ) | ( (FT_UInt32)('a') << 16 ) | ( (FT_UInt32)('n') << 8 ) | (FT_UInt32)('s') ),
    FT_ENCODING_JOHAB = ( ( (FT_UInt32)('j') << 24 ) | ( (FT_UInt32)('o') << 16 ) | ( (FT_UInt32)('h') << 8 ) | (FT_UInt32)('a') ),


    FT_ENCODING_MS_SJIS = FT_ENCODING_SJIS,
    FT_ENCODING_MS_GB2312 = FT_ENCODING_GB2312,
    FT_ENCODING_MS_BIG5 = FT_ENCODING_BIG5,
    FT_ENCODING_MS_WANSUNG = FT_ENCODING_WANSUNG,
    FT_ENCODING_MS_JOHAB = FT_ENCODING_JOHAB,

    FT_ENCODING_ADOBE_STANDARD = ( ( (FT_UInt32)('A') << 24 ) | ( (FT_UInt32)('D') << 16 ) | ( (FT_UInt32)('O') << 8 ) | (FT_UInt32)('B') ),
    FT_ENCODING_ADOBE_EXPERT = ( ( (FT_UInt32)('A') << 24 ) | ( (FT_UInt32)('D') << 16 ) | ( (FT_UInt32)('B') << 8 ) | (FT_UInt32)('E') ),
    FT_ENCODING_ADOBE_CUSTOM = ( ( (FT_UInt32)('A') << 24 ) | ( (FT_UInt32)('D') << 16 ) | ( (FT_UInt32)('B') << 8 ) | (FT_UInt32)('C') ),
    FT_ENCODING_ADOBE_LATIN_1 = ( ( (FT_UInt32)('l') << 24 ) | ( (FT_UInt32)('a') << 16 ) | ( (FT_UInt32)('t') << 8 ) | (FT_UInt32)('1') ),

    FT_ENCODING_OLD_LATIN_2 = ( ( (FT_UInt32)('l') << 24 ) | ( (FT_UInt32)('a') << 16 ) | ( (FT_UInt32)('t') << 8 ) | (FT_UInt32)('2') ),

    FT_ENCODING_APPLE_ROMAN = ( ( (FT_UInt32)('a') << 24 ) | ( (FT_UInt32)('r') << 16 ) | ( (FT_UInt32)('m') << 8 ) | (FT_UInt32)('n') )

  } FT_Encoding;
# 730 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_CharMapRec_
  {
    FT_Face face;
    FT_Encoding encoding;
    FT_UShort platform_id;
    FT_UShort encoding_id;

  } FT_CharMapRec;
# 761 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_Face_InternalRec_* FT_Face_Internal;
# 906 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_FaceRec_
  {
    FT_Long num_faces;
    FT_Long face_index;

    FT_Long face_flags;
    FT_Long style_flags;

    FT_Long num_glyphs;

    FT_String* family_name;
    FT_String* style_name;

    FT_Int num_fixed_sizes;
    FT_Bitmap_Size* available_sizes;

    FT_Int num_charmaps;
    FT_CharMap* charmaps;

    FT_Generic generic;




    FT_BBox bbox;

    FT_UShort units_per_EM;
    FT_Short ascender;
    FT_Short descender;
    FT_Short height;

    FT_Short max_advance_width;
    FT_Short max_advance_height;

    FT_Short underline_position;
    FT_Short underline_thickness;

    FT_GlyphSlot glyph;
    FT_Size size;
    FT_CharMap charmap;



    FT_Driver driver;
    FT_Memory memory;
    FT_Stream stream;

    FT_ListRec sizes_list;

    FT_Generic autohint;
    void* extensions;

    FT_Face_Internal internal;



  } FT_FaceRec;
# 1296 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_Size_InternalRec_* FT_Size_Internal;
# 1354 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_Size_Metrics_
  {
    FT_UShort x_ppem;
    FT_UShort y_ppem;

    FT_Fixed x_scale;
    FT_Fixed y_scale;

    FT_Pos ascender;
    FT_Pos descender;
    FT_Pos height;
    FT_Pos max_advance;

  } FT_Size_Metrics;
# 1389 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_SizeRec_
  {
    FT_Face face;
    FT_Generic generic;
    FT_Size_Metrics metrics;
    FT_Size_Internal internal;

  } FT_SizeRec;
# 1415 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_SubGlyphRec_* FT_SubGlyph;
# 1427 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_Slot_InternalRec_* FT_Slot_Internal;
# 1594 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_GlyphSlotRec_
  {
    FT_Library library;
    FT_Face face;
    FT_GlyphSlot next;
    FT_UInt reserved;
    FT_Generic generic;

    FT_Glyph_Metrics metrics;
    FT_Fixed linearHoriAdvance;
    FT_Fixed linearVertAdvance;
    FT_Vector advance;

    FT_Glyph_Format format;

    FT_Bitmap bitmap;
    FT_Int bitmap_left;
    FT_Int bitmap_top;

    FT_Outline outline;

    FT_UInt num_subglyphs;
    FT_SubGlyph subglyphs;

    void* control_data;
    long control_len;

    FT_Pos lsb_delta;
    FT_Pos rsb_delta;

    void* other;

    FT_Slot_Internal internal;

  } FT_GlyphSlotRec;
# 1660 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Init_FreeType( FT_Library *alibrary );
# 1679 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Done_FreeType( FT_Library library );
# 1748 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_Parameter_
  {
    FT_ULong tag;
    FT_Pointer data;

  } FT_Parameter;
# 1814 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_Open_Args_
  {
    FT_UInt flags;
    const FT_Byte* memory_base;
    FT_Long memory_size;
    FT_String* pathname;
    FT_Stream stream;
    FT_Module driver;
    FT_Int num_params;
    FT_Parameter* params;

  } FT_Open_Args;
# 1853 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_New_Face( FT_Library library,
               const char* filepathname,
               FT_Long face_index,
               FT_Face *aface );
# 1891 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_New_Memory_Face( FT_Library library,
                      const FT_Byte* file_base,
                      FT_Long file_size,
                      FT_Long face_index,
                      FT_Face *aface );
# 1944 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Open_Face( FT_Library library,
                const FT_Open_Args* args,
                FT_Long face_index,
                FT_Face *aface );
# 1968 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Attach_File( FT_Face face,
                  const char* filepathname );
# 2003 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Attach_Stream( FT_Face face,
                    FT_Open_Args* parameters );
# 2023 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Done_Face( FT_Face face );
# 2045 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Select_Size( FT_Face face,
                  FT_Int strike_index );
# 2092 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef enum FT_Size_Request_Type_
  {
    FT_SIZE_REQUEST_TYPE_NOMINAL,
    FT_SIZE_REQUEST_TYPE_REAL_DIM,
    FT_SIZE_REQUEST_TYPE_BBOX,
    FT_SIZE_REQUEST_TYPE_CELL,
    FT_SIZE_REQUEST_TYPE_SCALES,

    FT_SIZE_REQUEST_TYPE_MAX

  } FT_Size_Request_Type;
# 2132 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_Size_RequestRec_
  {
    FT_Size_Request_Type type;
    FT_Long width;
    FT_Long height;
    FT_UInt horiResolution;
    FT_UInt vertResolution;

  } FT_Size_RequestRec;
# 2151 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef struct FT_Size_RequestRec_ *FT_Size_Request;
# 2177 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Request_Size( FT_Face face,
                   FT_Size_Request req );
# 2218 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Set_Char_Size( FT_Face face,
                    FT_F26Dot6 char_width,
                    FT_F26Dot6 char_height,
                    FT_UInt horz_resolution,
                    FT_UInt vert_resolution );
# 2246 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Set_Pixel_Sizes( FT_Face face,
                      FT_UInt pixel_width,
                      FT_UInt pixel_height );
# 2288 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Load_Glyph( FT_Face face,
                 FT_UInt glyph_index,
                 FT_Int32 load_flags );
# 2323 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Load_Char( FT_Face face,
                FT_ULong char_code,
                FT_Int32 load_flags );
# 2596 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern void
  FT_Set_Transform( FT_Face face,
                    FT_Matrix* matrix,
                    FT_Vector* delta );
# 2657 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef enum FT_Render_Mode_
  {
    FT_RENDER_MODE_NORMAL = 0,
    FT_RENDER_MODE_LIGHT,
    FT_RENDER_MODE_MONO,
    FT_RENDER_MODE_LCD,
    FT_RENDER_MODE_LCD_V,

    FT_RENDER_MODE_MAX

  } FT_Render_Mode;
# 2709 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Render_Glyph( FT_GlyphSlot slot,
                   FT_Render_Mode render_mode );
# 2733 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  typedef enum FT_Kerning_Mode_
  {
    FT_KERNING_DEFAULT = 0,
    FT_KERNING_UNFITTED,
    FT_KERNING_UNSCALED

  } FT_Kerning_Mode;
# 2811 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Get_Kerning( FT_Face face,
                  FT_UInt left_glyph,
                  FT_UInt right_glyph,
                  FT_UInt kern_mode,
                  FT_Vector *akerning );
# 2840 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Get_Track_Kerning( FT_Face face,
                        FT_Fixed point_size,
                        FT_Int degree,
                        FT_Fixed* akerning );
# 2883 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Get_Glyph_Name( FT_Face face,
                     FT_UInt glyph_index,
                     FT_Pointer buffer,
                     FT_UInt buffer_max );
# 2909 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern const char*
  FT_Get_Postscript_Name( FT_Face face );
# 2941 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Select_Charmap( FT_Face face,
                     FT_Encoding encoding );
# 2970 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Set_Charmap( FT_Face face,
                  FT_CharMap charmap );
# 2992 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Int
  FT_Get_Charmap_Index( FT_CharMap charmap );
# 3020 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_UInt
  FT_Get_Char_Index( FT_Face face,
                     FT_ULong charcode );
# 3068 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_ULong
  FT_Get_First_Char( FT_Face face,
                     FT_UInt *agindex );
# 3102 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_ULong
  FT_Get_Next_Char( FT_Face face,
                    FT_ULong char_code,
                    FT_UInt *agindex );
# 3125 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_UInt
  FT_Get_Name_Index( FT_Face face,
                     FT_String* glyph_name );
# 3201 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Error
  FT_Get_SubGlyph_Info( FT_GlyphSlot glyph,
                        FT_UInt sub_index,
                        FT_Int *p_index,
                        FT_UInt *p_flags,
                        FT_Int *p_arg1,
                        FT_Int *p_arg2,
                        FT_Matrix *p_transform );
# 3289 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_UShort
  FT_Get_FSType_Flags( FT_Face face );
# 3371 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_UInt
  FT_Face_GetCharVariantIndex( FT_Face face,
                               FT_ULong charcode,
                               FT_ULong variantSelector );
# 3407 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Int
  FT_Face_GetCharVariantIsDefault( FT_Face face,
                                   FT_ULong charcode,
                                   FT_ULong variantSelector );
# 3438 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_UInt32*
  FT_Face_GetVariantSelectors( FT_Face face );
# 3471 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_UInt32*
  FT_Face_GetVariantsOfChar( FT_Face face,
                             FT_ULong charcode );
# 3505 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_UInt32*
  FT_Face_GetCharsOfVariant( FT_Face face,
                             FT_ULong variantSelector );
# 3562 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Long
  FT_MulDiv( FT_Long a,
             FT_Long b,
             FT_Long c );
# 3613 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Long
  FT_MulFix( FT_Long a,
             FT_Long b );
# 3642 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Long
  FT_DivFix( FT_Long a,
             FT_Long b );
# 3661 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Fixed
  FT_RoundFix( FT_Fixed a );
# 3680 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Fixed
  FT_CeilFix( FT_Fixed a );
# 3699 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Fixed
  FT_FloorFix( FT_Fixed a );
# 3720 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern void
  FT_Vector_Transform( FT_Vector* vec,
                       const FT_Matrix* matrix );
# 3798 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern void
  FT_Library_Version( FT_Library library,
                      FT_Int *amajor,
                      FT_Int *aminor,
                      FT_Int *apatch );
# 3829 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Bool
  FT_Face_CheckTrueTypePatents( FT_Face face );
# 3856 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h"
  extern FT_Bool
  FT_Face_SetUnpatentedHinting( FT_Face face,
                                FT_Bool value );





# 40 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h" 1
# 37 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h" 1
# 38 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h" 2
# 46 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"

# 69 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  typedef struct FT_Glyph_Class_ FT_Glyph_Class;
# 87 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  typedef struct FT_GlyphRec_* FT_Glyph;
# 108 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  typedef struct FT_GlyphRec_
  {
    FT_Library library;
    const FT_Glyph_Class* clazz;
    FT_Glyph_Format format;
    FT_Vector advance;

  } FT_GlyphRec;
# 127 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  typedef struct FT_BitmapGlyphRec_* FT_BitmapGlyph;
# 160 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  typedef struct FT_BitmapGlyphRec_
  {
    FT_GlyphRec root;
    FT_Int left;
    FT_Int top;
    FT_Bitmap bitmap;

  } FT_BitmapGlyphRec;
# 179 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  typedef struct FT_OutlineGlyphRec_* FT_OutlineGlyph;
# 208 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  typedef struct FT_OutlineGlyphRec_
  {
    FT_GlyphRec root;
    FT_Outline outline;

  } FT_OutlineGlyphRec;
# 234 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  extern FT_Error
  FT_Get_Glyph( FT_GlyphSlot slot,
                FT_Glyph *aglyph );
# 258 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  extern FT_Error
  FT_Glyph_Copy( FT_Glyph source,
                 FT_Glyph *target );
# 287 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  extern FT_Error
  FT_Glyph_Transform( FT_Glyph glyph,
                      FT_Matrix* matrix,
                      FT_Vector* delta );
# 317 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  typedef enum FT_Glyph_BBox_Mode_
  {
    FT_GLYPH_BBOX_UNSCALED = 0,
    FT_GLYPH_BBOX_SUBPIXELS = 0,
    FT_GLYPH_BBOX_GRIDFIT = 1,
    FT_GLYPH_BBOX_TRUNCATE = 2,
    FT_GLYPH_BBOX_PIXELS = 3

  } FT_Glyph_BBox_Mode;
# 413 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  extern void
  FT_Glyph_Get_CBox( FT_Glyph glyph,
                     FT_UInt bbox_mode,
                     FT_BBox *acbox );
# 525 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  extern FT_Error
  FT_Glyph_To_Bitmap( FT_Glyph* the_glyph,
                      FT_Render_Mode render_mode,
                      FT_Vector* origin,
                      FT_Bool destroy );
# 543 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  extern void
  FT_Done_Glyph( FT_Glyph glyph );
# 576 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  extern void
  FT_Matrix_Multiply( const FT_Matrix* a,
                      FT_Matrix* b );
# 596 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftglyph.h"
  extern FT_Error
  FT_Matrix_Invert( FT_Matrix* matrix );






# 41 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h" 1
# 25 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/freetype.h" 1
# 26 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h" 2
# 34 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"

# 108 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern FT_Error
  FT_Outline_Decompose( FT_Outline* outline,
                        const FT_Outline_Funcs* func_interface,
                        void* user );
# 142 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern FT_Error
  FT_Outline_New( FT_Library library,
                  FT_UInt numPoints,
                  FT_Int numContours,
                  FT_Outline *anoutline );


  extern FT_Error
  FT_Outline_New_Internal( FT_Memory memory,
                           FT_UInt numPoints,
                           FT_Int numContours,
                           FT_Outline *anoutline );
# 180 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern FT_Error
  FT_Outline_Done( FT_Library library,
                   FT_Outline* outline );


  extern FT_Error
  FT_Outline_Done_Internal( FT_Memory memory,
                            FT_Outline* outline );
# 204 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern FT_Error
  FT_Outline_Check( FT_Outline* outline );
# 231 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern void
  FT_Outline_Get_CBox( const FT_Outline* outline,
                       FT_BBox *acbox );
# 252 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern void
  FT_Outline_Translate( const FT_Outline* outline,
                        FT_Pos xOffset,
                        FT_Pos yOffset );
# 277 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern FT_Error
  FT_Outline_Copy( const FT_Outline* source,
                   FT_Outline *target );
# 301 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern void
  FT_Outline_Transform( const FT_Outline* outline,
                        const FT_Matrix* matrix );
# 346 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern FT_Error
  FT_Outline_Embolden( FT_Outline* outline,
                       FT_Pos strength );
# 370 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern void
  FT_Outline_Reverse( FT_Outline* outline );
# 405 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern FT_Error
  FT_Outline_Get_Bitmap( FT_Library library,
                         FT_Outline* outline,
                         const FT_Bitmap *abitmap );
# 447 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern FT_Error
  FT_Outline_Render( FT_Library library,
                     FT_Outline* outline,
                     FT_Raster_Params* params );
# 488 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  typedef enum FT_Orientation_
  {
    FT_ORIENTATION_TRUETYPE = 0,
    FT_ORIENTATION_POSTSCRIPT = 1,
    FT_ORIENTATION_FILL_RIGHT = FT_ORIENTATION_TRUETYPE,
    FT_ORIENTATION_FILL_LEFT = FT_ORIENTATION_POSTSCRIPT,
    FT_ORIENTATION_NONE

  } FT_Orientation;
# 521 "/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2/freetype/ftoutln.h"
  extern FT_Orientation
  FT_Outline_Get_Orientation( FT_Outline* outline );






# 42 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 113 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h"
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTPoint.h" 1
# 114 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTBBox.h" 1
# 115 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTBuffer.h" 1
# 116 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2

# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGlyph.h" 1
# 123 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGlyph.h"

# 132 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGlyph.h"
struct _FTGLGlyph;
typedef struct _FTGLglyph FTGLglyph;
# 145 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGlyph.h"
 FTGLglyph *ftglCreateCustomGlyph(FTGLglyph *base, void *data,
    void (*renderCallback) (FTGLglyph *, void *, FTGL_DOUBLE, FTGL_DOUBLE,
                             int, FTGL_DOUBLE *, FTGL_DOUBLE *),
    void (*destroyCallback) (FTGLglyph *, void *));






 void ftglDestroyGlyph(FTGLglyph *glyph);
# 170 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGlyph.h"
 void ftglRenderGlyph(FTGLglyph *glyph, FTGL_DOUBLE penx,
                                 FTGL_DOUBLE peny, int renderMode,
                                 FTGL_DOUBLE *advancex, FTGL_DOUBLE *advancey);






 float ftglGetGlyphAdvance(FTGLglyph *glyph);
# 188 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGlyph.h"
 void ftglGetGlyphBBox(FTGLglyph *glyph, float bounds[6]);







 FT_Error ftglGetGlyphError(FTGLglyph* glyph);


# 118 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTBitmapGlyph.h" 1
# 69 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTBitmapGlyph.h"








 FTGLglyph *ftglCreateBitmapGlyph(FT_GlyphSlot glyph);


# 119 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTBufferGlyph.h" 1
# 120 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2

# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTOutlineGlyph.h" 1
# 75 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTOutlineGlyph.h"

# 88 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTOutlineGlyph.h"
 FTGLglyph *ftglCreateOutlineGlyph(FT_GlyphSlot glyph, float outset,
                                  int useDisplayList);


# 122 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2


# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTTextureGlyph.h" 1
# 79 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTTextureGlyph.h"

# 92 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTTextureGlyph.h"
 FTGLglyph *ftglCreateTextureGlyph(FT_GlyphSlot glyph, int id,
                                              int xOffset, int yOffset,
                                              int width, int height);


# 125 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2

# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h" 1
# 389 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"

# 398 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
struct _FTGLFont;
typedef struct _FTGLfont FTGLfont;
# 409 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
 FTGLfont *ftglCreateCustomFont(char const *fontFilePath,
                                           void *data,
                   FTGLglyph * (*makeglyphCallback) (FT_GlyphSlot, void *));






 void ftglDestroyFont(FTGLfont* font);
# 429 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
 int ftglAttachFile(FTGLfont* font, const char* path);
# 441 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
 int ftglAttachData(FTGLfont* font, const unsigned char * data,
                               size_t size);
# 451 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
 int ftglSetFontCharMap(FTGLfont* font, FT_Encoding encoding);







 unsigned int ftglGetFontCharMapCount(FTGLfont* font);







 FT_Encoding* ftglGetFontCharMapList(FTGLfont* font);
# 478 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
 int ftglSetFontFaceSize(FTGLfont* font, unsigned int size,
                                    unsigned int res);







 unsigned int ftglGetFontFaceSize(FTGLfont* font);
# 496 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
 void ftglSetFontDepth(FTGLfont* font, float depth);
# 507 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
 void ftglSetFontOutset(FTGLfont* font, float front, float back);
# 516 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
 void ftglSetFontDisplayList(FTGLfont* font, int useList);







 float ftglGetFontAscender(FTGLfont* font);







 float ftglGetFontDescender(FTGLfont* font);







 float ftglGetFontLineHeight(FTGLfont* font);
# 552 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
 void ftglGetFontBBox(FTGLfont* font, const char *string,
                                 int len, float bounds[6]);
# 562 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
 float ftglGetFontAdvance(FTGLfont* font, const char *string);
# 571 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTFont.h"
 void ftglRenderFont(FTGLfont* font, const char *string, int mode);







 FT_Error ftglGetFontError(FTGLfont* font);


# 127 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGLBitmapFont.h" 1
# 88 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGLBitmapFont.h"

# 98 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGLBitmapFont.h"
 FTGLfont *ftglCreateBitmapFont(const char *file);


# 128 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTBufferFont.h" 1
# 84 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTBufferFont.h"

# 94 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTBufferFont.h"
 FTGLfont *ftglCreateBufferFont(const char *file);


# 129 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2

# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGLOutlineFont.h" 1
# 88 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGLOutlineFont.h"

# 98 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGLOutlineFont.h"
 FTGLfont *ftglCreateOutlineFont(const char *file);


# 131 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2


# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGLTextureFont.h" 1
# 88 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGLTextureFont.h"

# 98 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTGLTextureFont.h"
 FTGLfont *ftglCreateTextureFont(const char *file);


# 134 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2

# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTLayout.h" 1
# 145 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTLayout.h"





struct _FTGLlayout;
typedef struct _FTGLlayout FTGLlayout;






 void ftglDestroyLayout(FTGLlayout* layout);
# 168 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTLayout.h"
 void ftglGetLayoutBBox(FTGLlayout *layout, const char* string,
                                   float bounds[6]);
# 178 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTLayout.h"
 void ftglRenderLayout(FTGLlayout *layout, const char *string,
                                  int mode);







 FT_Error ftglGetLayoutError(FTGLlayout* layout);


# 136 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 1 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTSimpleLayout.h" 1
# 172 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/FTSimpleLayout.h"


 FTGLlayout *ftglCreateSimpleLayout(void);

 void ftglSetLayoutFont(FTGLlayout *, FTGLfont*);
 FTGLfont *ftglGetLayoutFont(FTGLlayout *);

 void ftglSetLayoutLineLength(FTGLlayout *, const float);
 float ftglGetLayoutLineLength(FTGLlayout *);

 void ftglSetLayoutAlignment(FTGLlayout *, const int);
 int ftglGetLayoutAlignement(FTGLlayout *);

 void ftglSetLayoutLineSpacing(FTGLlayout *, const float);
 float ftglGetLayoutLineSpacing(FTGLlayout *);


# 137 "/usr/local/iphone/iPhoneSimulator3.1.2/include/FTGL/ftgles.h" 2
# 96 "ftgl.c" 2







































# 146 "ftgl.c"
static long ___H__20_ftgl (___processor_state ___ps){ register long ___pc, ___start, ___temp; register int ___s32_temp; register unsigned int ___u32_temp; register long ___s64_temp; register unsigned long ___u64_temp; register long ___r0; register long ___r1;
static void *___hlbl_tbl[]={
0,
&&___L0__20_ftgl,
0}; if (__builtin_expect((___ps == 0),0)) return ((long)(___hlbl_tbl));
___r0=___ps->r[0]; ___r1=___ps->r[1]; ___start=___lp+((1)*4*8); ___pc=___ps->pc; goto *((void*)(((___label_struct*)(___pc-1))->host_label));
___L0__20_ftgl:
   if (__builtin_expect((___ps->na==0),1)) {} else
   {___ps->temp1=___start+((1 -1 +0)*4*8); { {___pc=(&___gstate)->handler_wrong_nargs;goto ___jumpext;}}}
___L__20_ftgl:
   ___G_FTGL__RENDER__ALL.val=(((long)(65535L))<<2);
   ___G_ftglCreateTextureFont.val=___start+((3 -1)*4*8);
   ___G_ftglCreateBufferFont.val=___start+((6 -1)*4*8);
   ___G_ftglSetFontFaceSize.val=___start+((9 -1)*4*8);
   ___G_ftglGetFontFaceSize.val=___start+((12 -1)*4*8);
   ___G_ftglRenderFont.val=___start+((21 -1)*4*8);
   ___G_ftglGetFontError.val=___start+((18 -1)*4*8);
   ___r1=(((((long)(-5))<<2)+2));
   { {___pc=___r0;goto ___jump;}}
___jump: {void *___host_label=((void*)(((___label_struct*)(___pc-1))->host_label)); if (__builtin_expect((((___label_struct*)(___pc-1))->host == ___H__20_ftgl),1)) goto *___host_label;} ___jumpext:
___ps->pc=___pc; ___ps->r[1]=___r1; return ___pc; }
# 178 "ftgl.c"
static long ___H__20_ftgl_23_0 (___processor_state ___ps){ register long ___pc, ___start, ___temp; register int ___s32_temp; register unsigned int ___u32_temp; register long ___s64_temp; register unsigned long ___u64_temp; register long *___fp; register long ___r0; register long ___r1;
static void *___hlbl_tbl[]={
0,
&&___L0__20_ftgl_23_0,
&&___L1__20_ftgl_23_0,
0}; if (__builtin_expect((___ps == 0),0)) return ((long)(___hlbl_tbl));
___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1]; ___start=___lp+((3)*4*8); ___pc=___ps->pc; goto *((void*)(((___label_struct*)(___pc-1))->host_label));
___L0__20_ftgl_23_0:
   if (__builtin_expect((___ps->na==1),1)) {} else
   {___ps->temp1=___start+((3 -3 +0)*4*8); { {___pc=(&___gstate)->handler_wrong_nargs;goto ___jumpext;}}}
___L__20_ftgl_23_0:
   ___fp[-(1)]=(___r1);
   ___fp[-(2)]=(___r0);
   ___r0=___start+((3 -3 +1)*4*8);
   ___fp-=(4);


___fp[-(-0)]=(___r0); ___ps->fp=___fp; ___ps->r[0]=___r0; {long ___err=(((long)(0))<<2);long ___errmsg=((((long)(-1))<<2)+2);void* ___result_voidstar;

char* ___arg1;
if ((___err=___SCMOBJ_to_CHARSTRING(___ps->fp[-((1 -(((((1 +1)+1)+(4)-1)/(4))*(4))))],&___arg1,1))==(((long)(0))<<2)){
___ps->fp=___fp; ___ps->r[0]=___r0; { ___jmpbuf_struct ___jbuf, *___old_catcher = ___ps->catcher; ___ps->catcher = &___jbuf; ___err = setjmp (___jbuf.buf); if (___err==(((long)(0))<<2)) {

___result_voidstar = ftglCreateTextureFont(___arg1);



if ((___err=___POINTER_to_SCMOBJ((void*)___result_voidstar,(((long)((((long*)((((long)(___cns_tbl))+((8)-1))&~((8)-1)))+0*(2 +1))))+(3)),___release_pointer,&___ps->r[1],127))==(((long)(0))<<2)){

___release_scmobj(___ps->r[1]);}
 } ___ps->catcher = ___old_catcher; } ___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1];
___release_string(___arg1);}


if ((___err!=(((long)(0))<<2))&&(___err!=(((long)((((((int)(-1))<<29)+(((int)(448))<<16)+(0))+0)))<<2))) {___ps->temp1=___err;___ps->temp2=___errmsg;___ps->temp3=___start+((3 -3 +0)*4*8);{___ps->na=1; {___pc=(&___gstate)->handler_cfun_conv_error;;goto ___jumpext;}}} ___propagate_error (___err);
} ___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1];


   { {___pc=___r0;goto ___jump;}}
___L1__20_ftgl_23_0:
   ___fp-=(-4);
   { {___pc=___fp[-(2)];goto ___jump;}}
___jump: {void *___host_label=((void*)(((___label_struct*)(___pc-1))->host_label)); if (__builtin_expect((((___label_struct*)(___pc-1))->host == ___H__20_ftgl_23_0),1)) goto *___host_label;} ___jumpext:
___ps->pc=___pc; ___ps->fp=___fp; ___ps->r[0]=___r0; return ___pc; }
# 233 "ftgl.c"
static long ___H__20_ftgl_23_1 (___processor_state ___ps){ register long ___pc, ___start, ___temp; register int ___s32_temp; register unsigned int ___u32_temp; register long ___s64_temp; register unsigned long ___u64_temp; register long *___fp; register long ___r0; register long ___r1;
static void *___hlbl_tbl[]={
0,
&&___L0__20_ftgl_23_1,
&&___L1__20_ftgl_23_1,
0}; if (__builtin_expect((___ps == 0),0)) return ((long)(___hlbl_tbl));
___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1]; ___start=___lp+((6)*4*8); ___pc=___ps->pc; goto *((void*)(((___label_struct*)(___pc-1))->host_label));
___L0__20_ftgl_23_1:
   if (__builtin_expect((___ps->na==1),1)) {} else
   {___ps->temp1=___start+((6 -6 +0)*4*8); { {___pc=(&___gstate)->handler_wrong_nargs;goto ___jumpext;}}}
___L__20_ftgl_23_1:
   ___fp[-(1)]=(___r1);
   ___fp[-(2)]=(___r0);
   ___r0=___start+((6 -6 +1)*4*8);
   ___fp-=(4);


___fp[-(-0)]=(___r0); ___ps->fp=___fp; ___ps->r[0]=___r0; {long ___err=(((long)(0))<<2);long ___errmsg=((((long)(-1))<<2)+2);void* ___result_voidstar;

char* ___arg1;
if ((___err=___SCMOBJ_to_CHARSTRING(___ps->fp[-((1 -(((((1 +1)+1)+(4)-1)/(4))*(4))))],&___arg1,1))==(((long)(0))<<2)){
___ps->fp=___fp; ___ps->r[0]=___r0; { ___jmpbuf_struct ___jbuf, *___old_catcher = ___ps->catcher; ___ps->catcher = &___jbuf; ___err = setjmp (___jbuf.buf); if (___err==(((long)(0))<<2)) {

___result_voidstar = ftglCreateBufferFont(___arg1);



if ((___err=___POINTER_to_SCMOBJ((void*)___result_voidstar,(((long)((((long*)((((long)(___cns_tbl))+((8)-1))&~((8)-1)))+0*(2 +1))))+(3)),___release_pointer,&___ps->r[1],127))==(((long)(0))<<2)){

___release_scmobj(___ps->r[1]);}
 } ___ps->catcher = ___old_catcher; } ___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1];
___release_string(___arg1);}


if ((___err!=(((long)(0))<<2))&&(___err!=(((long)((((((int)(-1))<<29)+(((int)(448))<<16)+(0))+0)))<<2))) {___ps->temp1=___err;___ps->temp2=___errmsg;___ps->temp3=___start+((6 -6 +0)*4*8);{___ps->na=1; {___pc=(&___gstate)->handler_cfun_conv_error;;goto ___jumpext;}}} ___propagate_error (___err);
} ___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1];


   { {___pc=___r0;goto ___jump;}}
___L1__20_ftgl_23_1:
   ___fp-=(-4);
   { {___pc=___fp[-(2)];goto ___jump;}}
___jump: {void *___host_label=((void*)(((___label_struct*)(___pc-1))->host_label)); if (__builtin_expect((((___label_struct*)(___pc-1))->host == ___H__20_ftgl_23_1),1)) goto *___host_label;} ___jumpext:
___ps->pc=___pc; ___ps->fp=___fp; ___ps->r[0]=___r0; return ___pc; }
# 288 "ftgl.c"
static long ___H__20_ftgl_23_2 (___processor_state ___ps){ register long ___pc, ___start, ___temp; register int ___s32_temp; register unsigned int ___u32_temp; register long ___s64_temp; register unsigned long ___u64_temp; register long *___fp; register long ___r0; register long ___r1; register long ___r2; register long ___r3;
static void *___hlbl_tbl[]={
0,
&&___L0__20_ftgl_23_2,
&&___L1__20_ftgl_23_2,
0}; if (__builtin_expect((___ps == 0),0)) return ((long)(___hlbl_tbl));
___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1]; ___r2=___ps->r[2]; ___r3=___ps->r[3]; ___start=___lp+((9)*4*8); ___pc=___ps->pc; goto *((void*)(((___label_struct*)(___pc-1))->host_label));
___L0__20_ftgl_23_2:
   if (__builtin_expect((___ps->na==3),1)) {} else
   {___ps->temp1=___start+((9 -9 +0)*4*8); { {___pc=(&___gstate)->handler_wrong_nargs;goto ___jumpext;}}}
___L__20_ftgl_23_2:
   ___fp[-(1)]=(___r1);
   ___fp[-(2)]=(___r2);
   ___fp[-(3)]=(___r3);
   ___fp[-(4)]=(___r0);
   ___r0=___start+((9 -9 +1)*4*8);
   ___fp-=(8);

___fp[-(-0)]=(___r0); ___ps->fp=___fp; ___ps->r[0]=___r0; {long ___err=(((long)(0))<<2);long ___errmsg=((((long)(-1))<<2)+2);unsigned int ___result;

void* ___arg1_voidstar;

if ((___err=___SCMOBJ_to_POINTER(___ps->fp[-((1 -(((((3 +1)+1)+(4)-1)/(4))*(4))))],(void**)&___arg1_voidstar,(((long)((((long*)((((long)(___cns_tbl))+((8)-1))&~((8)-1)))+0*(2 +1))))+(3)),1))==(((long)(0))<<2)){

unsigned int ___arg2;
if ((___err=___SCMOBJ_to_UINT(___ps->fp[-((2 -(((((3 +1)+1)+(4)-1)/(4))*(4))))],&___arg2,2))==(((long)(0))<<2)){

unsigned int ___arg3;
if ((___err=___SCMOBJ_to_UINT(___ps->fp[-((3 -(((((3 +1)+1)+(4)-1)/(4))*(4))))],&___arg3,3))==(((long)(0))<<2)){


___result = ftglSetFontFaceSize(((FTGLFont*)(___arg1_voidstar)),___arg2,___arg3);



if ((___err=___UINT_to_SCMOBJ(___result,&___ps->r[1],127))==(((long)(0))<<2)){

___release_scmobj(___ps->r[1]);}

}


}


}



if ((___err!=(((long)(0))<<2))&&(___err!=(((long)((((((int)(-1))<<29)+(((int)(448))<<16)+(0))+0)))<<2))) {___ps->temp1=___err;___ps->temp2=___errmsg;___ps->temp3=___start+((9 -9 +0)*4*8);{___ps->na=3; {___pc=(&___gstate)->handler_cfun_conv_error;;goto ___jumpext;}}}
} ___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1]; ___r2=___ps->r[2]; ___r3=___ps->r[3];

   { {___pc=___r0;goto ___jump;}}
___L1__20_ftgl_23_2:
   ___fp-=(-8);
   { {___pc=___fp[-(4)];goto ___jump;}}
___jump: {void *___host_label=((void*)(((___label_struct*)(___pc-1))->host_label)); if (__builtin_expect((((___label_struct*)(___pc-1))->host == ___H__20_ftgl_23_2),1)) goto *___host_label;} ___jumpext:
___ps->pc=___pc; ___ps->fp=___fp; ___ps->r[0]=___r0; return ___pc; }
# 357 "ftgl.c"
static long ___H__20_ftgl_23_3 (___processor_state ___ps){ register long ___pc, ___start, ___temp; register int ___s32_temp; register unsigned int ___u32_temp; register long ___s64_temp; register unsigned long ___u64_temp; register long *___fp; register long ___r0; register long ___r1;
static void *___hlbl_tbl[]={
0,
&&___L0__20_ftgl_23_3,
&&___L1__20_ftgl_23_3,
0}; if (__builtin_expect((___ps == 0),0)) return ((long)(___hlbl_tbl));
___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1]; ___start=___lp+((12)*4*8); ___pc=___ps->pc; goto *((void*)(((___label_struct*)(___pc-1))->host_label));
___L0__20_ftgl_23_3:
   if (__builtin_expect((___ps->na==1),1)) {} else
   {___ps->temp1=___start+((12 -12 +0)*4*8); { {___pc=(&___gstate)->handler_wrong_nargs;goto ___jumpext;}}}
___L__20_ftgl_23_3:
   ___fp[-(1)]=(___r1);
   ___fp[-(2)]=(___r0);
   ___r0=___start+((12 -12 +1)*4*8);
   ___fp-=(4);

___fp[-(-0)]=(___r0); ___ps->fp=___fp; ___ps->r[0]=___r0; {long ___err=(((long)(0))<<2);long ___errmsg=((((long)(-1))<<2)+2);unsigned int ___result;

void* ___arg1_voidstar;

if ((___err=___SCMOBJ_to_POINTER(___ps->fp[-((1 -(((((1 +1)+1)+(4)-1)/(4))*(4))))],(void**)&___arg1_voidstar,(((long)((((long*)((((long)(___cns_tbl))+((8)-1))&~((8)-1)))+0*(2 +1))))+(3)),1))==(((long)(0))<<2)){


___result = ftglGetFontFaceSize(((FTGLFont*)(___arg1_voidstar)));



if ((___err=___UINT_to_SCMOBJ(___result,&___ps->r[1],127))==(((long)(0))<<2)){

___release_scmobj(___ps->r[1]);}

}



if ((___err!=(((long)(0))<<2))&&(___err!=(((long)((((((int)(-1))<<29)+(((int)(448))<<16)+(0))+0)))<<2))) {___ps->temp1=___err;___ps->temp2=___errmsg;___ps->temp3=___start+((12 -12 +0)*4*8);{___ps->na=1; {___pc=(&___gstate)->handler_cfun_conv_error;;goto ___jumpext;}}}
} ___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1];

   { {___pc=___r0;goto ___jump;}}
___L1__20_ftgl_23_3:
   ___fp-=(-4);
   { {___pc=___fp[-(2)];goto ___jump;}}
___jump: {void *___host_label=((void*)(((___label_struct*)(___pc-1))->host_label)); if (__builtin_expect((((___label_struct*)(___pc-1))->host == ___H__20_ftgl_23_3),1)) goto *___host_label;} ___jumpext:
___ps->pc=___pc; ___ps->fp=___fp; ___ps->r[0]=___r0; return ___pc; }
# 412 "ftgl.c"
static long ___H__20_ftgl_23_4 (___processor_state ___ps){ register long ___pc, ___start, ___temp; register int ___s32_temp; register unsigned int ___u32_temp; register long ___s64_temp; register unsigned long ___u64_temp; register long *___fp; register long ___r0; register long ___r1; register long ___r2; register long ___r3;
static void *___hlbl_tbl[]={
0,
&&___L0__20_ftgl_23_4,
&&___L1__20_ftgl_23_4,
0}; if (__builtin_expect((___ps == 0),0)) return ((long)(___hlbl_tbl));
___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1]; ___r2=___ps->r[2]; ___r3=___ps->r[3]; ___start=___lp+((15)*4*8); ___pc=___ps->pc; goto *((void*)(((___label_struct*)(___pc-1))->host_label));
___L0__20_ftgl_23_4:
   if (__builtin_expect((___ps->na==3),1)) {} else
   {___ps->temp1=___start+((15 -15 +0)*4*8); { {___pc=(&___gstate)->handler_wrong_nargs;goto ___jumpext;}}}
___L__20_ftgl_23_4:
   ___fp[-(1)]=(___r1);
   ___fp[-(2)]=(___r2);
   ___fp[-(3)]=(___r3);
   ___fp[-(4)]=(___r0);
   ___r0=___start+((15 -15 +1)*4*8);
   ___fp-=(8);

___fp[-(-0)]=(___r0); ___ps->fp=___fp; ___ps->r[0]=___r0; {long ___err=(((long)(0))<<2);long ___errmsg=((((long)(-1))<<2)+2);

void* ___arg1_voidstar;

if ((___err=___SCMOBJ_to_POINTER(___ps->fp[-((1 -(((((3 +1)+1)+(4)-1)/(4))*(4))))],(void**)&___arg1_voidstar,(((long)((((long*)((((long)(___cns_tbl))+((8)-1))&~((8)-1)))+0*(2 +1))))+(3)),1))==(((long)(0))<<2)){

char* ___arg2;
if ((___err=___SCMOBJ_to_CHARSTRING(___ps->fp[-((2 -(((((3 +1)+1)+(4)-1)/(4))*(4))))],&___arg2,2))==(((long)(0))<<2)){

unsigned int ___arg3;
if ((___err=___SCMOBJ_to_UINT(___ps->fp[-((3 -(((((3 +1)+1)+(4)-1)/(4))*(4))))],&___arg3,3))==(((long)(0))<<2)){
___ps->fp=___fp; ___ps->r[0]=___r0; { ___jmpbuf_struct ___jbuf, *___old_catcher = ___ps->catcher; ___ps->catcher = &___jbuf; ___err = setjmp (___jbuf.buf); if (___err==(((long)(0))<<2)) {

ftglRenderFont(((FTGLFont*)(___arg1_voidstar)),___arg2,___arg3);



___ps->r[1] = ((((long)(-5))<<2)+2);
 } ___ps->catcher = ___old_catcher; } ___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1]; ___r2=___ps->r[2]; ___r3=___ps->r[3];
}


___release_string(___arg2);}


}



if ((___err!=(((long)(0))<<2))&&(___err!=(((long)((((((int)(-1))<<29)+(((int)(448))<<16)+(0))+0)))<<2))) {___ps->temp1=___err;___ps->temp2=___errmsg;___ps->temp3=___start+((15 -15 +0)*4*8);{___ps->na=3; {___pc=(&___gstate)->handler_cfun_conv_error;;goto ___jumpext;}}} ___propagate_error (___err);
} ___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1]; ___r2=___ps->r[2]; ___r3=___ps->r[3];

   { {___pc=___r0;goto ___jump;}}
___L1__20_ftgl_23_4:
   ___fp-=(-8);
   { {___pc=___fp[-(4)];goto ___jump;}}
___jump: {void *___host_label=((void*)(((___label_struct*)(___pc-1))->host_label)); if (__builtin_expect((((___label_struct*)(___pc-1))->host == ___H__20_ftgl_23_4),1)) goto *___host_label;} ___jumpext:
___ps->pc=___pc; ___ps->fp=___fp; ___ps->r[0]=___r0; return ___pc; }
# 479 "ftgl.c"
static long ___H__20_ftgl_23_5 (___processor_state ___ps){ register long ___pc, ___start, ___temp; register int ___s32_temp; register unsigned int ___u32_temp; register long ___s64_temp; register unsigned long ___u64_temp; register long *___fp; register long ___r0; register long ___r1;
static void *___hlbl_tbl[]={
0,
&&___L0__20_ftgl_23_5,
&&___L1__20_ftgl_23_5,
0}; if (__builtin_expect((___ps == 0),0)) return ((long)(___hlbl_tbl));
___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1]; ___start=___lp+((18)*4*8); ___pc=___ps->pc; goto *((void*)(((___label_struct*)(___pc-1))->host_label));
___L0__20_ftgl_23_5:
   if (__builtin_expect((___ps->na==1),1)) {} else
   {___ps->temp1=___start+((18 -18 +0)*4*8); { {___pc=(&___gstate)->handler_wrong_nargs;goto ___jumpext;}}}
___L__20_ftgl_23_5:
   ___fp[-(1)]=(___r1);
   ___fp[-(2)]=(___r0);
   ___r0=___start+((18 -18 +1)*4*8);
   ___fp-=(4);

___fp[-(-0)]=(___r0); ___ps->fp=___fp; ___ps->r[0]=___r0; {long ___err=(((long)(0))<<2);long ___errmsg=((((long)(-1))<<2)+2);unsigned int ___result;

void* ___arg1_voidstar;

if ((___err=___SCMOBJ_to_POINTER(___ps->fp[-((1 -(((((1 +1)+1)+(4)-1)/(4))*(4))))],(void**)&___arg1_voidstar,(((long)((((long*)((((long)(___cns_tbl))+((8)-1))&~((8)-1)))+0*(2 +1))))+(3)),1))==(((long)(0))<<2)){


___result = ftglGetFontError(((FTGLFont*)(___arg1_voidstar)));



if ((___err=___UINT_to_SCMOBJ(___result,&___ps->r[1],127))==(((long)(0))<<2)){

___release_scmobj(___ps->r[1]);}

}



if ((___err!=(((long)(0))<<2))&&(___err!=(((long)((((((int)(-1))<<29)+(((int)(448))<<16)+(0))+0)))<<2))) {___ps->temp1=___err;___ps->temp2=___errmsg;___ps->temp3=___start+((18 -18 +0)*4*8);{___ps->na=1; {___pc=(&___gstate)->handler_cfun_conv_error;;goto ___jumpext;}}}
} ___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1];

   { {___pc=___r0;goto ___jump;}}
___L1__20_ftgl_23_5:
   ___fp-=(-4);
   { {___pc=___fp[-(2)];goto ___jump;}}
___jump: {void *___host_label=((void*)(((___label_struct*)(___pc-1))->host_label)); if (__builtin_expect((((___label_struct*)(___pc-1))->host == ___H__20_ftgl_23_5),1)) goto *___host_label;} ___jumpext:
___ps->pc=___pc; ___ps->fp=___fp; ___ps->r[0]=___r0; return ___pc; }
# 534 "ftgl.c"
static long ___H_ftglRenderFont (___processor_state ___ps){ register long ___pc, ___start, ___temp; register int ___s32_temp; register unsigned int ___u32_temp; register long ___s64_temp; register unsigned long ___u64_temp; register long *___fp; register long ___r0; register long ___r1; register long ___r2; register long ___r3; register long ___r4;
static void *___hlbl_tbl[]={
0,
&&___L0_ftglRenderFont,
&&___L1_ftglRenderFont,
&&___L2_ftglRenderFont,
&&___L3_ftglRenderFont,
&&___L4_ftglRenderFont,
&&___L5_ftglRenderFont,
0}; if (__builtin_expect((___ps == 0),0)) return ((long)(___hlbl_tbl));
___fp=___ps->fp; ___r0=___ps->r[0]; ___r1=___ps->r[1]; ___r2=___ps->r[2]; ___r3=___ps->r[3]; ___r4=___ps->r[4]; ___start=___lp+((21)*4*8); ___pc=___ps->pc; goto *((void*)(((___label_struct*)(___pc-1))->host_label));
___L0_ftglRenderFont:
   if (__builtin_expect((___ps->na==2),1)) {___r3=(((((long)(-3))<<2)+2));} else
   if (___ps->na>=0){___ps->temp1=___start+((21 -21 +0)*4*8); { {___pc=(&___gstate)->handler_get_rest;goto ___jumpext;}}}
___L_ftglRenderFont:
   if (!((___r3)==((((long)(-1))<<2)+2))) {
   goto ___L7_ftglRenderFont;
   }
   ___r3=(___G_FTGL__RENDER__ALL.val);
   if (__builtin_expect((___fp<___ps->stack_trip),0)) {___ps->temp1=___start+((21 -21 +1)*4*8); { {___pc=(&___gstate)->handler_stack_limit;goto ___jumpext;}} }
___L1_ftglRenderFont:
   goto ___L6_ftglRenderFont;
___L2_ftglRenderFont:
   ___r3=(___r1);
   ___r2=(___fp[-(-1)]);
   ___r1=(___fp[-(-2)]);
   ___r0=___fp[-(-3)];
   ___fp-=(-4);
   if (__builtin_expect((___fp<___ps->stack_trip),0)) {___ps->temp1=___start+((21 -21 +3)*4*8); { {___pc=(&___gstate)->handler_stack_limit;goto ___jumpext;}} }
___L3_ftglRenderFont:
___L6_ftglRenderFont:
   {___ps->na=3; {___pc=___start+((15 -21)*4*8);goto ___jump;}}
___L7_ftglRenderFont:
   if (!((___G_car.val)==(___G_car.prm))) {
   goto ___L8_ftglRenderFont;
   }
   if (!(((___r3)&((1<<2)-1))==(3))) {
   goto ___L8_ftglRenderFont;
   }
   ___r3=((*((((long*)((___r3)-(3)))+1)+1)));
   if (__builtin_expect((___fp<___ps->stack_trip),0)) {___ps->temp1=___start+((21 -21 +4)*4*8); { {___pc=(&___gstate)->handler_stack_limit;goto ___jumpext;}} }
___L4_ftglRenderFont:
   goto ___L6_ftglRenderFont;
___L8_ftglRenderFont:
   ___fp[-(1)]=(___r0);
   ___fp[-(2)]=(___r1);
   ___fp[-(3)]=(___r2);
   ___r1=(___r3);
   ___r0=___start+((21 -21 +2)*4*8);
   ___fp-=(4);
   if (__builtin_expect((___fp<___ps->stack_trip),0)) {___ps->temp1=___start+((21 -21 +5)*4*8); { {___pc=(&___gstate)->handler_stack_limit;goto ___jumpext;}} }
___L5_ftglRenderFont:
   {___ps->na=1; ___r4=___G_car.val; if (__builtin_expect((((((___temp=(___r4)))&((1<<2)-1))==1&&((((*((long*)((___temp)-(1)))))&(((1<<5)-1)<<3))==(((14))<<3)))),1)) {___pc=((___label_struct*)(___r4-1))->entry_or_descr;goto ___jump;} ___ps->temp4=(long)&___G_car;{ {___pc=(&___gstate)->handler_not_proc_glo;goto ___jumpext;}}}
___jump: {void *___host_label=((void*)(((___label_struct*)(___pc-1))->host_label)); if (__builtin_expect((((___label_struct*)(___pc-1))->host == ___H_ftglRenderFont),1)) goto *___host_label;} ___jumpext:
___ps->pc=___pc; ___ps->fp=___fp; ___ps->r[0]=___r0; ___ps->r[1]=___r1; ___ps->r[2]=___r2; ___ps->r[3]=___r3; ___ps->r[4]=___r4; return ___pc; }




static ___label_struct ___lbl_tbl[]={
 {((((1<<3))<<(3 +5))+((0)<<3)+(6)),((((long)(-1))<<2)+2),((void*)(" ftgl")),((___host)(0))}
,{((((0<<12)+0)<<(3 +5))+((14)<<3)+(6)),0,0,___H__20_ftgl}
,{((((2<<3))<<(3 +5))+((0)<<3)+(6)),((((long)(-1))<<2)+2),((void*)(" ftgl#0")),((___host)(0))}
,{((((0<<12)+1)<<(3 +5))+((14)<<3)+(6)),0,0,___H__20_ftgl_23_0}
,{((((3<<3))<<(3 +5))+((15)<<3)+(6)),(0x3L<<12)+(1<<7)+(2<<2)+1,0,___H__20_ftgl_23_0}
,{((((2<<3))<<(3 +5))+((0)<<3)+(6)),((((long)(-1))<<2)+2),((void*)(" ftgl#1")),((___host)(0))}
,{((((0<<12)+1)<<(3 +5))+((14)<<3)+(6)),0,0,___H__20_ftgl_23_1}
,{((((3<<3))<<(3 +5))+((15)<<3)+(6)),(0x3L<<12)+(1<<7)+(2<<2)+1,0,___H__20_ftgl_23_1}
,{((((2<<3))<<(3 +5))+((0)<<3)+(6)),((((long)(-1))<<2)+2),((void*)(" ftgl#2")),((___host)(0))}
,{((((0<<12)+3)<<(3 +5))+((14)<<3)+(6)),0,0,___H__20_ftgl_23_2}
,{((((3<<3))<<(3 +5))+((15)<<3)+(6)),(0xfL<<12)+(3<<7)+(4<<2)+1,0,___H__20_ftgl_23_2}
,{((((2<<3))<<(3 +5))+((0)<<3)+(6)),((((long)(-1))<<2)+2),((void*)(" ftgl#3")),((___host)(0))}
,{((((0<<12)+1)<<(3 +5))+((14)<<3)+(6)),0,0,___H__20_ftgl_23_3}
,{((((3<<3))<<(3 +5))+((15)<<3)+(6)),(0x3L<<12)+(1<<7)+(2<<2)+1,0,___H__20_ftgl_23_3}
,{((((2<<3))<<(3 +5))+((0)<<3)+(6)),((((long)(-1))<<2)+2),((void*)(" ftgl#4")),((___host)(0))}
,{((((0<<12)+3)<<(3 +5))+((14)<<3)+(6)),0,0,___H__20_ftgl_23_4}
,{((((3<<3))<<(3 +5))+((15)<<3)+(6)),(0xfL<<12)+(3<<7)+(4<<2)+1,0,___H__20_ftgl_23_4}
,{((((2<<3))<<(3 +5))+((0)<<3)+(6)),((((long)(-1))<<2)+2),((void*)(" ftgl#5")),((___host)(0))}
,{((((0<<12)+1)<<(3 +5))+((14)<<3)+(6)),0,0,___H__20_ftgl_23_5}
,{((((3<<3))<<(3 +5))+((15)<<3)+(6)),(0x3L<<12)+(1<<7)+(2<<2)+1,0,___H__20_ftgl_23_5}
,{((((6<<3))<<(3 +5))+((0)<<3)+(6)),((((long)(-1))<<2)+2),((void*)(0)),((___host)(0))}
,{((((0<<12)+3)<<(3 +5))+((14)<<3)+(6)),0,0,___H_ftglRenderFont}
,{((((3<<3))<<(3 +5))+((15)<<3)+(6)),(0x3fL<<12)+(0<<7)+(0<<2)+2,0,___H_ftglRenderFont}
,{((((3<<3))<<(3 +5))+((15)<<3)+(6)),(0x7L<<12)+(0<<7)+(3<<2)+1,0,___H_ftglRenderFont}
,{((((3<<3))<<(3 +5))+((15)<<3)+(6)),(0x3fL<<12)+(0<<7)+(0<<2)+2,0,___H_ftglRenderFont}
,{((((3<<3))<<(3 +5))+((15)<<3)+(6)),(0x3fL<<12)+(0<<7)+(0<<2)+2,0,___H_ftglRenderFont}
,{((((3<<3))<<(3 +5))+((15)<<3)+(6)),(0x3f7L<<12)+(0<<7)+(4<<2)+2,0,___H_ftglRenderFont}
};

static long ___init_proc (void){
___G__20_ftgl.prm=___G__20_ftgl.val=___lp+((1)*4*8);
___G__20_ftgl_23_0.prm=___G__20_ftgl_23_0.val=___lp+((3)*4*8);
___G__20_ftgl_23_1.prm=___G__20_ftgl_23_1.val=___lp+((6)*4*8);
___G__20_ftgl_23_2.prm=___G__20_ftgl_23_2.val=___lp+((9)*4*8);
___G__20_ftgl_23_3.prm=___G__20_ftgl_23_3.val=___lp+((12)*4*8);
___G__20_ftgl_23_4.prm=___G__20_ftgl_23_4.val=___lp+((15)*4*8);
___G__20_ftgl_23_5.prm=___G__20_ftgl_23_5.val=___lp+((18)*4*8);
return (((long)(0))<<2); }

static ___module_struct ___module_descr = { 405002, 0, " ftgl", 1, 0,0,0,0, ___sym_tbl,1,0, 0,0,0, &___lp,___lbl_tbl,27, 0,0, ___cns_tbl,1, 0,0, ___init_proc, 0, 0, 0,0,0,0,0,0,0,0 }; ___mod_or_lnk ____20_ftgl (___global_state_struct *___gs) {
___sym_tbl[0]=(long*)(___S_FTGLFont_2a_);
return (___mod_or_lnk)&___module_descr; }
