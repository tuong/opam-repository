#ifndef JANE_H
#define JANE_H

#ifndef CAML_NAME_SPACE
#define CAML_NAME_SPACE
#endif

#if __GNUC__ >= 3
# define inline inline __attribute__ ((always_inline))
# define __pure __attribute__ ((pure))
# define __malloc __attribute__ ((malloc))
# define __unused __attribute__ ((unused))
# define __noreturn __attribute__ ((noreturn))
# define likely(x) __builtin_expect (!!(x), 1)
# define unlikely(x) __builtin_expect (!!(x), 0)
#else
# define inline
# define __pure
# define __const
# define __malloc
# define __unused
# define __noreturn
# define likely(x) (x)
# define unlikely(x) (x)
#endif

static inline void short_memcpy(char *dst, const char *src, unsigned int n)
{
  while (n--) dst[n] = src[n];
}

#endif /* JANE_H */
