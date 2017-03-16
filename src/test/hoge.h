extern int func();
extern int func3();
extern int func33();


#ifdef NNNN
extern int func44();
#endif

#define VVVVVVV

#define STRUCT( X )                             \
    typedef struct {                            \
        int aaa;                                \
    }  struct_1##X;                             \

#define STRUCT2( X )                            \
    typedef struct {                            \
        int aaa;                                \
    }  struct_1##X;                             \
    typedef struct {                            \
        int aaa;                                \
    }  struct_2##X;

STRUCT( AA );
STRUCT2( BB );
