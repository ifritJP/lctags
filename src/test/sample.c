typedef enum {
    enum_val0,
    enum_val1,
    enum_val2,
} enum_t;

const char * convert( enum_t val )
{
    switch (val) {
    case enum_val0:
	return "enum_val0";
    case enum_val1:
	return "enum_val1";
    case enum_val2:
	return "enum_val2";
    default:
	return NULL;
    }
}
    
