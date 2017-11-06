#include <string.h>
#include <stdio.h>
int main() {
    const char * pTxt = "hello\n";
    fwrite( pTxt, strlen( pTxt ), 1, stdout );
}
