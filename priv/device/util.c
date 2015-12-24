#include "stdio.h"
#include "util.h"

void print_char(unsigned char val)
{ 
    if(val <= 9)
    { 
        printf("%c", val + 48); 
    } 
    else 
    { 
        printf("%c", val + 55); 
    }
}

void print_buf(const unsigned char *buf, unsigned short buf_size)
{
    unsigned short i;
    unsigned char v; 

    for(i = 0; i < buf_size; i++) 
    { 
        if(i !=0 && i % 16 == 0) 
        { 
            printf("\r\n"); 
        } 

        v = buf[i] >> 4; 
        print_char(v); 

        v = buf[i] & 0x0F; 
        print_char(v); 

        printf(" "); 
    }
}
