#ifndef _MAIN_H_  
#define _MAIN_H_  

#define UDP_PORT 12476
#define DEVICE_TYPE "wx7ba5597861fecd32"
#define DEVICE_ID   "BD5D78991122"

#define ERR_EXIT(m) \
    do { \
        perror(m); \
        exit(EXIT_FAILURE); \
    } while (0)

void loop(int sock);

#endif /* __MAIN_H */
