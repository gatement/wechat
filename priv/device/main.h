#ifndef _MAIN_H_  
#define _MAIN_H_  

#define UDP_PORT 12476
#define DEVICE_TYPE "gh_5a92d3949746"
#define DEVICE_ID   "gh_5a92d3949746_b2f233a55779bbe9d326df34f7b16f00"

#define ERR_EXIT(m) \
    do { \
        perror(m); \
        exit(EXIT_FAILURE); \
    } while (0)

void loop(int sock);

#endif /* __MAIN_H */
