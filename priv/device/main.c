#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>
#include<errno.h>
#include<sys/types.h>
#include<sys/socket.h>
#include<netinet/in.h>
#include<string.h>

#include "main.h"
#include "util.h"
#include "airkiss.h"

unsigned char buf[1024] = {0};
unsigned short n = 0;
int i;

const airkiss_config_t akconf = {
    (airkiss_memset_fn)&memset,
    (airkiss_memcpy_fn)&memcpy,
    (airkiss_memcmp_fn)&memcmp,
    (airkiss_printf_fn)&printf
};

int main(void)
{
    printf("airkiss version: %s\n", airkiss_version()); 
    printf("listening on udp port: %d\n", UDP_PORT); 

    int sock;
    if ((sock = socket(PF_INET, SOCK_DGRAM, 0)) < 0)
    {
        ERR_EXIT("socket error");
    }

    struct sockaddr_in servaddr;
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_port = htons(UDP_PORT);
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);

    if (bind(sock, (struct sockaddr *)&servaddr, sizeof(servaddr)) < 0)
    {
        ERR_EXIT("bind error");
    }

    loop(sock);

    return 0;
}

void loop(int sock)
{
    struct sockaddr_in peeraddr;
    socklen_t peerlen = sizeof(peeraddr);

    while (1)
    {
        i = recvfrom(sock, buf, sizeof(buf), 0, (struct sockaddr *)&peeraddr, &peerlen);

        if (i == -1)
        {
            if (errno == EINTR)
            {
                continue; 
            }
            ERR_EXIT("recvfrom error");
        } 
        else if(i > 0) 
        { 
            print_buf(buf, i);
            printf("\r\n"); 

            airkiss_lan_ret_t recv_ret = airkiss_lan_recv(buf, i, &akconf);
            if (recv_ret != AIRKISS_LAN_SSDP_REQ)
            {
                printf("call airkiss_lan_recv return: %d!\n", recv_ret);
            }
            else
            {
                n = sizeof(buf);
                airkiss_lan_ret_t pack_ret = airkiss_lan_pack(AIRKISS_LAN_SSDP_RESP_CMD, DEVICE_TYPE, DEVICE_ID, 0, 0, buf, &n, &akconf);
                if (pack_ret != AIRKISS_LAN_PAKE_READY)
                {
                    printf("call airkiss_lan_pack return: %d!\n", pack_ret);
                }
                else
                {
                    sendto(sock, buf, n, 0, (struct sockaddr *)&peeraddr, peerlen); 
                    printf("sent SSDP_RESP (%d bytes)\n", n);
                }
            }
        } 
    }

    close(sock);
}
