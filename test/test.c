#include <stdio.h>
#include <netinet/in.h>

int main(int argc, int** argv) {
    const char* port = argv[1];
    const char* webroot = argv[2];

    int server_fd = open_listenfd((char*) port);
    if (server_fd < 0) { die("Cannot"); }

    while (1) {
        int client_fd = Accept(server_fd, NULL, NULL);
        if (client_fd < 0) { fatal("Client"); }
        server_chat_with
    }
}